(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 12-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Support checksum.
 * 06-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Support local rpc.
 * 26-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Vmtp;
FROM VmtpPktFormat IMPORT IPEntity, Header, TID;
FROM VmtpPktFormat IMPORT SegmentSize, DeliveryMask;
FROM VmtpPktFormat IMPORT RequestRetries, ResponseRetries;
FROM VmtpUtils IMPORT EntityToString, ErrorMsg;
IMPORT VmtpNameServer;
IMPORT VmtpUtils;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT Debugger;
IMPORT IpPktFormat;
IMPORT IpGen;
IMPORT Ip;
IMPORT Sema;
IMPORT IO, Fmt;
IMPORT Word;
IMPORT Thread;
IMPORT Mutex;
IMPORT Clock;
IMPORT Strand;
IMPORT Spy;
IMPORT Net;
<*NOWARN*>IMPORT ThreadExtra;

CONST
  HeaderSize = BYTESIZE(Header);
  IpHeaderSize = BYTESIZE(IpPktFormat.Header);
  ProtocolID = 81;
  MTU = 1400;
  MIDDLE_SIZE = 512 * 2;
  (* INT..SIZE = Rounddown(MTU, 512); *)
  MIDDLE_STRIDE = 2;
  (* MIDDLE_SIZE/512 *)
  MIDDLE_BITS = 3;
  LAST_BITS = 7;

TYPE
  InputMbufGroup = RECORD
    size: [0 .. 65535];
    finalMask, mask: DeliveryMask;
    (* "finalMask" holds the bitpattern that represent the
       completed mask for the segment "size". Eg, if "size=1024",
       it will be 2_11. If "size=2044", then it will be 2_111. so on. *)
    (* Each bit in "mask" indicates that the packet corresponding to that
       bit is acknowledged by the receiver. *)
    mbufs: ARRAY [0 .. 31] OF Mbuf.T;
    offs: ARRAY [0 .. 31] OF CARDINAL;
    (* offset to start reading/writing mbuf. *)
  END;
  OutputMbufGroup = RECORD
    size: [0 .. 65535];
    finalMask, mask: DeliveryMask;
    mbuf: Mbuf.T;
  END;
  
  LClientState = {AwaitingResponse, ReceivingResponse, Idle};
  RClientState = {AwaitingRequest, ReceivingRequest, Responded,
		  ResponseDiscarded, Processing, Forwarded, Probing};
  CSRType = {LClient, RClient, LServer};
  ChecksumType = {All, HeaderOnly, None};

CONST
  LClientStateNames = ARRAY LClientState OF TEXT
    { "Lwaiting response", "Lreceiving response", "Lidle"};
  RClientStateNames = ARRAY RClientState OF TEXT
    {"Rwaiting request", "Rreceiving request",
     "Rresponded", "Rresp discarded", "Rprocessing", "Rforwarded",
     "RProbing"};
  Checksum = ChecksumType.All;
						  
REVEAL
  T = Public BRANDED OBJECT
    mu: MUTEX;
    type: CSRType;
    link: T; (* hash link. *)
    entity: Entity; (* my entity ID *)
    transMask: Ctypes.unsigned_int;
    valid: BOOLEAN;
    header: Header; (* request MCB if state is AwaitingResponse,
		       reply MCB if  state is ReceivingResponse.
		       void if Idle. *)
    ipHeader: IpPktFormat.Header;
    (* IP header for the current outgoing message*)
    tid: TID;
    inMg: InputMbufGroup; (* set of packets that we've received. *)
    outMg: OutputMbufGroup; (* set of packets we have to send. *)
  END;
  LocalClient = T BRANDED OBJECT
    state: LClientState;
    retval: CARDINAL; (* RPC status code. *)
    sema: MUTEX;
  END;
  RemoteClient = T BRANDED OBJECT
    state: RClientState;
    clientEntity: Entity; (* client entity *)
    releasable: BOOLEAN;
    prio: INTEGER; (* priority. *)
    server: LocalServer; (* the server CSR that's serving the request.
			    NIL when the state is AwaitingRequest. *)
    link: RemoteClient; (* if there are multiple incoming requests for a
			   server entity, reqs are linked using this field. *)
  END;
  LocalServer = T BRANDED OBJECT
    sema: Sema.T; (* count = # of pending reqs. *)
    firstReq, lastReq: RemoteClient;
    requestCallback: PROCEDURE (ls: LocalServer);
    deleteClientCallback: PROCEDURE (ls: LocalServer; rc: RemoteClient);
  END;

VAR
  clientSpy: Spy.T;
  serverSpy: Spy.T;
  
PROCEDURE IsLocked (t: MUTEX): BOOLEAN =
  BEGIN
    RETURN NOT Mutex.TryLock(t);
  END IsLocked;
  
PROCEDURE ComputeDeliveryMaskForSize (size: SegmentSize): DeliveryMask =
  VAR mask: DeliveryMask := 0;
  BEGIN
    LOOP
      mask := Word.Or(Word.LeftShift(mask, 1), 1);
      IF size < 512 THEN EXIT; END;
      DEC(size, 512);
    END;
    RETURN mask;
  END ComputeDeliveryMaskForSize;

(* Append "mbuf" to the receive buffer "g". *)
PROCEDURE AppendMbuf (VAR g: InputMbufGroup; newMask: DeliveryMask;
		      size: SegmentSize; mbuf: Mbuf.T; off: CARDINAL) =
  BEGIN
    IF Debug THEN 
      IF g.size # 0 AND size # g.size THEN
	Debugger.Enter();
      END;
    END;
    IF g.size = 0 THEN
      (* received the first packet in the group. *)
      g.size := size;
      g.finalMask := ComputeDeliveryMaskForSize(size);
    END;
    (* XXX we should have been using ffs(3) here, but I'm not really sure
       if that's a win because most of the bits flock around the right end
       anyway. *)
    FOR i := 0 TO 31 DO
      IF Word.And(newMask, Word.LeftShift(1, i)) # 0 THEN
	IF g.mbufs[i] # NIL THEN
	  (* Duplicate message. *)
	  IF Debug THEN
	    (* Make sure that the two contents match. *)
	    WITH
	      ar1 = Mbuf.Array(g.mbufs[i])^,
	      ar2 = Mbuf.Array(mbuf)^,
	      a1 = SUBARRAY(ar1, g.offs[i], NUMBER(ar1)-g.offs[i]),
	      a2 = SUBARRAY(ar2, off, NUMBER(ar2)-off) DO
	      IF NUMBER(a1) # NUMBER(a2) OR a1 # a2 THEN
		IO.Put("duplicate message, contents differ.\n");
		Debugger.Enter();
	      END;
	      IF Word.Or(g.mask, newMask) # g.mask THEN
		IO.Put("duplicate message, masks differ.\n");
		Debugger.Enter();
	      END;
	    END;
	  END;
	  RETURN;
	END;
	g.mbufs[i] := mbuf;
	g.offs[i] := off;
	g.mask := Word.Or(g.mask, newMask);
	RETURN;
      END;
    END;
    IO.Put("AppendMbuf : mask = 0");
    Debugger.Enter();
  END AppendMbuf;

PROCEDURE IsPacketGroupComplete (READONLY g: InputMbufGroup): BOOLEAN =
  BEGIN
    IF Word.And(g.mask, g.finalMask) = g.finalMask THEN
      (* segment complete. *)
      RETURN TRUE;
    ELSE
      (* not yet. *)
      RETURN FALSE;
    END;
  END IsPacketGroupComplete;

(* Initialize the mbuf group for send. *)  
PROCEDURE ConstructPacketGroup (VAR g: OutputMbufGroup; mbuf: Mbuf.T) =
  VAR
    size := Mbuf.m_length(mbuf);
  BEGIN
    g.mask := 0;
    g.size := size;
    g.finalMask := ComputeDeliveryMaskForSize(g.size);
    IF g.mbuf # NIL THEN
      Mbuf.m_freem(g.mbuf);
    END;
    g.mbuf := mbuf;
  END ConstructPacketGroup;

(* Initialize the mbuf group for receive. *)  
PROCEDURE DiscardOutputMbufGroup (VAR g: OutputMbufGroup) =
  BEGIN
    IF g.mbuf # NIL THEN
      Mbuf.m_freem(g.mbuf);
      g.mbuf := NIL;
    END;
    g.size := 0;
    g.mask := 0;
    g.finalMask := 16_ffffffff;
  END DiscardOutputMbufGroup;
  
PROCEDURE DiscardInputMbufGroup (VAR g: InputMbufGroup) =
  BEGIN
    FOR i := 0 TO LAST(g.mbufs) DO
      IF g.mbufs[i] # NIL THEN
	Mbuf.m_freem(g.mbufs[i]);
	g.mbufs[i] := NIL;
      END;
    END;
    g.size := 0;
    g.mask := 0;
    g.finalMask := 16_ffffffff;
  END DiscardInputMbufGroup;

(* Copy all the mbufs into "mbufs". *)
PROCEDURE CopyMbufsFromMbufGroup(VAR mbufs: Mbufs; VAR g: InputMbufGroup) =
  VAR
    j: CARDINAL := 0;
    size: CARDINAL;
  BEGIN
    FOR i := 0 TO 31 DO
      IF size >= g.size THEN
	IF Debug THEN
	  FOR k := i TO 31 DO 
	    <*ASSERT g.mbufs[k]=NIL*>
	  END;
	END;
	RETURN;
      END;
      IF g.mbufs[i] # NIL THEN
	mbufs.offs[j] := g.offs[i];
	mbufs.mbufs[j] := g.mbufs[i];
	INC(j);
	INC(size, Mbuf.m_length(g.mbufs[i])-g.offs[i]);
	g.mbufs[i] := NIL; (* clear it so that we won't free it twice. *)
      END;
    END;
  END CopyMbufsFromMbufGroup;
  
PROCEDURE Timeout (t: T; timeout: INTEGER; p: Clock.TimeoutFunc) =
  BEGIN
    EVAL Clock.CancelAlarm(p, t);
    Clock.SetAlarm(timeout, p, t);
  END Timeout;
  
PROCEDURE SendRequest (lc: LocalClient; VAR mcb: MCB; mbuf: Mbuf.T): INTEGER =
  BEGIN
    LOCK lc.mu DO
      IF lc.state # LClientState.Idle THEN
	(* We don't allow multiple outstanding messages. *)
	RETURN BUSY;
      END;
      lc.state := LClientState.AwaitingResponse;
      lc.tid := Word.And(lc.tid+1, 16_FFFFFFFF);
      lc.header.tid := lc.tid;
      lc.header.retransCount := 0;
      lc.header.mcb := mcb;
      lc.header.isResponse := FALSE;
      lc.header.apg := FALSE;
      ConstructPacketGroup(lc.outMg, mbuf);
      lc.ipHeader.daddr := VIEW(mcb.server, IPEntity).ipAddr;
      SendPacketGroup(lc);
      DiscardInputMbufGroup(lc.inMg);
    END;
    IF mcb.dgm THEN
      (* Datagram packet requires no reply. *)
    ELSE
      Timeout(lc, lc.TC1, LocalClientTimeout);
    END;
    RETURN OK;
  END SendRequest;

PROCEDURE HandleResponse (lc: LocalClient; VAR p: Header;
			  mbuf: Mbuf.T; off: CARDINAL) =
  BEGIN
    IF p.nrs THEN
      (* No streaming now. *)
      Debugger.Enter();
    END;
    LOCK lc.mu DO 
      IF Debug THEN 
	IO.Put("handleRes: tid=" & Fmt.Int(lc.tid) & "<->" & Fmt.Int(p.tid)
	       & " state=" & LClientStateNames[lc.state] & ".\n");
      END;
      IF lc.tid # p.tid THEN
	IF Debug THEN
	  IO.Put("handleResponse: tid " & Fmt.Int(lc.tid) & "<->"
		 & Fmt.Int(p.tid) & ".\n");
	END;
	NotifyServer(lc, p, BAD_TRANSACTION_ID);
	RETURN;
      END;
      IF lc.state # LClientState.Idle THEN
	IF p.nsr OR p.ner THEN (* not start run/not end run *)
	  NotifyServer(lc, p, ERROR);
	END;
	lc.state := LClientState.ReceivingResponse;
	lc.header.mcb := p.mcb; (* XXX we are copying mcb each time packed is
			   received. actually we have to do it only once when
			   first packet in packetgroup is received. *)
	AppendMbuf(lc.inMg, p.deliveryMask, p.mcb.segSize, mbuf, off);
	(* updateentitycache *)
      ELSE
	IO.Put("handleResp: weird client state=" &
	       LClientStateNames[lc.state] & ".\n");
	IF p.apg OR p.ner THEN 
	  (* Send Response to stop response packets. *)
	  NotifyServer(lc, p, RESPONSE_DISCARDED);
	END;
	RETURN;
      END;
      IF NOT IsPacketGroupComplete(lc.inMg) THEN
	Timeout(lc, lc.TS1, LocalClientTimeout);
      ELSE
	(*IO.Put("waking up the client " & EntityToString(lc.entity) & ".\n");*)
	IF p.apg THEN NotifyServer(lc, p, OK); END;
	lc.state := LClientState.Idle;
	lc.retval := OK;
	Spy.Exit(clientSpy);
	Thread.Release(lc.sema);
	Strand.Yield();
      END;
    END;	
  END HandleResponse;

PROCEDURE Invoke (lc: LocalClient; VAR mcb: MCB;
		  input: Mbuf.T;
		  VAR reply: Mbufs;
		  <*UNUSED*>timeout: INTEGER): INTEGER =
  VAR status: INTEGER;
  BEGIN
    Spy.Enter(clientSpy);
    status := SendRequest(lc, mcb, input);
    IF status # OK THEN
      VmtpUtils.ErrorMsg("Invoke", status);
      RETURN status;
    END;
    Thread.Acquire(lc.sema);
    LOCK lc.mu DO
      CopyMbufsFromMbufGroup(reply, lc.inMg);
    END;
    RETURN lc.retval;
  END Invoke;

PROCEDURE LocalClientTimeout (r: REFANY) =
  VAR t: LocalClient := r;
  BEGIN
    LOCK t.mu DO
      IF Debug THEN 
	IO.Put("client timeout: tid=" & Fmt.Int(t.tid)
	       & " state=" & LClientStateNames[t.state] &".\n");
      END;
      CASE t.state OF
      | LClientState.AwaitingResponse =>
	IF t.header.retransCount > RequestRetries THEN
	  t.retval := RETRANS_TIMEOUT;
	  t.state := LClientState.Idle;
	  Spy.Exit(clientSpy);
	  Thread.Release(t.sema);
	ELSE
	  INC(t.header.retransCount);
	  t.header.apg := TRUE;
	  SendPacketGroup(t);
	  Timeout(t, t.TC2, LocalClientTimeout);
	END;
      | LClientState.ReceivingResponse =>
	IF t.header.mcb.dgm
	  OR t.header.retransCount > RequestRetries THEN
	  IF t.header.mcb.mdm THEN
	    t.header.mcb.msgDeliveryMask := t.transMask;
	    t.retval := OK;
	  ELSE
	    t.retval := BAD_REPLY_SEGMENT;
	  END;
	  t.state := LClientState.Idle;
	  Spy.Exit(clientSpy);
	  Thread.Release(t.sema);
	  RETURN;
	END;
	INC(t.header.retransCount);
	NotifyServer(t, t.header, RETRY);
	Timeout(t, t.TC3, LocalClientTimeout);
      ELSE
      END;
    END;
  END LocalClientTimeout;

PROCEDURE RecvRequest (ls: LocalServer;
		       VAR mcb: MCB;
		       VAR rc: RemoteClient;
		       VAR request: Mbufs;
		       <*UNUSED*>timeout: INTEGER) =
  BEGIN
    Sema.P(ls.sema);
    LOCK ls.mu DO
      rc := ls.firstReq;
      (* Dequeue "rc" from the server msg queue. *)
      ls.firstReq := rc.link;
      IF ls.firstReq = NIL THEN
	(* last message dequeued. *)
	<*ASSERT ls.lastReq = rc*>
	ls.lastReq := NIL;
      END;
      mcb := rc.header.mcb;
      CopyMbufsFromMbufGroup(request, rc.inMg);
    END;
  END RecvRequest;

PROCEDURE SendReply (rc: RemoteClient; VAR mcb: MCB; mbuf: Mbuf.T) =
  BEGIN
    LOCK rc.mu DO
      Spy.Exit(serverSpy);
      ConstructPacketGroup(rc.outMg, mbuf);
      rc.header.retransCount := 0;
      rc.header.mcb := mcb;
      rc.header.isResponse := TRUE;
      rc.header.apg := FALSE;
      rc.ipHeader.daddr := VIEW(rc.entity, IPEntity).ipAddr;
      SendPacketGroup(rc);
      rc.state := RClientState.Responded;
    END;      
    
    IF mcb.dgm THEN
      rc.releasable := TRUE;
      Timeout(rc, rc.TS4, FreeCsr);
    ELSE
      Timeout(rc, rc.TS5, RemoteClientTimeout);
    END;
  END SendReply;

PROCEDURE DeliverSegmentToServer (rc: RemoteClient) =
  VAR ls: LocalServer;
  BEGIN
    TYPECASE FindCSR(rc.header.mcb.server, CSRType.LServer) OF
    | NULL =>
      IO.Put("deliver:" & EntityToString(rc.header.mcb.server)
	     & "not found.\n");
      NotifyClient(rc, rc.header, NONEXISTENT_ENTITY);
      RETURN;
    | LocalServer(x) =>
      ls := x;
    ELSE
      IO.Put("deliver:" &
	     EntityToString(rc.header.mcb.server) & " bad type.\n");
      NotifyClient(rc, rc.header, NONEXISTENT_ENTITY);
      RETURN;
    END;      
    LOCK ls.mu DO
      rc.state := RClientState.Processing;
      rc.server := ls;
      (* Append "rc" to the last of "ls" queue. *)
      IF ls.firstReq = NIL THEN
	(* No message queued now. *)
	ls.firstReq := rc;
	ls.lastReq := rc;
      ELSE
	<*ASSERT ls.lastReq.link = NIL*>
	ls.lastReq.link := rc;
	ls.lastReq := rc;
      END;
      rc.link := NIL;
      (* Wake up the server. *)
      Spy.Enter(serverSpy);
      Sema.V(ls.sema);
      Strand.Yield();
      IF NOT ls.header.mcb.dgm THEN
	(* queue for response. *)
      ELSE
	Timeout(rc, rc.TS4, FreeCsr);
      END;
    END;
  END DeliverSegmentToServer;
  
PROCEDURE HandleRequest (rc: RemoteClient; VAR h: Header;
			 mbuf: Mbuf.T; off: CARDINAL) =
  PROCEDURE DeliverRequest () =
    BEGIN
      IF h.apg THEN
	NotifyClient(rc, h, OK);
      END;
      IF h.mcb.cmd AND rc.state = RClientState.Processing THEN
	(* discard *)
      ELSE
	DeliverSegmentToServer(rc);
      END;
    END DeliverRequest;
  BEGIN
    LOCK rc.mu DO
      IF Debug THEN 
	IO.Put("handleReq: tid=" & Fmt.Int(rc.tid) & "<->" & Fmt.Int(h.tid)
	       & " state=" & RClientStateNames[rc.state] & ".\n");
      END;
      IF rc.tid = h.tid THEN
	(* Re-receipt of the previous message. *)
	CASE rc.state OF
	| RClientState.Probing =>
	  IO.Put("handleReq: still probing "
		 & EntityToString(h.client) & ".\n");
	  RETURN;
	| RClientState.ReceivingRequest, RClientState.AwaitingRequest =>
	  AppendMbuf(rc.inMg, h.deliveryMask, h.mcb.segSize, mbuf, off);
	  IF NOT IsPacketGroupComplete(rc.inMg) THEN
	    Timeout(rc, rc.TS3, RemoteClientTimeout);
	    RETURN;
	  END;
	  DeliverRequest();
	| RClientState.Responded =>
	  (* Duplicate *)
	  VAR timeout: CARDINAL;
	  BEGIN
	    IF Debug THEN
	      IO.Put("handleReq:got dup message tid=" & Fmt.Int(rc.tid)
		     & "state = " & RClientStateNames[rc.state] & ".\n");
	    END;
	    IF NOT rc.header.mcb.dgm THEN
	      (* non-DGM message has to be re-replied in case the reply was
		 lost. *)
	      IF rc.header.mcb.segSize > 0 THEN
		rc.header.apg := TRUE;
	      END;
	      SendPacketGroup(rc);
	      IF rc.header.mcb.segSize > 0 THEN
		timeout := rc.TS5;
	      ELSE
		timeout := rc.TS4;
	      END;
	      Timeout(rc, timeout, RemoteClientTimeout);
	    END;
	  END;
	| RClientState.ResponseDiscarded =>
	  (* Response was sent long time ago, but I haven't received any
	     ack. No way I can handle. Tell client I'm lost... *)
	  IO.Put("handleReq:got dup message tid=" & Fmt.Int(rc.tid)
		 & "state = " & RClientStateNames[rc.state] & ".\n");
	  NotifyClient(rc, h, RESPONSE_DISCARDED);
	ELSE
	  (* Retransmission *) 
	  NotifyClient(rc, h, OK);
	END;
	RETURN;
      ELSE
	(* Transaction ID mismatch *)

	(* See if the tid is really old. *)
	IF h.tid < rc.tid THEN
	  IF LAST(TID)-rc.tid+h.tid >= 10000 THEN 
	    (* Ignore re-receipt of an old transaction. *)
	    IO.Put("got old message " & Fmt.Int(rc.tid)
		   & "<=>" & Fmt.Int(h.tid) & ".\n");
	    Mbuf.m_freem(mbuf);
	    RETURN;
	  END;
	ELSE
	  IF h.tid - rc.tid >= 10000 THEN
	    (* Ignore re-receipt of an old transaction. *)
	    IO.Put("got old message2 " & Fmt.Int(rc.tid)
		   & "<=>" & Fmt.Int(h.tid) & ".\n");
	    Mbuf.m_freem(mbuf);
	    RETURN;
	  END;
	END;

	(* New transaction. *)
	(* XXX Abort the ongoing transaction. *)
	CASE rc.state OF
	| RClientState.Probing =>
	  IO.Put("handleReq: still probing "
		 & EntityToString(h.client) & ".\n");
	  Mbuf.m_freem(mbuf);
	  RETURN;
	| RClientState.ReceivingRequest, RClientState.Forwarded =>
	  IO.Put("handleReq: next message came in???\n");
	  Mbuf.m_freem(mbuf);
	  RETURN;
	ELSE
	  rc.state := RClientState.ReceivingRequest;
	END;
	
	IF h.nsr OR h.ner THEN
	  NotifyClient(rc, h, STREAMING_NOT_SUPPORTED);
	  Mbuf.m_freem(mbuf);
	  RETURN;
	END;

	IF h.tid # Word.And(rc.tid+1, 16_FFFFFFFF) THEN
	  (* the transaction ID must be consecutive.
	     Otherwise, it is either an out-of-order streaming
	     message(which we don't support), or a goof on the client side. *)
	  Debugger.Enter();
	END;
	
	rc.tid := h.tid;
	rc.prio := h.prio;
	rc.transMask := 0;
	rc.header := h;
	DiscardInputMbufGroup(rc.inMg);
	AppendMbuf(rc.inMg, h.deliveryMask, h.mcb.segSize, mbuf, off);
      END;
      IF NOT IsPacketGroupComplete(rc.inMg) THEN
	Timeout(rc, rc.TS3, RemoteClientTimeout);
	RETURN;
      END;
      DeliverRequest();
    END;
  END HandleRequest;

    
PROCEDURE RemoteClientTimeout (r: REFANY) =
  VAR rc: RemoteClient := r;
  BEGIN
    LOCK rc.mu DO
      IF Debug THEN
	IO.Put("server timeout: tid=" & Fmt.Int(rc.tid)
	       & " state=" & RClientStateNames[rc.state] &".\n");
      END;
      CASE rc.state OF
      | RClientState.Probing =>
	IF rc.header.retransCount > RequestRetries THEN 
	  Timeout(rc, rc.TS4, FreeCsr);
	ELSE
	  INC(rc.header.retransCount);
	  ProbeEntity(rc);
	  Timeout(rc, rc.TS3, RemoteClientTimeout);
	END;
      | RClientState.Responded =>
	IF rc.header.retransCount > RequestRetries THEN
	  DiscardOutputMbufGroup(rc.outMg);
	  rc.state := RClientState.ResponseDiscarded;
	  Timeout(rc, rc.TS4, RemoteClientTimeout);
	  RETURN;
	ELSE
	  INC(rc.header.retransCount);
	  rc.header.apg := TRUE;
	  SendPacketGroup(rc);
	  Timeout(rc, rc.TS3, RemoteClientTimeout);
	  RETURN;
	END;
      | RClientState.ResponseDiscarded =>
	rc.valid := FALSE; (* mark it for reuse. *)
	RETURN;
      | RClientState.ReceivingRequest =>
	IF rc.header.retransCount > ResponseRetries 
	  OR rc.header.mcb.dgm
	  (* XXX OR rc.nrt *) THEN
	  rc.header.mcb.msgDeliveryMask := rc.transMask;
	  IF rc.header.mcb.mdm THEN
	    DeliverSegmentToServer(rc);
	  ELSE
	    rc.state := RClientState.AwaitingRequest;
	    IO.Put("??? request timed out.\n");
	    (* Note: don't have to remember that req is discarded. *)
	    (* XXX this is not right!!!! *)
	  END;
	  RETURN;
	END;
	INC(rc.header.retransCount);
	NotifyClient(rc, rc.header, RETRY);
	Timeout(rc, rc.TS3, RemoteClientTimeout);
      ELSE
	IO.Put("remote client timeout: state = " &
	       RClientStateNames[rc.state] & ".\n");
      END;
    END;
  END RemoteClientTimeout;

PROCEDURE HandleNotifyServer (rc: RemoteClient; VAR h: Header) =
  BEGIN
    IF Debug THEN
      IO.Put("rec notify server tid=" & Fmt.Int(rc.tid));
      IO.Put(" state=" & RClientStateNames[rc.state]);
      ErrorMsg(" msg=", h.mcb.userData[0]);
      IO.Put(".\n");
    END;
    LOCK rc.mu DO 
      IF rc.state # RClientState.Responded THEN
	RETURN;
      END;
      IF rc.tid # h.tid THEN
	IF Debug THEN
	  IO.Put("rec notify server: got old msg "
		 & Fmt.Int(rc.tid) & "<->" & Fmt.Int(h.tid) & ".\n");
	END;
	RETURN;
      END;
      IF rc.header.mcb.dgm THEN
	(*  if transmission of Response in progress then
	   Abort transmission
	   if code is migrated then
	   restart transmission with new host addr.
	   if Retry then Report protocol error *)
	RETURN;
      END;
      
      CASE h.mcb.userData[0] OF 
      | RETRY =>
	IF rc.header.retransCount > ResponseRetries THEN 
	  DiscardOutputMbufGroup(rc.outMg);
	  rc.state := RClientState.ResponseDiscarded;
	  RETURN;
	END;
	INC(rc.header.retransCount);
	rc.outMg.mask := h.mcb.msgDeliveryMask;
	IF Debug THEN
	  IO.Put("retry " & Fmt.Int(h.mcb.msgDeliveryMask, 2) & ".\n");
	END;
	SendPacketGroup(rc);
	Timeout(rc, rc.TS3, RemoteClientTimeout);
	RETURN;
      | BUSY =>
	IO.Put("client busy????.\n");
	RETURN;
      ELSE
	DiscardOutputMbufGroup(rc.outMg);
	rc.state := RClientState.ResponseDiscarded;
	(* if NERset(csr) and subsequent csr then
	   Deallocate csr and use later csr for
	   future duplicate suppression.
	   I don'TEXT see this... *)
	RETURN;
      END;
    END;
  END HandleNotifyServer;
  
PROCEDURE HandleProbeEntity (lc: LocalClient; VAR h: Header) =
  VAR mbuf: Mbuf.T;
  BEGIN
    IO.Put("rec probe entity: " & EntityToString(lc.entity) &".\n");
    mbuf := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    <*ASSERT mbuf # NIL*>
    mbuf.mh_hdr.mh_len := HeaderSize;
    WITH header = VIEW(Mbuf.Array(mbuf)^, Header) DO
      header := lc.header;
      header.mcb.server := h.mcb.server;
      header.mcb.req := RepProbeEntity;
      header.mcb.pic := TRUE;
      header.mcb.userData[0] := lc.tid;
      header.tid := lc.tid;
      (* XXX other status info is not returned. *)
      lc.ipHeader.daddr := VIEW(header.mcb.server, IPEntity).ipAddr;
      lc.ipHeader.tot_len := HeaderSize + IpHeaderSize;
      IpGen.PacketSend(lc.ipHeader, mbuf);
    END;
  END HandleProbeEntity;

(* Handle reply packet for ProbeEntity request. *)
PROCEDURE HandleProbeEntityReply (rc: RemoteClient; VAR h: Header) =
  BEGIN
    IO.Put("server:resolving " & EntityToString(rc.entity)
	   & "tid=" & Fmt.Int(h.mcb.userData[0]) & " state="
	   & RClientStateNames[rc.state] & ".\n");
    LOCK rc.mu DO 
      rc.tid := h.mcb.userData[0];
      <*ASSERT rc.state = RClientState.Probing*>
      rc.state := RClientState.ReceivingRequest;
      NotifyClient(rc, h, RETRY_ALL);
      Timeout(rc, rc.TS3, RemoteClientTimeout);
    END;
  END HandleProbeEntityReply;
  
PROCEDURE HandleNotifyClient (lc: LocalClient; VAR h: Header) =
  BEGIN
    IF Debug THEN 
      IO.Put("rec notify client tid=" & Fmt.Int(lc.tid));
      IO.Put(" state=" & LClientStateNames[lc.state]);
      ErrorMsg(" msg=", h.mcb.userData[0]);
      IO.Put(".\n");
    END;
    LOCK lc.mu DO
      IF lc.state # LClientState.AwaitingResponse THEN
	IF Debug THEN
	  IO.Put("rec notify client: bogus state.\n");
	END;
	RETURN;
      END;
      IF lc.tid # h.tid THEN
	IF Debug THEN
	  IO.Put("rec notify client: got old msg "
		 & Fmt.Int(lc.tid) & "<->" & Fmt.Int(h.tid) & ".\n");
	END;
	RETURN;
      END;
      CASE h.mcb.userData[0] OF 
      | OK =>
	(* Notify ack'ed segment blocks from delivery *)
	lc.header.retransCount := 0;
	Timeout(lc, lc.TC1, LocalClientTimeout);
	RETURN;
      | RETRY =>
	(* csr.TransmissionMask to missing segment blocks,
	   as specified by delivery *)
	lc.outMg.mask := h.mcb.msgDeliveryMask;
	IF Debug THEN
	  IO.Put("retry " & Fmt.Int(h.mcb.msgDeliveryMask, 2) & ".\n");
	END;
	SendPacketGroup(lc);
	Timeout(lc, lc.TC1, LocalClientTimeout);
	RETURN;
      | RETRY_ALL =>
	(* Set csr.TransmissionMask to retransmit all blocks. *)
	lc.outMg.mask := 0;
	SendPacketGroup(lc);
	Timeout(lc, lc.TC1, LocalClientTimeout);
	RETURN;
      ELSE
	lc.retval := h.mcb.userData[0];
	lc.state := LClientState.Idle;
	Spy.Exit(clientSpy);
	Thread.Release(lc.sema);
	RETURN;
      END;
    END;
  END HandleNotifyClient;
  
PROCEDURE FreeCsr (r: REFANY) =
  VAR rc: RemoteClient := r;
  BEGIN
    LOCK rc.mu DO
      rc.valid := FALSE;
    END;
  END FreeCsr;

(* Send the next packet(s) that is in the queue. *)
PROCEDURE SendPacketGroup (t: T) =
  VAR
    mbuf: Mbuf.T;
    sendBits: Word.T;
    curMask: Word.T;
    curLen: CARDINAL;
    off: CARDINAL := 0;
  BEGIN
    IF NOT IsLocked(t.mu) THEN
      Debugger.Enter();
    END;

    IF t.outMg.mbuf = NIL THEN
      IO.Put("sendpack: mbuf nil???");
      RETURN;
    END;

    sendBits := Word.And(t.outMg.finalMask, Word.Not(t.outMg.mask));
    
    FOR i := 0 TO 31 BY MIDDLE_STRIDE DO
      mbuf := NIL;
      IF t.outMg.size - off <= MTU THEN
	(* last packet is special because we can send
	   the last bytes in 1024..MTU bytes range. *)
	curMask := Word.LeftShift(LAST_BITS, i);
	IF Word.And(sendBits, curMask) # 0 THEN 
	  mbuf := Mbuf.m_copym(t.outMg.mbuf, off,
			       Mbuf.M_COPYALL, Mbuf.M_WAIT);
	END;
	off := t.outMg.size;
      ELSE
	(* all the packets except the last one are 1024 bytes long. *)
	curMask := Word.LeftShift(MIDDLE_BITS, i);
	IF Word.And(sendBits, curMask) # 0 THEN
	  mbuf := Mbuf.m_copym(t.outMg.mbuf, off, MIDDLE_SIZE, Mbuf.M_WAIT);
	END;
	INC(off, MIDDLE_SIZE);
      END;
      IF mbuf # NIL THEN
	curLen := Mbuf.m_length(mbuf);
	mbuf := Mbuf.M_PREPEND(mbuf, HeaderSize, Mbuf.M_WAIT);
	t.ipHeader.tot_len := curLen + HeaderSize + IpHeaderSize;
	WITH arr = Mbuf.Array(mbuf)^,
	     header = VIEW(arr, Header) DO
	  header := t.header;
	  header.deliveryMask := Word.And(curMask, 16_FFFFFFFF);
	  IF Debug THEN
	    IO.Put("sendpk: tid=" & Fmt.Int(header.tid) & " mask="
		   & Fmt.Int(header.deliveryMask, 2) & ".\n");
	  END;

	  
	  (* Compute the checksum... *)
	  header.checksum := 0;
	  IF Checksum = ChecksumType.None THEN
	  ELSIF Checksum = ChecksumType.HeaderOnly THEN
	    header.hco := TRUE;
	    header.checksum := Net.checksum(arr, NUMBER(arr));
	  ELSE
	    header.checksum := Net.checksum(arr, NUMBER(arr));
	    WITH arr2 = Mbuf.Array(mbuf.mh_hdr.mh_next)^ DO
	      header.checksum := Net.checksum(arr2, NUMBER(arr2),
					      header.checksum);
	    END;
	  END;
	END;
 	IpGen.PacketSend(t.ipHeader, mbuf);
      END;
      IF off >= t.outMg.size THEN EXIT; END;
    END;
  END SendPacketGroup;

(* Send a message consisting only of "h" to the host "addr".
   There is no guarantee of delivery. *)
PROCEDURE SendControlMessage (VAR h: Header;
			      VAR ipHeader: IpPktFormat.Header) =
  VAR
    mbuf: Mbuf.T;
  BEGIN
    mbuf := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_DATA);
    mbuf.mh_hdr.mh_len := HeaderSize;
  
    WITH out = VIEW(Mbuf.Array(mbuf)^, Header) DO
      out := h;
      out.mcb.pic := TRUE;
      ipHeader.tot_len := HeaderSize + IpHeaderSize;
    END;
    IpGen.PacketSend(ipHeader, mbuf);
  END SendControlMessage;
  
PROCEDURE ProbeEntity (rc: RemoteClient) =
  VAR
    h: Header;
    ipHeader: IpPktFormat.Header;
  BEGIN
    h := rc.header;
    h.mcb.req := ReqProbeEntity;
    h.mcb.pic := TRUE;
    ipHeader := rc.ipHeader;
    ipHeader.daddr := VIEW(rc.entity, IPEntity).ipAddr;
    SendControlMessage(h, ipHeader);
    Timeout(rc, rc.TS1, RemoteClientTimeout);
  END ProbeEntity;
PROCEDURE NotifyServer (ls: LocalClient; READONLY h: Header; code: CARDINAL) =
  VAR
    header: Header;
    ipHeader: IpPktFormat.Header;
  BEGIN
    <*ASSERT IsLocked(ls.mu)*>
    IF Debug THEN
      ErrorMsg("notify server", code);
    END;
    header := ls.header;
    header.mcb.server := h.mcb.server;
    header.mcb.msgDeliveryMask := ls.inMg.mask;
    header.mcb.req := ReqNotifyVmtpServer;
    header.mcb.userData[0] := code;
    header.tid := h.tid;
    ipHeader := ls.ipHeader;
    ipHeader.daddr := VIEW(header.mcb.server, IPEntity).ipAddr;
    SendControlMessage(header, ipHeader);
  END NotifyServer;

PROCEDURE NotifyClient (rc: RemoteClient; VAR h: Header; code: CARDINAL) =
  VAR
    header: Header;
    ipHeader: IpPktFormat.Header;
  BEGIN
    <*ASSERT IsLocked(rc.mu)*>
    IF Debug THEN
      ErrorMsg("notify client", code);
    END;
    header := rc.header;
    header.mcb.server := h.mcb.server;
    header.mcb.msgDeliveryMask := rc.inMg.mask;
    header.mcb.req := ReqNotifyVmtpClient;
    header.mcb.userData[0] := code;
    header.client := h.client;
    header.tid := h.tid;
    ipHeader := rc.ipHeader;
    ipHeader.daddr := VIEW(rc.entity, IPEntity).ipAddr;
    ipHeader.tot_len := HeaderSize + IpHeaderSize;            
    SendControlMessage(header, ipHeader);
  END NotifyClient;


  
PROCEDURE AcceptNewClient (curr: Mbuf.T; off: CARDINAL) =
  VAR
    rc: RemoteClient;
  BEGIN
    WITH currBuf = Mbuf.Array(curr)^,
         h = VIEW(SUBARRAY(currBuf, off, HeaderSize), Header) DO
      IO.Put("Creating rem client " & EntityToString(h.client) & ".\n");
      rc := NEW(RemoteClient,
		type := CSRType.RClient,
		entity := h.client,
		state := RClientState.AwaitingRequest);
      LOCK csrMu DO
	InitializeCSR(rc);
      END;
      rc.header := h;
      rc.tid := Word.And(h.tid-1, 16_FFFFFFFF);
      IF FALSE THEN
	(* This is the default action specified in rfc. *)
	(* throw away the message. *)
	Mbuf.m_freem(curr);
	ProbeEntity(rc);
      ELSE
	HandleRequest(rc, h, curr, off + HeaderSize);
      END;
    END;
  END AcceptNewClient;
  
FUNCTIONAL
PROCEDURE Guard (<*UNUSED*> packet: Mbuf.T; curr: Mbuf.T; 
		 offset: CARDINAL): BOOLEAN =
  BEGIN
    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^, offset, IpHeaderSize),
         ipHeader = VIEW(ipHeaderBuf,IpPktFormat.Header) DO
      RETURN ipHeader.protocol = ProtocolID AND (* VMTP packet ? *)
        ipHeader.hlen = 5 AND (* No IP options ? *)
        ipHeader.vers = 4; (* IP version 4  ? *)
    END;
  END Guard;
  
PROCEDURE PacketArrived (<*UNUSED*>packet: Mbuf.T;
			 curr: Mbuf.T; off: CARDINAL):BOOLEAN =
  VAR
    t: T;
    csum, origCsum: INTEGER;
  BEGIN
    WITH currBuf = Mbuf.Array(curr)^ DO
      INC(off, IpHeaderSize);
      IF off >= BYTESIZE(currBuf) THEN
        curr := curr.mh_hdr.mh_next;
        <* ASSERT(curr # NIL) *>
        off := 0;
      END;
      IF NUMBER(currBuf)-off < HeaderSize THEN
	IO.Put("vmtp receive: short packet.\n");
	RETURN FALSE; 
      END;

      (* Note that the return value from this procedure is ignored, and
	 mbufs are always freed. We have to create a clone here to keep it. *)
      curr := Mbuf.m_copym(curr, 0, Mbuf.M_COPYALL, Mbuf.M_WAIT);
      
      WITH arr = SUBARRAY(currBuf, off, HeaderSize),
	   h = VIEW(arr, Header) DO
	IF h.checksum # 0 THEN
	  origCsum := h.checksum;
	  h.checksum := 0; (* this has to be 0 to compute the correct csum. *)
	  IF h.hco THEN
	    (* Header checksum only. *)
	    csum := Net.checksum(arr);
	  ELSE
	    csum := Net.checksum(arr);
	    csum := Net.checksum(SUBARRAY(currBuf, off+HeaderSize,
					  NUMBER(currBuf)-off-HeaderSize),
				 0, csum);
	  END;
	  IF origCsum # csum THEN 
	    IO.Put("checksum mismatch " & Fmt.Int(origCsum, 16)
		   & "<->" & Fmt.Int(csum, 16) & ".\n");
	    RETURN TRUE;
	  END;
	END;
	IF h.mcb.pic = TRUE THEN
	  (* Internal procedure request. *)
	  IF h.mcb.req = ReqEntityDeleted THEN
	    t := FindCSR(h.client, CSRType.LClient);
	    IO.Put("client " & EntityToString(h.client) & " is deleted.\n");
	    WITH ls = NARROW(FindCSR(h.mcb.server, CSRType.LServer),
			     LocalServer) DO
	      IF ls.deleteClientCallback # NIL THEN 
		ls.deleteClientCallback(ls, t);
	      END;
	    END;
	    Timeout(t, t.TS4, FreeCsr);
	  ELSIF h.mcb.req = ReqProbeEntity THEN
	    t := FindCSR(h.client, CSRType.LClient);
	    IO.Put("req probe\n");
	    IF t # NIL THEN HandleProbeEntity(t, h); END;
	    Mbuf.m_freem(curr);
	  ELSIF h.mcb.req = RepProbeEntity THEN
	    t := FindCSR(h.client, CSRType.RClient);
	    IF t # NIL THEN HandleProbeEntityReply(t, h); END;
	    Mbuf.m_freem(curr);
	  ELSIF h.mcb.req = ReqNotifyVmtpClient THEN
	    t := FindCSR(h.client, CSRType.LClient);
	    IF t # NIL THEN HandleNotifyClient(t, h); END;
	    Mbuf.m_freem(curr);
	  ELSIF h.mcb.req = ReqNotifyVmtpServer THEN
	    t := FindCSR(h.client, CSRType.RClient);
	    IF t # NIL THEN HandleNotifyServer(t, h); END;
	    Mbuf.m_freem(curr);
	  ELSE
	    IO.Put("???? unknown req: " & Fmt.Int(h.mcb.req));
	  END;
	ELSIF h.isResponse THEN
	  t := FindCSR(h.client, CSRType.LClient);
	  IF t # NIL THEN
	    HandleResponse(t, h, curr, off + HeaderSize);
	  ELSE
	    IO.Put("xxxx");
	  END;
	ELSE
	  t := FindCSR(h.client, CSRType.RClient);
	  IF t # NIL THEN
	    HandleRequest(t, h, curr, off + HeaderSize);
	  ELSE
	    AcceptNewClient(curr, off);
	  END;
	END;
	RETURN TRUE;
      END;
    END;
  END PacketArrived;

(*
   Server/client registration.
*)

PROCEDURE CreateClient (): LocalClient =
  VAR
    ipe: IPEntity;
    idx: CARDINAL;
    t: LocalClient;
  BEGIN
    ipe.disc := Word.And(VmtpUtils.Random(), 16_ffffffff);
    ipe.ipAddr := VmtpUtils.MyIpAddr();
    IO.Put("Creating client " & EntityToString(VIEW(ipe, Entity)) & ".\n");
    idx := EntityHash(VIEW(ipe, Entity));
    LOCK csrMu DO
      t := NEW(LocalClient,
	       type := CSRType.LClient,
	       entity := VIEW(ipe, Entity),
	       sema := NEW(MUTEX),
	       state := LClientState.Idle);
      <*ASSERT FindCSRInternal(t.entity, CSRType.LClient)= NIL*>
      Thread.Acquire(t.sema);
      InitializeCSR(t);
      t.header.client := t.entity;
    END;
    RETURN t;
  END CreateClient;
  
PROCEDURE CreateServer (name: TEXT): LocalServer =
  VAR
    e: Entity;
    t: LocalServer;
  BEGIN
    IF NOT VmtpNameServer.Lookup(name & "@default", e) THEN
      IO.Put("server " & name & " not found in name server.\n");
      RETURN NIL;
    END;
    IO.Put("Creating server " & EntityToString(e) & ".\n");
    LOCK csrMu DO
      <*ASSERT FindCSRInternal(e, CSRType.LServer)= NIL*>
      t := NEW(LocalServer, type := CSRType.LServer,
	       entity := e, sema := Sema.Alloc());
      InitializeCSR(t);
      RETURN t;
    END;
  END CreateServer;

VAR
  csrMu := NEW(MUTEX);
  csrTable : ARRAY [0 .. 255] OF T;
  (* chained hash table of active entity control block. *)

PROCEDURE EntityHash (READONLY e: Entity): CARDINAL =
  BEGIN
    RETURN Word.Mod(Word.Plus(e.low, e.high), NUMBER(csrTable));
  END EntityHash;
  
PROCEDURE InitializeCSR (t: T) =
  VAR idx := EntityHash(t.entity);
    hostid: INTEGER;
  BEGIN
    <*ASSERT FindCSRInternal(t.entity, t.type) = NIL*>
    t.mu := NEW(MUTEX);
    t.link := csrTable[idx];
    csrTable[idx] := t;
    t.transMask := 0;
    t.header.version := 0; (* vmtp version 0 *)
    t.header.domain := 1; (* IP domain. See rfc IV.1 *)
    t.header.hco := FALSE;
    t.ipHeader.hlen := 5; (* no options. see the Comer book p93. *)
    t.ipHeader.vers := 4; (* IP v4 *)
    t.ipHeader.tos := 0;  (* no specific service type. *)
    t.ipHeader.tot_len := 0;
    t.ipHeader.protocol := ProtocolID;
    t.ipHeader.ttl := 31; (* some random good number > 0 *)
    hostid := VmtpUtils.MyIpAddr();
    WITH addr = VIEW(hostid, ARRAY OF CHAR) DO
      t.ipHeader.saddr := VIEW(addr, IpPktFormat.Address);
    END;
  END InitializeCSR;


(* Find a client state record(T) for the entity "e". For remote client
   state records, the CSR table is just a cache, so the entry may be
   purged out(in theory. in realty, csrs are always there).
   If entity is the local guy, it's always there unless
   someone called DeleteCSR. *)
PROCEDURE FindCSR (READONLY e: Entity; type: CSRType): T =
  VAR
    idx := Word.Mod(Word.Plus(e.high, e.low), NUMBER(csrTable));
    t: T;
  BEGIN
    LOCK csrMu DO
      t := csrTable[idx];
      WHILE t # NIL DO
	IF t.entity = e AND t.type = type THEN RETURN t; END;
	t := t.link;
      END;
      RETURN NIL;
    END;
  END FindCSR;

PROCEDURE FindCSRInternal (READONLY e: Entity; type: CSRType): T =
  VAR
    idx := EntityHash(e);
    t: T;
  BEGIN
    t := csrTable[idx];
    WHILE t # NIL DO
      IF t.entity = e AND t.type = type THEN RETURN t; END;
      t := t.link;
    END;
    RETURN NIL;
  END FindCSRInternal;

PROCEDURE DeleteCSR (t: T) =
  VAR
    hash := EntityHash(t.entity);
    tmp, lastT: T;
  BEGIN
    tmp := csrTable[hash];
    lastT := NIL;
    WHILE tmp # NIL DO
      IF tmp = t THEN
	(* Remove "t" from the hash table. *)
	IF lastT = NIL THEN
	  csrTable[hash] := t.link;
	ELSE
	  lastT.link := t.link;
	END;

	(* If "t" is handle to a remote server, notify the server
	   that i'm going to die. This is just a hint to the server,
	   so we really don't care if this message gets lost. *)
	TYPECASE t OF
	| LocalClient(lc) =>
	  VAR
	    header := lc.header;
	    ipHeader := lc.ipHeader;
	  BEGIN
	    header.mcb.req := ReqEntityDeleted;
	    ipHeader.daddr := VIEW(lc.entity, IPEntity).ipAddr;
	    SendControlMessage(header, ipHeader);
	  END;
	ELSE
	END;
	t.valid := FALSE;
	RETURN;
      END;
      tmp := tmp.link;
    END;
    IO.Put("deletecsr: csr not found.\n");
    Debugger.Enter();
  END DeleteCSR;

PROCEDURE GetEntityID (t: T): Entity =
  BEGIN
    RETURN t.entity;
  END GetEntityID;
  
BEGIN
  EVAL Ip.Install(Ip.PacketArrived, Guard, PacketArrived);
  clientSpy := Spy.Create("vmtp-client", FALSE, 0);
  serverSpy := Spy.Create("vmtp-server", FALSE, 0);
  Debug := TRUE;
END Vmtp.
