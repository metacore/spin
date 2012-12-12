(*
   RPCSunUDP.m3
   Sun RPC on UDP datagrams.
   David Nichols, Xerox PARC
   July, 1991

   Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.
   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)
(* 
 * HISTORY 
 * 21-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Supported concurrent clients.
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed the socket select support.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use new Select interface.
 *
 * 26-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Module heavily hacked by me.  Right now it is fairly specific to
 *	SPIN.  
 *
 *)

MODULE RPCSunUDP EXPORTS RPCSun, RPCSunPriv;

(* ONC RPC *)
IMPORT RPC, RPCOS;
IMPORT RPCAuth;
IMPORT XDR;
IMPORT XDRMem;
IMPORT Random;
IMPORT Thread;
IMPORT IO, Debugger; <*NOWARN*>

(* M3 LIBS *)

TYPE
  UDPClient = Client OBJECT
    mu: MUTEX;
    cond: Thread.Condition;
    bindingInfo : BindingInfo;
    remoteAddr : RPCOS.SockaddrInT;
  OVERRIDES
    StartCall := StartCall;
    SendCall  := SendCall;
    EndCall   := EndCall;
    GetRemote := GetRemote;
    Destroy   := Destroy;
  END;
  
  UDPServer = Server OBJECT
                sp                    : ServerProc;
                prog, vers            : RPCOS.UINT32;
              OVERRIDES
                StartReply := StartReply;
              END;


CONST
  BufSize   = 8800;             (* Magic number from Sun code. *)
  StackSize = 10000;            (* words *)

  Debug = TRUE; <* NOWARN *>
(*
 * Client
 *)

PROCEDURE CreateTransactionRecord (c: UDPClient): Transaction
  RAISES {RPC.Failed}=
  VAR
    tr: Transaction;
    sendBuffer := NEW(REF ARRAY OF CHAR, BufSize);
    recvBuffer := NEW(REF ARRAY OF CHAR, BufSize);
  BEGIN
    tr := NEW(Transaction,
	      inUse := FALSE,
	      socket := GetUDPSocket(),
	      sendBuffer := sendBuffer,
	      recvBuffer := recvBuffer,
	      sink := XDRMem.NewSink(sendBuffer),
	      source := XDRMem.NewSource(recvBuffer));
    RPCOS.ConnectUDPSocket(tr.socket, c.remoteAddr);
    RETURN tr;
  END CreateTransactionRecord;
  
(* Import for UDP.  Assumes port has been looked up. *)
PROCEDURE UDPImportService (bi: BindingInfo): Client
  RAISES {RPC.Failed} =
  VAR c := NEW(UDPClient);
  BEGIN
    SetAddr(c.remoteAddr, bi.hostAddr, bi.port);
    c.mu := NEW(MUTEX);
    c.cond := NEW(Thread.Condition);
    c.bindingInfo := bi;
    c.cred := NEW(RPCAuth.Credentials_Default, flavor := RPCAuth.AUTH_NONE);
    c.verf := NEW(RPCAuth.Credentials_Default, flavor := RPCAuth.AUTH_NONE);
    c.xid := NEW(Random.Default).init().integer(FIRST(RPCOS.UINT32),
						LAST(RPCOS.UINT32));
    c.tr[0] := CreateTransactionRecord(c);
    
    (* MEF HACK *)
    (* c.totalTimeout := 20.0; Clock.TimeVal{20, 0}; *)
    (* c.retryTimeout := 2.0; Clock.TimeVal{2, 0}; *)
    RETURN c;
  END UDPImportService;

PROCEDURE StartCall (c: UDPClient; proc: RPCOS.UINT32): Transaction
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR tr: Transaction := NIL;
  BEGIN
    TRY
      LOCK c.mu DO
	(* Find the first unused transaction record. *)
	LOOP
	  FOR i := 0 TO MaxClientConcurrency-1 DO
	    IF c.tr[i] = NIL THEN
	      c.tr[i] := CreateTransactionRecord(c);
	      tr := c.tr[i];
	      EXIT;
	    ELSIF NOT c.tr[i].inUse THEN
	      tr := c.tr[i];
	      EXIT;
	    END;
	  END;
	  IF tr # NIL THEN EXIT; END;
	  Thread.Wait(c.mu, c.cond);
	END;

	INC(c.xid);
	tr.inUse := TRUE;
	tr.xid := c.xid;
	
	XDRMem.SetSinkPos(tr.sink, 0);
	PutCallHeader(tr.sink, c.xid, c.bindingInfo.progNum,
		      c.bindingInfo.progVersion, proc, c.cred, c.verf);
	RETURN tr;
      END;
    EXCEPT
    | XDR.Failed (e) =>
      RAISE RPC.Failed(NEW(RPC.ZeroTimesFailure,
			   info := "Couldn't marshal call header",
			   subArg := e));
    END;
  END StartCall;

PROCEDURE SendCall (<*UNUSED*>c: UDPClient; tr: Transaction): XDR.Source
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR
    xid, accept, code, authWhy, low, high: RPCOS.UINT32;
    verf                                 : Credentials;
    nBytes                               : CARDINAL;
    timedOut: BOOLEAN := TRUE;
    nready, retrySend, retrySelect : CARDINAL;
    socketReady: RPCOS.SocketReadyT;
  BEGIN
    retrySend := 0;
    REPEAT
      nBytes := XDRMem.GetSinkPos(tr.sink);
      RPCOS.Send(tr.socket, tr.sendBuffer, nBytes);

      retrySelect := 0;
      REPEAT
        (* select for at most 1.1 seconds *)
	socketReady.socket := tr.socket;
	socketReady.ready := RPCOS.SocketReadyStates{RPCOS.SocketReadyState.read};
        nready := RPCOS.Select(socketReady, 0,500000);
        IF nready # 0 AND RPCOS.SocketReadyState.read IN socketReady.ready THEN
          TRY
            nBytes := BufSize;
            IF RPCOS.Recv(tr.socket, tr.recvBuffer, nBytes) THEN
              XDRMem.SetSourcePos(tr.source, 0);
              XDRMem.SetSourceLen(tr.source, nBytes);
              GetReplyHeader(tr.source, xid, accept, code,
			     authWhy, low, high, verf);
              (* If xids match, we finally have a reply. *)
              IF xid = tr.xid THEN timedOut := FALSE; EXIT; END;
            END;
          EXCEPT
	  | XDR.Failed => EXIT;   (* just toss it if we can't decode it *)
          END;
        ELSE
          INC(retrySelect);
        END;
      UNTIL timedOut = FALSE OR retrySelect = 4;
      INC(retrySend);
    UNTIL timedOut = FALSE OR retrySend = 10;

    IF timedOut THEN
      RAISE RPC.Failed(NEW(TimeoutFailure, info := "Call timed out."));
    END;

    (* Should be smarter in processing error conditions. *)
    CASE accept OF
      MSG_ACCEPTED =>
        CASE code OF
	| ACCEPT_SUCCESS => RETURN tr.source;
        | ACCEPT_PROG_UNAVAIL =>
            RAISE RPC.Failed(NEW(RejectFailure,
                                 info := "Rejected: program unavailable"));
        | ACCEPT_PROC_UNAVAIL =>
            RAISE
              RPC.Failed(NEW(RejectFailure,
                             info := "Rejected: procedure unavailable"));
        | ACCEPT_GARBAGE_ARGS =>
            RAISE
              RPC.Failed(NEW(RejectFailure, info := "Rejected: bad args"));
        | ACCEPT_PROG_MISMATCH =>
            RAISE RPC.Failed(
                    NEW(RejectFailure,
                        info := "Rejected: program version unavailable"));
        ELSE
          RAISE RPC.Failed(
                  NEW(RPC.Failure, info := "Received bad accept code"));
        END;
    | MSG_DENIED =>
        CASE code OF
          REJECT_RPC_MISMATCH =>
            RAISE
              RPC.Failed(NEW(RejectFailure,
                             info := "Rejected: RPC version unavailable"));
        | REJECT_AUTH_ERROR =>
            RAISE
              RPC.Failed(NEW(RejectFailure,
                             info := "Rejected: Authentication failure"));
        ELSE
          RAISE RPC.Failed(
                  NEW(RPC.Failure, info := "Received bad reject code"));
        END;
    ELSE
      RAISE
        RPC.Failed(NEW(RPC.Failure, info := "Received bad reply packet"));
    END;
  END SendCall;

PROCEDURE EndCall (c: UDPClient; tr: Transaction) =
  BEGIN
    IF Debug THEN
      VAR found := FALSE;
      BEGIN
	FOR i := 0 TO MaxClientConcurrency-1 DO
	  IF c.tr[i] = tr THEN found := TRUE; END;
	END;
	IF NOT found THEN
	  IO.PutError("RPCSunUDP.EndCall: wrong transaction.\n");
	  Debugger.Enter();
	END;
      END;
    END;
    tr.inUse := FALSE;
    Thread.Signal(c.cond);
  END EndCall;

PROCEDURE GetRemote (c: UDPClient): BindingInfo =
  BEGIN
    RETURN c.bindingInfo;
  END GetRemote;

(* Shut down the connection.  We just close the socket to free up the file
   descrtiptor.  The buffers will float away with the client object. *)
PROCEDURE Destroy (c: UDPClient) RAISES {} =
  VAR tr: Transaction;
  BEGIN
    FOR i := 0 TO MaxClientConcurrency-1 DO
      IF c.tr[i] # NIL THEN
	tr := c.tr[i];
	c.tr[i] := NIL;
	RPCOS.Close(tr.socket);
      END;
    END;
  END Destroy;

(*
 * Server
 *)

PROCEDURE ExportUDP (sp: ServerProc; prog, vers: RPCOS.UINT32; socket: RPCOS.SocketT):
  BindingInfo RAISES {Erred, RPC.Failed, Thread.Alerted} =
  VAR
    host : RPCOS.InaddrT;
    port : RPCOS.UINT16;
    s    : UDPServer;
  BEGIN
    GetHostPortFromSocket(socket, host, port);
    PortMapperRegister(prog, vers, port, Protocol.UDP);
    s := NEW(UDPServer);
    s.sp := sp;
    s.socket := socket;
    s.prog := prog;
    s.vers := vers;
    s.sendBuffer := NEW(REF ARRAY OF CHAR, BufSize);
    s.recvBuffer := NEW(REF ARRAY OF CHAR, BufSize);
    s.sink := XDRMem.NewSink(s.sendBuffer);
    s.source := XDRMem.NewSource(s.recvBuffer);
    s.cred := NIL;
    s.verf := NIL;
    EVAL
      Thread.Fork(NEW(ServerClosure, stackSize := StackSize, server := s));
    RETURN CreateBindingInfo(host, prog, vers, port, Protocol.UDP);
  END ExportUDP;

TYPE
  ServerClosure = Thread.SizedClosure OBJECT
                    server: UDPServer;
                  OVERRIDES
                    apply := ServerLoop;
                  END;

PROCEDURE ServerLoop (cl: ServerClosure): REFANY RAISES {} =
  VAR
    prog, vers, proc: RPCOS.UINT32;
    server                                   := cl.server;
    ok              : BOOLEAN;
    fromAddr        : RPCOS.SockaddrInT;
    (* mef sockSet         : Unix.FDSet;*)
    nBytes          : CARDINAL;
  BEGIN
    TRY                         (* global error handler *)
      LOOP
        TRY                     (* one call *)
          ok := TRUE;
          (******************************************************
          sockSet := Unix.FDSet{cl.server.socket};
          EVAL RTScheduler.IOSelect(
                 Unix.MAX_FDSET, ADR(sockSet), NIL, ADR(sockSet));
          *******************************************************)
          nBytes := BufSize;
          IF NOT RPCOS.RecvFrom(
                   server.socket, server.recvBuffer, nBytes, fromAddr) THEN
            RETURN NIL;
          END;
          XDRMem.SetSourcePos(server.source, 0);
          XDRMem.SetSourceLen(server.source, nBytes);
          TRY
            GetCallHeader(server.source, server.xid, prog, vers, proc,
                          server.cred, server.verf);
          EXCEPT
            HeaderError =>
              SendRejection(server, RejectReason.RPCVersion);
              ok := FALSE;
          END;
          IF ok THEN
            IF prog # cl.server.prog THEN
              SendRejection(server, RejectReason.ProgUnavail);
            ELSIF vers # cl.server.vers THEN
              SendRejection(server, RejectReason.ProgMismatch);
            ELSE
              (* Do auth check here someday. *)
              server.sp.HandleCall(server, proc, server.source);
            END;
          END;
        EXCEPT
          XDR.Failed =>
            SendRejection(server, RejectReason.BadArgs);
            ok := FALSE;
        END;
        (* We've either sent the reply or a rejection, so send the
           packet. *)
        nBytes := XDRMem.GetSinkPos(server.sink);
        RPCOS.SendTo(server.socket, 
                   server.sendBuffer, 
                   nBytes,
                   fromAddr);
      END;
    EXCEPT
      (* If anything goes wrong, shut down the socket and give up. *)
      RPC.Failed, Erred, Thread.Alerted =>
        RPCOS.Close(server.socket);
      RETURN NIL;
    END;
  END ServerLoop;

PROCEDURE StartReply (s: UDPServer): XDR.Sink
  RAISES {RPC.Failed} <*NOWARN*> =
  <* FATAL XDR.Failed, Thread.Alerted *>
  BEGIN
    XDRMem.SetSinkPos(s.sink, 0);
    PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                   code := ACCEPT_SUCCESS, verf := s.verf);
    RETURN s.sink;
  END StartReply;

TYPE
  RejectReason = {RPCVersion, ProgUnavail, ProgMismatch, BadProc, BadArgs};
PROCEDURE SendRejection (s: UDPServer; why: RejectReason)
  RAISES {RPC.Failed} <*NOWARN*> =
  <* FATAL XDR.Failed, Thread.Alerted *>
  BEGIN
    CASE why OF
      RejectReason.RPCVersion =>
        PutReplyHeader(
          s := s.sink, xid := s.xid, accept := MSG_DENIED,
          code := REJECT_RPC_MISMATCH, low := RPCVERS, high := RPCVERS, verf := s.verf);
    | RejectReason.ProgUnavail =>
        PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                       code := ACCEPT_PROG_UNAVAIL, verf := s.verf);
    | RejectReason.ProgMismatch =>
        PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                       code := ACCEPT_PROG_MISMATCH, low := s.vers,
                       high := s.vers, verf := s.verf);
    | RejectReason.BadProc =>
        PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                       code := ACCEPT_PROC_UNAVAIL, verf := s.verf);
    | RejectReason.BadArgs =>
        PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                       code := ACCEPT_GARBAGE_ARGS, verf := s.verf);
    END;
  END SendRejection;

BEGIN
END RPCSunUDP.
