(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Renamed from Proxy.m3
 * 01-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)
UNSAFE MODULE UNIXRPC EXPORTS TransRPC, UNIXRPC;
IMPORT SID;
IMPORT TID;

IMPORT HostID, UNIXHostID, IP, IntRefTbl;
IMPORT TCP, ConnFD;
IMPORT Text, IO, Fmt;
IMPORT Ctypes;
IMPORT Round;

FROM TransUtils IMPORT Debug, Msg;

(*
   Incoming packet unmarshalling.
*)
REVEAL UNIXRecvBuf = TPublic BRANDED OBJECT
OVERRIDES
  currentIdx := CurrentIdx;
  reset := ResetRecv;
  stretchIfNecessary := StretchIfNecessaryRecv;
  unpackInt := UnpackInt;
  unpackInt2 := UnpackInt2;
  unpackInt3 := UnpackInt3;
  unpackInt4 := UnpackInt4;
  unpackHeader := UnpackHeader;
  unpackHeader2 := UnpackHeader2;
  unpackHeader3 := UnpackHeader3;
  unpackHeader4 := UnpackHeader4;
  
  unpackBool := UnpackBool;
  unpackText := UnpackText;
  unpackArray := UnpackArray;
  endUnpack := EndUnpack;
END;

CONST SizeofInt = BYTESIZE(INTEGER);

PROCEDURE CurrentIdx (t: UNIXRecvBuf): CARDINAL =
  BEGIN
    RETURN t.idx;
  END CurrentIdx;
  
PROCEDURE ResetRecv (t: UNIXRecvBuf; size: INTEGER) =
  BEGIN
    <*ASSERT t.buf # NIL *>
    <*ASSERT size <= NUMBER(t.buf^)*>
    t.maxSize := size;
    t.idx := 0;
  END ResetRecv;

(*
 Unpack routines
 *)
  
PROCEDURE UnpackInt (t: UNIXRecvBuf): INTEGER =
  VAR val: INTEGER;
  BEGIN
    val := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    INC(t.idx, SizeofInt);
    <*ASSERT t.idx <= t.maxSize*>
    RETURN val;
  END UnpackInt;
  
PROCEDURE UnpackInt2 (t: UNIXRecvBuf; VAR v1, v2: INTEGER) =
  BEGIN
    v1 := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    v2 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER);
    INC(t.idx, SizeofInt*2);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackInt2;
PROCEDURE UnpackInt3 (t: UNIXRecvBuf; VAR v1, v2, v3: INTEGER) =
  BEGIN
    v1 := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    v2 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER);
    v3 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER);
    INC(t.idx, SizeofInt*3);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackInt3;
PROCEDURE UnpackInt4 (t: UNIXRecvBuf; VAR v1, v2, v3, v4: INTEGER) =
  BEGIN
    v1 := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    v2 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt), INTEGER);
    v3 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt), INTEGER);
    v4 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt), INTEGER);
    INC(t.idx, SizeofInt*4);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackInt4;

PROCEDURE UnpackHeader (t: UNIXRecvBuf; VAR sid: SID.T) =
  BEGIN
    sid.hid := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    sid.lid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt),
		    INTEGER);
    INC(t.idx, SizeofInt*2);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackHeader;
PROCEDURE UnpackHeader2 (t: UNIXRecvBuf; VAR sid: SID.T; VAR tid: TID.T) =
  BEGIN
    sid.hid := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    sid.lid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt),
		    INTEGER);
    tid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt),
		  INTEGER);
    INC(t.idx, SizeofInt*3);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackHeader2;
  
PROCEDURE UnpackHeader3 (t: UNIXRecvBuf; VAR sid: SID.T; VAR tid: TID.T;
			 VAR v: INTEGER) =
  BEGIN
    sid.hid := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    sid.lid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt),
		    INTEGER);
    tid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt),
		  INTEGER);
    v := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt),
		  INTEGER);
    INC(t.idx, SizeofInt*4);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackHeader3;
  
PROCEDURE UnpackHeader4 (t: UNIXRecvBuf; VAR sid: SID.T; VAR tid: TID.T;
			 VAR v1, v2: INTEGER) =
  BEGIN
    sid.hid := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    sid.lid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt, SizeofInt),
		    INTEGER);
    tid := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*2, SizeofInt),
		  INTEGER);
    v1 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*3, SizeofInt),
		  INTEGER);
    v2 := VIEW(SUBARRAY(t.buf^, t.idx+SizeofInt*4, SizeofInt),
	       INTEGER);
    INC(t.idx, SizeofInt*5);
    <*ASSERT t.idx <= t.maxSize*>
  END UnpackHeader4;
  

PROCEDURE UnpackBool (t: UNIXRecvBuf): BOOLEAN =
  VAR val: INTEGER;
  BEGIN
    val := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    INC(t.idx, SizeofInt);
    <*ASSERT t.idx <= t.maxSize*>
    <*ASSERT val = -999 OR val = 999*>
    RETURN val = 999;
  END UnpackBool;

PROCEDURE UnpackText (t: UNIXRecvBuf): TEXT =
  VAR 
    len: INTEGER;
    r: TEXT;
  BEGIN
    len := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    <*ASSERT len < 128*>
    r := Text.FromChars(SUBARRAY(t.buf^, t.idx+SizeofInt, len));
    INC(t.idx, Round.Up8(len) + SizeofInt);
    <*ASSERT t.idx <= t.maxSize*>
    RETURN r;
  END UnpackText;

PROCEDURE UnpackArray (t: UNIXRecvBuf; VAR x: ARRAY OF CHAR): CARDINAL =
  VAR 
    len: CARDINAL;
  BEGIN
    <*ASSERT t.idx <= t.maxSize*>
    len := VIEW(SUBARRAY(t.buf^, t.idx, SizeofInt), INTEGER);
    <*ASSERT len <= NUMBER(x)*>
    SUBARRAY(x, 0, len) := SUBARRAY(t.buf^, t.idx+8, len);
    INC(t.idx, Round.Up8(len)+8);
    RETURN len;
  END UnpackArray;

PROCEDURE StretchIfNecessaryRecv (t: UNIXRecvBuf; size: INTEGER) =
  BEGIN
    IF size >= NUMBER(t.buf^) THEN
      t.buf := NEW(REF ARRAY OF CHAR, MAX(size, 2*NUMBER(t.buf^)));
    END;
  END StretchIfNecessaryRecv;

PROCEDURE EndUnpack(t: UNIXRecvBuf) =
  BEGIN
    <*ASSERT t.idx = t.maxSize*>
    t.idx := 0;
  END EndUnpack;

(*
   Connection management and RPC
*)

REVEAL Server = ServerPublic BRANDED OBJECT
  fd: ConnFD.T;
END;

VAR
  servers := NEW(IntRefTbl.Default).init();
  (* Map of hostname->Server.
   Domain of "servers" is the set of servers currently used by this host *)

PROCEDURE OpenServer(hid: HostID.T; port: CARDINAL): Server =
  VAR r: REFANY;
    ep: IP.Endpoint;
    server: Server;
  BEGIN
    
    ep.addr := UNIXHostID.HostIDToIPAddr(hid);
    ep.port := port;
    IF port = 0 THEN 
      ep.port := UNIXHostID.GetPortForServer(hid);
    END;
    
    IF servers.get(hid, r) THEN
      server := r;
    ELSE
      server := NEW(Server, hid := hid);

      (* Open the TCP connection with the server *)
      server.fd := TCP.Connect(ep);
      
      EVAL servers.put(hid, server);
    END;
    RETURN server;
  END OpenServer;

PROCEDURE CloseServer(<*UNUSED*>server: Server) =
  BEGIN
    (* XXX do nothing *)
  END CloseServer;
  
PROCEDURE FindFromID(hid: HostID.T): Server =
  VAR r: REFANY;
  BEGIN
    IF servers.get(hid, r) THEN
      RETURN r;
    ELSE
      <*ASSERT FALSE*>
    END;
  END FindFromID;

PROCEDURE QueueRPC (<*UNUSED*>server: Server;
		    <*UNUSED*>out: SendBuf) =
  BEGIN
  END QueueRPC;
  
PROCEDURE DoRPC (server: Server; out: SendBuf; in_: RecvBuf): BOOLEAN =
  VAR
    status: INTEGER;
    lenBuf: ARRAY [0..7] OF CHAR;
    len, rest: INTEGER;
    nRequests: INTEGER;
    off: INTEGER;
    in := NARROW(in_, UNIXRecvBuf);
  BEGIN
    out.endPack();
    nRequests := VIEW(SUBARRAY(out.buf^, 4, 4), Ctypes.int);
    IF Debug THEN
      Msg("send:", Fmt.Int(nRequests), " reqs,", Fmt.Int(out.idx), " bytes");
    END;
    server.fd.put(SUBARRAY(out.buf^, 0, out.idx));
    out.clear();
    
    status := server.fd.get(lenBuf);
    IF status < BYTESIZE(lenBuf) THEN
      IO.Put("NO REPLY RECEIVED\n");
      RETURN FALSE;
    END;
    
    len := VIEW(lenBuf, INTEGER);
    DEC(len, 8); (* len includes the "len" field itself and "nrequests". *)
    IF Debug THEN
      Msg("=> got ", Fmt.Int(len), "bytes.\n");
    END;
    in.stretchIfNecessary(len);

    rest := len;
    off := 0;
    WHILE rest > 0 DO 
      status := server.fd.get(SUBARRAY(in.buf^, off, rest));
      IF status <= 0 THEN
	IO.Put("reply too short: " & Fmt.Int(status) & " bytes.\n");
	RETURN FALSE;
      END;
      DEC(rest, status);
      INC(off, status);
    END;
    
    in.reset(len);
    
    (* Unpack the first "nRequests-1" responses. *)
    FOR i := 0 TO nRequests - 2 DO
      status := in.unpackInt();
      IF status # OK THEN
	IO.Put("rpc error at " & Fmt.Int(i) & "th req. "
	       & "stat1 = " & Fmt.Int(status)
	       & ".\n");
	in.endUnpack();
	RETURN FALSE;
      END;
    END;
      
    status := in.unpackInt();
    IF status # OK THEN
      IO.Put("rpc error: " & Fmt.Int(status) & ".\n");
      in.endUnpack();
    END;
    RETURN TRUE;
  END DoRPC;

PROCEDURE CreateRecvBuf(): RecvBuf =
  VAR t := NEW(UNIXRecvBuf);
  BEGIN
    t.buf := NEW(REF ARRAY OF CHAR, 8192);
    ResetRecv(t, 0);
    RETURN t;
  END CreateRecvBuf;
  
BEGIN
END UNIXRPC.
