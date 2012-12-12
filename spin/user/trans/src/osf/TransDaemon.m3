(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)
UNSAFE MODULE TransDaemon;
IMPORT Unix;
IMPORT IO, Fmt, Text, Wr;
IMPORT TCP;
IMPORT IP;
IMPORT TCPPeer;
IMPORT Thread;
IMPORT ConnFD;
IMPORT HostID;
IMPORT UNIXHostID;
IMPORT StorageRemote;
IMPORT TransRPC, UNIXRPC;
IMPORT TransGroup;
IMPORT Ctypes;
FROM TransUtils IMPORT Debug, Msg;

TYPE MyClosure = Thread.Closure OBJECT
  fd : ConnFD.T;
OVERRIDES
  apply := Apply;
END;

PROCEDURE GetHostName () : TEXT =
  BEGIN
    RETURN IP.GetCanonicalByAddr(UNIXHostID.HostIDToIPAddr(HostID.myID));
  END GetHostName;

EXCEPTION SomethingsWrong;
  
PROCEDURE Apply (c: MyClosure) : REFANY =
  VAR 
    lenBuf : ARRAY [0..7] OF CHAR;
    len, nRequests : INTEGER;
    func, status : INTEGER;
    rest, off: CARDINAL;
    group: TransGroup.T;
    inBuf := NARROW(TransRPC.CreateRecvBuf(), UNIXRPC.UNIXRecvBuf);
    outBuf :=  TransRPC.CreateSendBuf();
    sockAddr : IP.Endpoint;
    hid : HostID.T;
  BEGIN
    (* Get the host id of the client *)
    sockAddr := TCPPeer.Get(c.fd);
    hid := UNIXHostID.IPAddrToHostID(sockAddr.addr);
    group := NEW(TransGroup.T).init();
    
    (* Go into event loop *)
    TRY
      LOOP
	IF c.fd.get(lenBuf) = 0 THEN
	  RAISE SomethingsWrong;
	END;
	
	len := VIEW(lenBuf, Ctypes.unsigned_int);
	nRequests := VIEW(lenBuf, ARRAY [0 .. 1] OF Ctypes.int)[1];
	
	IF Debug THEN
	  Msg("pkt nreq=", Fmt.Int(nRequests), "len=", Fmt.Int(len), ":");
	END;

	DEC(len, 8); (* "len" includes the len field itself. *)
	inBuf.stretchIfNecessary(len);

	rest := len;
	off := 0;
	WHILE rest > 0 DO 
	  status := c.fd.get(SUBARRAY(inBuf.buf^, off, rest));
	  IF status <= 0 THEN
	    IO.Put("req too short: " & Fmt.Int(status) & " bytes.\n");
	    RAISE SomethingsWrong;
	  END;
	  DEC(rest, status);
	  INC(off, status);
	END;
	inBuf.reset(len);
	outBuf.clear();
      
	FOR i := 0 TO nRequests-1 DO 
	  func := inBuf.unpackInt();
	  IF func >= TransRPC.Hello AND func <= TransRPC.Last THEN
	    StorageRemote.Callbacks[func](inBuf, outBuf, group);
	  ELSE
	    Msg("I don't know this func " & Fmt.Int(func));
	    outBuf.packInt(TransRPC.UnknownFunc);
	  END;
	END;
	
	(* send out a reply *)
	outBuf.endPack();
	IF Debug THEN
	  Msg("send reply: " & Fmt.Int(outBuf.idx) & " bytes.\n");
	END;
	c.fd.put(SUBARRAY(outBuf.buf^, 0, outBuf.idx));
      END;
    EXCEPT
    | SomethingsWrong =>
      c.fd.close();
      Msg("Connection terminated");
      group.shutDown();
    END;
    RETURN NIL;
  END Apply;

PROCEDURE Loop (port: CARDINAL) =
  VAR
    ep2 := IP.Endpoint{addr:=IP.NullAddress, port:=port};
    conn : TCP.Connector;
    fd : ConnFD.T;
    host : TEXT;
    wr : Wr.T;
    name : ARRAY [0 .. 256] OF CHAR;
  BEGIN
    conn := TCP.NewConnector(ep2);
    port := TCP.GetEndPoint(conn).port; (* port # is chosen by OS whet it was
					initially 0 *)
    host := GetHostName();
    
    IO.Put("The server is waiting on " & host & ":" & Fmt.Int(port) & "\n");

    (* Create a directory named "host" *)
    Text.SetChars(name, host);
    name[Text.Length(host)] := '\000';
    EVAL Unix.mkdir(ADR(name), 8_755);

    (* Go to that directory. *)
    EVAL Unix.chdir(ADR(name));
    
    wr := IO.OpenWrite("ADDR");
    IO.PutInt(port, wr);
    Wr.Close(wr);
    LOOP
      fd := TCP.Accept(conn);
      EVAL Thread.Fork(NEW(MyClosure, fd := fd));
    END;
  END Loop;

PROCEDURE Stop () =
  BEGIN
    
  END Stop;
  
BEGIN
END TransDaemon.
