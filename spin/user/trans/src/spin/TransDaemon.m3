(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Fleshed up.
 * 26-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)

MODULE TransDaemon;
IMPORT IO, Fmt;
IMPORT Thread, ThreadExtra;
IMPORT Errno;
IMPORT StorageRemote;
IMPORT TransRPC;
IMPORT SpinRPC;
IMPORT TransGroup;
IMPORT Socket, SocketRep, SocketAddr, SocketAddrIn;
IMPORT Mbuf;
IMPORT Net;
IMPORT HostID;
IMPORT RefRefTbl;

FROM TransUtils IMPORT DebugMsg, Msg;

EXCEPTION SomethingsWrong;

VAR
  MbufMethods := NEW(Mbuf.Methods, free := SendDone, csum := NIL);
  
  sockTable := NEW(RefRefTbl.Default).init();
  (* Records all the sockets currently open. Used in "Stop" to shutdown
     all the socks automatically. *)
  sockTableMu := NEW(MUTEX);
  (* Guards "sockTable". *)
  
PROCEDURE SendDone (<*UNUSED*>buf: REF ARRAY OF CHAR;
		    <*UNUSED*>size: CARDINAL; arg: REFANY) =
  VAR
    mu := NARROW(arg, MUTEX);
  BEGIN
    Thread.Release(mu);
  END SendDone;

PROCEDURE ShutdownSocket(sock: Socket.T) =
  VAR r: REFANY;
  BEGIN
    TRY
      LOCK sockTableMu DO
	EVAL sockTable.delete(sock, r);
      END;
      Socket.Shutdown(sock, 2);
    EXCEPT
    ELSE
    END;
  END ShutdownSocket;
PROCEDURE ServeClient (ref: REFANY): REFANY =
  VAR
    group: TransGroup.T;
    (* The group the client runs in. *)
    n: CARDINAL; (* bytes actually read *)
    sock := NARROW(ref, Socket.T);
    sin: SocketAddrIn.T; (* peer address *)
    sinLen: CARDINAL;
    mbuf, sendMbuf: Mbuf.T;
    (* holds first 2 words in the request. *)
    len, nRequests : INTEGER;
    (* total length, and # of subrequests in a request packed. *)
    func: INTEGER;
    inBuf := NARROW(TransRPC.CreateRecvBuf(), SpinRPC.SpinRecvBuf);
    outBuf := TransRPC.CreateSendBuf();
    hid : HostID.T;
    mu := NEW(MUTEX); (* sync between socket send notifier. *)
  BEGIN
    group := NEW(TransGroup.T).init();
    Thread.Acquire(mu); (* make "mu" in locked state. "mu" is used as
			   a binary semaphore with initial value 0. *)
    
    (* Go into event loop *)
    TRY
      Socket.Getpeername(sock, VIEW(sin, SocketAddr.T), sinLen);
      hid := sin.sin_addr;
      IO.Put("accepted connection from " & Fmt.Int(hid, 16)
	     & ":" & Fmt.Int(sin.sin_port));
      LOOP
	n := Socket.Recv(sock, mbuf, BYTESIZE(TransRPC.Header));
	IF n # BYTESIZE(TransRPC.Header) THEN
	  Msg("transdaemon.loop:serveclient got only ", Fmt.Int(n),
	      " bytes.");
	  RAISE SomethingsWrong;
	END;
	
	WITH hdr = VIEW(Mbuf.Array(mbuf)^, TransRPC.Header) DO
	  len := hdr.len;
	  nRequests := hdr.nRequests;
	  IF DebugMsg THEN
	    Msg("pkt nreq=", Fmt.Int(nRequests), "len=", Fmt.Int(len), ":");
	  END;
	END;
	DEC(len, BYTESIZE(TransRPC.Header)); (* "len" includes header itself *)

	Mbuf.m_freem(mbuf);
	mbuf := NIL;

	n := Socket.Recv(sock, mbuf, len);
	<*ASSERT n <= len*>
	inBuf.reset(mbuf, len, sock);
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
	inBuf.endUnpack();
	
	(* send out a reply.
	   XXX This code doesn't allow overlapping sends... maybe ok. *)
	outBuf.endPack();
	sendMbuf := Mbuf.MclGetOa(outBuf.buf, outBuf.idx, MbufMethods, mu);
	Socket.Send(sock, sendMbuf);
	Thread.Acquire(mu); (* wait till send is done. *)
      END;
    EXCEPT
    | Errno.E(ec) =>
      Msg("Socket error", Fmt.Int(ec));
      ShutdownSocket(sock);
      group.shutDown();
    | Mbuf.LengthMismatch =>
      Msg("Internal mbuf length error");
      ShutdownSocket(sock);
      group.shutDown();
    | SomethingsWrong =>
      Msg("Connection error, terminating the client.");
      ShutdownSocket(sock);
      group.shutDown();
    END;
    RETURN NIL;
  END ServeClient;

PROCEDURE Loop (port: CARDINAL) =
  VAR
    sock, subSock: Socket.T; (* accept socket & per-connection socket. *)
    sin: SocketAddrIn.T;
  BEGIN
    IF port = 0 THEN
      port := TransRPC.Port;
    END;
    
    TRY
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      LOCK sockTableMu DO EVAL sockTable.put(sock, sock); END;
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(port),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := 0,
			    sin_zero := SocketAddrIn.SIN_ZERO};
      Socket.Bind(sock, sin);
      Socket.Listen(sock, 5);
      Msg("The server is waiting on TCP ", Fmt.Int(TransRPC.Port), "\n");

      LOOP
	subSock := Socket.Accept(sock);
	LOCK sockTableMu DO EVAL sockTable.put(subSock, subSock); END;
	EVAL ThreadExtra.PFork(ServeClient, subSock);
      END;
    EXCEPT
    | Errno.E(ec) =>
      IO.Put("transdaemon.loop:" & Errno.Fmt(ec)
	     & ", errno=" & Fmt.Int(ec) & "\n");
      RETURN;
    END;
  END Loop;

PROCEDURE Stop () =
  VAR
    itr := sockTable.iterate();
    r1, r2: REFANY;
  BEGIN
    WHILE itr.next(r1, r2) DO
      WITH sock = NARROW(r1, Socket.T) DO
	TRY
	  Socket.Shutdown(sock, 2);
	EXCEPT
	ELSE
	END;
      END;
    END;
    sockTable := NEW(RefRefTbl.Default).init();
  END Stop;
  
BEGIN
END TransDaemon.
