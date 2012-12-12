(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 01-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This module doesn't actually export any procedures.
   Thus, EXPORTS phrase is a dummy. *)

MODULE VmtpCmd EXPORTS Vmtp;
IMPORT IO, Fmt, Scan;
IMPORT Commands;
IMPORT ParseParams;
IMPORT Vmtp, VmtpUtils;
IMPORT VmtpNameServer;
IMPORT Text;
IMPORT Mbuf;
IMPORT Net;
IMPORT NetDb;
IMPORT Spy;
IMPORT Socket, SocketRep, SocketAddr, SocketAddrIn, Errno;
IMPORT ProfileSupport;
IMPORT Ctypes;
IMPORT Lex;

TYPE
  TransType = {Vmtp, Udp, Tcp, SlowTcp};
  BenchInfo = RECORD
    name: TEXT;
    type := TransType.Vmtp;
    nTrans := 20;
    segSize := 16;
  END;    
    
CONST
  MaxSegSize = 16*1024;
  
VAR
  verbose := TRUE;
  vmtpSpy, tcpSpy, slowSpy, udpSpy: Spy.T;
  profile := FALSE;
  fastTimeout := FALSE;

PROCEDURE ParsePortAndHost (name: TEXT; VAR port: CARDINAL; VAR host: TEXT) =
  BEGIN
    TRY
      WITH pos = Text.FindChar(name, '@') DO 
	IF pos = -1 THEN
	  port := Scan.Int(name);
	  host := "localhost";
	ELSE
	  port := Scan.Int(Text.Sub(name, 0, pos));
	  host := Text.Sub(name, pos+1);
	END;
      END;
    EXCEPT
    | Lex.Error =>
      IO.Put("The server name must be PORTNUM@HOST.\n");
      port := 1001;
      host := name;
    END;
  END ParsePortAndHost;
  
PROCEDURE DisplayMbuf (mbuf: Mbuf.T; off := 0) =
  VAR len: CARDINAL;
  BEGIN
    len := Mbuf.m_length(mbuf);
    LOOP
      WITH a = Mbuf.Array(mbuf)^ DO
	IF NUMBER(a) > 0 THEN
	  WITH msg = SUBARRAY(a, off, len-off) DO
	    IO.Put("msg len=" & Fmt.Int(NUMBER(msg)) & ":");
	    FOR j := 0 TO LAST(msg) DO
	      IF msg[j] # '\000' THEN
		IO.Put(Text.FromChar(msg[j]));
	      END;
	    END;
	    EXIT;
	  END;
	ELSE
	  mbuf := mbuf.mh_hdr.mh_next;
	END;
      END;
    END;
  END DisplayMbuf;
  
PROCEDURE FreeMbufs (VAR m: Vmtp.Mbufs) =
  BEGIN
    FOR i := 0 TO 31 DO
      IF m.mbufs[i] = NIL THEN RETURN; END;
      Mbuf.m_freem(m.mbufs[i]);
      m.mbufs[i] := NIL;
    END;
  END FreeMbufs;

PROCEDURE CreateMessage (VAR x: ARRAY OF CHAR; template: TEXT; i: CARDINAL;
			 READONLY info: BenchInfo) =
  VAR
    j := 0;
    msg: TEXT;
  BEGIN
    msg := "x" & Fmt.Int(i) & template & " hello.";
    Text.SetChars(SUBARRAY(x, 4, NUMBER(x)-4), msg);
    FOR i := 512 TO 16*1024 BY 512 DO
      IF i >= info.segSize THEN EXIT; END;
      x[i] := VAL(ORD('a') + j, CHAR);
    END;
    IF verbose THEN
      IO.Put("sending message \"" & msg & "\".\n");
    END;
  END CreateMessage;

PROCEDURE TweakMessage (VAR x: ARRAY OF CHAR; input: Mbuf.T; off := 0) =
  VAR len: CARDINAL;
  BEGIN
    len := Mbuf.m_length(input);
    LOOP
      WITH a = Mbuf.Array(input)^ DO
	IF NUMBER(a) > 0 THEN 
	  WITH msg = SUBARRAY(a, off+4, len-off-4) DO
	    SUBARRAY(x, 4, 9) := SUBARRAY(msg, 0, 9);
	    x[4] := 's';
	    (* Send back the first 9 letters of the
	       message, and prepend 's'. *)
	    RETURN;
	  END;
	END;
	input := input.mh_hdr.mh_next;
      END;
    END;
  END TweakMessage;

PROCEDURE CheckMessage (VAR x: ARRAY OF CHAR; out: Mbuf.T; off := 0) =
  VAR len: CARDINAL;
  BEGIN
    len := Mbuf.m_length(out);
    WITH a = Mbuf.Array(out)^ DO
      IF NUMBER(a) > 0 THEN
	WITH msg = SUBARRAY(a, off, len-off) DO
	  IF msg[4] # 's' OR SUBARRAY(x, 5, 8) # SUBARRAY(msg, 5, 8) THEN
	    IO.Put("!!! message corruption.\n");
	    DisplayMbuf(out, off);
	  ELSE
	    IO.Put("ok.\n");
	  END;
	END;
	RETURN;
      ELSE
	out := out.mh_hdr.mh_next;	
      END;
    END;
  END CheckMessage;

PROCEDURE GetSegSize (mbuf: Mbuf.T; off := 0): CARDINAL =
  BEGIN
    LOOP
      WITH a = Mbuf.Array(mbuf)^ DO
	IF NUMBER(a) > 0 THEN
	  RETURN VIEW(SUBARRAY(a, off, 4), Ctypes.unsigned_int);
	END;
	mbuf := mbuf.mh_hdr.mh_next;
      END;
    END;
  END GetSegSize;

PROCEDURE SetTimeout (t: Vmtp.T) =
  BEGIN
    t.TC1 := 1500;
    t.TC2 := 100;
    t.TC3 := 10;
    t.TS1 := 10;
    t.TS2 := 3000; (* notused *)
    t.TS3 := 100;
    t.TS4 := 30000;
    t.TS5 := 1500;
  END SetTimeout;
  
PROCEDURE VmtpServer (VAR info: BenchInfo) =
  VAR
    ls := CreateServer(info.name);
    rc: RemoteClient;
    mcb: Vmtp.MCB;
    req: Vmtp.Mbufs;
    client: Entity;
    segSize: INTEGER;
    x := NEW(REF ARRAY OF CHAR, 16*1024);
  BEGIN
    TRY
      WHILE TRUE DO 
	RecvRequest(ls, mcb, rc, req, 0);
	IF fastTimeout THEN
	  SetTimeout(rc);
	END;
      
	segSize := GetSegSize(req.mbufs[0], req.offs[0]);
	IF verbose THEN
	  IO.Put("server: got msg size=" & Fmt.Int(segSize) & ".\n");
	  client := Vmtp.GetEntityID(rc);
	  IO.Put(" from " & VmtpUtils.EntityToString(client) & " ");
	  TweakMessage(x^, req.mbufs[0], req.offs[0]);
	END;
	mcb.segSize := segSize;
	SendReply(rc, mcb, Mbuf.MclGetOa(x, segSize));
	FreeMbufs(req);
      END;
    EXCEPT
    | Mbuf.LengthMismatch =>
      IO.Put("mbuf length mismatch.\n");
    END;
  END VmtpServer;
  
PROCEDURE VmtpClient (VAR info: BenchInfo) =
  VAR
    x := NEW(REF ARRAY OF CHAR, info.segSize);
    mcb: Vmtp.MCB;
    lc: Vmtp.LocalClient;
    server: Vmtp.Entity;
    reply: Mbufs;
    retval: INTEGER;
  BEGIN
    TRY
      lc := Vmtp.CreateClient();
      IF fastTimeout THEN
	SetTimeout(lc);
      END;
      IF NOT VmtpNameServer.Lookup(info.name, server) THEN
	IO.Put("server " & info.name & " not found.\n");
	RETURN;
      END;
      VIEW(x^, Ctypes.unsigned_int) := info.segSize;
      FOR i := 1 TO info.nTrans DO
	IF verbose THEN CreateMessage(x^, "client", i, info); END;
	mcb.server := server;
	mcb.req := i;
	mcb.segSize := info.segSize;
	Spy.Enter(vmtpSpy);
	retval := Vmtp.Invoke(lc, mcb, Mbuf.MclGetOa(x, info.segSize),
			      reply, 0);
	Spy.Exit(vmtpSpy);
	IF verbose THEN
	  VmtpUtils.ErrorMsg("client: got reply", retval);
	  IF retval = Vmtp.OK THEN
	    CheckMessage(x^, reply.mbufs[0], reply.offs[0]);
	  END;
	END;
	FreeMbufs(reply);
      END;
      Spy.DumpTime(vmtpSpy);
    EXCEPT
    | Mbuf.LengthMismatch =>
      IO.Put("mbuf length mismatch.\n");
    END;
  END VmtpClient;

VAR
  tcpPort := 1001;
  udpPort := 1001;
  
PROCEDURE TcpServer (<*UNUSED*>VAR info: BenchInfo) =
  VAR 
    sock, subSock: Socket.T; (* accept socket & per-connection socket. *)
    sin: SocketAddrIn.T;
    mbuf: Mbuf.T;
    n, total: INTEGER;
    segSize: INTEGER;
    x := NEW(REF ARRAY OF CHAR, 16*1024);
  BEGIN
    TRY
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(tcpPort),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := 0,
			    sin_zero := SocketAddrIn.SIN_ZERO};
      Socket.Bind(sock, sin);
      Socket.Listen(sock, 5);
    EXCEPT
    | Errno.E =>
      IO.Put("Some random socket error happened. I have no clue.\n");
      RETURN;
    END;
    
    IO.Put("server is waiting on TCP " & Fmt.Int(tcpPort) & "\n");
    INC(tcpPort);
    
    LOOP
      TRY
	subSock := Socket.Accept(sock);
      EXCEPT
      | Errno.E =>
	IO.Put("???");
	RETURN;
      END;
      
      IO.Put("server: accepted\n");
      TRY
	LOOP
	  (* First 4 bytes of the first packet hold the seg size. *)
	  total := Socket.Recv(subSock, mbuf, MaxSegSize);
	  IF total <= 0 THEN IO.Put("server error\n"); EXIT; END;
	  segSize := GetSegSize(mbuf);
	  IF verbose THEN
	    IO.Put("server: seg size=" & Fmt.Int(segSize) & ".\n");
	    DisplayMbuf(mbuf);
	    TweakMessage(x^, mbuf);
	  END;
	  Mbuf.m_freem(mbuf);

	  (* Receive subsequest packets. *)
	  WHILE total < segSize DO 
	    n := Socket.Recv(subSock, mbuf, segSize-total);
	    IF n <= 0 THEN IO.Put("server error\n"); EXIT; END;
	    INC(total, n);
	    Mbuf.m_freem(mbuf);
	  END;
	  IF total < segSize THEN
	    EXIT;
	  END;
	  Socket.Send(subSock, Mbuf.MclGetOa(x, segSize));
	END;
      EXCEPT
      | Errno.E(e) =>
	IO.Put("server sock error " & Fmt.Int(e) & ".\n");
      ELSE
      END;
      IO.Put("server: shut down.\n");
    END;
  END TcpServer;

PROCEDURE TcpClient (VAR info: BenchInfo) =
  VAR
    port: CARDINAL;
    host: TEXT;
    sock: Socket.T;
    sin: SocketAddrIn.T;    
    x := NEW(REF ARRAY OF CHAR, 16*1024);
    mbuf: Mbuf.T;
    n, total: CARDINAL;
  BEGIN
    TRY
      ParsePortAndHost(info.name, port, host);
      IO.Put("connecting to " & Fmt.Int(port) & "@" & host & ".\n");
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(port),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := NetDb.GetHostByName(host),
			    sin_zero := SocketAddrIn.SIN_ZERO};
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      Socket.Connect(sock, sin);
      IO.Put("client:connected\n");
      VIEW(x^, Ctypes.unsigned_int) := info.segSize;
      FOR i := 0 TO info.nTrans DO
	IF verbose THEN CreateMessage(x^, "client", i, info); END;
	Spy.Enter(tcpSpy);
	Socket.Send(sock, Mbuf.MclGetOa(x, info.segSize));
	total := 0;
	WHILE total < info.segSize DO 
	  n := Socket.Recv(sock, mbuf, info.segSize-total);
	  IF n <= 0 THEN
	    IO.Put("client error\n");
	    EXIT;
	  END;
	  INC(total, n);
	  IF verbose THEN
	  IO.Put("client: got reply:");
	    CheckMessage(x^, mbuf);
	  END;
	  Mbuf.m_freem(mbuf);
	END;
	Spy.Exit(tcpSpy);
      END;
      Spy.DumpTime(tcpSpy);
      Socket.Close(sock);
      IO.Put("tcp client end.\n");
    EXCEPT
    ELSE
      IO.Put("socket error.\n");
    END;
  END TcpClient;

(* This tcp server creates a new connection per RPC. *)
PROCEDURE SlowTcpServer (<*UNUSED*>VAR info: BenchInfo) =
  VAR 
    sock, subSock: Socket.T; (* accept socket & per-connection socket. *)
    sin: SocketAddrIn.T;
    mbuf: Mbuf.T;
    n, total: INTEGER;
    segSize: INTEGER;
    x := NEW(REF ARRAY OF CHAR, 16*1024);
  BEGIN
    TRY
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(tcpPort),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := 0,
			    sin_zero := SocketAddrIn.SIN_ZERO};
      Socket.Bind(sock, sin);
      Socket.Listen(sock, 5);
      IO.Put("server is waiting on TCP " & Fmt.Int(tcpPort) & "\n");
      INC(tcpPort);
    EXCEPT
    | Errno.E =>
      IO.Put("Some random socket error happened. I have no clue.\n");
      RETURN;
    END;
    
    LOOP
      TRY
	subSock := Socket.Accept(sock);
      EXCEPT
      | Errno.E =>
	IO.Put("???");
	RETURN;
      END;
      IF verbose THEN IO.Put("server: accepted\n"); END;
      TRY
	LOOP
	  (* First 4 bytes of the first packet hold the seg size. *)
	  total := Socket.Recv(subSock, mbuf, MaxSegSize);
	  IF total <= 0 OR mbuf = NIL THEN EXIT; END;
	  segSize := GetSegSize(mbuf);
	  IF verbose THEN
	    IO.Put("server: seg size=" & Fmt.Int(segSize) & ".\n");
	    DisplayMbuf(mbuf);
	    TweakMessage(x^, mbuf);
	  END;
	  Mbuf.m_freem(mbuf);
	  
	  (* Receive subsequest packets. *)
	  WHILE total < segSize DO 
	    n := Socket.Recv(subSock, mbuf, segSize-total);
	    IF n <= 0 THEN IO.Put("server error\n"); EXIT; END;
	    INC(total, n);
	    Mbuf.m_freem(mbuf);
	  END;
	  Socket.Send(subSock, Mbuf.MclGetOa(x, segSize));
	END;
	Socket.Close(subSock);
      EXCEPT
      | Errno.E(e) =>
	IO.Put("server sock error " & Fmt.Int(e) & ".\n");
      ELSE
      END;
    END;
  END SlowTcpServer;

PROCEDURE SlowTcpClient (VAR info: BenchInfo) =
  VAR
    port: CARDINAL;
    host: TEXT;
    sock: Socket.T;
    sin: SocketAddrIn.T;    
    x := NEW(REF ARRAY OF CHAR, 16*1024);
    mbuf: Mbuf.T;
    n, total: CARDINAL;
  BEGIN
    TRY
      ParsePortAndHost(info.name, port, host);
      IO.Put("connecting to " & Fmt.Int(port) & "@" & host & ".\n");
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(port),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := NetDb.GetHostByName(host),
			    sin_zero := SocketAddrIn.SIN_ZERO};
      VIEW(x^, Ctypes.unsigned_int) := info.segSize;
      FOR i := 0 TO info.nTrans DO
	Spy.Enter(slowSpy);
	sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
	IF verbose THEN IO.Put("client:connected\n"); END;
	Socket.Connect(sock, sin);
	IF verbose THEN CreateMessage(x^, "client", i, info); END;
	Socket.Send(sock, Mbuf.MclGetOa(x, info.segSize));
	total := 0;
	WHILE total < info.segSize DO 
	  n := Socket.Recv(sock, mbuf, info.segSize-total);
	  IF n <= 0 THEN
	    IO.Put("client error\n");
	    EXIT;
	  END;
	  INC(total, n);
	  IF verbose THEN
	    IO.Put("client: got reply:");
	    CheckMessage(x^, mbuf);
	  END;
	  Mbuf.m_freem(mbuf);
	END;
	Socket.Shutdown(sock, 2);
	Socket.Close(sock);
	Spy.Exit(slowSpy);
      END;
      Spy.DumpTime(slowSpy);
      IO.Put("slowtcp client end.\n");
    EXCEPT
    | Errno.E(e) =>
      IO.Put("client sock error " & Fmt.Int(e) & ".\n");
    ELSE
    END;
  END SlowTcpClient;
  
PROCEDURE UdpServer (<*UNUSED*>VAR info: BenchInfo) =
  VAR 
    sock: Socket.T; (* accept socket & per-connection socket. *)
    sin: SocketAddrIn.T;
    mbuf: Mbuf.T;
    n: INTEGER;
    x := NEW(REF ARRAY OF CHAR, 16*1024);
    addr: SocketAddr.T;
  BEGIN
    TRY
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_DGRAM, 0);
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(udpPort),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := 0,
			    sin_zero := SocketAddrIn.SIN_ZERO};
      Socket.Bind(sock, sin);
      IO.Put("server is waiting on Udp " & Fmt.Int(udpPort) & "\n");
      INC(udpPort);
      LOOP
	n := Socket.Recvfrom(sock, mbuf, MaxSegSize, 0, addr);
	IF verbose THEN
	  IO.Put("server: got " & Fmt.Int(n) & "bytes.\n");
	  DisplayMbuf(mbuf);
	  TweakMessage(x^, mbuf);
	END;
	Socket.Sendto(sock, Mbuf.MclGetOa(x, n), 0, addr);
	Mbuf.m_freem(mbuf);
      END;
    EXCEPT
    | Errno.E =>
      IO.Put("Some random socket error happened. I have no clue.\n");
      RETURN;
    | Mbuf.LengthMismatch =>
      IO.Put("mbuf length mismatch.\n");
    END;
  END UdpServer;

PROCEDURE UdpClient (VAR info: BenchInfo) =
  VAR
    port: CARDINAL;
    host: TEXT;
    sock: Socket.T;
    sin, myAddr, serverAddr: SocketAddrIn.T;    
    x := NEW(REF ARRAY OF CHAR, 16*1024);
    mbuf: Mbuf.T;
    n: CARDINAL;
  BEGIN
    TRY
      ParsePortAndHost(info.name, port, host);
      sin := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			    sin_port := Net.htons(port),
			    sin_len := BYTESIZE(SocketAddrIn.T),
			    sin_addr := NetDb.GetHostByName(host),
			    sin_zero := SocketAddrIn.SIN_ZERO};
      sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_DGRAM, 0);
      myAddr := SocketAddrIn.T{sin_family := SocketRep.AF_INET,
			       sin_port := 0,
			       sin_len := BYTESIZE(SocketAddrIn.T),
			       sin_addr := 0,
			       sin_zero := SocketAddrIn.SIN_ZERO};
      Socket.Bind(sock, myAddr);
      FOR i := 1 TO info.nTrans DO
	IF verbose THEN CreateMessage(x^, "client", i, info); END;
	Spy.Enter(udpSpy);
	Socket.Sendto(sock, Mbuf.MclGetOa(x, info.segSize), 0,
		      VIEW(sin, SocketAddr.T));
	n := Socket.Recvfrom(sock, mbuf, info.segSize, 0,
			     VIEW(serverAddr, SocketAddr.T));
	Spy.Exit(udpSpy);
	IF verbose THEN
	  IO.Put("client: got reply ");
	  CheckMessage(x^, mbuf);
	END;
	Mbuf.m_freem(mbuf);
      END;
      Spy.DumpTime(udpSpy);
      Socket.Close(sock);
      IO.Put("udp client end.\n");
    EXCEPT
    | Errno.E =>
      IO.Put("Some random socket error happened. I have no clue.\n");
      RETURN;
    | Mbuf.LengthMismatch =>
      IO.Put("mbuf length mismatch.\n");
    | NetDb.HostNotFound =>
      IO.Put("host not found.\n");
    END;
  END UdpClient;
  
PROCEDURE Run (<*UNUSED*>c: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR
    mode: TEXT;
    info: BenchInfo;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();

      LOOP
	IF pp.testNext("-vmtp") THEN
	  info.type := TransType.Vmtp;
	ELSIF pp.testNext("-tcp") THEN
	  info.type := TransType.Tcp;
	ELSIF pp.testNext("-udp") THEN
	  info.type := TransType.Udp;
	ELSIF pp.testNext("-slow") THEN
	  info.type := TransType.SlowTcp;
	ELSIF pp.testNext("-n") THEN
	  info.nTrans := pp.getNextInt();
	ELSIF pp.testNext("-s") THEN
	  info.segSize := pp.getNextInt();
	ELSE
	  EXIT;
	END;
      END;
      
      IO.Put("type = ");
      CASE info.type OF
      | TransType.Vmtp =>IO.Put("vmtp");
      | TransType.Tcp =>IO.Put("tcp");
      | TransType.Udp =>IO.Put("udp");
      | TransType.SlowTcp =>IO.Put("slowtcp");
      END;
      
      IO.Put(", segment size = " & Fmt.Int(info.segSize) & ", "
	     & Fmt.Int(info.nTrans) & " tranactions.\n");
      mode := pp.getNext();
      info.name := pp.getNext();
      
      IF Text.Equal(mode, "client") THEN
	(*INC(Strand.GetCurrent().pri, 10);*)
	Spy.Reset();
	IF profile THEN
	  EVAL ProfileSupport.On();
	END;
	CASE info.type OF
	| TransType.Vmtp =>VmtpClient(info);
	| TransType.Tcp =>TcpClient(info);
	| TransType.Udp =>UdpClient(info);
	| TransType.SlowTcp =>SlowTcpClient(info);
	END;
	IF profile THEN
	  EVAL ProfileSupport.Off();
	END;
      ELSIF Text.Equal(mode, "server") THEN
	CASE info.type OF
	| TransType.Vmtp =>VmtpServer(info);
	| TransType.Tcp =>TcpServer(info);
	| TransType.Udp =>UdpServer(info);
	| TransType.SlowTcp =>SlowTcpServer(info);
	END;
	(*DEC(Strand.GetCurrent().pri, 10);*)
      ELSIF Text.Equal(mode, "verbose") THEN
	IF Text.Equal(info.name, "off") THEN
	  verbose := FALSE;
	  Vmtp.Debug := FALSE;
	ELSE
	  verbose := TRUE;
	  Vmtp.Debug := TRUE;
	END;
	IO.Put("new verbose value : " & Fmt.Int(ORD(verbose)) & ".\n");
      ELSIF Text.Equal(mode, "timeout") THEN
	IF Text.Equal(info.name, "fast") THEN
	  fastTimeout := TRUE;
	ELSE
	  fastTimeout := FALSE;
	END;
	IO.Put("new timeout : " & Fmt.Int(ORD(fastTimeout)) & ".\n");
      ELSIF Text.Equal(mode, "prof") THEN
	IF Text.Equal(info.name, "off") THEN
	  profile := FALSE;
	ELSE
	  profile := TRUE;
	END;
	IO.Put("new profile value : " & Fmt.Int(ORD(profile)) & ".\n");
      ELSE
	Help();
      END;
    EXCEPT
    | ParseParams.Error => 
      Help();
      RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;
  
CONST CommandHelp = "vmtp [-n NRPC] [-s MSGSIZE] {-vmtp|-udp|-tcp|-slow} {server|client} port@host";
PROCEDURE Help () =
  BEGIN
    IO.Put(CommandHelp);
    IO.Put(".\n");
  END Help;

BEGIN
  vmtpSpy := Spy.Create("vmtp-test", FALSE, 0);
  tcpSpy := Spy.Create("tcp-test", FALSE, 0);
  udpSpy := Spy.Create("udp-test", FALSE, 0);
  slowSpy := Spy.Create("slowtcp-test", FALSE, 0);
  EVAL Commands.Install(Run, "vmtp", CommandHelp);
END VmtpCmd.
