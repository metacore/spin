(*
   RPCSunTCP.m3
   Sun RPC on TCP Streams.
   David Nichols, Xerox PARC
   July, 1991

   $Id: RPCSunTCP.m3,v 1.2 1997/10/29 17:53:55 yasushi Exp $
*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

UNSAFE MODULE RPCSunTCP EXPORTS RPCSun, RPCSunPriv;

IMPORT Rd, RPC, Thread, UFileRdWr, Uin, Unix, Usocket, Wr, XDR, XDRRec;

TYPE
  TCPClient = Client OBJECT
                socket     : INTEGER;        (* TCP connection *)
                source     : XDRRec.Source;
                sink       : XDRRec.Sink;
                bindingInfo: BindingInfo;
                xid        : INTEGER;
                cred, verf : Credentials;
              OVERRIDES
                StartCall := StartCall;
                SendCall  := SendCall;
                EndCall   := EndCall;
                GetRemote := GetRemote;
                Destroy   := Destroy;
              END;

  TCPServer = Server OBJECT
                sp        : ServerProc;
                socket    : INTEGER;
                xid       : INTEGER;
                prog, vers: INTEGER;
                cred, verf: Credentials;
                source    : XDRRec.Source;
                sink      : XDRRec.Sink;
              OVERRIDES
                StartReply := StartReply;
              END;

CONST StackSize = 10000;        (* words *)

(*
 * Client.
 *)

(* Import server for TCP.  Assumes port has been looked up. *)
PROCEDURE TCPImportService (bi: BindingInfo): Client RAISES {RPC.Failed} =
  VAR
    c : TCPClient;
    rd: Rd.T;
    wr: Wr.T;
    clock: Clock.TimeVal;
  BEGIN
    Clock.TimeOfDay(clock);
    c := NEW(TCPClient);
    c.socket := GetClientSocket(bi.hostAddr, bi.port);
    TRY
      rd := UFileRdWr.CreateFileReader(c.socket);
      wr := UFileRdWr.CreateFileWriter(c.socket, TRUE);
    EXCEPT
      Rd.Failure, Wr.Failure =>
        EVAL Unix.close(c.socket);
        RAISE RPC.Failed(
                NEW(RPC.Failure,
                    info := "Can't get reader and writer for socket"));
    END;
    c.source := XDRRec.NewSource(rd);
    c.sink := XDRRec.NewSink(wr);
    c.bindingInfo := bi;
    c.xid := clock.tv_sec;
    c.cred := NIL;
    c.verf := NIL;
    RETURN c;
  END TCPImportService;

PROCEDURE StartCall (c: TCPClient; proc: INTEGER): XDR.Sink
  RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    TRY
      INC(c.xid);
      PutCallHeader(c.sink, c.xid, c.bindingInfo.progNum,
                    c.bindingInfo.progVersion, proc, c.cred, c.verf);
      (* Return the sink so the user can toss in his args. *)
      RETURN c.sink;
    EXCEPT
      XDR.Failed (e) =>
        RAISE RPC.Failed(
                NEW(RPC.ZeroTimesFailure,
                    info := "Couldn't marshal call header", subArg := e));
    END;
  END StartCall;

PROCEDURE SendCall (c: TCPClient): XDR.Source
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR
    xid, accept, code, authWhy, low, high: INTEGER;
    verf                                 : Credentials;
  BEGIN
    TRY
      XDRRec.NewRecord(c.sink);
      (* TODO: Make sure we've consumed all the args. *)
      (* TODO: Wait for reply.  Right now it's implicit in trying to
         unmarshal the reply. *)
      GetReplyHeader(c.source, xid, accept, code, authWhy, low, high, verf);
    EXCEPT
      XDR.Failed (e) =>
        RAISE RPC.Failed(
                NEW(RPC.Failure, info := "Marshalling problem during call",
                    subArg := e));
    END;
    IF xid # c.xid THEN
      RAISE RPC.Failed(
              NEW(RPC.Failure, info := "Protocol error: xid mismatch"));
    END;
    (* Should be smarter in processing error conditions. *)
    CASE accept OF
      MSG_ACCEPTED =>
        CASE code OF
          ACCEPT_SUCCESS => RETURN c.source;
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

PROCEDURE EndCall (c: TCPClient) RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    TRY
      XDRRec.NextRecord(c.source);
    EXCEPT
      XDR.Failed (e) =>
        RAISE RPC.Failed(
                NEW(RPC.Failure, info := "Marshalling problem ending call",
                    subArg := e));
    END;
  END EndCall;

PROCEDURE GetRemote (c: TCPClient): BindingInfo =
  BEGIN
    RETURN c.bindingInfo;
  END GetRemote;

(* Shut down the connection.  We just close the socket to free up the file
   descrtiptor.  The buffers will float away with the client object. *)
PROCEDURE Destroy (c: TCPClient) =
  BEGIN
    EVAL Unix.close(c.socket);
  END Destroy;

(*
 * Server
 *)

(* Caller has a bound listening socket.  The runtime will accept new
   connections on this socket. *)
PROCEDURE ExportTCPListener (sp        : ServerProc;
                             prog, vers: INTEGER;
                             socket    : INTEGER     ): BindingInfo
  RAISES {Erred, RPC.Failed, Thread.Alerted} =
  VAR host, port: INTEGER;
  BEGIN
    GetHostPortFromSocket(socket, host, port);
    PortMapperRegister(prog, vers, port, Protocol.TCP);
    EVAL
      Thread.Fork(NEW(ListenerClosure, stackSize := StackSize, s := socket,
                      sp := sp, prog := prog, vers := vers));
    RETURN CreateBindingInfo(host, prog, vers, port, Protocol.TCP);
  END ExportTCPListener;

TYPE
  ListenerClosure = Thread.SizedClosure OBJECT
                      s         : INTEGER;     (* listening socket *)
                      sp        : ServerProc;
                      prog, vers: INTEGER;
                    OVERRIDES
                      apply := Listener;
                    END;

PROCEDURE Listener (cl: ListenerClosure): REFANY RAISES {} =
  VAR
    sockSet  : Unix.FDSet;
    n, s, len: INTEGER;
    addr     : Uin.struct_sockaddr_in;
  BEGIN
    LOOP
      sockSet := Unix.FDSet{cl.s};
      n := RTScheduler.IOSelect(
             Unix.MAX_FDSET, ADR(sockSet), NIL, ADR(sockSet));
      s := Usocket.accept(cl.s, ADR(addr), ADR(len));
      (* If failed, assume error and shut it down. *)
      IF s = -1 THEN RETURN NIL END;
      TRY
        NoDelay(s);
        EVAL ExportTCP(cl.sp, cl.prog, cl.vers, s);
      EXCEPT
        RPC.Failed, Erred => EVAL Unix.close(s);
      END;
    END;
  END Listener;

(* Caller has connected socket (e.g.  via inetd) to client. *)
PROCEDURE ExportTCP (sp: ServerProc; prog, vers: INTEGER; socket: INTEGER):
  BindingInfo RAISES {Erred, RPC.Failed} =
  VAR
    host, port: INTEGER;
    s         : TCPServer;
    rd        : Rd.T;
    wr        : Wr.T;
  BEGIN
    GetHostPortFromSocket(socket, host, port);
    s := NEW(TCPServer, sp := sp, socket := socket, prog := prog,
             vers := vers, socket := socket);
    TRY
      rd := UFileRdWr.CreateFileReader(socket);
      wr := UFileRdWr.CreateFileWriter(socket, TRUE);
    EXCEPT
      Rd.Failure, Wr.Failure =>
        RAISE RPC.Failed(
                NEW(RPC.Failure,
                    info := "Can't get reader and writer for socket"));
    END;
    s.source := XDRRec.NewSource(rd);
    s.sink := XDRRec.NewSink(wr);
    EVAL
      Thread.Fork(NEW(ServerClosure, stackSize := StackSize, server := s));
    RETURN CreateBindingInfo(host, prog, vers, port, Protocol.TCP);
  END ExportTCP;

TYPE
  ServerClosure = Thread.SizedClosure OBJECT
                    server: TCPServer;
                  OVERRIDES
                    apply := ServerLoop;
                  END;

(* Main loop for server thread.  Waits for incoming calls and calls the
   server callback. *)
PROCEDURE ServerLoop (cl: ServerClosure): REFANY RAISES {} =
  VAR
    prog, vers, proc: INTEGER;
    server                    := cl.server;
    ok              : BOOLEAN;
  BEGIN
    TRY                         (* global error handler *)
      LOOP
        TRY                     (* one call *)
          ok := TRUE;
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
        (* We've either sent the reply or a rejection, so flush the
           data. *)
        XDRRec.NewRecord(server.sink);
        (* Skip to next call *)
        XDRRec.NextRecord(server.source);
      END;
    EXCEPT
      (* If anything goes wrong, shut down the socket and give up. *)
      XDR.Failed, RPC.Failed, Erred, Thread.Alerted =>
        EVAL Unix.close(server.socket);
        RETURN NIL;
    END;
  END ServerLoop;

PROCEDURE StartReply (s: TCPServer): XDR.Sink
  RAISES {Erred, RPC.Failed, Thread.Alerted} =
  BEGIN
    TRY
      PutReplyHeader(s := s.sink, xid := s.xid, accept := MSG_ACCEPTED,
                     code := ACCEPT_SUCCESS, verf := s.verf);
      RETURN s.sink;
    EXCEPT
      XDR.Failed (e) =>
        RAISE RPC.Failed(
                NEW(RPC.Failure, info := "Failed marshalling reply header",
                    subArg := e));
    END;
  END StartReply;

TYPE
  RejectReason = {RPCVersion, ProgUnavail, ProgMismatch, BadProc, BadArgs};
PROCEDURE SendRejection (s: TCPServer; why: RejectReason)
  RAISES {XDR.Failed, RPC.Failed, Erred, Thread.Alerted} =
  BEGIN
    CASE why OF
      RejectReason.RPCVersion =>
        PutReplyHeader(
          s := s.sink, xid := s.xid, accept := MSG_DENIED,
          code := REJECT_RPC_MISMATCH, low := RPCVERS, high := RPCVERS);
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

(*
 * Socket stuff.
 *)

(* Get a connected socket to a given host & port. *)
PROCEDURE GetClientSocket (host, port: INTEGER): INTEGER
  RAISES {RPC.Failed} =
  VAR
    addr: Uin.struct_sockaddr_in;
    s   : INTEGER;
  BEGIN
    SetAddr(addr, host, port);
    s := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0);
    IF s = -1 THEN
      RAISE
        RPC.Failed(NEW(RPC.Failure, info := "Couldn't get TCP socket."));
    END;
    IF Usocket.connect(s, ADR(addr), BYTESIZE(addr)) = -1 THEN
      RAISE RPC.Failed(NEW(RPC.Failure,
                           info := "Connect to remote address failed."));
    END;
    NoDelay(s);
    RETURN s;
  END GetClientSocket;

(* Get a listing TCP socket at a particular port. *)
PROCEDURE GetListeningSocket (port: INTEGER): INTEGER RAISES {RPC.Failed} =
  VAR
    addr: Uin.struct_sockaddr_in;
    s   : INTEGER;
  BEGIN
    SetAddr(addr, 0, port);
    s := Usocket.socket(Usocket.AF_INET, Usocket.SOCK_STREAM, 0);
    IF s = -1 THEN
      RAISE
        RPC.Failed(NEW(RPC.Failure, info := "Couldn't get TCP socket."));
    END;
    IF Usocket.bind(s, ADR(addr), BYTESIZE(addr)) = -1 THEN
      EVAL Unix.close(s);
      RAISE RPC.Failed(NEW(RPC.Failure,
                           info := "Connect to remote address failed."));
    END;
    IF Usocket.listen(s, 5) = -1 THEN
      EVAL Unix.close(s);
      RAISE RPC.Failed(NEW(RPC.Failure, info := "Listen failed."));
    END;
    RETURN s;
  END GetListeningSocket;

(* Mark a socket as "don't delay on write." *)
PROCEDURE NoDelay (s: INTEGER) RAISES {RPC.Failed} =
  CONST TCP_NODELAY = 1;        (* from netinet/tcp.h *)
  VAR on: INTEGER := 1;
  BEGIN
    IF Usocket.setsockopt(
         s, Uin.IPPROTO_TCP, TCP_NODELAY, ADR(on), BYTESIZE(on)) = -1 THEN
      RAISE RPC.Failed(NEW(RPC.Failure, info := "Setsockopt failed."));
    END;
  END NoDelay;

BEGIN
END RPCSunTCP.
