(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

MODULE SpinRPC EXPORTS TransRPC, SpinRPC;
IMPORT Thread;
IMPORT Socket, SocketRep, SocketAddrIn;
IMPORT IntRefTbl;
IMPORT HostID;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT Net;
IMPORT TransRPC;
IMPORT IO, Fmt;
IMPORT Error;
IMPORT Errno;
FROM TransUtils IMPORT DebugMsg, Msg;

REVEAL Server = ServerPublic BRANDED OBJECT
  mu: MUTEX;
  sock: Socket.T;
END;

VAR
  mu := NEW(MUTEX); (* guards servers *)
  servers := NEW(IntRefTbl.Default).init();
  (* Map of hostname->Server.
     The domain of "servers" is the set of servers currently used by this
     host *)

PROCEDURE OpenServer (hid: HostID.T; port: CARDINAL): Server RAISES {Error.E}=
  VAR
    sin: SocketAddrIn.T;
    server: Server;
    r: REFANY;
  BEGIN
    LOCK mu DO 
      IF servers.get(hid, r) THEN
	server := r;
      ELSE
	sin.sin_family := SocketRep.AF_INET;
	IF port = 0 THEN
	  port := TransRPC.Port;
	END;
	sin.sin_port := Net.htons(port);
	sin.sin_len := BYTESIZE(sin);
	sin.sin_addr := hid;
	sin.sin_zero := SocketAddrIn.SIN_ZERO;
	IO.Put("spinrpc.openserver: connecting to "
	       & Fmt.Int(hid, 16) & ":" &  Fmt.Int(port) & ".\n");
	server := NEW(Server, hid := hid, mu := NEW(MUTEX));
	TRY
	  (* Open the TCP connection with the server *)
	  server.sock := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
	  Socket.Connect(server.sock, sin);
	  (* "server.mu" is used to synchronize the msg sender and socket
	     low level routine. This is initially locked. *)
	  Thread.Acquire(server.mu);
	EXCEPT
	| Errno.E(errno) => 
	  IO.PutError("spinrpc.openserver: " & Fmt.Int(errno) & ".\n");
	  RAISE Error.E(NEW(Error.T).init(errno));
	END;
	EVAL servers.put(hid, server);
      END;
    END;
    RETURN server;
  END OpenServer;

PROCEDURE CloseServer (server: Server) RAISES {Error.E} =
  VAR r: REFANY;
  BEGIN
    LOCK mu DO
      TRY
	Socket.Shutdown(server.sock, 2);
      EXCEPT
      | Errno.E(errno) =>
	IO.Put("spinrpc.closeserver: error " & Fmt.Int(errno) & ".\n");
	RAISE Error.E(NEW(Error.T).init(errno));
      END;
      EVAL servers.delete(server.hid, r);
    END;
  END CloseServer;
  
PROCEDURE QueueRPC (<*UNUSED*>server: Server;
		    <*UNUSED*>out: SendBuf) =
  BEGIN
  END QueueRPC;

VAR
  MbufMethods := NEW(Mbuf.Methods,
		     free := SendDone,
		     csum := NIL);

PROCEDURE SendDone (<*UNUSED*>buf: REF ARRAY OF CHAR;
		    <*UNUSED*>size: CARDINAL; arg: REFANY) =
  VAR
    mu := NARROW(arg, MUTEX);
  BEGIN
    Thread.Release(mu);
  END SendDone;
  
PROCEDURE DoRPC (server: Server; out: SendBuf; in_: RecvBuf): BOOLEAN =
  VAR
    status: INTEGER;
    mbuf: Mbuf.T;
    len: INTEGER;
    nRequests: INTEGER;
    n: CARDINAL;
    in := NARROW(in_, SpinRecvBuf);
  BEGIN
    out.endPack();
    nRequests := VIEW(SUBARRAY(out.buf^, 4, 4), Ctypes.int);
    IF DebugMsg THEN
      Msg("send ", Fmt.Int(nRequests), "req(s),", Fmt.Int(out.idx), "bytes");
    END;

    TRY
      mbuf := Mbuf.MclGetOa(out.buf, out.idx, MbufMethods, server.mu);
      Socket.Send(server.sock, mbuf);
      Thread.Acquire(server.mu); (* wait till send is done. *)
      out.clear();
      mbuf := NIL; (* plexus will be confused otherwise... ask mef *)
      n := Socket.Recv(server.sock, mbuf, BYTESIZE(TransRPC.Header));
      (* Get reply length and "nrequests" field which is always 1. *)
      IF n # BYTESIZE(TransRPC.Header) THEN
	IO.Put("NO REPLY RECEIVED\n");
	RETURN FALSE;
      END;
      WITH hdr = VIEW(Mbuf.Array(mbuf)^, TransRPC.Header) DO
	len := hdr.len;
      END;
    
      DEC(len, BYTESIZE(TransRPC.Header));
      (* "len" includes the header len itself, so exclude it.*)
      
      Mbuf.m_freem(mbuf);
      mbuf := NIL; (* plexus will be confused otherwise... ask mef *)
      n := Socket.Recv(server.sock, mbuf, len);
      IF DebugMsg THEN
	Msg("=>total ", Fmt.Int(len), ", got ", Fmt.Int(n));
      END;
      <*ASSERT n <= len*>
    EXCEPT
    | Mbuf.LengthMismatch =>
      IO.Put("spinrpc.dorpc: mbuf.lengthmismatch.\n");
      RETURN FALSE;
    | Errno.E(e) =>
      IO.Put("spinrpc.dorpc: error " & Fmt.Int(e) & ".\n");
      RETURN FALSE;
    END;
    
    in.reset(mbuf, len, server.sock);
    
    (* Unpack the first "nRequests" responses. Each is
       an 1 word response, either ok or not. *)
    FOR i := 0 TO nRequests - 1 DO
      status := in.unpackInt();
      IF status # OK THEN
	Msg("dorpc:error@", Fmt.Int(i), "th req, stat=", Fmt.Int(status));
	in.endUnpack();
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END DoRPC;

PROCEDURE CreateRecvBuf (): RecvBuf =
  BEGIN
    RETURN NEW(SpinRecvBuf);
  END CreateRecvBuf;
  
  
BEGIN
END SpinRPC.
