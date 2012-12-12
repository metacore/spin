(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE Proxy;
IMPORT HostID, FastIntRefTbl;
IMPORT TransStub;
IMPORT IO, Fmt;
IMPORT SocketRep, Socket;
IMPORT Net, Mbuf;

REVEAL Server = ServerPublic BRANDED OBJECT
  fd : Socket.T;
END;

VAR
  servers := NEW(FastIntRefTbl.Default).init();
  (* Map of hostname->Server.
   Domain of "servers" is the set of servers currently used by this host *)

PROCEDURE Open (hid: HostID.T): Server =
  VAR
    r : REFANY;
    rem, me : SocketRep.sockaddr_in;
    server : Server;
  BEGIN
    IF servers.get(hid, r) THEN
      server := r;
    ELSE
      server := NEW(Server, hid := hid);

      (* Open the TCP connection with the server *)
      server.fd := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      rem.sin_len := BYTESIZE(rem);
      rem.sin_family := SocketRep.AF_INET;
      rem.sin_port := Net.htons(29999);
      rem.sin_addr := hid;

      me.sin_port := 0;
      me.sin_len  := BYTESIZE(me);
      me.sin_addr := 0;
      me.sin_zero := ARRAY [1..8] OF CHAR{'\000','\000','\000','\000',
					  '\000','\000','\000','\000'};

      Socket.Bind(server.fd, me);
      IO.Put("bound\n");

      Socket.Connect(server.fd, rem);
      IO.Put("connected\n");
      
      EVAL servers.put(hid, server);
    END;
    RETURN server;
  END Open;

PROCEDURE Close(<*UNUSED*>server : Server) =
  BEGIN
    (* XXX do nothing *)
  END Close;
  
PROCEDURE FindFromID(hid : HostID.T) : Server =
  VAR r : REFANY;
  BEGIN
    IF servers.get(hid, r) THEN
      RETURN r;
    ELSE
      <*ASSERT FALSE*>
    END;
  END FindFromID;
  
PROCEDURE QueueRPC (server : Server; buf : TransStub.T) =
  BEGIN
  END QueueRPC;

PROCEDURE DoRPC (server : Server; buf : TransStub.T): BOOLEAN =
  VAR status: INTEGER;
    len: INTEGER;
    nRequests: INTEGER;
    mbuf, lenRecvMbuf : Mbuf.T := NIL;
  BEGIN
    buf.endPack();
    mbuf := Mbuf.MclGetOa(buf.buf, buf.idx);
    nRequests := VIEW(SUBARRAY(buf.buf^, BYTESIZE(INTEGER), BYTESIZE(INTEGER)),
		      INTEGER);

    Socket.Send(server.fd, mbuf);

    EVAL Socket.Recv(server.fd, lenRecvMbuf, BYTESIZE(INTEGER), 0);
    
    len := VIEW(Mbuf.Array(lenRecvMbuf)^, INTEGER);

    IO.Put("reply len=" & Fmt.Int(len) & ".\n");
    buf.stretchIfNecessary(len);

    mbuf := Mbuf.MclGetOa(buf.buf, len);
    len := Socket.Recv(server.fd, mbuf, len, 0);
    
    EVAL buf.initRead(len-BYTESIZE(INTEGER)); (* to exclude the len header *)
    status := buf.unpackInt(); (* skip the "nRequests", since it is always 1 *)
    <*ASSERT status = 1*>
    
    (* Unpack the first "nRequests-1" responses. *)
    FOR i := 0 TO nRequests - 2 DO
      status := buf.unpackInt();
      IF status # TransStub.OK THEN
	IO.Put("rpc error at " & Fmt.Int(i) & "th req. "
	       & "stat1 = " & Fmt.Int(status)
	       & ".\n");
	buf.endUnpack();
	RETURN FALSE;
      END;
    END;
    
    status := buf.unpackInt();
    IF status # TransStub.OK THEN
      IO.Put("rpc error : " & Fmt.Int(status) & ".\n");
      buf.endUnpack();
    END;
    RETURN TRUE;

  END DoRPC;
  
BEGIN
END Proxy.
