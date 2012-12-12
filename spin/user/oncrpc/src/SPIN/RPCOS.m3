(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed the socket select support.
 *
 * 07-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed Select to call new socket.select function.
 *      Changed Close not to raise exceptions.
 *
 * 23-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed bug in Recv() and RecvFrom function.  Needed to free the
 *	mbuf after receiving it.
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use new mbuf mclgetoa.
 *
 * 150-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  SPIN specific socket and net utility functions.
 *)

MODULE RPCOS EXPORTS RPCOS, RPCSunPriv;

IMPORT RPC, Net, NetDb, Socket, SocketRep, Mbuf, Errno, IO;

CONST 
  debug = FALSE;

PROCEDURE LookupHost (hostname: TEXT): InaddrT RAISES {RPC.Failed} =
  VAR
    ri : InaddrT;
  BEGIN
    TRY
      (* NetDb response already in network order *)
      WITH addr = NetDb.GetHostByName(hostname),
           hostAddr = ntohl(VIEW(addr,UINT32))
       DO
        ri := VIEW(hostAddr,InaddrT);
      END;
      RETURN ri;
    EXCEPT
    | NetDb.HostNotFound =>
      RAISE RPC.Failed(NEW(RPC.Failure, info := "hostname not found."));
    END;
  END LookupHost; 

PROCEDURE ntohl(x: UINT32): UINT32 =
  BEGIN
    RETURN Net.nltoh(x);
  END ntohl;

PROCEDURE ntohs(x: UINT16): UINT16 =
  BEGIN
    RETURN Net.nstoh(x);
  END ntohs;

PROCEDURE htonl(x: UINT32): UINT32 =
  BEGIN
    RETURN Net.htonl(x);
  END htonl;

PROCEDURE htons(x: UINT16): UINT16 =
  BEGIN
    RETURN Net.htons(x);
  END htons;


(* Shut down the connection.  We just close the socket to free up the file
   descrtiptor.  The buffers will float away with the client object. *)
PROCEDURE Close (s: SocketT) =
  BEGIN
    TRY
      Socket.Close(s);
    EXCEPT 
    | Errno.E(err) =>
      (* NOTHING *)
    END;
  END Close;

PROCEDURE Send (
    socket        : SocketT;
    READONLY data : REF ARRAY OF CHAR;
    VAR      len  : CARDINAL) 
  RAISES {RPC.Failed} =
  VAR sendData: Mbuf.T;
  BEGIN
    IF data = NIL THEN 
      RAISE RPC.Failed(NEW(RPC.Failure, info := "send no data")); 
    END;
    IF len > BYTESIZE(data^) THEN len := BYTESIZE(data^); END;

    TRY
      sendData := Mbuf.MclGetOa(data, len);
    EXCEPT
    | Mbuf.LengthMismatch =>
    END;

    TRY
      Socket.Send(socket, sendData);
    EXCEPT
    | Errno.E(err) =>  
      RAISE RPC.Failed(NEW(RPC.Failure, 
                           info := "Send failed: " & Errno.Fmt(err)));
    END;
    RETURN;
  END Send;

PROCEDURE SendTo (            
    socket  : SocketT;
    READONLY
    data    : REF ARRAY OF CHAR;
    VAR
    len     : CARDINAL;
    READONLY
    toAddr  : SockaddrInT)
  RAISES {RPC.Failed} =
  VAR sendData: Mbuf.T;
  BEGIN
    IF data = NIL THEN 
      RAISE RPC.Failed(NEW(RPC.Failure, info := "sendto no data")); 
    END;
    IF len > BYTESIZE(data^) THEN len := BYTESIZE(data^); END;

    TRY
      sendData := Mbuf.MclGetOa(data, len);
    EXCEPT
    | Mbuf.LengthMismatch =>
    END;

    TRY
      WITH to = VIEW(toAddr, SockaddrT) DO
        Socket.Sendto(socket, sendData, 0, to);
      END;
    EXCEPT
    | Errno.E(err) =>  
      RAISE RPC.Failed(NEW(RPC.Failure, 
                           info := "Sendto failed: " & Errno.Fmt(err)));
    END;
    RETURN;
  END SendTo;

PROCEDURE Recv (         socket    : SocketT;
                      READONLY data      : REF ARRAY OF CHAR;
                      VAR      nBytes    : CARDINAL):
  BOOLEAN RAISES{RPC.Failed} =
  VAR
    recvData: Mbuf.T;
    pos: CARDINAL;
  BEGIN
    IF data = NIL THEN 
      RAISE RPC.Failed(NEW(RPC.Failure, info := "recv no data")); 
    END;
    IF nBytes > BYTESIZE(data^) THEN nBytes := BYTESIZE(data^); END;
    TRY
      nBytes := Socket.Recv(socket, recvData, nBytes, 0);
      IF debug THEN
        IO.Put("Received ");
        IO.PutInt(nBytes);
        IO.Put(" from Recv.\n");
      END;
      (* Slow copy from mbuf chain to contiguous buffer -  ugh *)
      pos := 0;
      WHILE recvData # NIL DO
        WITH recvBuf = Mbuf.Array(recvData)^ DO
          SUBARRAY(data^,pos,BYTESIZE(recvBuf)) := recvBuf;
          INC(pos,BYTESIZE(recvBuf));
        END;
        recvData := Mbuf.m_free(recvData);
      END;

      RETURN TRUE;
    EXCEPT
    | Errno.E(err) => 
      RETURN FALSE;
    END;
  END Recv;

PROCEDURE RecvFrom (         socket    : SocketT;
                      READONLY data      : REF ARRAY OF CHAR;
                      VAR      nBytes    : CARDINAL;
                      VAR      fromAddr: SockaddrInT):
  BOOLEAN RAISES{RPC.Failed} =
  VAR
    recvData: Mbuf.T;
    pos: CARDINAL;
  BEGIN
    IF data = NIL THEN 
      RAISE RPC.Failed(NEW(RPC.Failure, info := "recv no data")); 
    END;
    IF nBytes > BYTESIZE(data^) THEN nBytes := BYTESIZE(data^); END;
    TRY
      WITH from = VIEW(fromAddr, SockaddrT) DO
        nBytes := Socket.Recvfrom(socket, recvData, nBytes, 0, from);
      END;

      IF debug THEN
        IO.Put("Received ");
        IO.PutInt(nBytes);
        IO.Put(" from Recvfrom.\n");
      END;
      (* Slow copy from mbuf chain to contiguous buffer -  ugh *)
      pos := 0;
      WHILE recvData # NIL DO
        WITH recvBuf = Mbuf.Array(recvData)^ DO
          SUBARRAY(data^,pos,BYTESIZE(recvBuf)) := recvBuf;
          INC(pos,BYTESIZE(recvBuf));
        END;
        recvData := Mbuf.m_free(recvData);
      END;

      RETURN TRUE;
    EXCEPT
    | Errno.E(err) => 
      RETURN FALSE;
    END;
  END RecvFrom;

(* Get a generic UDP socket. *)
PROCEDURE GetUDPSocket (port : UINT16 := 0): SocketT RAISES {RPC.Failed} =
  VAR
    s   : SocketT;
    addr: SockaddrInT;
  BEGIN
    SetAddr(addr, INADDR_ANY, port);

    TRY 
      s := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_DGRAM, 0);
    EXCEPT
    | Errno.E(err) =>
      RAISE
        RPC.Failed(NEW(RPC.Failure, info := "Couldn't get UDP socket."));
    END;

    TRY
      WITH addrtmp = VIEW(addr, SockaddrInT) DO
        Socket.Bind(s,addrtmp);
      END;
    EXCEPT
    | Errno.E(err) =>
      Close(s);
      RAISE RPC.Failed(
                NEW(RPC.Failure, info := "Bind to remote address failed."));
    END;

    RETURN s;
  END GetUDPSocket;

PROCEDURE ConnectUDPSocket(
    s:SocketT;     
    addr: SockaddrInT) RAISES {RPC.Failed} = 
  BEGIN
    TRY
      WITH addrtmp = VIEW(addr, SockaddrInT) DO
        Socket.Connect(s,addrtmp);
      END;
    EXCEPT
    | Errno.E(err) =>
      Close(s);
      RAISE RPC.Failed(
                NEW(RPC.Failure, info := "Connect failed."));
    END;
  END ConnectUDPSocket;


PROCEDURE Select(VAR s: SocketReadyT; seconds: CARDINAL; useconds:CARDINAL):CARDINAL =
  VAR sockets : ARRAY [1..1] OF SocketReadyT;
      nready : CARDINAL;
  BEGIN
    sockets[FIRST(sockets)] := s;
    nready := Socket.Select(sockets,seconds,useconds);
    s := sockets[FIRST(sockets)];
    RETURN nready;
  END Select;

(*
 * Socket manipulation.
 *)

VAR myHostAddr: InaddrT;

PROCEDURE SetAddr (
    VAR addr : SockaddrInT; 
    host     : InaddrT;  
    port     : UINT16) =
  BEGIN
    addr.sin_family := SocketRep.AF_INET;
    addr.sin_addr   := htonl(VIEW(host,UINT32));
    addr.sin_port   := htons(port);
    addr.sin_zero   := SIN_ZERO;
  END SetAddr;

(* Deduce port number from a socket. *)
PROCEDURE GetHostPortFromSocket (
    s: SocketT; 
    VAR host: InaddrT;
    port: UINT16)
  RAISES {RPC.Failed} =
  VAR
    addr: SockaddrInT;
    len : CARDINAL := BYTESIZE(addr);
  BEGIN

    WITH addrtmp = VIEW(addr,SockaddrT) DO 
      TRY
        Socket.Getsockname(s, addrtmp, len);
      EXCEPT
      | Errno.E(err) => 
        RAISE RPC.Failed(NEW(RPC.Failure, info := "getsockname failed"));
      END;
    END;

    IF addr.sin_family # SocketRep.AF_INET THEN
      RAISE RPC.Failed(NEW(RPC.Failure, info := "not an Internet socket"));
    END;
    WITH addr = VIEW(myHostAddr, UINT32),
         hostAddr = ntohl(addr) 
     DO
      host := VIEW(hostAddr, InaddrT);
    END;
    port := ntohs(addr.sin_port);
  END GetHostPortFromSocket;

PROCEDURE GetLocalInetAddr(): InaddrT  = 
  BEGIN
    RETURN INADDR_ANY;
  END GetLocalInetAddr;

BEGIN
  myHostAddr := GetLocalInetAddr();
END RPCOS. 
