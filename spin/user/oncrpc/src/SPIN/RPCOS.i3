(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed the socket select support.
 *
 * 07-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed Select and Close interface.
 *
 * 150-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  SPIN specific socket and net utility functions.
 *)

INTERFACE RPCOS;
IMPORT RPC;
IMPORT Ctypes, Socket, SocketAddr, SocketAddrIn;

TYPE
  UINT32      = Ctypes.unsigned_int;
  UINT16      = Ctypes.unsigned_short;
  SocketT     = Socket.T;
  SocketReadyState = Socket.selectReadyState;
  SocketReadyStates = Socket.selectReadyStates;
  SocketReadyT= Socket.readyDes;
  SockaddrT   = SocketAddr.T;
  SockaddrInT = SocketAddrIn.T;
  InaddrT     = RECORD s_addr: UINT32; END;

CONST
  SIN_ZERO    = SocketAddrIn.SIN_ZERO;
  INADDR_ANY  = InaddrT{16_00000000};
  
(* Convenience routine for hostname translation.  Returns the first
   internet address (in host order, not network order) of the name provided
   that it finds with gethostbyname call. *)
PROCEDURE LookupHost (hostname: TEXT): InaddrT 
  RAISES {RPC.Failed};

PROCEDURE ntohl(x: UINT32): UINT32;
PROCEDURE ntohs(x: UINT16): UINT16;
PROCEDURE htonl(x: UINT32): UINT32;
PROCEDURE htons(x: UINT16): UINT16;

(* exported by rpcsunpriv
PROCEDURE GetHostPortFromSocket(
    s        : SocketT; 
    VAR host : InaddrT; 
    port     : UINT16) RAISES {RPC.Failed};

PROCEDURE SetAddr( 
    VAR addr : SockaddrInT;
    host     : InaddrT;
    port     : UINT16);

PROCEDURE GetUDPSocket (port : UINT16 := 0): SocketT
  RAISES {RPC.Failed};

*)

PROCEDURE Send (
    socket        : SocketT;
    READONLY data : REF ARRAY OF CHAR;
    VAR      len  : CARDINAL) 
  RAISES {RPC.Failed};

PROCEDURE SendTo (         
    socket          : SocketT;
    READONLY data   : REF ARRAY OF CHAR;
    VAR      len    : CARDINAL;
    READONLY toAddr : SockaddrInT)
  RAISES {RPC.Failed};

PROCEDURE Recv (         
    socket          : SocketT;
    READONLY data   : REF ARRAY OF CHAR;
    VAR      nBytes : CARDINAL): BOOLEAN
  RAISES{RPC.Failed};

PROCEDURE RecvFrom (         
    socket            : SocketT;
    READONLY data     : REF ARRAY OF CHAR;
    VAR      nBytes   : CARDINAL;
    VAR      fromAddr : SockaddrInT): BOOLEAN 
  RAISES{RPC.Failed};

PROCEDURE ConnectUDPSocket(
    s    : SocketT;     
    addr : SockaddrInT) 
  RAISES {RPC.Failed};

PROCEDURE Close (s: SocketT);
PROCEDURE Select(VAR s: SocketReadyT; seconds, useconds: CARDINAL):CARDINAL;

END RPCOS.
