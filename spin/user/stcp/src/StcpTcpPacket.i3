(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

INTERFACE StcpTcpPacket;

IMPORT StcpIpPktFormat, StcpTcpPktFormat;
IMPORT StcpMbuf;
IMPORT Sema, StcpIf;
IMPORT Ctypes;

(* ---------------------------------------------------------------- *)
PROCEDURE PacketGuard(m, cur : StcpMbuf.T ; offset : CARDINAL ; ipHeader : StcpIpPktFormat.T) : BOOLEAN;
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL ; ipHeader : StcpIpPktFormat.T) : StcpMbuf.T;

(* ---------------------------------------------------------------- *)
(* those are temporary...  It's quite different from the socket interface *)
CONST
  CLOSED = 0;
  SYN_SENT = 1;
  ESTABLISHED = 2;
  FIN_WAIT1 = 3;
  FIN_WAIT2 = 4;
  TIME_WAIT = 5;
  CLOSE_WAIT = 6;
  LAST_ACK = 7;
  CLOSING = 8;

TYPE tcpStatus = RECORD
  status : CARDINAL;  (* listening, ...??? *)
  seq : StcpTcpPktFormat.Seq;
  ack_seq : StcpTcpPktFormat.Seq;
  mqueue : StcpIf.ifqueue;
  sema : Sema.T;
  semaEstablished : Sema.T;
  semaCloseWait : Sema.T;
  semaLastAck : Sema.T;
  sport : Ctypes.unsigned_short := 5490;
  dport : Ctypes.unsigned_short := 80;
END;

EXCEPTION
  NoPortAvailable(TEXT);
  NoResponseFromServer(TEXT);

(* ---------------------------------------------------------------- *)
PROCEDURE Connect() RAISES {NoPortAvailable, NoResponseFromServer}; 
PROCEDURE Close();
PROCEDURE Send(data : TEXT);
PROCEDURE Recv(VAR buf : REF ARRAY OF CHAR) RAISES {NoResponseFromServer};
PROCEDURE FreeMemory();

PROCEDURE Init();

END StcpTcpPacket.
