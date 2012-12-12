(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE An1Gen;
IMPORT IpPktFormat;
IMPORT An1PktFormat;
IMPORT An1Arp;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT If;

CONST MTU = 1500;

<* OBSOLETE *> TYPE Errors = { NONE,  HOST_UNREACHABLE,  ALREADY_USED,  PERMISSION_DENIED};
(* These errors are unused so far. *)

PROCEDURE PacketSend(
    VAR ifp : If.ifnet;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.M3sockaddr;
    rte     : ADDRESS := NIL): Ctypes.int;
END An1Gen.
