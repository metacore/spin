(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE T3Gen;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT If;

CONST MTU = 8 * 1024;
(* These errors are unused so far. *)
<* OBSOLETE *> 
TYPE Errors = { NONE,  HOST_UNREACHABLE,  ALREADY_USED,  PERMISSION_DENIED};

PROCEDURE PacketSend(
    VAR ifp : If.ifnet;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.M3sockaddr;
    rte     : ADDRESS := NIL): Ctypes.int;
END T3Gen.

