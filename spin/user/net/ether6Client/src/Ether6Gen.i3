(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

INTERFACE Ether6Gen;
IMPORT SocketAddr, Mbuf, Ctypes, NetDev, Net;

(* Ethernet Maximum Transmission Unit *)
CONST MTU = 1500;
PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE PacketSend(
    dev   : NetDev.T;
    mbuf  : Mbuf.T; 
    VAR s : SocketAddr.T;
    rte   : ADDRESS := NIL): Ctypes.int;

VAR
  debug_level: Net.Level := Net.oLevel.NODEBUG;
END Ether6Gen.
