(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added explicit Init()ialization function called from EtherClient.
 *	Obsoleted the errors used by this module.
 *
 *)

INTERFACE EtherGen;
IMPORT SocketAddr, Mbuf, Ctypes, NetDev, Net;

(* Ethernet Maximum Transmission Unit *)
CONST MTU = 1500;

PROCEDURE Init(verbose:BOOLEAN);

PROCEDURE PacketSend(
    dev: NetDev.T;
    mbuf    : Mbuf.T; 
    VAR s   : SocketAddr.T;
    rte     : ADDRESS := NIL): Ctypes.int;

VAR
  debug_level: Net.Level := Net.oLevel.NODEBUG;
END EtherGen.
