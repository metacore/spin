(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE LoopbackTunnel;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT If;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "loopback";
      CommandHelp = "-- -debug level| -synctoggle";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

(* Loopbacknet Maximum Transmission Unit *)
CONST MTU = LAST(CARDINAL);

PROCEDURE PacketSend(
    VAR ifp : If.ifnet;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.sockaddr;
    rte     : ADDRESS := NIL): Ctypes.int;

END LoopbackTunnel. 
