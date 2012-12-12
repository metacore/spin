(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE LoopbackGen;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT If;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "loopbackgen";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

(* Loopbacknet Maximum Transmission Unit *)
CONST MTU = LAST(CARDINAL);

PROCEDURE Init();

PROCEDURE PacketSend(
    VAR ifp : If.ifnet;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.sockaddr;
    rte     : ADDRESS := NIL): Ctypes.int;

END LoopbackGen.

