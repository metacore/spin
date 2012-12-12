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
 *)

INTERFACE LoopbackTunnel;
IMPORT SocketRep;
IMPORT Mbuf;
IMPORT Ctypes;
IMPORT NetDev;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "loopback";
      CommandHelp = "-- -debug level| -synctoggle";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

(* Loopbacknet Maximum Transmission Unit *)
CONST MTU = LAST(Ctypes.unsigned_short);

PROCEDURE PacketSend(
    dev: NetDev.T;
    mbuf    : Mbuf.T; 
    VAR s   : SocketRep.sockaddr;
    rte     : ADDRESS := NIL): Ctypes.int;

PROCEDURE Init();

END LoopbackTunnel. 
