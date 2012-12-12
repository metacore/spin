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
 * 09-Apr-97  Tsutomu Owa (owa)  at the University of Washington
 *	Get packets from user/stcp.
 *
 * 02-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Put ether output here.
 *
 *)

INTERFACE EtherPacket;
IMPORT Mbuf, EtherDev, ParseParams, Ctypes;

(* shell command support *)
CONST CommandName = "etherpacket";
      CommandHelp = "-- -debug level| -synctoggle | -profile";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

PROCEDURE Arrived(dev: EtherDev.T; m: Mbuf.T);
(* Takes an incoming packet from the ethernet and pushes it up through
   the protocol decission tree.  *For asynchronous dispatch enqueue
   the packet and kick the worker thread.  Otherwise, the event
   handlers are invoked as part of the interrupt handler.  This
   support will be supported and checked by the SPIN dispatcher.
   Executed at CPU.InterruptLevel.High.  *)

PROCEDURE Init();
(* function called to initialize M3 module.  Cannot rely on M3
   strongly connected graph ordering to initialize modules in the
   right order.  *)

PROCEDURE Output( dev: EtherDev.T; m: Mbuf.T): Ctypes.int;

END EtherPacket.
