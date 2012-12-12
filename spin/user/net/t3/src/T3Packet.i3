(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE T3Packet;
IMPORT If;
IMPORT Mbuf;

PROCEDURE Arrived(ifp: UNTRACED REF If.ifnet; packet: Mbuf.T);
  (* Takes an incoming packet from the t3net and pushes it up
   * through the protocol decission tree.  *For asynchronous dispatch
   * enqueue the packet and kick the worker thread.  Otherwise, the
   * event handlers are invoked as part of the interrupt handler.
   * This support will be supported and checked by the SPIN
   * dispatcher.  Executed at Machine.Spl.High.
   *)

PROCEDURE Init();

(* function called to initialize M3 module.  Cannot rely on M3
 * strongly connected graph ordering to initialize modules in the
 * right order.
 *)


END T3Packet.
