(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 30-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Took out the atm_input upcall variable.  
 *	Needs to go back after SOSP.
 *
 *)

INTERFACE AtmPacket;
IMPORT Mbuf;
IMPORT If;

(* 
<* EXTERNAL "fore_input_upcall" *>
VAR atm_input: PROCEDURE( ifp: UNTRACED REF If.ifnet; m: Mbuf.T);
*)

PROCEDURE Arrived(ifp: UNTRACED REF If.ifnet; m: Mbuf.T);
  (* Takes an incoming packet from the atmnet and pushes it up
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


END AtmPacket.
