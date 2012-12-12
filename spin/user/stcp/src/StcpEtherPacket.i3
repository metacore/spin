(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created for simpile tcp.
 *	Some code came from user/net/ether/src/StcpEtherPacket.i3 
 *)

INTERFACE StcpEtherPacket;
IMPORT StcpMbuf;
IMPORT Ctypes;
IMPORT StcpSocketAddr;
IMPORT StcpEtherDev;
IMPORT Dispatcher;

VAR
  etherhandler: Dispatcher.Binding;

PROCEDURE ArrivedGuard(dev: StcpEtherDev.T; m: StcpMbuf.T): BOOLEAN;
  (* return TRUE if pkt is for stcp *)

PROCEDURE Arrived(dev: StcpEtherDev.T; m: StcpMbuf.T);
  (* Takes an incoming packet from the ethernet and pushes it up
   * through the protocol decission tree.  *For asynchronous dispatch
   * enqueue the packet and kick the worker thread.  Otherwise, the
   * event handlers are invoked as part of the interrupt handler.
   * This support will be supported and checked by the SPIN
   * dispatcher.  Executed at CPU.InterruptLevel.High.
   *)

PROCEDURE Init();
(* function called to initialize M3 module.  Cannot rely on M3
   strongly connected graph ordering to initialize modules in the
   right order.  *)

PROCEDURE Output( dev: StcpEtherDev.T; m: StcpMbuf.T): Ctypes.int;

PROCEDURE PacketSend(
  VAR ifp : StcpEtherDev.T;
  mbuf    : StcpMbuf.T;
  VAR s   : StcpSocketAddr.T;
  rte: ADDRESS) : Ctypes.int;
(* ---------------------------------------------------------------- *)
(*
PROCEDURE PacketArrived(m, cur : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T;
*)

END StcpEtherPacket.
