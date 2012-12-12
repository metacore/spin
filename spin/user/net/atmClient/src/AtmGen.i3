(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE AtmGen;
IMPORT Mbuf;
IMPORT If;
IMPORT Ctypes;
IMPORT SocketRep;

CONST MTU = 8 * 1024;

PROCEDURE PacketSend(
    VAR ifp: If.ifnet;
    mbuf: Mbuf.T;
    VAR s: SocketRep.M3sockaddr;
    rt: ADDRESS := NIL): Ctypes.int;

  (* READONLY mbuf:Mbuf.T;  type: AtmPktFormat.AtmType := AtmPktFormat.ATMTYPE_IP*)
(* Sendout IP packet over the fore device.  The fore device driver
   implements its own ARP support and communication with the switch.
   The fore driver only handles IP packets.  

  *)

END AtmGen.
