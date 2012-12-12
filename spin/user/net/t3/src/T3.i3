(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE T3;
IMPORT Net;
IMPORT Mbuf;

TYPE T = Net.Payload;
CONST NumArgs = 2;

TYPE PacketArrivedEvent = PROCEDURE (READONLY packet: Mbuf.T; READONLY payload: T):BOOLEAN;
PROCEDURE PacketArrived(READONLY packet: Mbuf.T; READONLY payload: T):BOOLEAN;
(* Defines the T3.PacketArrived event definition *)
END T3.
