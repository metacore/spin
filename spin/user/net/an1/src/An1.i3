(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE An1;
IMPORT Net;
IMPORT Mbuf;

TYPE T = Net.Payload;
CONST NumArgs = 2;

PROCEDURE PacketArrived(READONLY packet: Mbuf.T; READONLY payload: T):BOOLEAN;
(* Defines the An1.PacketArrived event definition *)
END An1.
