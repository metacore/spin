(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Defines the T3 event.  Does nothing, yet.
 *
 *)

(* Untrusted *) 
MODULE T3;
IMPORT Mbuf;
PROCEDURE PacketArrived(<* UNUSED *> READONLY packet: Mbuf.T; <* UNUSED *> READONLY payload: T):BOOLEAN =
  BEGIN
    RETURN FALSE;
  END PacketArrived;
BEGIN
END T3.
