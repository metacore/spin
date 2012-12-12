(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Defines the An1 event.  Does nothing, yet.
 *
 *)

(* Untrusted *) 
MODULE An1;
IMPORT Mbuf;
PROCEDURE PacketArrived(READONLY packet: Mbuf.T; READONLY payload: T):BOOLEAN =
  BEGIN
    RETURN FALSE;
  END PacketArrived;
BEGIN
END An1.
