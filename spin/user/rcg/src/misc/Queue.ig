(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

GENERIC INTERFACE Queue(Elem);

TYPE
  T <: REFANY;

PROCEDURE NewQ(): T;

PROCEDURE Enqueue(q: T; e: Elem.T);
PROCEDURE EnqueueUnique(q: T; e: Elem.T);

PROCEDURE Dequeue(q: T): Elem.T;

PROCEDURE IsEmpty(q: T): BOOLEAN;

END Queue.
