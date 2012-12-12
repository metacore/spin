(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 27-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make operations EPHEMERAL to match interface declarations
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Fast list implementation based on RAS.
 *
 * This module is unsafe because it imports AtomicOps.
 *
 *)
UNSAFE MODULE FastList;

IMPORT AtomicOps;

EPHEMERAL
PROCEDURE Dequeue(list: REF T) : T =
  BEGIN
     RETURN AtomicOps.Dequeue(list);
  END Dequeue;

EPHEMERAL
PROCEDURE Enqueue(elem: T; list: REF T) =
   BEGIN
     AtomicOps.Enqueue(elem, list);
   END Enqueue;

BEGIN
END FastList.
