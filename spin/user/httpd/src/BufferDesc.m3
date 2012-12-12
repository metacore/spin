(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added buckets for differently sized buffers.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Buffer manager and recycler.
 *)
MODULE BufferDesc;
IMPORT AtomicOps, FastList, IO;

VAR 
  numfree : INTEGER := 0;
  freelist: REF FastList.T;

PROCEDURE Allocate() : T =
  VAR
    bd: T;
  BEGIN
    bd := FastList.Dequeue(freelist);
    IF bd = NIL THEN
      bd := NEW(T);
    ELSE
      EVAL AtomicOps.AtomicDec(numfree);
    END;
    RETURN bd;
  END Allocate;

PROCEDURE Deallocate(bufferdesc: T) =
  BEGIN
    IF AtomicOps.AtomicInc(numfree) > maxBufferDesc THEN
      (* 
       * Too many, give them back by dropping them on the floor
       * and letting GC collect them.
       *     give some sort of hint to gc.
       *     DISPOSE(bufferdesc);
       *)
    ELSE
      FastList.Enqueue(bufferdesc, freelist);
    END;
  END Deallocate;

BEGIN
  freelist := NEW(REF FastList.T);
  IO.Put("Buffer descriptors are initialized...\n");
END BufferDesc.
