(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 15-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	added WRITEBARRIER flag.
 *
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

(* reference counting collector interface *)
UNSAFE INTERFACE RTRefCount;

TYPE
  Transaction = RECORD
    lhs, rhs: ADDRESS;
  END;
  (* what's written into the log by the compiler generated code *)

CONST
  RefCount = TRUE;
  WRITEBARRIER = FALSE;

VAR
  inCollector := FALSE;
  (* do not reenter the collector *)

VAR
  inMutator: BOOLEAN := FALSE;
  (* do not reenter the code that swaps and processes buffers *)

PROCEDURE Mutator (addr: ADDRESS; pc: ADDRESS);
PROCEDURE EnableTraps ();
PROCEDURE FS (lloc, lptr, rptr, pc: ADDRESS);

PROCEDURE ProcessOutstanding (free: BOOLEAN);
(* Process all the outstanding buffers *)

PROCEDURE Init ();
(* Initialize reference counting, call during heap initialization *)

PROCEDURE Decrement (lptr: ADDRESS);
(* Decrement reference count of an object *)

PROCEDURE Increment (rptr: ADDRESS);
(* Increment reference count of an object *)

PROCEDURE FreeZeroObjects ();

PROCEDURE Check ();

END RTRefCount.
