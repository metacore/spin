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
 *	added WRITEBARRIER flage.
 *
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

(* reference counting collector interface *)
INTERFACE RTRefCount;

CONST
  RefCount = FALSE;
  WRITEBARRIER = TRUE; (* all caps so it doesn't conflict with 
                           real write barrier proc *)

PROCEDURE ProcessOutstanding (free: BOOLEAN);
(* Process all the outstanding buffers *)

PROCEDURE Decrement (lptr: ADDRESS);
(* Decrement reference count of an object *)

PROCEDURE Increment (rptr: ADDRESS);
(* Increment reference count of an object *)

PROCEDURE FreeZeroObjects ();

PROCEDURE Check ();

PROCEDURE Init ();
(* Initialize reference counting, call during heap initialization *)

END RTRefCount.
