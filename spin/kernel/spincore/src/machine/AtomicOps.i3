(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added CompareAndSwapAddr.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for blocking locks.
 *
 * 02-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Moved the setup functions to AtomicOpsPrivate so this interface
 *      can be exported to untrusted clients.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Interface to atomic operations.
 *)
INTERFACE AtomicOps;
IMPORT FastList, Ctypes, Strand;
TYPE Int32 = BITS 32 FOR Ctypes.int;

(* Atomic inc and dec operations. They return the new value of the *)
(* variable in question. *)
PROCEDURE AtomicInc(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER;
PROCEDURE AtomicDec(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER;

PROCEDURE AtomicIncAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS;
PROCEDURE AtomicDecAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS;

PROCEDURE AtomicInc32(VAR a: Int32; delta: INTEGER := 1) : Int32;
PROCEDURE AtomicDec32(VAR a: Int32; delta: INTEGER := 1) : Int32;

PROCEDURE CompareAndSwap(v: REF REFANY; old, new: REFANY): BOOLEAN;
PROCEDURE CompareAndSwapInt(VAR v: INTEGER; old,new: INTEGER):BOOLEAN;
PROCEDURE CompareAndSwapAddr(VAR a: ADDRESS; old, new: ADDRESS): BOOLEAN;

(* Atomic lock operations *)
PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN;
PROCEDURE Unlock(VAR lock: INTEGER);

PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER;
(* Atomic list operations *)
PROCEDURE Dequeue(list: REF FastList.T) : FastList.T;
PROCEDURE Enqueue(elem: FastList.T; list: REF FastList.T);
  
PROCEDURE EnqueueAddr(head, elem, link: ADDRESS);
(* *link := *head;
   *head := elem; *)

END AtomicOps.
