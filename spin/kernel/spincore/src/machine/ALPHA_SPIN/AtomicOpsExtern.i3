(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 *	Added writebarrier version of EnqueueAddr.
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added declarations of reference counting versions of atomic
 *	operations that make trace reference assignements.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for blocking locks.
 *
 * 20-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed CompareAndSwapInt to take a VAR argument for easier use.
 *
 * 21-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added atomic increment for 32 bit integers.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created Fast list implementation based on RAS.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created atomic ras increment and decrement support.
 *)
UNSAFE (* for externals *)
INTERFACE AtomicOpsExtern;
IMPORT FastList, Ctypes, Strand;
TYPE Int32 = BITS 32 FOR Ctypes.int;

(* Atomic inc and dec operations. They return the new value of the *)
(* variable in question. *)
<*EXTERNAL*>PROCEDURE AtomicInc(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER;
<*EXTERNAL*>PROCEDURE AtomicDec(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER;

<*EXTERNAL AtomicInc*>PROCEDURE AtomicIncAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS;
<*EXTERNAL AtomicDec*>PROCEDURE AtomicDecAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS;

<*EXTERNAL*>PROCEDURE AtomicInc32(VAR a: Int32; delta: INTEGER := 1) : Int32;
<*EXTERNAL*>PROCEDURE AtomicDec32(VAR a: Int32; delta: INTEGER := 1) : Int32;

<*EXTERNAL*>PROCEDURE CompareAndSwap(v: REF REFANY; old, new: REFANY): BOOLEAN;
<*EXTERNAL*>PROCEDURE CompareAndSwapInt(VAR v: INTEGER; old,new: INTEGER):BOOLEAN;
<*EXTERNAL CompareAndSwap *>
PROCEDURE CompareAndSwapAddr(VAR a: ADDRESS; old, new: ADDRESS): BOOLEAN;

(* Atomic lock operations *)
<*EXTERNAL RAS_TryLock*>PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN;
<*EXTERNAL*>PROCEDURE Unlock(VAR lock: INTEGER);

<*EXTERNAL*>PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
<*EXTERNAL*>PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER;


(* Atomic list operations *)
<*EXTERNAL*>PROCEDURE Dequeue(list: REF FastList.T) : FastList.T;
<*EXTERNAL*>PROCEDURE Enqueue(elem: FastList.T; list: REF FastList.T);
<*EXTERNAL*>PROCEDURE EnqueueAddr(head, elem, link: ADDRESS);

(* some of the above with reference counting *)
<*EXTERNAL*>PROCEDURE DequeueUpd(list: REF FastList.T) : FastList.T;
<*EXTERNAL*>PROCEDURE EnqueueUpd(elem: FastList.T; list: REF FastList.T);
<*EXTERNAL*>PROCEDURE EnqueueAddrUpd(elem: FastList.T; list: REF FastList.T);
<*EXTERNAL*>PROCEDURE DequeueLog(list: REF FastList.T) : FastList.T;
<*EXTERNAL*>PROCEDURE EnqueueLog(elem: FastList.T; list: REF FastList.T);
<*EXTERNAL*>PROCEDURE CompareAndSwapUpd(v: REF REFANY; old, new: REFANY): BOOLEAN;
<*EXTERNAL*>PROCEDURE CompareAndSwapLog(v: REF REFANY; old, new: REFANY): BOOLEAN;
<*EXTERNAL*>PROCEDURE LockOrEnqueueUpd1(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
<*EXTERNAL*>PROCEDURE LockOrEnqueueUpd2(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
<*EXTERNAL*>PROCEDURE LockOrEnqueueLog(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
<*EXTERNAL*>PROCEDURE UnlockAndDequeueUpd1(VAR lock: INTEGER) : INTEGER;
<*EXTERNAL*>PROCEDURE UnlockAndDequeueUpd2(VAR lock: INTEGER) : INTEGER;
<*EXTERNAL*>PROCEDURE UnlockAndDequeueLog(VAR lock: INTEGER) : INTEGER;

END AtomicOpsExtern.
