(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)
UNSAFE (* for externals *)
INTERFACE MachineAtomicOpsExtern;
IMPORT Strand, FastList, Ctypes;
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
<*EXTERNAL*>PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN;
<*EXTERNAL*>PROCEDURE Unlock(VAR lock: INTEGER);

<*EXTERNAL*>PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN;
<*EXTERNAL*>PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER;


(* Atomic list operations *)
<*EXTERNAL*>PROCEDURE Dequeue(list: REF FastList.T) : FastList.T;
<*EXTERNAL*>PROCEDURE Enqueue(elem: FastList.T; list: REF FastList.T);

(* for setting up ras regions *)
<*EXTERNAL*>PROCEDURE UnlockAndDequeue_RASbegin();
<*EXTERNAL*>PROCEDURE UnlockAndDequeue_RASend();

<*EXTERNAL*>PROCEDURE LockOrEnqueue_RASbegin();
<*EXTERNAL*>PROCEDURE LockOrEnqueue_RASend();

<*EXTERNAL*>PROCEDURE Dequeue_end();
<*EXTERNAL*>PROCEDURE Enqueue_end();

END MachineAtomicOpsExtern.
