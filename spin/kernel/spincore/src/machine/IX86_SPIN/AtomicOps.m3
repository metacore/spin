(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 25-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added blocking mutex synchronization primitives.
 *      Fixed major synchronization problem with RAS regions, where
 *      instructions past the store were included in the region.
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *)
UNSAFE (* imports external interface *)
MODULE AtomicOps EXPORTS AtomicOps, AtomicOpsPrivate;

IMPORT Strand, FastList, AtomicOpsExtern;
IMPORT IO, Dispatcher, DispatcherPrivate;
IMPORT CodeRegions, Region, Word;

(* Atomic inc and dec operations. They return the new value of the *)
(* variable in question. *)

PROCEDURE AtomicInc(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER =
  BEGIN
    RETURN AtomicOpsExtern.AtomicInc(a, delta);
  END AtomicInc;

PROCEDURE AtomicDec(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER =
  BEGIN
    RETURN AtomicOpsExtern.AtomicDec(a, delta);
  END AtomicDec;

PROCEDURE AtomicIncAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS =
  BEGIN
    RETURN AtomicOpsExtern.AtomicIncAddr(a, delta);
  END AtomicIncAddr;

PROCEDURE AtomicDecAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS =
  BEGIN
    RETURN AtomicOpsExtern.AtomicDecAddr(a, delta);
  END AtomicDecAddr;

PROCEDURE AtomicInc32(VAR a: Int32; delta: INTEGER := 1) : Int32 =
  BEGIN
    RETURN AtomicOpsExtern.AtomicInc32(a, delta);
  END AtomicInc32;

PROCEDURE AtomicDec32(VAR a: Int32; delta: INTEGER := 1) : Int32 =
  BEGIN
    RETURN AtomicOpsExtern.AtomicDec32(a, delta);
  END AtomicDec32;

PROCEDURE CompareAndSwap(v: REF REFANY; old, new: REFANY): BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.CompareAndSwap(v, old, new);
  END CompareAndSwap; 

PROCEDURE CompareAndSwapInt(VAR v: INTEGER; old,new: INTEGER):BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.CompareAndSwapInt(v, old, new);
  END CompareAndSwapInt;
 
PROCEDURE CompareAndSwapAddr(VAR v: ADDRESS; old, new: ADDRESS): BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.CompareAndSwapAddr(v, old, new);
  END CompareAndSwapAddr;
 
(* Atomic lock operations *)
PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.TryLock(lock);
  END TryLock; 

PROCEDURE Unlock(VAR lock: INTEGER) =
  BEGIN
    AtomicOpsExtern.Unlock(lock);
  END Unlock; 

PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.LockOrEnqueue(lock, strand);
  END LockOrEnqueue;

PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER =
  BEGIN
    RETURN AtomicOpsExtern.UnlockAndDequeue(lock);
  END UnlockAndDequeue;

(* Atomic list operations *)
PROCEDURE Dequeue(list: REF FastList.T) : FastList.T =
  BEGIN
    RETURN AtomicOpsExtern.Dequeue(list);
  END Dequeue; 

PROCEDURE Enqueue(elem: FastList.T; list: REF FastList.T) =
  BEGIN
    AtomicOpsExtern.Enqueue(elem, list);
  END Enqueue;
  
PROCEDURE EnqueueAddr(head, elem, link: ADDRESS) =
  BEGIN
    AtomicOpsExtern.EnqueueAddr(head, elem, link);
  END EnqueueAddr;

PROCEDURE InitKernelRASRegions(ras: CodeRegions.T) =
  VAR region: Region.T;
      begin: Word.T;
      end: Word.T;
  BEGIN
    begin := LOOPHOLE(AtomicOpsExtern.Enqueue, Word.T);
    end := LOOPHOLE(AtomicOpsExtern.Enqueue_end, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);
    
    begin := LOOPHOLE(AtomicOpsExtern.Dequeue, Word.T);
    end := LOOPHOLE(AtomicOpsExtern.Dequeue_end, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);

    begin := LOOPHOLE(AtomicOpsExtern.LockOrEnqueue_RASbegin, Word.T);
    end := LOOPHOLE(AtomicOpsExtern.LockOrEnqueue_RASend, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);

    begin := LOOPHOLE(AtomicOpsExtern.UnlockAndDequeue_RASbegin,Word.T);
    end := LOOPHOLE(AtomicOpsExtern.UnlockAndDequeue_RASend, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);
  END InitKernelRASRegions;

PROCEDURE Init (verbose: BOOLEAN) =
  BEGIN
    IF verbose THEN IO.Put("\tMachineAtomicOps:\n"); END;

    TRY
      (* Enqueue *)
      IF verbose THEN IO.Put("\t\tEnqueue\n"); END;
      DispatcherPrivate.Bypass(Enqueue, AtomicOpsExtern.Enqueue);
      DispatcherPrivate.Bypass(EnqueueAddr, AtomicOpsExtern.EnqueueAddr);

      (* Dequeue *)
      IF verbose THEN IO.Put("\t\tDequeue\n"); END;
      DispatcherPrivate.Bypass(Dequeue, AtomicOpsExtern.Dequeue);

      (* Unlock *)
      IF verbose THEN IO.Put("\t\tUnlock\n"); END;
      DispatcherPrivate.Bypass(Unlock,
			       AtomicOpsExtern.Unlock);

      (* TryLock *)
      IF verbose THEN IO.Put("\t\tTryLock\n"); END;
      DispatcherPrivate.Bypass(TryLock,
			       AtomicOpsExtern.TryLock);

      (* LockOrEnqueue *)
      IF verbose THEN IO.Put("\t\tLockOrEnqueue\n"); END;
      DispatcherPrivate.Bypass(LockOrEnqueue,
			       AtomicOpsExtern.LockOrEnqueue);

      (* UnlockAndDequeue *)
      IF verbose THEN IO.Put("\t\tUnlockAndDequeue\n"); END;
      DispatcherPrivate.Bypass(UnlockAndDequeue,
			       AtomicOpsExtern.UnlockAndDequeue);

      (* CompareAndSwap *)
      IF verbose THEN IO.Put("\t\tCompareAndSwap\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwap,
			       AtomicOpsExtern.CompareAndSwap);

      (* CompareAndSwapInt *)
      IF verbose THEN IO.Put("\t\tCompareAndSwapInt\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwapInt,
			       AtomicOpsExtern.CompareAndSwapInt);

      (* CompareAndSwapAddr *)
      IF verbose THEN IO.Put("\t\tCompareAndSwapAddr\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwapAddr,
			       AtomicOpsExtern.CompareAndSwap);

      (* AtomicDec32 *)
      IF verbose THEN IO.Put("\t\tAtomicDec32\n"); END;
      DispatcherPrivate.Bypass(AtomicDec32,
			       AtomicOpsExtern.AtomicDec32);

      (* AtomicInc32 *)
      IF verbose THEN IO.Put("\t\tAtomicInc32\n"); END;
      DispatcherPrivate.Bypass(AtomicInc32,
			       AtomicOpsExtern.AtomicInc32);

      (* AtomicDecAddr *)
      IF verbose THEN IO.Put("\t\tAtomicDecAddr\n"); END;
      DispatcherPrivate.Bypass(AtomicDecAddr,
			       AtomicOpsExtern.AtomicDecAddr);

      (* AtomicIncAddr *)
      IF verbose THEN IO.Put("\t\tAtomicIncAddr\n"); END;
      DispatcherPrivate.Bypass(AtomicIncAddr,
			       AtomicOpsExtern.AtomicIncAddr);

      (* AtomicDec *)
      IF verbose THEN IO.Put("\t\tAtomicDec\n"); END;
      DispatcherPrivate.Bypass(AtomicDec,
			       AtomicOpsExtern.AtomicDec);

      (* AtomicInc *)
      IF verbose THEN IO.Put("\t\tAtomicInc\n"); END;
      DispatcherPrivate.Bypass(AtomicInc,
			       AtomicOpsExtern.AtomicInc);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("AtomicOps.Init: dispatcher error.\n");
    END;
  END Init;

BEGIN
END AtomicOps.
