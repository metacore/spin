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
MODULE MachineAtomicOps EXPORTS MachineAtomicOps, MachineAtomicOpsPrivate;

IMPORT Strand, FastList, MachineAtomicOpsExtern;
IMPORT IO, Dispatcher, DispatcherPrivate;
IMPORT CodeRegions, Region, Word;

(* Atomic inc and dec operations. They return the new value of the *)
(* variable in question. *)

PROCEDURE AtomicInc(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicInc(a, delta);
  END AtomicInc;

PROCEDURE AtomicDec(VAR a: INTEGER; delta: INTEGER:= 1) : INTEGER =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicDec(a, delta);
  END AtomicDec;

PROCEDURE AtomicIncAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicIncAddr(a, delta);
  END AtomicIncAddr;

PROCEDURE AtomicDecAddr(VAR a: ADDRESS; delta: INTEGER:= 1) : ADDRESS =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicDecAddr(a, delta);
  END AtomicDecAddr;

PROCEDURE AtomicInc32(VAR a: Int32; delta: INTEGER := 1) : Int32 =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicInc32(a, delta);
  END AtomicInc32;

PROCEDURE AtomicDec32(VAR a: Int32; delta: INTEGER := 1) : Int32 =
  BEGIN
    RETURN MachineAtomicOpsExtern.AtomicDec32(a, delta);
  END AtomicDec32;

PROCEDURE CompareAndSwap(v: REF REFANY; old, new: REFANY): BOOLEAN =
  BEGIN
    RETURN MachineAtomicOpsExtern.CompareAndSwap(v, old, new);
  END CompareAndSwap; 

PROCEDURE CompareAndSwapInt(VAR v: INTEGER; old,new: INTEGER):BOOLEAN =
  BEGIN
    RETURN MachineAtomicOpsExtern.CompareAndSwapInt(v, old, new);
  END CompareAndSwapInt;
 
PROCEDURE CompareAndSwapAddr(VAR v: ADDRESS; old, new: ADDRESS): BOOLEAN =
  BEGIN
    RETURN MachineAtomicOpsExtern.CompareAndSwapAddr(v, old, new);
  END CompareAndSwapAddr;
 
(* Atomic lock operations *)
PROCEDURE TryLock(VAR lock: INTEGER) : BOOLEAN =
  BEGIN
    RETURN MachineAtomicOpsExtern.TryLock(lock);
  END TryLock; 

PROCEDURE Unlock(VAR lock: INTEGER) =
  BEGIN
    MachineAtomicOpsExtern.Unlock(lock);
  END Unlock; 

PROCEDURE LockOrEnqueue(VAR lock: INTEGER; strand: Strand.T) : BOOLEAN =
  BEGIN
    RETURN MachineAtomicOpsExtern.LockOrEnqueue(lock, strand);
  END LockOrEnqueue;

PROCEDURE UnlockAndDequeue(VAR lock: INTEGER) : INTEGER =
  BEGIN
    RETURN MachineAtomicOpsExtern.UnlockAndDequeue(lock);
  END UnlockAndDequeue;

(* Atomic list operations *)
PROCEDURE Dequeue(list: REF FastList.T) : FastList.T =
  BEGIN
    RETURN MachineAtomicOpsExtern.Dequeue(list);
  END Dequeue; 

PROCEDURE Enqueue(elem: FastList.T; list: REF FastList.T) =
  BEGIN
    MachineAtomicOpsExtern.Enqueue(elem, list);
  END Enqueue; 

PROCEDURE InitKernelRASRegions(ras: CodeRegions.T) =
  VAR region: Region.T;
      begin: Word.T;
      end: Word.T;
  BEGIN
    begin := LOOPHOLE(MachineAtomicOpsExtern.Enqueue, Word.T);
    end := LOOPHOLE(MachineAtomicOpsExtern.Enqueue_end, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);
    
    begin := LOOPHOLE(MachineAtomicOpsExtern.Dequeue, Word.T);
    end := LOOPHOLE(MachineAtomicOpsExtern.Dequeue_end, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);

    begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueue_RASbegin, Word.T);
    end := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueue_RASend, Word.T);
    region.begin := begin;
    region.length  := Word.Minus(end, begin);
    ras.add(region);

    begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeue_RASbegin,Word.T);
    end := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeue_RASend, Word.T);
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
      DispatcherPrivate.Bypass(Enqueue,
			       MachineAtomicOpsExtern.Enqueue);

      (* Dequeue *)
      IF verbose THEN IO.Put("\t\tDequeue\n"); END;
      DispatcherPrivate.Bypass(Dequeue,
			       MachineAtomicOpsExtern.Dequeue);

      (* Unlock *)
      IF verbose THEN IO.Put("\t\tUnlock\n"); END;
      DispatcherPrivate.Bypass(Unlock,
			       MachineAtomicOpsExtern.Unlock);

      (* TryLock *)
      IF verbose THEN IO.Put("\t\tTryLock\n"); END;
      DispatcherPrivate.Bypass(TryLock,
			       MachineAtomicOpsExtern.TryLock);

      (* LockOrEnqueue *)
      IF verbose THEN IO.Put("\t\tLockOrEnqueue\n"); END;
      DispatcherPrivate.Bypass(LockOrEnqueue,
			       MachineAtomicOpsExtern.LockOrEnqueue);

      (* UnlockAndDequeue *)
      IF verbose THEN IO.Put("\t\tUnlockAndDequeue\n"); END;
      DispatcherPrivate.Bypass(UnlockAndDequeue,
			       MachineAtomicOpsExtern.UnlockAndDequeue);

      (* CompareAndSwap *)
      IF verbose THEN IO.Put("\t\tCompareAndSwap\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwap,
			       MachineAtomicOpsExtern.CompareAndSwap);

      (* CompareAndSwapInt *)
      IF verbose THEN IO.Put("\t\tCompareAndSwapInt\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwapInt,
			       MachineAtomicOpsExtern.CompareAndSwapInt);

      (* CompareAndSwapAddr *)
      IF verbose THEN IO.Put("\t\tCompareAndSwapAddr\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwapAddr,
			       MachineAtomicOpsExtern.CompareAndSwap);

      (* AtomicDec32 *)
      IF verbose THEN IO.Put("\t\tAtomicDec32\n"); END;
      DispatcherPrivate.Bypass(AtomicDec32,
			       MachineAtomicOpsExtern.AtomicDec32);

      (* AtomicInc32 *)
      IF verbose THEN IO.Put("\t\tAtomicInc32\n"); END;
      DispatcherPrivate.Bypass(AtomicInc32,
			       MachineAtomicOpsExtern.AtomicInc32);

      (* AtomicDecAddr *)
      IF verbose THEN IO.Put("\t\tAtomicDecAddr\n"); END;
      DispatcherPrivate.Bypass(AtomicDecAddr,
			       MachineAtomicOpsExtern.AtomicDecAddr);

      (* AtomicIncAddr *)
      IF verbose THEN IO.Put("\t\tAtomicIncAddr\n"); END;
      DispatcherPrivate.Bypass(AtomicIncAddr,
			       MachineAtomicOpsExtern.AtomicIncAddr);

      (* AtomicDec *)
      IF verbose THEN IO.Put("\t\tAtomicDec\n"); END;
      DispatcherPrivate.Bypass(AtomicDec,
			       MachineAtomicOpsExtern.AtomicDec);

      (* AtomicInc *)
      IF verbose THEN IO.Put("\t\tAtomicInc\n"); END;
      DispatcherPrivate.Bypass(AtomicInc,
			       MachineAtomicOpsExtern.AtomicInc);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("Dispatcher error during initialization of MachineAtomicOps\n");
    END;
  END Init;

BEGIN
END MachineAtomicOps.



