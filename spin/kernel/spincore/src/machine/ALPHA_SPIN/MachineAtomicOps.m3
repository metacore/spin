(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Shortened one RAS for writebarrier.
 *
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added new set of RAS-es for writebarrier
 *
 * 26-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed sizes of LockOrEnqueue and UnlockAndDequeue RAS-es.
 *
 * 19-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added CompareAndSwapAddr(), added conditionals to distinguish
 *	between regular procedures and their reference counting versions.
 *	Renamed RTCollectorSpinExtern to RTMachineCollectorExtern.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Added support for blocking locks.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled bypassing. 
 *
 * 20-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed CompareAndSwapInt to take a VAR argument for easier use.
 *
 * 29-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Pass through Functions are now bypassed using the dispatcher.
 *	That is, these procedure calls go directly through to their
 *	EXTERNAL.
 *
 * 16-Mar-96  Stefan Savage (savage) at the University of Washington
 *	Wrapper for machine dependent atomic operations.
 *
 *)
UNSAFE (* imports external interfaces *)
MODULE MachineAtomicOps EXPORTS MachineAtomicOps, MachineAtomicOpsPrivate;
IMPORT Strand, FastList, MachineAtomicOpsExtern;
IMPORT IO, DispatcherPrivate, Dispatcher;
IMPORT CodeRegions, Region, Word;
IMPORT RTMachineCollectorExtern, RTHeapRep, RTHeapStats;

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
  CONST InstSize = 4;
  VAR region: Region.T;
  BEGIN
    (*
     * The following routines are Restartable Atomic Sequences (RAS).
     * Their correctness depends on their start pc address and the
     * number of instructions that need to be atomic wrt interrupts
     * and preemption.  The region.{begin,length} values must be in
     * sync with the routines defined in AtomicOps.s.  All these loopholes
     * are safe.
     *)
    region.begin := LOOPHOLE(MachineAtomicOpsExtern.TryLock, Word.T);
    region.length  := 4 * InstSize;
    ras.add(region);

    (* atomic sections which do traced ref assignements require special 
       versions if reference counting is enabled *)
    IF NOT RTHeapRep.RefCount AND NOT RTHeapRep.WRITEBARRIER THEN
      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueue, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);
      region.begin := Word.Plus(region.begin, 5 * InstSize);
      region.length  := 7 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeue,Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);
      region.begin := Word.Plus(region.begin, 6 * InstSize);
      region.length  := 8 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.Enqueue, Word.T);
      region.length  := 3 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.Dequeue, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.CompareAndSwap, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);
    END;

    (*
    region.beginpc := LOOPHOLE(Delete, Word.T); (* SAFE *)
    region.length  := 5 * InstSize;
    ras.Add(region);
    *)

    region.begin := LOOPHOLE(MachineAtomicOpsExtern.AtomicInc, Word.T);
    region.length  := 3 * InstSize;
    ras.add(region);

    region.begin := LOOPHOLE(MachineAtomicOpsExtern.AtomicDec, Word.T);
    region.length  := 3 * InstSize;
    ras.add(region);

    region.begin := LOOPHOLE(MachineAtomicOpsExtern.AtomicInc32, Word.T);
    region.length  := 3 * InstSize;
    ras.add(region);

    region.begin := LOOPHOLE(MachineAtomicOpsExtern.AtomicDec32, Word.T);
    region.length  := 3 * InstSize;
    ras.add(region);

    region.begin := LOOPHOLE(MachineAtomicOpsExtern.CompareAndSwapInt, Word.T);
    region.length  := 4 * InstSize;
    ras.add(region);

    (* regular reference counting atomic sections *)
    IF RTHeapRep.RefCount THEN
      region.begin := LOOPHOLE(RTMachineCollectorExtern.UpdateRC, Word.T);
      region.length  := RTMachineCollectorExtern.UpdateRCRASLength * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.EnqueueRC1, Word.T);
      region.length  := RTMachineCollectorExtern.EnqueueRC1RASLength*InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.EnqueueRC2, Word.T);
      region.length  := RTMachineCollectorExtern.EnqueueRC2RASLength*InstSize;
      ras.add(region);
    END;

    (* counting versions of atomic sections which do reference assignements *)
    IF RTHeapRep.RefCount THEN
      region.begin := LOOPHOLE(MachineAtomicOpsExtern.EnqueueUpd, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.EnqueueLog, Word.T);
      region.length  := 5 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.DequeueUpd, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.DequeueLog, Word.T);
      region.length  := 5 * InstSize;
      ras.add(region);

      region.begin :=LOOPHOLE(MachineAtomicOpsExtern.CompareAndSwapUpd,Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin :=LOOPHOLE(MachineAtomicOpsExtern.CompareAndSwapLog,Word.T);
      region.length  := 5 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueueUpd1,
                               Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueueUpd2,
                               Word.T);
      region.length  := 6 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueueLog,
                               Word.T);
      region.length  := 5 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeueUpd1,
                               Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeueUpd2,
                               Word.T);
      region.length  := 3 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeueLog,
                               Word.T);
      region.length  := 5 * InstSize;
      ras.add(region);
    END;

    IF RTHeapRep.WRITEBARRIER THEN
      region.begin := LOOPHOLE(MachineAtomicOpsExtern.EnqueueUpd, Word.T);
      region.length  := 3 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.DequeueUpd, Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin :=LOOPHOLE(MachineAtomicOpsExtern.CompareAndSwapUpd,Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueueUpd1,
                               Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.LockOrEnqueueUpd2,
                               Word.T);
      region.length  := 7 * InstSize; (* could be 7 *)
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeueUpd1,
                               Word.T);
      region.length  := 4 * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(MachineAtomicOpsExtern.UnlockAndDequeueUpd2,
                               Word.T);
      region.length  := 8 * InstSize; 
      ras.add(region);

    END;

    IF RTHeapStats.DoTracing THEN
      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS0, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS0Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS1, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS1Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS2, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS2Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS3, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS3Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS4, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS4Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS5, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS5Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS6, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS6Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS7, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS7Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.TraceRAS8, Word.T);
      region.length  := RTMachineCollectorExtern.TraceRAS8Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS0, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS0Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS1, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS1Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS2, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS2Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS3, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS3Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS4, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS4Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS5, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS5Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS6, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS6Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS7, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS7Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.GlobalRAS8, Word.T);
      region.length  := RTMachineCollectorExtern.GlobalRAS8Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.CountRAS1, Word.T);
      region.length  := RTMachineCollectorExtern.CountRAS1Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.CountRAS2, Word.T);
      region.length  := RTMachineCollectorExtern.CountRAS2Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.CountRAS3, Word.T);
      region.length  := RTMachineCollectorExtern.CountRAS3Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.CountRAS4, Word.T);
      region.length  := RTMachineCollectorExtern.CountRAS4Length * InstSize;
      ras.add(region);

      region.begin := LOOPHOLE(RTMachineCollectorExtern.CountRAS5, Word.T);
      region.length  := RTMachineCollectorExtern.CountRAS5Length * InstSize;
      ras.add(region);
    END;
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

      (* TryLock *)
      IF verbose THEN IO.Put("\t\tTryLock\n"); END;
      DispatcherPrivate.Bypass(TryLock,
                               MachineAtomicOpsExtern.TryLock);

      (* Unlock *)
      IF verbose THEN IO.Put("\t\tUnlock\n"); END;
      DispatcherPrivate.Bypass(Unlock,
                               MachineAtomicOpsExtern.Unlock);

      (* LockOrEnqueue *)
      IF verbose THEN IO.Put("\t\tLockOrEnqueue\n"); END;
      DispatcherPrivate.Bypass(LockOrEnqueue,
                               MachineAtomicOpsExtern.LockOrEnqueue);

      (* UnlockAndDequeue *)
      IF verbose THEN IO.Put("\t\tUnlockAndDequeue\n"); END;
      DispatcherPrivate.Bypass(UnlockAndDequeue,
                               MachineAtomicOpsExtern.UnlockAndDequeue);

      (* CompareAndSwapInt *)
      IF verbose THEN IO.Put("\t\tCompareAndSwapInt\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwapInt,
                               MachineAtomicOpsExtern.CompareAndSwapInt);

      (* CompareAndSwap *)
      IF verbose THEN IO.Put("\t\tCompareAndSwap\n"); END;
      DispatcherPrivate.Bypass(CompareAndSwap,
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
