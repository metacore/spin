(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

UNSAFE MODULE RTRefCount EXPORTS RTRefCount, RTMachineCollectorExtern;

IMPORT Word, RT0, RTOS, RTIO, AddrIntUtbl, RTOSMachine, RTHeapDep, RTHeapRep;
IMPORT RTType, Thread, ThreadF, RTStrongRef, RTMachine;
IMPORT RTCollectorSRC;
FROM RTHeapRep IMPORT verbose (*, doSanity*), TrackRef, TrackTC;

(* WARNING: the following number of pages must be manually synchronized with
   SpinProgram.c in kernel/start *)
CONST
  BufferPageSize = 19;                         (* number of pages per buffer *)
  PageSize = RTHeapRep.BytesPerPage;
  BufferFullPageSize = BufferPageSize + 1;     (* one extra for the sentinel *)
  BufferByteSize = BufferPageSize * PageSize;  (* number of bytes for a log  *)
  BufferSize = BufferByteSize DIV ADRSIZE(Transaction); (* num. log entries  *)

CONST
  InitSize = 5;

TYPE
  TransactionBuffer = UNTRACED REF RECORD
    size     : INTEGER;                         (* number of written entries *)
    buffer   : UNTRACED REF Transaction := NIL; (* begining of the log *)
    sentinel : ADDRESS := NIL;                  (* first addr. after the log *)
  END;
  (* a descriptor that maintains information about the current log buffer
     area, buffer is the pointer to the log, size is its size after it was
     deactivated (it is not maintained by the logging code)
     don't try to deallocate CurrentBuffer.buffer: it is not malloc-ed!!! *)

TYPE
  TBArray = UNTRACED REF ARRAY OF TransactionBuffer;
  (* if first=last, buffer is empty otherwise, buffer has elements
     from first -> (last-1) mod size overflow when last = (first-1) mod
     size one element of buffer is wasted, oh well *)

(* multiple threads that can access these data structures at a time

   mutator can trap on the current buffer calling into Mutator ()
   which queues the current buffer and demands a new buffer
       CurrentBuffer -> QueuedRingBuffer
       FreeRingBuffer -> CurrentBuffer

   collector thread can be working through the queued buffer, and
   put them on the free queue (the Collector() procedure)
       QueuedRingBuffer -> FreeRingBuffer

   We may not use any MUTEXes in this code because the Mutator()
   can be called from an interrup and with spl-high.  We therefore
   use spl-s to guard the code.  We could use non-blocking synchronization
   instead.  The commented out mutex calls demark the necessary 
   synchronization places.
*)

VAR
  CurrentBuffer  : TransactionBuffer;
  (* CurrentMutex: MUTEX; *)
  (* buffer currently being written to *)

VAR
  FreeRingBuffer: TBArray;
  FBFirst, FBNext : CARDINAL;
  (* FMutex : MUTEX;*)

VAR
  QueuedRingBuffer: TBArray;
  QBFirst, QBNext : CARDINAL;
  (* QMutex : MUTEX;*)

VAR
  OverflowTable : AddrIntUtbl.Default;
  (* keeps track of overflowed ref counts *)

VAR
  collectorThread : Thread.T;
  mu              : MUTEX;
  cond            : Thread.Condition;
  noBuffer        : BOOLEAN;
  
(*
 * This procedure is called when the buffer overflows.
 * We detect this by a transaction trapping on a write to 
 * protected page we have stuck right after the transaction buffer.
 * This procedure should be run at spl-high
 * 
 * CurrentBuffer -> QueuedRingBuffer
 * FreeBuffers -> CurrentRingBuffer
 *)

PROCEDURE Mutator (addr: ADDRESS; pc: ADDRESS) =
  BEGIN
    (* LOCK CurrentMutex DO *)
    RTOS.LockHeap();
    (* RTIO.PutText("<M>");*)
    (*
    RTIO.PutInt((TransactionQueue - CurrentBuffer.buffer) DIV
                     ADRSIZE(Transaction));
    RTIO.PutText(">");
    *)
    IF verbose > 1 THEN RTIO.PutText("GC-RC >> Mutator\n"); END;
    IF inMutator THEN
      RTIO.PutText("GC-RC ERROR >> mutator entered recursively\n");
      RTOS.Crash();
    END;
    IF addr # TransactionQueue THEN 
      RTIO.PutText("GC-RC ERROR >> trap not on the transaction Queue:\n");
      RTIO.PutAddr(TransactionQueue); RTIO.PutText(" - ");
      RTIO.PutAddr(addr); RTIO.PutText("; @ ");
      RTIO.PutAddr(pc); RTIO.PutText("\n");
      RTOS.Crash();
    END;
    IF CurrentBuffer = NIL THEN
      RTIO.PutText("GC-RC ERROR >> uninitialized mutator buffers\n");
      RTOS.Crash();
    END;
    IF TransactionQueue # CurrentBuffer.sentinel THEN 
      RTIO.PutText("GC-RC ERROR >> new buffer was swapped non-atomically:\n");
      RTIO.PutAddr(TransactionQueue); RTIO.PutText(" @ ");
      RTIO.PutAddr(addr); RTIO.PutText("\n");
      RTIO.PutAddr(CurrentBuffer.buffer); RTIO.PutText(" - ");
      RTIO.PutAddr(CurrentBuffer.sentinel); RTIO.PutText("\n");
      RTOS.Crash();
    END;

    (* enqueue the buffer *)
    QueueTB (CurrentBuffer);

    (* get a new buffer and reset the log to the begining of the new buffer *)
    CurrentBuffer := GetFreeTB ();
    TransactionQueue := CurrentBuffer.buffer;
    TransactionQueueEnd := CurrentBuffer.buffer + BufferByteSize;

    IF verbose > 2 THEN
      RTIO.PutText("TQ: "); RTIO.PutAddr(TransactionQueue);
      RTIO.PutText(" - "); RTIO.PutAddr(TransactionQueueEnd); 
      RTIO.PutText("\n");
    END;

    (* trigger the collector *)
    noBuffer := FALSE;
    (* Thread.Signal(cond);*)

    IF verbose > 1 THEN RTIO.PutText("GC-RC >> Mutator done\n"); END;
    RTOS.UnlockHeap();
    (* END; *)
  END Mutator;


(* capture what has accumulated so far and process it *)
PROCEDURE ProcessOutstanding (free: BOOLEAN) =
  BEGIN
    (* LOCK CurrentMutex DO *)
    RTOS.LockHeap();
    IF verbose > 1 THEN RTIO.PutText("GC-RC >> ProcessOutstanding\n"); END;
    IF inMutator THEN
      RTIO.PutText("GC-RC ERROR >> mutator entered recursively\n");
      RTOS.Crash();
    END;
    (* for sanity checking *)
    inMutator := TRUE;

    (* set the size of the buffer to what's accumulated there *)
    CurrentBuffer.size := (TransactionQueue - CurrentBuffer.buffer) DIV
                           ADRSIZE(Transaction);

    (* put it in the queue *)
    QueueTB (CurrentBuffer);

    (* get a new buffer and reset the log to the begining of the new buffer *)
    CurrentBuffer := GetFreeTB();
    TransactionQueue := CurrentBuffer.buffer;
    TransactionQueueEnd := CurrentBuffer.buffer + BufferByteSize;

    IF verbose > 2 THEN
      RTIO.PutText("TQ: "); RTIO.PutAddr(TransactionQueue);
      RTIO.PutText(" - "); RTIO.PutAddr(TransactionQueueEnd); 
      RTIO.PutText("\n");
    END;

    (* process the logs *)
    Collector(NIL, free);

    IF verbose > 1 THEN RTIO.PutText("GC-RC >> ProcessOutstanding done\n");END;
    inMutator := FALSE;
    RTOS.UnlockHeap();
    (* END; *)
  END ProcessOutstanding;

(*
 * QueuedRingBuffer -> FreeRingBuffer
 *)
PROCEDURE Collector (tb: TransactionBuffer; free: BOOLEAN) =
  VAR
    tq: ADDRESS;
  BEGIN
    IF verbose > 1 THEN RTIO.PutText("GC-RC >> Collector\n"); END;
    IF inCollector THEN
      RTIO.PutText("GC-RC ERROR >> Collector entered recursively\n");
    END;
    inCollector := TRUE;

    (* create a digest of ambiguous roots information *)
    ambCnt := 0;
    ThreadF.ProcessStacks(NoteStackLocations);
    RTStrongRef.ProcessRefs(NoteStackLocations);
    (* RTCollectorSRC.SweepUntracedHeap(NoteStackLocations);*)

    (* there should be no reference assignements during buffer scanning,
       capture the current log pointer and verify when we are done *)
    tq := TransactionQueue;

    (* process the argument buffer but do not queue it *)
    IF tb # NIL THEN
      DoBuffer(tb);
    END;

    (* process all queued buffers *)
    LOOP
      tb := DequeueTB();
      IF tb = NIL THEN EXIT; END;
      DoBuffer(tb);
      FreeTB(tb);
    END;
    
    (* scanning the stacks makes reference assignements, process those *)
    CurrentBuffer.size := (TransactionQueue - CurrentBuffer.buffer) DIV 
                                ADRSIZE(Transaction);
    DoBuffer(CurrentBuffer);

    (* free whatever's left in the zero list and has no ambiguous roots
       and the caller wants us to free them *)
    IF free THEN
      FreeZeroObjects();
    END;

    IF tq # TransactionQueue THEN
      RTIO.PutText("GC-RC ERROR >> TransactionQueue used during collection\n");
      RTOS.Crash();
    END;

    (* reset the current buffer back to the beginning *)
    TransactionQueue := CurrentBuffer.buffer;
    CurrentBuffer.size := BufferSize;

    IF verbose > 2 THEN
      RTIO.PutText("TQ: "); RTIO.PutAddr(TransactionQueue);
      RTIO.PutText(" - "); RTIO.PutAddr(TransactionQueueEnd); 
      RTIO.PutText("\n");
    END;

    inCollector := FALSE;
    IF verbose > 1 THEN RTIO.PutText("GC-RC >> Collector done\n"); END;
  END Collector;

VAR
  x: INTEGER := 0;

PROCEDURE Check () =
  BEGIN
    (*
    INC(x);
    IF x MOD 1000 = 0 THEN RTIO.PutText("?"); END;
    *)
    IF noBuffer (* OR NOT ShouldCollect()*) THEN RETURN; END;
    (* RTIO.PutText("<S>");*)
    (* RTIO.PutText("SynchCollector\n");*)
    Collector (NIL, TRUE);
    noBuffer := TRUE;
  END Check;

PROCEDURE CollectorThread (self: Thread.Closure): REFANY =
  BEGIN
    RTIO.PutText("CollectorThread: started\n");
    LOOP
      LOCK mu DO
        WHILE noBuffer DO
          (* RTIO.PutText("CollectorThread: wait\n");*)
          (* Thread.Wait(mu, cond);*)
          RTOSMachine.Yield();
          (* RTIO.PutText("CollectorThread: test\n");*)
        END;
        RTOS.LockHeap();
        IF NOT noBuffer AND ShouldCollect() THEN
          (* RTIO.PutText("<C>");*)
          (* RTIO.PutText("CollectorThread: collect\n"); *)
          Collector (NIL, TRUE);
          noBuffer := TRUE;
        END;
        RTOS.UnlockHeap();
      END;
    END;
  END CollectorThread;

(* collect only if 2 buffers free *)
PROCEDURE ShouldCollect (): BOOLEAN =
  VAR 
    s: INTEGER;
  BEGIN
    s := QBFirst - QBNext;
    IF s < 0 THEN s := NUMBER(QueuedRingBuffer^) + s; END;
    RETURN s <= 2;
  END ShouldCollect;

(******************************************************************************
 *
 * Processing of the log
 *
 *****************************************************************************)

(* read a log buffer and update reference counts for objects *)
PROCEDURE DoBuffer (tb: TransactionBuffer) =
  VAR
    tptr       : UNTRACED REF Transaction;
    size       : INTEGER;
  BEGIN
    (* RTIO.PutText("<B>");*)
    IF verbose > 1 THEN 
      RTIO.PutText("GC-RC >> DoBuffer: "); 
      RTIO.PutAddr(tb.buffer);  RTIO.PutText(" - "); 
      RTIO.PutAddr(tb.buffer + tb.size * ADRSIZE(Transaction));
      RTIO.PutText(" : "); RTIO.PutInt(tb.size); RTIO.PutText("\n"); 
    END;

    size := tb.size;
    tptr := tb.buffer;
    IF tptr > tb.sentinel THEN
      RTIO.PutText("GC-RC ERROR >> buffer bounds violated\n");
      RTOS.Crash();
    END;

    FOR i := 0 TO size-1 DO
      (* decrement lhs and increment rhs *)
      Decrement(tptr.lhs);
      Increment(tptr.rhs);
      INC(tptr, ADRSIZE(Transaction));
    END;

    (* reset the size to the full buffer *)
    tb.size := BufferSize;
  END DoBuffer;

(* decrement reference count for an object starting at lptr *)
PROCEDURE Decrement (lptr: ADDRESS) =
  VAR
    current    : INTEGER;
    check      : BOOLEAN;
    min := RTHeapRep.MinAddress ();
    max := RTHeapRep.MaxAddress ();
  BEGIN
    IF lptr = NIL THEN
      IF verbose > 5 THEN RTIO.PutText("NIL"); END;
      RETURN;
    ELSIF lptr < min OR lptr > max THEN
      IF verbose > 5 THEN RTIO.PutText("UNTRACED"); END;
      RETURN;
    END;
    WITH lhs = LOOPHOLE(lptr-ADRSIZE(RT0.RefHeader),
                        UNTRACED REF RT0.RefHeader) 
     DO
      IF lhs.forwarded THEN
        RTIO.PutText("GC-RC ERROR >> Decrementing freed object's count: ");
        RTIO.PutAddr(lhs); RTIO.PutText(" ");
        RTIO.PutInt(lhs.typecode); RTIO.PutText("\n");
      END;
      IF verbose > 5 THEN RTIO.PutAddr(lhs); END;
      IF lhs.typecode = TrackTC THEN 
        RTIO.PutText("<-"); RTIO.PutAddr(lhs); RTIO.PutText(">");  
      ELSIF TrackRef # NIL AND lhs = TrackRef THEN 
        RTIO.PutText("<--"); RTIO.PutAddr(lhs); RTIO.PutText(">"); 
      END;
      IF lhs.typecode = 0 THEN
        RTIO.PutText("TC0 in Dec: "); RTIO.PutAddr(lhs);
        RTIO.PutText(" "); RTIO.PutInt(lhs.typecode);
        RTIO.PutText(" "); RTIO.PutInt(LOOPHOLE(lhs, UNTRACED REF INTEGER)^);
        RTIO.PutText(" "); RTIO.PutHex(LOOPHOLE(lhs, UNTRACED REF INTEGER)^);
        RTIO.PutText("\n"); 
        RTOS.Crash();
      END;
      IF lhs.forwarded THEN 
        RTIO.PutText("D: "); RTIO.PutAddr(lhs); 
        RTIO.PutText(" "); RTIO.PutInt(lhs.typecode); 
        (* RTIO.PutText(" "); RTIO.PutInt(ORD(lhs.gcStatus)); *)
        RTIO.PutText(" "); RTIO.PutString(RTType.Get(lhs.typecode).name); 
        RTIO.PutText("\n");
        RTOS.Crash();
      END;

      IF lhs.refcount = RT0.LowRef THEN
        IF lhs.V THEN
          check := OverflowTable.get (lptr, current);
          IF NOT check THEN
            RTIO.PutText("GC-RC ERROR >> lhs overflow inconsistency\n");
            RTOS.Crash();
          END;
        ELSE
          RTIO.PutText("GC-RC ERROR >> refcount decrementing from zero\n");
          RTIO.PutAddr(lhs); RTIO.PutText(" ");
          RTIO.PutInt(lhs.typecode); RTIO.PutText(" ");
          RTIO.PutString(RTType.Get(lhs.typecode).name); RTIO.PutText("\n");
          RTOS.Crash();
        END;
        DEC (current, RT0.MidRef - RT0.LowRef);
        IF current < 0 THEN
          RTIO.PutText("GC-RC ERROR >> refcount less than zero\n");
          RTIO.PutAddr(lhs); RTIO.PutText(" ");
          RTIO.PutInt(current); RTIO.PutText(" ");
          RTIO.PutInt(lhs.typecode); RTIO.PutText(" ");
          RTIO.PutString(RTType.Get(lhs.typecode).name); RTIO.PutText("\n");
          RTOS.Crash();
        END;
        IF current # 0 THEN
          EVAL OverflowTable.put (lptr, current);
          IF lhs.typecode = TrackTC THEN 
            RTIO.PutText("(O:"); RTIO.PutInt(current); RTIO.PutText(")"); 
          ELSIF TrackRef # NIL AND lhs = TrackRef THEN 
            RTIO.PutText("(O:"); RTIO.PutInt(current); RTIO.PutText(")"); 
          END;
          lhs.V := TRUE;
        ELSE
          IF NOT OverflowTable.delete (lptr, current) THEN
            RTIO.PutText("GC-RC ERROR >> could not delete overflow\n");
            RTIO.PutAddr(lhs); RTIO.PutText(" ");
            RTIO.PutInt(current); RTIO.PutText(" ");
            RTIO.PutInt(lhs.typecode); RTIO.PutText(" ");
            RTIO.PutString(RTType.Get(lhs.typecode).name); 
            RTIO.PutText("\n");
            RTOS.Crash();
          END;
          IF lhs.typecode = TrackTC THEN 
            RTIO.PutText("(OD:"); RTIO.PutInt(current); RTIO.PutText(")"); 
          ELSIF TrackRef # NIL AND lhs = TrackRef THEN 
            RTIO.PutText("(OD:"); RTIO.PutInt(current); RTIO.PutText(")"); 
          END;
          lhs.V := FALSE;
        END;
        lhs.refcount := RT0.MidRef - 1;
      ELSE
        DEC (lhs.refcount);
      END;
      IF lhs.typecode = TrackTC THEN 
        RTIO.PutText("("); RTIO.PutInt(lhs.refcount); RTIO.PutText(")"); 
      ELSIF TrackRef # NIL AND lhs = TrackRef THEN 
        RTIO.PutText("("); RTIO.PutInt(lhs.refcount); RTIO.PutText(")"); 
      END;

      IF lhs.refcount = 0 AND NOT lhs.V AND NOT lhs.Z THEN
        AddZeroCntObject(lhs);
        lhs.Z := TRUE;
      END;
    END;
  END Decrement;

(* increment reference count for an object starting at lptr *)
PROCEDURE Increment (rptr: ADDRESS) =
  VAR
    current    : INTEGER;
    check      : BOOLEAN;
    min := RTHeapRep.MinAddress ();
    max := RTHeapRep.MaxAddress ();
  BEGIN
    IF rptr = NIL THEN
      IF verbose > 5 THEN RTIO.PutText("NIL"); END;
      RETURN;
    ELSIF rptr < min OR rptr > max THEN
      IF verbose > 5 THEN RTIO.PutText("UNTRACED"); END;
      RETURN;
    END;
    WITH rhs = LOOPHOLE (rptr-BYTESIZE(RT0.RefHeader),
                         UNTRACED REF RT0.RefHeader) 
     DO
      IF verbose > 5 THEN RTIO.PutAddr(rptr); END;
      IF rhs.typecode = TrackTC THEN 
        RTIO.PutText("<+"); RTIO.PutAddr(rhs); RTIO.PutText(">"); 
      ELSIF TrackRef # NIL AND rhs = TrackRef THEN 
        RTIO.PutText("<++"); RTIO.PutAddr(rhs); RTIO.PutText(">"); 
      END;
      IF rhs.typecode = 0 THEN
        RTIO.PutText("TC0 in Inc: "); RTIO.PutAddr(rhs);
        RTIO.PutText(" "); RTIO.PutInt(rhs.typecode);
        RTIO.PutText("\n"); 
        RTOS.Crash();
      END;
      IF rhs.forwarded THEN 
        RTIO.PutText("I: "); RTIO.PutAddr(rhs); 
        RTIO.PutText(" "); RTIO.PutInt(rhs.typecode); 
        (* RTIO.PutText(" "); RTIO.PutInt(ORD(rhs.gcStatus)); *)
        RTIO.PutText(" "); RTIO.PutString(RTType.Get(rhs.typecode).name); 
        RTIO.PutText("\n");
        RTOS.Crash();
      END;

      IF rhs.refcount = RT0.HighRef THEN
        IF rhs.V THEN
          check := OverflowTable.get (rptr, current);
          IF NOT check THEN
            RTIO.PutText("GC-RC ERROR >> rhs overflow inconsistency\n");
            RTOS.Crash();
          END;
          <* ASSERT check *>
        ELSE
          current := 0;
        END;
        INC (current, RT0.HighRef - RT0.MidRef + 1);
        EVAL OverflowTable.put (rptr, current);
        IF rhs.typecode = TrackTC THEN 
          RTIO.PutText("(O:"); RTIO.PutInt(current); RTIO.PutText(")"); 
        ELSIF TrackRef # NIL AND rhs = TrackRef THEN 
          RTIO.PutText("(O:"); RTIO.PutInt(current); RTIO.PutText(")"); 
        END;
        rhs.V := TRUE;
        rhs.refcount := RT0.MidRef;
      ELSE
        INC (rhs.refcount);
      END;
      IF rhs.typecode = TrackTC THEN 
        RTIO.PutText("("); RTIO.PutInt(rhs.refcount); RTIO.PutText(")"); 
      ELSIF TrackRef # NIL AND rhs = TrackRef THEN 
        RTIO.PutText("("); RTIO.PutInt(rhs.refcount); RTIO.PutText(")"); 
      END;
      IF rhs.Z THEN
        RemoveZeroCntObject(rhs);
        rhs.Z := FALSE;
      END;
    END;
  END Increment;

(******************************************************************************
 *
 * Buffer and queue management
 *
 *****************************************************************************)

(* manage the queue *)
PROCEDURE QueueTB (tb: TransactionBuffer) =
  VAR
    next : CARDINAL;
  BEGIN
    IF tb = NIL THEN
      RTIO.PutText("GC-RC ERROR >> queueing NIL buffer\n");
      RTOS.Crash();
    END;

    (* LOCK QMutex DO*)
    QueuedRingBuffer[QBNext] := tb;
    (*
      RTIO.PutText("Q: "); RTIO.PutInt(QBNext); RTIO.PutText("\n");
    *)
    next := (QBNext+1) MOD NUMBER (QueuedRingBuffer^);
    IF next = QBFirst THEN
      ExtendRing (QueuedRingBuffer, QBFirst, QBNext);
      next := (QBNext+1) MOD NUMBER (QueuedRingBuffer^);
    END;
    QBNext := next;
    (* END;*)
  END QueueTB;

PROCEDURE DequeueTB () : TransactionBuffer =
  VAR
    tb : TransactionBuffer;
  BEGIN
    (* LOCK QMutex DO*)
    IF QBFirst = QBNext THEN
      (* 
         RTIO.PutText("D: NIL "); RTIO.PutInt(QBFirst); RTIO.PutText(" ");
         RTIO.PutInt(QBNext); RTIO.PutText("\n");
      *)
      RETURN NIL;
    ELSE
      tb := QueuedRingBuffer[QBFirst];
      (*
        RTIO.PutText("D: "); RTIO.PutInt(QBFirst); RTIO.PutText("\n");
      *)
      IF tb = NIL THEN
        RTIO.PutText("GC-RC ERROR >> NIL buffer in the queue\n");
        RTOS.Crash();
      END;
      QBFirst := (QBFirst+1) MOD NUMBER (QueuedRingBuffer^);
      RETURN tb;
    END;
    (* END; *)
  END DequeueTB;


(* manage the free list *)
PROCEDURE FreeTB (tb: TransactionBuffer) =
  VAR
    next: CARDINAL;
  BEGIN
    (* LOCK FMutex DO*)
    FreeRingBuffer[FBNext] := tb;
    (*
      RTIO.PutText("F: "); RTIO.PutInt(FBNext); RTIO.PutText("\n");
    *)
    next := (FBNext+1) MOD NUMBER (FreeRingBuffer^);
    IF next = FBFirst THEN
      ExtendRing (FreeRingBuffer, FBFirst, FBNext);
      next := (FBNext+1) MOD NUMBER (FreeRingBuffer^);
    END;
    FBNext := next;
    (* END;*)
  END FreeTB;

PROCEDURE GetFreeTB () : TransactionBuffer =
  VAR
    tb: TransactionBuffer; 
  BEGIN
    (* LOCK FMutex DO*)
    IF FBFirst = FBNext THEN
      RTIO.PutText("GC-RC ERROR >> run out of buffers\n");
      RTOS.Crash();
      RETURN NEW (TransactionBuffer);
    ELSE
      tb := FreeRingBuffer[FBFirst];
      (*
        RTIO.PutText("G: "); RTIO.PutInt(FBFirst); RTIO.PutText("\n");
      *)
      FBFirst := (FBFirst+1) MOD NUMBER (FreeRingBuffer^);
      RETURN tb;
    END;
    (* END;*)
  END GetFreeTB;

(* ring buffer management *)
PROCEDURE ExtendRing (VAR ring: TBArray; first, last: INTEGER) =
  VAR
    num := NUMBER (ring^);
    new_ring := NEW (TBArray, num+1);
  BEGIN
    RTIO.PutText("GC-RC ERROR >> trying to get more buffers\n");
    RTOS.Crash();
    IF first < last THEN
      <* ASSERT first = 0 AND last = num *>
      FOR i := first TO last DO
        new_ring[i] := ring[i];
      END;
    ELSE
      FOR i := 0 TO last DO
        new_ring[i] := ring[i];
      END;
      FOR i := first TO num DO
        new_ring[i+1] := new_ring[i];
      END;
    END;
    DISPOSE (ring);
    ring := new_ring;
  END ExtendRing;

(******************************************************************************
 *
 * Processing ambiguous roots
 *
 *****************************************************************************)

PROCEDURE NoteStackLocations (start, stop: ADDRESS) =
  VAR
    fp : UNTRACED REF ADDRESS := start;
    p  : ADDRESS;
    firstAllocatedAddress    := RTHeapRep.MinAddress();
    firstNonAllocatedAddress := RTHeapRep.MaxAddress();
  BEGIN
    IF verbose > 3 THEN
      RTIO.PutText("Ambiguous region: "); 
      RTIO.PutAddr(start); RTIO.PutText(" ");
      RTIO.PutAddr(stop); RTIO.PutText("\n"); 
    END;
    IF Word.LT(LOOPHOLE(fp, Word.T), 16_100000000) THEN
      RTIO.PutText("Ambiguous region: "); 
      RTIO.PutAddr(start); RTIO.PutText(" ");
      RTIO.PutAddr(stop); RTIO.PutText("\n"); 
      RTOS.Crash();
    END;
    WHILE fp <= stop DO
      p := fp^;
      IF firstAllocatedAddress <= p AND p <= firstNonAllocatedAddress THEN
        AddAmbiguousRoot(p);
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END NoteStackLocations;

VAR
  ambRoots : UNTRACED REF ARRAY OF ADDRESS;
  ambCnt   : INTEGER;

PROCEDURE AddAmbiguousRoot(addr: ADDRESS) =
  BEGIN
    IF verbose > 4 THEN
      RTIO.PutText("Amb: "); RTIO.PutAddr(addr); RTIO.PutText("\n");
    END;
    IF ambCnt = NUMBER(ambRoots^) THEN
      RTIO.PutText("GC-RC ERROR >> not enough space for amb. roots\n");
      RTOS.Crash();
    END;
    ambRoots[ambCnt] := addr;
    INC(ambCnt);
  END AddAmbiguousRoot;

PROCEDURE HasAmbiguousRoots(header: UNTRACED REF RT0.RefHeader): BOOLEAN =
  VAR
    end := header + header.size;
  BEGIN
    FOR i := 0 TO ambCnt DO
      WITH root = ambRoots[i] DO
        IF root >= header AND root < end THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END HasAmbiguousRoots;

(******************************************************************************
 *
 * Zero count list
 *
 *****************************************************************************)

VAR
  zeroList : UNTRACED REF ARRAY OF UNTRACED REF RT0.RefHeader;
  zeroCnt  : INTEGER;

PROCEDURE AddZeroCntObject(addr: UNTRACED REF RT0.RefHeader) =
  BEGIN
    IF addr = TrackRef THEN
      RTIO.PutText("<AZ"); RTIO.PutAddr(addr);
      RTIO.PutText(">"); 
    END;
    IF zeroCnt = NUMBER(zeroList^) THEN
      RTIO.PutText("GC-RC ERROR >> not enough space for zero objects\n");
      RTOS.Crash();
    END;
    zeroList[zeroCnt] := addr;
    INC(zeroCnt);
  END AddZeroCntObject; 

PROCEDURE RemoveZeroCntObject(addr: UNTRACED REF RT0.RefHeader) =
  VAR
    i : INTEGER;
  BEGIN
    IF addr = TrackRef THEN
      RTIO.PutText("<RZ"); RTIO.PutAddr(addr);
      RTIO.PutText(">"); 
    END;
    i := 0;
    WHILE i < zeroCnt DO
      IF zeroList[i] = addr THEN EXIT; END;
      INC(i);
    END;
    IF i = zeroCnt THEN
      RTIO.PutText("GC-RC ERROR >> zero object not found\n");
      RTOS.Crash();
    END;
    FOR j := i TO zeroCnt-2 DO
      zeroList[j] := zeroList[j+1];
    END;
    DEC(zeroCnt);
  END RemoveZeroCntObject; 

PROCEDURE FreeZeroObjects () =
  VAR
    i: INTEGER;
  BEGIN
    i := 0;
    WHILE i < zeroCnt DO
      WITH h = zeroList[i] DO
        IF h.typecode = 0 THEN
          RTIO.PutText("TC0 in Free: "); RTIO.PutAddr(h);
          RTIO.PutText(" "); RTIO.PutInt(h.typecode);
          RTIO.PutText("\n"); 
          RTOS.Crash();
        END;
        IF NOT HasAmbiguousRoots(h) THEN
          IF h.typecode = TrackTC OR h = TrackRef THEN
            RTIO.PutText("<F"); RTIO.PutAddr(h);
            RTIO.PutText(" "); RTIO.PutInt(h.typecode);
            RTIO.PutText(">"); 
          END;

          h.forwarded := TRUE;
          h.Z := FALSE;
          (* SIRPA *)
          RTHeapRep.ReturnMem(zeroList[i]+ADRSIZE(RT0.RefHeader));
          (* *)
          FOR j := i TO zeroCnt-2 DO
            zeroList[j] := zeroList[j+1];
          END;
          DEC(zeroCnt);
        ELSE
          INC(i);
        END;
      END;
    END;
  END FreeZeroObjects;

(******************************************************************************
 *
 * Memory pool for reference counting
 *
 *****************************************************************************)

(* 
 * set up the permanent buffers, we couldn't have done it during initialization
 * because we could not protect pages until now
 *)

PROCEDURE SetupPermanentBuffers () =
  VAR
    availablePtr : ADDRESS;
  BEGIN
    (* beginning of the dedicated pool of memory *)
    availablePtr := RTOSMachine.refcount_start;

    (* create the free list *)
    FreeRingBuffer := NEW (TBArray, InitSize);
    FBFirst := 0;
    FBNext := InitSize - 1;

    (* create the used bufffers list *)
    QueuedRingBuffer := NEW (TBArray, InitSize);
    QBFirst := 0;
    QBNext := 0;

    (* create the buffers and add them to free list *)
    FOR i := 0 TO InitSize-1 DO
      WITH tb = NEW (TransactionBuffer) DO
        (* add it to the free list *)
        FreeRingBuffer[i] := tb;

        (* allocate log pages *)
        tb.buffer := availablePtr;
        INC(availablePtr, BufferPageSize * PageSize);

        (* allocate and protect the sentinel page *)
        tb.sentinel := availablePtr;
        INC(availablePtr, PageSize);
        RTHeapDep.ProtectAddr(tb.sentinel, availablePtr - ADRSIZE(ADDRESS),
                              FALSE, FALSE);
      END;
    END;

    (* make sure that we set aside enough memory in SpinProgram.c *)
    IF availablePtr > RTOSMachine.refcount_end THEN
      RTIO.PutText("GC-RC ERROR >> too few pages for reference counting\n");
      RTOS.Crash();
    END;

    (* announce we are for real now *)
    IF verbose > 0 THEN
      RTIO.PutText("GC-RC >> permanent refcount buffers: ");
      RTIO.PutAddr(RTOSMachine.refcount_start);
      RTIO.PutText(" - ");
      RTIO.PutAddr(availablePtr);
      RTIO.PutText(" (limit: ");
      RTIO.PutAddr(RTOSMachine.refcount_end);
      RTIO.PutText(")\n");
    END;
  END SetupPermanentBuffers;

(* 
 * this procedure must be run at spl-high to prevent any REF assigments
 *)
PROCEDURE EnableTraps () =
  VAR
    initBuffer: TransactionBuffer;
  BEGIN
    RTOS.LockHeap();
    IF verbose > 0 THEN 
      RTIO.PutText("GC-RC >> Switching to permanent buffers\n");
    END;
    IF inMutator THEN
      RTIO.PutText("GC-RC ERROR >> mutator entered recursively\n");
      RTOS.Crash();
    END;
    inMutator := TRUE;

    (* have we ran out of the buffer? *)
    IF TransactionQueue > RTOSMachine.irefcount_end THEN
      RTIO.PutText("GC-RC ERROR >> initial refcount buffer too small\n");
      RTOS.Crash();
    END;

    IF verbose > 0 THEN
      RTIO.PutText("GC-RC >> initial refcount buffer: ");
      RTIO.PutAddr(RTOSMachine.irefcount_start); RTIO.PutText(" ");
      RTIO.PutAddr(TransactionQueue); RTIO.PutText(" ");
      RTIO.PutAddr(RTOSMachine.irefcount_end); RTIO.PutText("\n");
    END;

    (* set the size of the initial buffer to what's accumulated there *)
    initBuffer := NEW(TransactionBuffer, 
                      size := 
                          (TransactionQueue - RTOSMachine.irefcount_start) DIV
                              ADRSIZE(Transaction),
                      buffer := RTOSMachine.irefcount_start,
                      sentinel := RTOSMachine.irefcount_end);

    (* get the real buffers *)
    SetupPermanentBuffers();

    (* set up the log and current buffer *)
    CurrentBuffer := GetFreeTB ();
    TransactionQueue := CurrentBuffer.buffer;
    TransactionQueueEnd := CurrentBuffer.sentinel;

    IF verbose > 2 THEN
      RTIO.PutText("TQ: "); RTIO.PutAddr(TransactionQueue);
      RTIO.PutText(" - "); RTIO.PutAddr(TransactionQueueEnd); 
      RTIO.PutText("\n");
    END;

    (* collect what is accumulated so far *)
    Collector(initBuffer, TRUE);

    (* return the pages since we won't use them again *)
    (* FIXME *)

    (* get rid of the intial descriptor *)
    DISPOSE(initBuffer);

    (* create the collector thread *)
    noBuffer := TRUE;
    mu := NEW(MUTEX);
    cond := NEW(Thread.Condition);
    collectorThread := Thread.Fork(NEW(Thread.Closure, 
                                       apply := CollectorThread));

    inMutator := FALSE;
    RTOS.UnlockHeap();
  END EnableTraps;

(******************************************************************************
 *
 * Tracking all the assignements
 *
 *****************************************************************************)

(* rptr = -1 means that we called AssignedKnown *)
PROCEDURE FS (lloc, lptr, rptr, pc: ADDRESS) =
  VAR
    lhs, rhs: UNTRACED REF RT0.RefHeader := NIL;
  BEGIN
    RETURN;
    Decrement(lptr);
    Increment(rptr);
    RETURN;

    IF lptr # NIL THEN
      lhs := LOOPHOLE(lptr-ADRSIZE(RT0.RefHeader), UNTRACED REF RT0.RefHeader);
      (*
      WITH h = RTHeapRep.LocateHeaderOf(lhs) DO
        IF h # lhs AND lhs.typecode # 1 THEN
          RTIO.PutText("GC-RC ERROR >> lhs not a header!!!\n");
          RTIO.PutAddr(pc); RTIO.PutText(" ");
          RTIO.PutAddr(h); RTIO.PutText(" ");
          RTIO.PutAddr(lhs); RTIO.PutText("\n");
          RTOS.Crash();
        END;
        IF lhs.typecode = 0 THEN
          RTIO.PutText("TC0 in FS lhs: "); RTIO.PutAddr(lhs);
          RTIO.PutText(" "); RTIO.PutInt(lhs.typecode);
          RTIO.PutText("\n"); 
          RTOS.Crash();
        END;
      END;
      *)
    END;
    IF rptr # NIL AND LOOPHOLE(rptr, INTEGER) # 16_ffffffffffffffff THEN
      rhs := LOOPHOLE(rptr-ADRSIZE(RT0.RefHeader), UNTRACED REF RT0.RefHeader);
      (*
      WITH h = RTHeapRep.LocateHeaderOf(rhs) DO
        IF h # rhs AND rhs.typecode # 1 THEN
          RTIO.PutText("GC-RC ERROR >> rhs not a header!!!\n");
          RTIO.PutAddr(pc); RTIO.PutText(" ");
          RTIO.PutAddr(h); RTIO.PutText(" ");
          RTIO.PutAddr(rhs); RTIO.PutText("\n");
          RTOS.Crash();
        END;
        IF rhs.typecode = 0 THEN
          RTIO.PutText("TC0 in FS rhs: "); RTIO.PutAddr(rhs);
          RTIO.PutText(" "); RTIO.PutInt(rhs.typecode);
          RTIO.PutText("\n"); 
          RTOS.Crash();
        END;
      END;
      *)
    END;

    IF (TrackRef # NIL AND (lhs = TrackRef OR rhs = TrackRef)) OR
      (lhs # NIL AND lhs.typecode = TrackTC) OR 
      (rhs # NIL AND rhs.typecode = TrackTC)
     THEN
      RTIO.PutText("<"); RTIO.PutAddr(pc); RTIO.PutText(" ");
      RTIO.PutAddr(lloc); RTIO.PutText(" ");
      RTIO.PutAddr(lhs); RTIO.PutText(" ");
      RTIO.PutAddr(rhs); RTIO.PutText(">");
    END;
  END FS;

(******************************************************************************
 *
 * Initialization
 *
 *****************************************************************************)

(* initialize the data structures *)
PROCEDURE Init () =
  VAR
    initSize: INTEGER;
  BEGIN
    RTIO.PutText("GC >> using the mark-and-sweep collector\n");

    IF verbose > 0 THEN RTIO.PutText ("GC-RC >> initializing\n"); END;
    TrackRef := LOOPHOLE(16_0, ADDRESS);

    (*
     * Set up the initial buffer used until we can use the permanent ones.
     * No descriptor is necessary because we use that pool of memory only once.
     *
     * Note that it is impossible to set protection on a sentinal page until
     * later in the boot process.  If we run out of that initial space memory
     * will be overwritten silently. 
     *)

    initSize := (RTOSMachine.irefcount_end - RTOSMachine.irefcount_start) DIV
                   ADRSIZE(Transaction);

    TransactionQueue := RTOSMachine.irefcount_start;
    TransactionQueueEnd := TransactionQueue + initSize * ADRSIZE(Transaction);

    IF verbose > 2 THEN
      RTIO.PutText("TQ: "); RTIO.PutAddr(TransactionQueue);
      RTIO.PutText(" - "); RTIO.PutAddr(TransactionQueueEnd); 
      RTIO.PutText("\n");
    END;

    noBuffer := TRUE;

    (*
     * create the collector's data structures
     *)

    (* the overflow table *)
    OverflowTable := NEW (AddrIntUtbl.Default).init (1000);

    (* allocate the data structure that will remember ambiguous roots during 
       collection *)
    ambRoots := NEW (UNTRACED REF ARRAY OF ADDRESS, 100000);
    zeroList := NEW (UNTRACED REF ARRAY OF UNTRACED REF RT0.RefHeader, 100000);

    (* announce that we are up *)
    IF verbose > 0 THEN
      RTIO.PutText("GC-RC >> initial refcount buffer: ");
      RTIO.PutAddr(RTOSMachine.irefcount_start);
      RTIO.PutText("; size: "); RTIO.PutInt(initSize); RTIO.PutText("\n");
    END;
  END Init;

BEGIN
END RTRefCount.
