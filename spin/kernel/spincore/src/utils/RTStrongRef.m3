(* 
 * HISTORY
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *	Reimplemented using a one element cache to catch the common
 *	case of references removed right after they are added and 
 *	a log to catch the other common case of references added for
 *	a short period of time.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Moved the timing flag to DebugOption.
 *
 * 25-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made TrackStrand conditional.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Named StrongRefWalker thread
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Print strongrefs only if count is non-zero.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to have a separate thread clean out dead strongref
 *	in the table.  The thread must be initialized in the module's
 *      main body, as the Init() procedure is called before the thread
 *      subsystem has been initialized.
 *
 * 15-May-96 Przemek Pardyak (pardy) at the University of Washington
 *	It is not kosher to allocate memory using NEW during garbage
 *	collection.  Table.Delete does allocation and ProcessRefs
 *	is called during collection.  I have undone the optimistic
 *	removal of strong-refs. Also removed Lookup.
 *
 * 13-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Optimized hash table implementation to keep unused entries around
 *	until GC.  At GC the unused table entries are deleted.  
 *      Changed dump routine to iterate through the table.
 *      Cleaned up a bit.
 *
 * 08-Apr-96 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to hash tables. 
 *
 * 21-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Fixed a bug in Remove.
 *
 * 05-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	StrongRef-s are counted and released only if the number of Remove
 *	operations exceeds the number of Add operations. 
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Removed unused Thread IMPORT.
 *
 * 28-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Use SPL temporarily to synchronize with interrupt handlers that
 *	may call Add/Remove.
 * 
 * 6-Nov-95 Przemek Pardyak (pardy) at the University of Washington
 *      Simplified interface.
 *
 * 27-Mar-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

(* This module implements a strong reference scheme that indicates to
   the collector which referents may not be moved or collected, as
   there might be an outstanding reference to it from an untraced
   pointer.  This basic support enables us to share references to
   traced data safely with C code.  The references are converted to
   Word.T to prevent dead strongref table entries keeping alive dead
   referents. *)

UNSAFE (* To loophole refs into Word.T's *)
MODULE RTStrongRef EXPORTS StrongRef, RTStrongRef;
IMPORT RTOSMachine, RTType, RTIO, Word, RTCollector, RTHeapRep, RTCollectorSRC;
IMPORT DebugOption, TrackStrand;  <*NOWARN*>
IMPORT IntIntTbl; 
IMPORT Thread, ThreadExtra;
IMPORT Spy;

(*
 * timing of strongref operations
 *)

CONST
  DoTiming = RTCollectorSRC.DoTimingsXtra;

VAR
  addTimer, rmTimer, isTimer: Spy.T;
  p1Timer, p2Timer, p3Timer, p4Timer: Spy.T;
  b1Timer, b2Timer: Spy.T;

(*
 * the cache for the case a strongref is added just to be removed
 *)
VAR
  last: INTEGER := 0;

(* 
 * the buffer which hold the log of operations on frequenty changing 
 * strongrefs
 *)
CONST
  MAX_BUFFER = 1000;

VAR
  buffer: ARRAY [0..MAX_BUFFER-1] OF INTEGER;
  next: INTEGER := 0;

(*
 * hash table which keeps track of stable strongrefs
 *)
CONST
  DEFAULT_SIZE = 64; (* default size for the table generic *)
  
VAR
  table: IntIntTbl.T;
  mutex : MUTEX;
  processDeadStrongRef: Thread.Condition;
  iterator: IntIntTbl.Iterator; (* iterator object over strongref table *)

VAR
  putter : RTIO.SimplePutter;

(* 
 * registers a referent as immobilized and uncollectible with
 * the garbage collector.
 *)
PROCEDURE Add (ref: REFANY) =
  VAR 
    sref: Word.T;
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    IF ref = NIL THEN RETURN END;
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    IF DoTiming THEN Spy.Enter(addTimer); END;

    sref := LOOPHOLE(ref, Word.T);
    IF last # 0 THEN
      IF DoTiming THEN Spy.Enter(b1Timer); END;
      AddToBuffer(last);
      IF DoTiming THEN Spy.Exit(b1Timer); END;
    END;
    last := sref;
    
    IF RTCollectorSRC.countStrongRef THEN
      WITH page = RTHeapRep.AddressToPage(LOOPHOLE(sref, ADDRESS)),
           desc = RTHeapRep.desc[page - RTHeapRep.p0]
       DO
        IF desc.used # 0 THEN
          IF desc.strong = 7 THEN
            IF NOT desc.verystrong THEN
              desc.verystrong := TRUE;
              INC(RTHeapRep.verystrong);
            END;
          ELSE
            IF desc.strong = 0 THEN
              INC(RTHeapRep.strong);
            END;
            INC(desc.strong);
          END;
        END;
      END;
    END;

    IF DoTiming THEN Spy.Exit(addTimer); END;
    RTOSMachine.RestoreInterruptMask(spl);
  END Add;

(* 
 * unregisters a referent as strongref with the garbage collector once
 * the strongref count goes down to zero.  It is lazily removed from
 * the strongref table in ProcessRefs. 
 *)
PROCEDURE Remove (ref: REFANY) =
  VAR 
    sref: Word.T;
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    IF ref = NIL THEN RETURN END;
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    IF DoTiming THEN Spy.Enter(rmTimer); END;

    sref := LOOPHOLE(ref, Word.T);
    IF sref = last THEN
      last := 0;
    ELSIF next # 0 AND buffer[next-1] = sref THEN
      DEC(next);
    ELSE
      (* set the lowest bit to one, we can always do it because all pointers
         are aligned to the size of Header *)
      IF DoTiming THEN Spy.Enter(b2Timer); END;
      AddToBuffer(Word.Or(sref, 16_1));
      IF DoTiming THEN Spy.Exit(b2Timer); END;
    END;

    IF RTCollectorSRC.countStrongRef THEN
      WITH page = RTHeapRep.AddressToPage(LOOPHOLE(sref, ADDRESS)),
           desc = RTHeapRep.desc[page - RTHeapRep.p0]
       DO
        IF NOT desc.verystrong AND desc.strong # 0 THEN
          DEC(desc.strong);
          IF desc.strong = 0 THEN
            DEC(RTHeapRep.strong);
          END;
        END;
      END;
    END;

    IF DoTiming THEN Spy.Exit(rmTimer); END;
    RTOSMachine.RestoreInterruptMask(spl);
  END Remove;

(* 
 * check whether a reference is strongref'd. 
 *)
PROCEDURE Exists (ref: REFANY): BOOLEAN =
  VAR 
    sref: Word.T := LOOPHOLE(ref, Word.T);
    count: INTEGER;
    result : BOOLEAN := FALSE;
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    IF ref = NIL THEN RETURN FALSE END;
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    IF DoTiming THEN Spy.Enter(isTimer); END;

    IF sref = last THEN
      result := TRUE;
    ELSE
      CompactBuffer();
      IF table.get(sref, count) THEN
        IF count > 0 THEN
          RETURN TRUE;
        END;
      END;
      FOR i := 0 TO next-1 DO
        IF buffer[next] = sref THEN
          result := TRUE;
          EXIT;
        END;
      END;
    END;

    IF DoTiming THEN Spy.Exit(isTimer); END;
    RTOSMachine.RestoreInterruptMask(spl);
    RETURN result;
  END Exists; 

(* ProcessRefs() is invoked by the garbage collector during
   collection.  The GC passes in a procedure that takes as its
   arguments a memory range that contains strong refs.  Unused
   strongref are not set as ambiguous roots and their entries are
   removed from the table. It is illegal to call new at this point. *)

PROCEDURE ProcessRefs (p: PROCEDURE (start, stop: ADDRESS)) =
  VAR 
    sref   : Word.T;
    count  : INTEGER;
    buf    : UNTRACED REF ARRAY OF INTEGER;
    i      : INTEGER;
    signal : BOOLEAN := FALSE;
  BEGIN
    (* process all the strongrefs stored in the hash table and the fast cashe*)
    (* IDEA: cache the last result and reuse unless changed (sth. removed) *)

    IF DoTiming THEN Spy.Enter(p1Timer); END;

    iterator.reset();
    buf := NEW(UNTRACED REF ARRAY OF INTEGER, table.size()+1);
    buf[0] := last;
    i := 1;
    WHILE iterator.next(sref, count) DO 
      IF count > 0 THEN
        buf[i] := sref;
        INC(i);
      ELSE
        signal := TRUE;
      END;
    END;
    p(ADR(buf[0]), ADR(buf[i-1]));

    IF DoTiming THEN Spy.Exit(p1Timer); END;

    (* process the buffer cache *)
    IF next # 0 THEN
      IF DoTiming THEN Spy.Enter(p2Timer); END;
      CompactBuffer();
      IF DoTiming THEN Spy.Exit(p2Timer); END;

      IF next # 0 THEN
        IF DoTiming THEN Spy.Enter(p3Timer); END;
        p(ADR(buffer[0]), ADR(buffer[next-1]));
        IF DoTiming THEN Spy.Exit(p3Timer); END;
      END;
    END;

    IF DoTiming THEN Spy.Enter(p4Timer); END;

    DISPOSE(buf);

    (* process the hash table *)
    IF signal THEN
      Thread.Signal(processDeadStrongRef);
    END;

    IF DoTiming THEN Spy.Exit(p4Timer); END;
  END ProcessRefs;

(*
 * hash table operations
 *)

PROCEDURE AddToTable (sref: Word.T; force: BOOLEAN): BOOLEAN =
  VAR
    count: INTEGER;
  BEGIN
    IF table.get(sref, count) THEN
      (* increment the count if already in the table *)
      INC(count);
      force := TRUE;
    ELSE
      (* initialize it otherwise *)
      count := 1;
    END;
    
    (* add to the table *)
    IF force THEN
      EVAL table.put(sref, count);
    END;
    RETURN force;
  END AddToTable;

PROCEDURE RemoveFromTable (sref: Word.T) =
  VAR 
    count: INTEGER;
  BEGIN
    IF table.get(sref, count) = TRUE AND NOT count = 0 THEN
      DEC(count);
      EVAL table.put(sref, count);
    ELSE 
      BadRemove(sref);
    END;
  END RemoveFromTable;

PROCEDURE BadRemove (sref: Word.T) =
  VAR
    ptr := RTType.Get(TYPECODE(LOOPHOLE(sref, REFANY)));
  BEGIN
    RTIO.PutText("ERROR >> removing a non-strongrefed reference ");
    RTIO.PutHex(sref); RTIO.PutText(" of type "); 
    RTIO.PutString(ptr.name); RTIO.PutText(" (typecode: ");
    RTIO.PutInt(ptr.typecode); RTIO.PutText(")\n"); 
  END BadRemove;

(* 
 * walk through the strongref table and removes unused entries. 
 *)
PROCEDURE Timeout (<* UNUSED *> arg: REFANY): REFANY =
  VAR spl   : RTOSMachine.InterruptLevel;
      sref  : Word.T;
      count : INTEGER;
  BEGIN
    LOOP
      LOCK mutex DO 
        Thread.Wait(mutex, processDeadStrongRef);
      END;
      spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);    
      TRY
        iterator.reset();
        WHILE iterator.next(sref, count) DO 
          IF count = 0 AND NOT table.delete(sref, count) THEN
            RTIO.PutText("ERROR >> could not update strongref hash table\n");
          END;
        END;
      FINALLY
        RTOSMachine.RestoreInterruptMask(spl);
      END;
    END;
  END Timeout; 

(*
 * buffer operations
 *)

PROCEDURE AddToBuffer (sref: INTEGER) =
  BEGIN
    buffer[next] := sref;
    INC(next);
    IF next = MAX_BUFFER THEN
      CompactBuffer();
      IF next = MAX_BUFFER THEN
        MoveBufferToTable();
      END;
    END;
  END AddToBuffer;

PROCEDURE CompactBuffer () =
  VAR
    sref: Word.T;
    found: BOOLEAN;
  BEGIN
    (* process all removes *)
    FOR i := next-1 TO 0 BY -1 DO
      sref := buffer[i];
      IF sref # 0 AND Word.And(sref, 16_1) = 16_1 THEN
        sref := Word.And(sref, 16_fffffffffffffffe);
        found := FALSE;
        FOR j := i-1 TO 0 BY -1 DO
          IF buffer[j] = sref THEN
            buffer[j] := 0;
            found := TRUE;
            EXIT;
          END;
        END;
        IF NOT found THEN
          RemoveFromTable(sref);
        END;
        buffer[i] := 0;
      END;
    END;

    (* compact the remaining adds *)
    next := 0;
    FOR i := 0 TO MAX_BUFFER-1 DO
      sref := buffer[i];
      IF sref # 0 THEN
        IF next # i THEN
          buffer[next] := sref;
          buffer[i] := 0;
        END;
        INC(next);
      END;
    END;
  END CompactBuffer;

PROCEDURE MoveBufferToTable () =
  VAR
    cnt := 0;
  BEGIN
    FOR i := 0 TO MAX_BUFFER-1 DO
      IF AddToTable(buffer[i], FALSE) THEN
        INC(cnt);
        buffer[i] := 0;
      END;
    END;
    IF cnt < MAX_BUFFER DIV 10 THEN
      FOR i := 0 TO MAX_BUFFER DIV 4 DO
        IF buffer[i] # 0 THEN
          EVAL AddToTable(buffer[i], TRUE);
          buffer[i] := 0;
        END;
      END;
    END;
    CompactBuffer();
  END MoveBufferToTable;

PROCEDURE ForceAllToTable () =
  BEGIN
    IF last # 0 THEN
      AddToBuffer(last);
    END;
    CompactBuffer();
    FOR i := 0 TO next-1 DO
      EVAL AddToTable(buffer[i], TRUE);
      buffer[i] := 0;
    END;
    next := 0;
  END ForceAllToTable;

(* 
 * print out the currently registered immobilized objects with their
 * strongreference counts. 
 *)

PROCEDURE Dump (p: RTIO.SimplePutter) =
  VAR 
    spl   : RTOSMachine.InterruptLevel;
    sref  : Word.T;
    count : INTEGER;
    cnt   : INTEGER;
  BEGIN
    IF p = NIL THEN
      IF putter = NIL THEN
        RTIO.PutText("ERROR >> putter is NIL in RTStrongRef.Dump\n"); 
        RETURN;
      END;
      p := putter;
    END;
    ForceAllToTable ();
    cnt := 0;
    iterator.reset();
    p.putText("strongrefs:\n");
    p.putText("ref:                cnt:   size:    type:\n");
    p.putText("------------------  -----  -------- --------------------------------\n");
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    RTCollector.Disable();
    WHILE iterator.next(sref, count) DO 
      IF count # 0 THEN
        INC(cnt);
        p.putAddr(LOOPHOLE(sref,ADDRESS)); 
        p.putText("  "); p.putInt(count, 4);
        p.putText(" ");
        p.putText("  "); 
        p.putInt(
            RTHeapRep.ReferentSize(
                LOOPHOLE(sref-ADRSIZE(RTHeapRep.Header),
                         RTHeapRep.RefHeader)), 8);
        p.putText(" ");
        p.putString(RTType.Get(TYPECODE(LOOPHOLE(sref, REFANY))).name);
        p.putText("\n"); 
      END;
    END;
    RTCollector.Enable();
    RTOSMachine.RestoreInterruptMask(spl);
    p.putText("total: "); p.putInt(cnt); p.putText(" strongrefs\n");
  END Dump;

(* 
 * initialize internal data structures used by StrongRef.  This
 * function is called early in the initialization phase of the
 * system. It is initialized by the RTCollector.Init().
 *)
PROCEDURE Init () =
  BEGIN
    table    := NEW(IntIntTbl.Default).init(DEFAULT_SIZE);
    iterator := table.iterate();
    processDeadStrongRef := NEW(Thread.Condition);
    mutex := NEW(MUTEX);
    putter := NEW(RTIO.SimplePutter);
    IF DoTiming THEN
      addTimer := Spy.Create("StrongRef.Add");
      rmTimer := Spy.Create("StrongRef.Remove");
      isTimer := Spy.Create("StrongRef.Exists");
      p1Timer := Spy.Create("StrongRef.Proc.1");
      p2Timer := Spy.Create("StrongRef.Proc.2");
      p3Timer := Spy.Create("StrongRef.Proc.3");
      p4Timer := Spy.Create("StrongRef.Proc.4");
      b1Timer := Spy.Create("StrongRef.AddB.1");
      b2Timer := Spy.Create("StrongRef.AddB.2");
    END;
  END Init;

BEGIN
  (* must be started here and not in the Init() function, because
     threads may not be initialized when Init() gets called. *)
  WITH t = ThreadExtra.PFork(Timeout,NIL) DO
    IF DebugOption.DoTrackStrand THEN
      TrackStrand.SetName(ThreadExtra.GetTracker(t),"StrongRefWalker");
    END;
  END;
END RTStrongRef.

