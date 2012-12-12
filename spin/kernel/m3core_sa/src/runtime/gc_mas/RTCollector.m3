(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* portions Copyright 1996, Critical Mass, Inc.              *)
(*                                                           *)
(*| Last modified on Fri Apr 26 10:29:11 PDT 1996 by heydon  *)
(*|      modified on Sat Nov 19 09:37:57 PST 1994 by kalsow  *)
(*|      modified on Fri Aug  5 14:04:35 PDT 1994 by jdd     *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)

UNSAFE MODULE RTCollector EXPORTS RTCollector, RTCollectorSRC,
                                  RTHeapRep, RTWeakRef;

IMPORT RTOS, RTIO, RTOSMachine, RT0u, RTHeapMAS;
IMPORT RTPerfTool, RTProcess, RTHeapEvent, RT0, RTType, RTMisc;
IMPORT Word;

(* Much of the code below incorrectly assumes no difference between ADRSIZE
   and BYTESIZE. *)

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

(*** RTCollector ***)

PROCEDURE Disable () = 
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishVM();
      INC(disableCount);
      (* INC(manu_cnt);*)
      (* partialCollectionNext := FALSE;*)
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END Disable;

PROCEDURE Enable () = 
  BEGIN
    RTOS.LockHeap();
    BEGIN
      IF disableCount > 0 THEN
        DEC(disableCount);
        CollectEnough(FALSE);
      END;
    END;
    RTOS.UnlockHeap();
    IF perfOn THEN PerfAllow(); END;
  END Enable;

PROCEDURE DisableMotion () = 
  BEGIN
  END DisableMotion;

PROCEDURE EnableMotion () = 
  BEGIN
  END EnableMotion;

(*
PROCEDURE Collect () = 
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishGC();
      StartGC();
      FinishGC();
    END;
    RTOS.UnlockHeap();
  END Collect;
*)

(*** RTCollectorSRC ***)

(* StartCollection starts a total collection, if none is in progress and if
   collection and motion are enabled. *)

PROCEDURE StartCollection () = 
  BEGIN
    Collect();
  END StartCollection;

(* FinishCollection finishes the current collection, if one is on
   progress. *)

PROCEDURE FinishCollection () =
  BEGIN
  END FinishCollection;

(* DisableVM disables the use of VM protection.  While VM protection is
   disabled, no objects on the heap will be protected.*)

PROCEDURE DisableVM () =
  BEGIN
  END DisableVM;

(* EnableVM reenables the use of VM protection if EnableVM has been called
   as many times as DisableVM.  It is a checked runtime error to call
   EnableVM more times than DisableVM. *)

PROCEDURE EnableVM () =
  BEGIN
  END EnableVM;

(* FinishVM is equivalent to DisableVM{}; EnableVM().  FinishVM unprotects
   all heap pages, and is intended for use from the debugger. *)

PROCEDURE FinishVM () =
  BEGIN
  END FinishVM;

PROCEDURE CollectEnough (<* UNUSED *>auto: BOOLEAN) =
  BEGIN
  END CollectEnough;

PROCEDURE StartBackgroundCollection () =
  BEGIN
    RTIO.PutText("GC ERROR >> StartBackgroundCollection not implemented\n");
  END StartBackgroundCollection;

PROCEDURE StartGC () =
  BEGIN
    StartCollection();
  END StartGC;

PROCEDURE FinishGC () =
  BEGIN
    FinishCollection();
  END FinishGC;

PROCEDURE Crash (): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Crash;

PROCEDURE VisitAllRefs (<* UNUSED *>v: RefVisitor; <* UNUSED *>fromGC: BOOLEAN := FALSE) =
  BEGIN
    RTIO.PutText("GC ERROR >> VisitAllRefs not implemented\n");
    <* ASSERT FALSE *> (* MAS *)
  END VisitAllRefs;

PROCEDURE WeakRefFromRef (<* UNUSED *>r: REFANY; <* UNUSED *>p: WeakRefCleanUpProc := NIL): WeakRef =
  BEGIN
    RTIO.PutText("GC ERROR >> WeakRefFromRef not implemented\n");
    <* ASSERT FALSE *> (* MAS *)
  END WeakRefFromRef;

PROCEDURE WeakRefToRef (<* UNUSED *>READONLY t: WeakRef): REFANY =
  BEGIN
    RTIO.PutText("GC ERROR >> WeakRefToRef not implemented\n");
    <* ASSERT FALSE *> (* MAS *)
  END WeakRefToRef;

PROCEDURE RegisterFinalCleanup (<* UNUSED *>r: REFANY; <* UNUSED *> p: PROCEDURE (r: REFANY)) =
  BEGIN
    RTIO.PutText("GC ERROR >> RegisterFinalCleanup not implemented\n");
    <* ASSERT FALSE *> (* MAS *)
  END RegisterFinalCleanup;

PROCEDURE RTInit() =
  BEGIN
  END RTInit;

(* ---------------------------------------------------- showheap hooks *)

VAR
  perfW  : RTPerfTool.Handle;
  perfOn : BOOLEAN := FALSE;

CONST
  EventSize = (BITSIZE(RTHeapEvent.T) + BITSIZE(CHAR) - 1) DIV BITSIZE(CHAR);

<* UNUSED *>
PROCEDURE PerfStart () =
  VAR i, j: Page;
  BEGIN
    IF RTPerfTool.Start("showheap", perfW) THEN
      perfOn := TRUE;
      RTProcess.RegisterExitor(PerfStop);
      PerfGrow(p0, p1 - p0);

      i := p0;
      WHILE i # Nil AND i < p1 DO
        j := i + 1;
        WHILE j < p1 AND desc[j - p0].continued DO INC(j); END;
        IF desc[i - p0].space # Space.Free THEN PerfChange(i, j - i); END;
        i := j;
      END;
    END;
  END PerfStart;

<* UNUSED *>
PROCEDURE PerfFlip () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Flip};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfFlip;

<* UNUSED *>
PROCEDURE PerfPromotedRoots () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Roots};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfPromotedRoots;

PROCEDURE PerfStop () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Bye};
  BEGIN
    (* UNSAFE, but needed to prevent deadlock if we're crashing! *)
    EVAL RTPerfTool.Send (perfW, ADR(e), EventSize);
    RTPerfTool.Close (perfW);
  END PerfStop;

PROCEDURE PerfAllow (<*UNUSED*> n: INTEGER := 0) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Off, nb :=
                       disableCount + disableMotionCount};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfAllow;

<* UNUSED *>
PROCEDURE PerfBegin () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Begin};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfBegin;

<* UNUSED *>
PROCEDURE PerfEnd () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.End};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfEnd;

PROCEDURE PerfChange (first: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Change, first := first,
                       nb := nb, desc := desc[first - p0]};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfChange;

PROCEDURE PerfGrow (firstNew: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{
           kind := RTHeapEvent.Kind.Grow, first := firstNew, nb := nb};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfGrow;

PROCEDURE DumpClean () =
  BEGIN
    RTIO.PutText("Clean regions:\n"); 
    IF cleanRegions = NIL THEN
      RTIO.PutText("  NIL\n"); 
    ELSE
      FOR i := 0 TO cleanRegionsCnt - 1 DO
        RTIO.PutInt(i, 3); RTIO.PutText(" ");
        RTIO.PutAddr(cleanRegions[i].start); RTIO.PutText(" "); 
        RTIO.PutAddr(cleanRegions[i].end); RTIO.PutText("\n"); 
      END;
    END;
  END DumpClean;

PROCEDURE PrintCloseClean (p: ADDRESS) =
  BEGIN
    RTIO.PutText("Close clean regions for: "); 
    RTIO.PutAddr(p); RTIO.PutText("\n"); 
    IF cleanRegions = NIL THEN
      RTIO.PutText("  NIL\n"); 
    ELSE
      FOR i := 0 TO cleanRegionsCnt - 1 DO
        IF (i>0 AND cleanRegions[i-1].end < p AND cleanRegions[i].start > p) OR
          (cleanRegions[i].start <= p AND cleanRegions[i].end >= p) OR
          (i<cleanRegionsCnt AND cleanRegions[i].start < p AND 
          cleanRegions[i+1].end > p)
         THEN
          RTIO.PutInt(i, 5); RTIO.PutText(" ");
          RTIO.PutAddr(cleanRegions[i].start); RTIO.PutText(" "); 
          RTIO.PutAddr(cleanRegions[i].end); RTIO.PutText("\n"); 
        END
      END;
      RTIO.PutText("\n"); 
    END;
  END PrintCloseClean;

PROCEDURE IsOnStack (p: ADDRESS): BOOLEAN =
  VAR
    spl: RTOSMachine.InterruptLevel;
    res: BOOLEAN;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);

    WITH x = LOOPHOLE(p, Word.T) DO
      (* FIXME: DO IT FOR REAL *)
      IF x > 16_ffffffffffff7000 AND x < 16_ffffffffffff8000 THEN
        RTOSMachine.RestoreInterruptMask(spl);
        RETURN TRUE;
      END;
    END;
    IF cleanRegions = NIL THEN
      RTOSMachine.RestoreInterruptMask(spl);
      RETURN FALSE;
    END;

    FOR i := 0 TO cleanRegionsCnt - 1 DO
      IF cleanRegions[i].start <= p AND cleanRegions[i].end >= p THEN
        res := cleanRegions[i].stack;
        RTOSMachine.RestoreInterruptMask(spl);
        RETURN res;
      ELSIF cleanRegions[i].start > p THEN
        RTOSMachine.RestoreInterruptMask(spl);
        RETURN FALSE;
      END;
    END;

    RTOSMachine.RestoreInterruptMask(spl);
    RETURN FALSE;
  END IsOnStack; 

PROCEDURE RegisterClean (start: ADDRESS; size: INTEGER; stack: BOOLEAN) =
  VAR
    pos: INTEGER;
    end: ADDRESS := start + size - ADRSIZE(ADDRESS);
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);

    IF traceClean THEN
      RTIO.PutText("RegisterClean: ");
      RTIO.PutAddr(start); RTIO.PutText(" ");
      RTIO.PutAddr(end); RTIO.PutText(" ");
      RTIO.PutBoolean(stack); RTIO.PutText("\n");
      PrintCloseClean(start);
    END;

    (* check whether the region is non-empty *)
    IF size <= 0 THEN
      RTIO.PutText("GC ERROR >> empty clean region\n");
      RTOS.Crash();
    END;

    (* check whether the region is within untraced heap *)
    IF start < RT0u.kmembase OR end > RT0u.kmemlimit THEN
      RTIO.PutText("GC ERROR >> clean region outside of the untraced heap\n");
      RTOS.Crash();
    END;

    (* create the list if not created yet *)
    IF cleanRegions = NIL THEN
      cleanRegions := NEW(RegionVector, 10000);
    END;

    (* reallocate the array to accommodate new information *)
    IF cleanRegionsCnt = NUMBER(cleanRegions^) THEN
      VAR 
        size := NUMBER(cleanRegions^);
        new := NEW(RegionVector, 2 * size);
      BEGIN
        SUBARRAY(new^, 0, size) := cleanRegions^;
        FOR i := FIRST(cleanRegions^) TO LAST(cleanRegions^) DO
          cleanRegions[i].start := NIL;
          cleanRegions[i].end   := NIL;
          cleanRegions[i].stack := FALSE;
        END;
        cleanRegions := new;
      END;
    END;

    (* find the index in the sorted table for the new region *)
    (* reject overlapping regions *)
    pos := 0;
    IF cleanRegionsCnt # 0 THEN
      WHILE pos < cleanRegionsCnt AND cleanRegions[pos].start < start DO
        INC(pos);
      END;
      IF (pos < cleanRegionsCnt AND cleanRegions[pos].start = start) OR
        (pos > 0 AND cleanRegions[pos-1].end >= start) OR
        (pos < cleanRegionsCnt-1 AND cleanRegions[pos].start <= end)
       THEN
        RTIO.PutText("\nGC ERROR >> overlapping clean regions: ");
        RTIO.PutInt(pos); RTIO.PutText(" "); 
        RTIO.PutInt(cleanRegionsCnt); RTIO.PutText("\nthis: "); 
        RTIO.PutAddr(start); RTIO.PutText(" "); 
        RTIO.PutAddr(end); 
        IF pos > 0 THEN 
          RTIO.PutText("\nprev: "); RTIO.PutAddr(cleanRegions[pos-1].start); 
          RTIO.PutText(" "); RTIO.PutAddr(cleanRegions[pos-1].end); 
          RTIO.PutText("\n"); 
        END;
        IF pos < cleanRegionsCnt THEN 
          RTIO.PutText("\nnext: "); RTIO.PutAddr(cleanRegions[pos].start);
          RTIO.PutText(" "); RTIO.PutAddr(cleanRegions[pos].end);
          RTIO.PutText("\n"); 
        END;
        RTIO.PutText("\n"); 
        IF pos < cleanRegionsCnt AND cleanRegions[pos].start = start THEN
          RTIO.PutText("ONE\n");
        END;
        IF pos > 0 AND cleanRegions[pos-1].end >= start THEN
          RTIO.PutText("TWO\n");
        END;
        IF pos < cleanRegionsCnt-1 AND cleanRegions[pos].start <= end THEN
          RTIO.PutText("THREE\n");
        END;
        RTOS.Crash();
      END;

      (* shift all regions by 1 *)
      FOR i := cleanRegionsCnt TO pos+1 BY -1  DO
        cleanRegions[i] := cleanRegions[i-1];
      END;
    END;

    (* add new region *)
    cleanRegions[pos].start := start;
    cleanRegions[pos].end := end;
    cleanRegions[pos].stack := stack;
    INC(cleanRegionsCnt);
    IF traceClean THEN
      PrintCloseClean(start);
    END;
    RTOSMachine.RestoreInterruptMask(spl);
  END RegisterClean;

PROCEDURE UnregisterClean (start: ADDRESS; size: INTEGER) =
  VAR
    spl: RTOSMachine.InterruptLevel;
    found := FALSE;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);

    IF traceClean THEN
      RTIO.PutText("UnregisterClean: ");
      RTIO.PutAddr(start); RTIO.PutText("\n");
      PrintCloseClean(start);
    END;

    FOR i := 0 TO cleanRegionsCnt-1 DO
      IF start = cleanRegions[i].start THEN
        IF size # -1 AND 
          cleanRegions[i].end # cleanRegions[i].start+size-ADRSIZE(ADDRESS)
         THEN
          RTIO.PutText("ERROR >> UnregisterClean: wrong size for ");
          RTIO.PutAddr(start); RTIO.PutText(": ");
          RTIO.PutInt(size); RTIO.PutText(" instead of ");
          RTIO.PutInt(cleanRegions[i].end - cleanRegions[i].start); 
          RTIO.PutText("\n");
          RTOS.Crash();
        END;
        FOR j := i TO cleanRegionsCnt-2 DO
          cleanRegions[j] := cleanRegions[j+1];
        END;
        DEC(cleanRegionsCnt);
        (*
        RTIO.PutText("DEC: "); RTIO.PutInt(cleanRegionsCnt);RTIO.PutText("\n");
        *)
        found := TRUE;
        EXIT;
      END;
    END;

    IF NOT found THEN
      RTIO.PutText("UnregisterClean - not found: ");
      RTIO.PutAddr(start); RTIO.PutText("\n");
      RTOS.Crash();
    END;
    IF traceClean THEN
      PrintCloseClean(start);
    END;
    RTOSMachine.RestoreInterruptMask(spl);
  END UnregisterClean;

PROCEDURE XXX(<* UNUSED *>a: ADDRESS) =
  BEGIN
  END XXX;

PROCEDURE SweepUntracedHeap (proc: PROCEDURE (start, end: ADDRESS)) =
  VAR
    start: ADDRESS;
  BEGIN
    start := RT0u.kmembase;
    FOR i := 0 TO cleanRegionsCnt-1 DO
      proc(start, cleanRegions[i].start-ADRSIZE(ADDRESS));
      start := cleanRegions[i].end+ADRSIZE(ADDRESS);
    END;
    proc(start, RT0u.kmemlimit);
  END SweepUntracedHeap;

(* ----------------------------------------------------------------------- *)

PROCEDURE GetStatistics(VAR s: Statistics; traverseHeap: BOOLEAN := TRUE) =
  BEGIN
  END GetStatistics;

PROCEDURE GetMoreStatistics (VAR s: Statistics) =
  BEGIN
  END GetMoreStatistics;

PROCEDURE CheckAmbiguous (i: INTEGER) =
  BEGIN
  END CheckAmbiguous;

PROCEDURE CheckAllRefs () =
  BEGIN
  END CheckAllRefs;

PROCEDURE ShowUnsafeAmbiguous(on: BOOLEAN) =
  BEGIN
  END ShowUnsafeAmbiguous;

PROCEDURE AnchorUnsafeAmbiguous(on: BOOLEAN) =
  BEGIN
  END AnchorUnsafeAmbiguous;

PROCEDURE SetSweepUntraced(on: BOOLEAN) =
  BEGIN
  END SetSweepUntraced;

PROCEDURE Verbose(level: INTEGER) =
  BEGIN
  END Verbose;

PROCEDURE Blink (on: BOOLEAN) =
  BEGIN
  END Blink;

PROCEDURE WarnLow (on: BOOLEAN) =
  BEGIN
  END WarnLow;

PROCEDURE GetReference(p: ADDRESS): REFANY =
  BEGIN
    RTIO.PutText("GC ERROR >> GetReference not implemented\n");
    <* ASSERT FALSE *>
  END GetReference;

PROCEDURE TypeName (ref: ADDRESS): TEXT =
  BEGIN
    RTIO.PutText("GC ERROR >> TypeName not implemented\n");
    <* ASSERT FALSE *>
  END TypeName;

PROCEDURE Moved (ref: ADDRESS): BOOLEAN =
  BEGIN
    RTIO.PutText("GC ERROR >> Moved not implemented\n");
    <* ASSERT FALSE *>
    RETURN FALSE;
  END Moved;

PROCEDURE SweepUntracedHeapCl (cl: SweepClosure) : BOOLEAN =
  BEGIN
  END SweepUntracedHeapCl;

PROCEDURE InstallSanityCheck () =
  BEGIN
  END InstallSanityCheck;

PROCEDURE UninstallSanityCheck () =
  BEGIN
  END UninstallSanityCheck;

PROCEDURE ResetStat () =
  BEGIN
  END ResetStat;

FUNCTIONAL PROCEDURE PageToAddress (p: Page): ADDRESS =
  BEGIN
    (* RTIO.PutText("GC ERROR >> PageToAddress not implemented\n");*)
    <* ASSERT FALSE *>
  END PageToAddress;

FUNCTIONAL PROCEDURE AddressToPage (a: ADDRESS): Page =
  BEGIN
    (* RTIO.PutText("GC ERROR >> AddressToPage not implemented\n");*)
    <* ASSERT FALSE *>
  END AddressToPage;

PROCEDURE StopBackgroundCollection() =
  BEGIN
  END StopBackgroundCollection;

PROCEDURE ReferentSize (h: RefHeader): CARDINAL =
  VAR
    res: INTEGER;
    tc: RTType.Typecode;
    def: RT0.TypeDefn;
    size: INTEGER;
  BEGIN
    tc := h.typecode;
    def := RTType.Get (tc);
    IF def.nDimensions = 0 THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize; (* do i want this or the real size??? *)
    END;
    (* ELSE, the referent is an open array; it has the following layout:
|         pointer to the elements (ADDRESS)
|         size 1
|         ....
|         size n
|         optional padding
|         elements
|         ....
       where n is the number of open dimensions (given by the definition)
       and each size is the number of elements along the dimension *)
    VAR
      sizes: UNTRACED REF INTEGER := h + ADRSIZE(Header) + ADRSIZE(ADDRESS);
                                                           (* ^ elt pointer*)
    BEGIN
      res := 1;
      FOR i := 0 TO def.nDimensions - 1 DO
        res := res * sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
      res := res * def.elementSize;
    END;
    res := RTMisc.Upper(res + def.dataSize, BYTESIZE(Header));
    RETURN res;

  END ReferentSize;

BEGIN
END RTCollector.
