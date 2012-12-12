(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Nov 18 17:25:33 PST 1994 by kalsow                   *)
(*      modified on Fri May  6 13:32:10 PDT 1994 by detlefs                  *)
(*      modified on Tue Jun 16 10:41:32 PDT 1992 by muller                   *)
(*      modified on Sun Mar  1 16:06:32 PST 1992 by meehan                   *)

(*
 * HISTORY
 * 03-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed a bit implementation to guarantee that no allocation
 *	is done during execution unless printing does.  Added 
 *	reinitailiaztion after the runtime changes (necessary because
 *	memory is preallocated with knowledge of the number of types).
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Differentiate between calls from inside the GC and from outside.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of a warning by making HeapText UNUSED. 
 *
 * 29-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Changed Walk procedure to convert a Typecode to a Typeidx.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Changed a call to RTType.MaxTypecode into a call to RTType.MaxTypeidx
 *)

UNSAFE MODULE RTutils;

IMPORT RTHeapRep, RTType, RTIO, RTCollectorSRC, RT0, RTOS;
IMPORT M3toC, Ctypes;

TYPE
  TypeDesc = RECORD
    count : INTEGER := 0;
    size  : INTEGER := 0;
  END;

CONST
  MAX_LOCATIONS = 5000;

VAR
  objects     : UNTRACED REF ARRAY OF ADDRESS;
  obj_cnt     : INTEGER;

TYPE
  R = REF ARRAY OF TypeDesc;
  Map = REF ARRAY OF INTEGER;
            
  Visitor = RTHeapRep.RefVisitor OBJECT
              r         : R;
              countSum  := 0;
              sizeSum   := 0
            OVERRIDES
              visit := Walk
            END;

VAR
  lock   : BOOLEAN;
  (* set if scan or printing in progress, abort recursive calls to force
     serialization of reports without forcing allocation, so that the 
     module can be called even if the heap is dead *)

VAR
  v      : Visitor;
  v_prev : Visitor;
  map    : Map;
  (* current visitor is in "v", "v_prev" keeps results of previous scan
     to be able to compare *)

VAR 
  simplePutter : RTIO.SimplePutter;
  (* preallocated default objects that prints results *)

(*
 * swap the previous and current visitor to remember the last scan
 * reset the new current visitor
 *)
PROCEDURE NewVisitor (): Visitor =
  VAR
    tmp : Visitor;
  BEGIN
    tmp := v_prev;
    v_prev := v;
    v := tmp;
    v.countSum := 0;
    v.sizeSum  := 0;
    FOR i := FIRST(v.r^) TO LAST (v.r^) DO
      v.r[i].count := 0;
      v.r[i].size  := 0;
    END;
    RETURN v;
  END NewVisitor;

(*
 * call the scan and report
 *)
PROCEDURE DoHeap (suppressZeros := FALSE;
                  presentation  := HeapPresentation.ByTypecode;
                  window        := LAST(INTEGER);
                  p: RTIO.SimplePutter := NIL;
                  fromGC : BOOLEAN := FALSE;
                  doDelta : BOOLEAN) =
  BEGIN
    IF v = NIL THEN 
      RTIO.PutText("ERROR >> RTutils not initialized yet (aborting)\n");
      RETURN;
    END;

    RTOS.LockHeap();
    IF lock THEN
      RTIO.PutText("ERROR >> RTutils entered recursively (aborting)\n");
      RTOS.UnlockHeap();
      RETURN;
    END;
    lock := TRUE;

    (*
    objects   := NEW(UNTRACED REF ARRAY OF ADDRESS, MAX_LOCATIONS);
    obj_cnt   := 0;
    *)

    v := NewVisitor();
    RTHeapRep.VisitAllRefs (v, fromGC);
    RTOS.UnlockHeap();
    
    IF p = NIL THEN p := simplePutter; END;

    VAR
      reportV : Visitor;
    BEGIN
      IF doDelta THEN
        reportV := Delta (v, v_prev);
      ELSE
        reportV := v;
      END;
      Report(p, reportV, suppressZeros, presentation, window);
    END;

    (*
    ReportObjects (p);
    DISPOSE (objects);
    *)

    RTOS.LockHeap();
    lock := FALSE;
    RTOS.UnlockHeap();
  END DoHeap;

(*
 * report the current state of the heap
 *)
PROCEDURE Heap (suppressZeros := FALSE;
                presentation  := HeapPresentation.ByTypecode;
                window        := LAST(INTEGER);
                p: RTIO.SimplePutter := NIL;
                fromGC : BOOLEAN := FALSE) =
  BEGIN
    DoHeap(suppressZeros, presentation, window, p, fromGC, FALSE);
  END Heap;

(*
 * report the difference since the last scan
 *)
PROCEDURE NewHeap (suppressZeros := FALSE;
                   presentation := HeapPresentation.ByTypecode;
                   window := LAST(INTEGER);
                   p: RTIO.SimplePutter := NIL;
                   fromGC : BOOLEAN := FALSE) =
  BEGIN
    DoHeap(suppressZeros, presentation, window, p, fromGC, TRUE);
  END NewHeap;

(*
 * Calculate the difference between the results between current and the
 * previous scans.  Overwrite the previous results since they will be 
 * replaced anyway.  Return the previous one.
 *)
PROCEDURE Delta (v1, v2: Visitor): Visitor =
  VAR v := v2;
  BEGIN
    v.countSum := v1.countSum - v2.countSum;
    v.sizeSum := v1.sizeSum - v2.sizeSum;
    FOR i := 0 TO LAST (v.r^) DO
      v.r [i].count := v1.r [i].count - v2.r [i].count;
      v.r [i].size := v1.r [i].size - v2.r [i].size
    END;
    RETURN v;
  END Delta;

(* 
 * produce a nice report
 *)
PROCEDURE Report (p: RTIO.SimplePutter;
		  v: Visitor;
                  suppressZeros: BOOLEAN;
                  presentation: HeapPresentation;
                  window: INTEGER ) =
  VAR
    nPrinted := 0;
  BEGIN
    FOR i := 0 TO LAST (map^) DO map[i] := i; END;
    CASE presentation OF
    | HeapPresentation.ByTypecode  => (*SKIP*)
    | HeapPresentation.ByNumber    => Sort (map, v.r, CompareCount)
    | HeapPresentation.ByByteCount => Sort (map, v.r, CompareSize)
    END;
    p.putText (
      (* 012345678901234567890123456789012345678901234567890 *)
        "Code   Count    %  TotSize    % AvgSize Name\n"
      & "---- ------- ---- -------- ---- ------- -------------------------\n");
    FOR i := 0 TO LAST (v.r^) DO
      IF (nPrinted >= window) THEN EXIT; END;
      WITH tc = map[i], zz =v.r[tc] DO
        IF (zz.count > 0) OR (NOT suppressZeros) THEN
          p.putInt (tc, 4);
          p.putInt (zz.count, 8); 
          p.putInt (zz.count * 1000 DIV v.countSum, 5); 
          p.putInt (zz.size, 9);
          p.putInt (zz.size * 1000 DIV v.sizeSum, 5); 
          IF (zz.count = 0)
            THEN p.putText ("         0");
            ELSE p.putInt  (zz.size DIV zz.count, 10);
          END;

          p.putChar (' ');
          VAR
            t := RTType.Get (tc);
            name: TEXT;
          BEGIN
            IF (t.name = NIL) THEN
              name := "<anon type>";
            ELSE
              name := M3toC.StoT (LOOPHOLE (t.name, Ctypes.char_star));
            END;
            p.putText (name);
          END;
          p.putChar ('\n');
          INC(nPrinted);
        END
      END;
    END;
    p.putText ("     --------- ---------\n    ");
    p.putInt  (v.countSum, 10);
    p.putInt  (v.sizeSum, 10);
    p.putChar ('\n');
    p.flush ();
  END Report;

(*
 * the visitor procedure, bump up the counters
 *)
PROCEDURE Walk (             v    : Visitor;
                             tc   : RTType.Typecode;
                <* UNUSED *> r    : REFANY;
                             size : CARDINAL): BOOLEAN =
  BEGIN
    INC (v.r[tc].count);
    INC (v.r[tc].size, size);
    INC (v.countSum);
    INC (v.sizeSum, size);
    RETURN TRUE
  END Walk;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE Sort (map: Map;  r: R;  cmp := CompareCount) =
  (* insertion sort such that:  i <= j =>  cmp (r[map[i]], r[map[j]]) <= 0 *)
  VAR n := NUMBER (map^);  j: INTEGER;
  BEGIN
    FOR i := 1 TO n-2 DO
      WITH key = r[map[i]] DO
        j := i-1;
        WHILE (j >= 0) AND cmp (key, r[map[j]]) < 0 DO
          map[j+1] := map[j];
          DEC (j);
        END;
        map[j+1] := i;
      END;
    END;
  END Sort;

PROCEDURE CompareCount (READONLY x, y: TypeDesc): INTEGER =
  BEGIN
    RETURN y.count - x.count;
  END CompareCount;

PROCEDURE CompareSize (READONLY x, y: TypeDesc): INTEGER =
  BEGIN
    RETURN y.size - x.size;
  END CompareSize;

(*--------------------------------------------------------------- extra -----*)

<* UNUSED *>
PROCEDURE AddObject (ref: ADDRESS) =
  BEGIN
    IF obj_cnt = MAX_LOCATIONS THEN RETURN; END;
    FOR i := 0 TO obj_cnt-1 DO
      IF objects[i] = ref THEN
        RETURN;
      END;
    END;
    objects[obj_cnt] := ref;
    INC(obj_cnt);
  END AddObject;

VAR
  verbose: BOOLEAN := FALSE;

(* the pointers to objects of a given type found in the traced heap *)
<* UNUSED *>
PROCEDURE ReportObjects (p: RTIO.SimplePutter) =
  VAR
    addr: ADDRESS;
    ref: REFANY;
    total_size := 0;
    size := 0;
  BEGIN
    IF obj_cnt = MAX_LOCATIONS THEN
      p.putText("\nERROR >> this report incomplete (ran out of space):\n");
    END;
    p.putText("\nObjects in the traced heap:\n");
    IF verbose THEN
      p.putText("reference:          #bytes:   type\n");
      p.putText("---------------------------------------------------------\n");
    END;
    FOR i := 0 TO obj_cnt-1 DO
      addr := objects[i];
      ref := RTCollectorSRC.GetReference(addr);
      size := RTHeapRep.ReferentSize(
                  LOOPHOLE(addr-8, UNTRACED REF RT0.RefHeader));
      INC(total_size, size);

      IF verbose THEN
        p.putInt(i, 4); p.putText("  ");
        p.putAddr(addr); p.putText("  ");
        p.putInt(size, 8);
        p.putText("  "); p.putString(RTType.Get(TYPECODE(ref)).name);
        p.putText("\n");
      END;
    END;
    p.putText("total cnt: "); p.putInt(obj_cnt);
    p.putText("\ntotal size: "); p.putInt(total_size);
    p.putText("\n");
  END ReportObjects;

(*
 * Create objects necessary to run the scan.
 * This procedure must be called after each reinitialization of the 
 * heap with new types.  
 *)
PROCEDURE Init () =
  BEGIN
    RTOS.LockHeap ();
    IF simplePutter = NIL THEN
      simplePutter := NEW (RTIO.SimplePutter);
    END;
    IF v = NIL THEN
      v := NEW (Visitor, r := NEW (R, RTType.MaxTypecode() + 1));
      v_prev := NEW (Visitor, r := NEW (R, RTType.MaxTypecode() + 1));
      map := NEW (Map, NUMBER (v.r^));
    END;
    RTOS.UnlockHeap ();
  END Init;

(* 
 * Reset the state of the module to force aborts until it is reinitailized.
 * Must be called with heap locked.
 *)
PROCEDURE Reset () = 
  BEGIN
    (* we cannot blindly do these assignements because this code is
       run the first time before the traced heap is initialized so
       we cannot do traced reference assignements *)
    IF v # NIL THEN
      v := NIL;
      v_prev := NIL;
      map := NIL;
    END;
  END Reset;

BEGIN 
END RTutils.
