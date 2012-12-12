(* Copyright (C) 1994, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Fri Nov 18 17:25:33 PST 1994 by kalsow                   *)
(*      modified on Fri May  6 13:32:10 PDT 1994 by detlefs                  *)
(*      modified on Tue Jun 16 10:41:32 PDT 1992 by muller                   *)
(*      modified on Sun Mar  1 16:06:32 PST 1992 by meehan                   *)

UNSAFE MODULE RTutils;

IMPORT RTHeapRep, RTType, RTTypeSRC, RTIO;

TYPE
  TypeDesc = RECORD
    count : INTEGER := 0;
    size  : INTEGER := 0;
  END;

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

VAR v := NewVisitor ();
    
PROCEDURE NewVisitor (): Visitor =
  BEGIN
    RETURN NEW (Visitor, r := NEW (R, RTType.MaxTypecode() + 1));
  END NewVisitor;

PROCEDURE Heap (suppressZeros := FALSE;
                presentation := HeapPresentation.ByTypecode;
                window := LAST(INTEGER)) =
  BEGIN
    Compute ();
    Report (v, suppressZeros, presentation, window)
  END Heap;
  
PROCEDURE NewHeap (suppressZeros := FALSE;
                   presentation := HeapPresentation.ByTypecode;
                   window := LAST(INTEGER)) =
  VAR oldv := v;
  BEGIN
    Compute ();
    Report (Delta (v, oldv), suppressZeros, presentation, window)
  END NewHeap;

PROCEDURE Compute () =
  BEGIN
    v := NewVisitor ();
    RTHeapRep.VisitAllRefs (v)
  END Compute;

PROCEDURE Delta (v1, v2: Visitor): Visitor =
  VAR v := NewVisitor ();
  BEGIN
    v.countSum := v1.countSum - v2.countSum;
    v.sizeSum := v1.sizeSum - v2.sizeSum;
    FOR i := 0 TO LAST (v.r^) DO
      v.r [i].count := v1.r [i].count - v2.r [i].count;
      v.r [i].size := v1.r [i].size - v2.r [i].size
    END;
    RETURN v
  END Delta;

PROCEDURE Report (v: Visitor;
                  suppressZeros: BOOLEAN;
                  presentation: HeapPresentation;
                  window: INTEGER) =
  VAR
    nPrinted := 0;
    map := NEW (Map, NUMBER (v.r^));
  BEGIN
    FOR i := 0 TO LAST (map^) DO map[i] := i; END;
    CASE presentation OF
    | HeapPresentation.ByTypecode  => (*SKIP*)
    | HeapPresentation.ByNumber    => Sort (map, v.r, CompareCount)
    | HeapPresentation.ByByteCount => Sort (map, v.r, CompareSize)
    END;
    RTIO.PutText (
      (* 012345678901234567890123456789012345678901234567890 *)
        "Code   Count   TotalSize  AvgSize  Name\n"
      & "---- --------- --------- --------- --------------------------\n");
    FOR i := 0 TO LAST (v.r^) DO
      IF (nPrinted >= window) THEN EXIT; END;
      WITH tc = map[i], zz =v.r[tc] DO
        IF (zz.count > 0) OR (NOT suppressZeros) THEN
          RTIO.PutInt (tc, 4);
          RTIO.PutInt (zz.count, 10);
          RTIO.PutInt (zz.size, 10);
          IF (zz.count = 0)
            THEN RTIO.PutText ("         0");
            ELSE RTIO.PutInt  (zz.size DIV zz.count, 10);
          END;
          RTIO.PutChar (' ');
          RTIO.PutText (RTTypeSRC.TypecodeName (tc));
          RTIO.PutChar ('\n');
          INC(nPrinted);
        END
      END;
    END;
    RTIO.PutText ("     --------- ---------\n    ");
    RTIO.PutInt  (v.countSum, 10);
    RTIO.PutInt  (v.sizeSum, 10);
    RTIO.PutChar ('\n');
    RTIO.Flush ();
    map := NIL;
  END Report;

PROCEDURE Walk (v    : Visitor;
                tc   : RTType.Typecode;
   <* UNUSED *> r    : REFANY;
                size : CARDINAL): BOOLEAN =
  BEGIN
    INC (v.r [tc].count);
    INC (v.r [tc].size, size);
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

BEGIN 
END RTutils.
