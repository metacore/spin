(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Mon May  1 16:30:49 PDT 1995 by kalsow     *)
(*|      modified on Wed May 25 14:41:19 PDT 1994 by detlefs    *)

(*
 * HISTORY							
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added printing of generation and protection in DumpPageStatus.
 *	Do not try to print if the module not initialized.
 *
 * 03-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made the putter optional argument and added creation if not 
 *	passed in.
 *
 * 22-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added DumpPageStatus.
 *
 * 24-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Support RTIO.SimplePutter.
 *
 *)


UNSAFE MODULE RTHeapDebug;

IMPORT RT0, RTCollector, RTHeapRep, RTHeapMap, RTIO, RTParams, RTTypeSRC;
IMPORT Text, WeakRef, Word;

CONST (* Log[n_bytes] = j  =>  2^j = n_bits,  n_bits = 8 * n_bytes *)
  Log = ARRAY [4..16] OF INTEGER {
          5, -1, -1, -1, 6, -1, -1, -1, -1, -1, -1, -1, 7 };

CONST
  MapGrain = 2 * BYTESIZE (RT0.RefHeader);  (* = 1 bit in the map *)
  MapBitsPerHeapPage = RTHeapRep.BytesPerPage DIV MapGrain;
  MapWordsPerHeapPage = MapBitsPerHeapPage DIV BITSIZE (Word.T);
  LogWordSize = Log [BYTESIZE (Word.T)];
  LogMapGrain = Log [MapGrain];

TYPE
  Path    = ARRAY [0..1023] OF INTEGER;
  Map     = REF ARRAY OF Word.T;
  IntList = REF ARRAY OF INTEGER;
  WRList  = REF ARRAY OF WeakRef.T;

TYPE
  Visitor = RTHeapMap.Visitor OBJECT
    freeAddrs : IntList := NIL;
    visited   : Map     := NIL;
    n_to_find : INTEGER := 0;
    heap_min  : INTEGER := 0;
    heap_max  : INTEGER := 0;
    path_len  : INTEGER := 0;
    path      : Path;
    putter    : RTIO.SimplePutter;
  OVERRIDES
    apply := WalkRefAtAddress;
  END;

VAR
  simplePutter : RTIO.SimplePutter := NIL;

VAR
  maxFree  : CARDINAL := GetMaxFree ();
  n_free   : CARDINAL := 0;
  freeRefs := NEW (WRList, maxFree);

PROCEDURE Free(r: REFANY) =
  BEGIN
    RTIO.PutText("freeing: "); RTIO.PutRef(r); RTIO.PutText("\n");
    freeRefs[ n_free MOD maxFree ] := WeakRef.FromRef (r);
    INC (n_free);
  END Free;

PROCEDURE WalkRefAtAddress(v: Visitor;  a: ADDRESS) =
  VAR
    ref := LOOPHOLE(a, UNTRACED REF INTEGER)^;
    optr, map_bit, map_word, mask, visited: INTEGER;
  BEGIN
    IF ref = LOOPHOLE (NIL, INTEGER) THEN RETURN END;
    IF (v.n_to_find <= 0) THEN RETURN END;
    IF (ref < v.heap_min) OR (v.heap_max <= ref) THEN RETURN END;

    map_bit  := Word.RightShift (ref - v.heap_min, LogMapGrain);
    map_word := Word.RightShift (map_bit, LogWordSize);
    mask     := Word.LeftShift (1, Word.And (map_bit, BITSIZE(Word.T)-1));
    visited  := v.visited [map_word];

    IF (Word.And (mask, visited) # 0) THEN (*already visited*) RETURN END;
    v.visited[map_word] := Word.Or (visited, mask);

    IF v.path_len = 0 THEN
      v.path[0] := LOOPHOLE(a, INTEGER);
      v.path_len := 1;
    END;

      FOR i := 0 TO v.n_to_find - 1 DO
        IF (v.freeAddrs[i] = ref) THEN
          Dump (v, a, ref);
          DEC (v.n_to_find);
          v.freeAddrs[i] := v.freeAddrs[v.n_to_find];
        END;
      END;

      IF (v.path_len <= LAST (v.path)) THEN v.path[v.path_len] := ref; END;
      INC (v.path_len);

        optr := ref - BYTESIZE(RT0.RefHeader);
        RTHeapMap.WalkRef (LOOPHOLE (optr, RTHeapMap.ObjectPtr), v);

      DEC (v.path_len);

    IF v.path_len = 1 THEN DEC (v.path_len); END;
  END WalkRefAtAddress;

PROCEDURE Dump (v: Visitor;  loc: ADDRESS;  ref: INTEGER) =
  VAR tc: INTEGER;
  BEGIN
    Out (v.putter, "Path to 'free' object:\n", "   Ref in root", v.path[0]);
    FOR j := 1 TO MIN (v.path_len-1, LAST (v.path)) DO
      tc := TYPECODE (LOOPHOLE(v.path[j], REFANY));
      Out (v.putter, "   Object of type ", RTTypeSRC.TypecodeName(tc),
           v.path[j]);
    END;
    tc := TYPECODE (LOOPHOLE(ref, REFANY));
    Out (v.putter, "   Free object of type ", RTTypeSRC.TypecodeName(tc),
         LOOPHOLE (loc, INTEGER));
  END Dump;

PROCEDURE Out (p: RTIO.SimplePutter; a, b: TEXT;  i: INTEGER) =
  BEGIN
    IF (a # NIL) THEN p.putText (a); END;
    IF (b # NIL) THEN p.putText (b); END;
    p.putText (" at address ");
    p.putHex  (i);
    p.putText ("...\n");
  END Out;

PROCEDURE CheckHeap(p: RTIO.SimplePutter := NIL) =
  VAR
    v       := NEW (Visitor);
    n_pages := RTHeapRep.p1 - RTHeapRep.p0;
    old_ref := freeRefs;
    new_ref := NEW (WRList, maxFree);
    n_alive : CARDINAL := 0;
    ref     : REFANY;
  BEGIN
    IF p = NIL THEN
      p := simplePutter;
    END;

    IF n_free = 0 THEN
      p.putText("No objects have been asserted free\n");
    END;

    v.freeAddrs := NEW (IntList, maxFree);
    v.visited   := NEW (Map, n_pages * MapWordsPerHeapPage);

    RTCollector.Disable();

      v.heap_min := RTHeapRep.p0 * RTHeapRep.BytesPerPage;
      v.heap_max := v.heap_min + n_pages * RTHeapRep.BytesPerPage;
      (* == the limits of the heap described by "v.visited" *)

      FOR i := 0 TO MIN (n_free, maxFree) - 1 DO
        ref := WeakRef.ToRef (old_ref[i]);
        IF ref # NIL THEN
          new_ref[n_alive] := old_ref[i];
          v.freeAddrs[n_alive] := LOOPHOLE (ref, INTEGER);
          INC (n_alive);
        END;
      END;

      freeRefs := new_ref;
      n_free := n_alive;

      IF n_alive > 0 THEN
        v.n_to_find := n_alive;
        RTHeapMap.WalkGlobals(v);
      END;

    RTCollector.Enable();
    p.flush ();

    (* give the collector a chance... *)
    v.freeAddrs := NIL;
    v.visited   := NIL;
    v := NIL;
  END CheckHeap;

PROCEDURE GetMaxFree (): CARDINAL =
  VAR
    txt : TEXT    := RTParams.Value ("heapDebugMaxFree");
    n   : INTEGER := 0;
    ch  : INTEGER;
  BEGIN
    IF (txt = NIL) OR Text.Length (txt) = 0 THEN RETURN MaxFree END;
    FOR i := 0 TO Text.Length(txt)-1 DO
      ch := ORD (Text.GetChar (txt, i)) - ORD ('0');
      IF (ch < 0) OR (9 < ch) THEN RETURN MaxFree END;
      n := 10 * n + ch;
    END;
    IF (n > 0)
      THEN RETURN n;
      ELSE RETURN MaxFree;
    END;
  END GetMaxFree;

PROCEDURE Init() =
  BEGIN
    IF simplePutter = NIL THEN
      simplePutter := NEW(RTIO.SimplePutter);
    END;
  END Init;

BEGIN
  <*ASSERT BYTESIZE (REFANY) = BYTESIZE (INTEGER)*>
END RTHeapDebug.
