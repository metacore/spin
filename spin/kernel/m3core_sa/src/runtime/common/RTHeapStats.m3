(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Dec 21 14:57:09 PST 1994 by kalsow     *)

(*
 * HISTORY
 * 17-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added code for memory utilization sampling and reporting.
 *
 * 21-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	removed debugging code per Wilson's request.
 *
 * 10-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed variance calculation
 *	cleaned up code
 *
 * 03-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added all sorts of stats code
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed to new interrupt scheme including both classes and levels.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 21-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for summary for a single reference.
 *
 * 12-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed to support three modes of operation parameterized by
 *	the arguments of ReportReachable (idx, pointers_only): 
 *	full reachability report (old style) - report reachability of all 
 *	objets (0, any); type reachablility - (idx, FALSE) reachability 
 *	of the type of index idx (non-zero); type referents - adresses 
 *	at which pointers to an object of a given type are found
 *	(idx, TRUE), the all_pointers argument indicates whether only
 *	one pointer or all pointers to a given object should be reported
 *	per object.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Attempted to reduce reliance on libm3 interfaces.  Fmt* functions
 *	 passed in. Need to remove all knowledge of html from this file.
 *
 * 30-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *	Changed to accomodate SPIN, in particular, the file uses 
 *      RTHeapRep.Typecode so it must reflect the changes to Typecode
 *      and Typeidx.
 *
 *)

(* SPIN - the following assumption is NOT TRUE in SPIN, accordingly 
   the code was changed so that information from saved registers 
   (which are saved on stacks anyway) is treated uniformly with 
   regular activation records *)

(* The code below makes the following NASTY assumption:
      ThreadF.ProcessStacks calls its argument twice for
      each thread -- the first time for the stack, the
      second time for its registers. *)

UNSAFE MODULE RTHeapStats;

IMPORT RT0, RT0u, RTCollector, RTModule, RTIO, RTHeapMap, RTHeapRep, RTMisc;
IMPORT RTOS, RTType, RTTypeSRC, RTProcedure, RTProcedureSRC, RTMachine; 
IMPORT ThreadF, Word, Text, RTCollectorSRC, RTStrongRef;
FROM RTutils IMPORT HeapPresentation;

IMPORT RTMachineCollectorExtern;
IMPORT TCDataUtbl, HeapData;
IMPORT AddrDataUtbl, AddrData;
IMPORT PCDataUtbl, PCData;
IMPORT IntAddr, IntAddrArraySort;
IMPORT Int2Addr, Int2AddrArraySort;
IMPORT AddrMapUtbl;
IMPORT RTTypeMap;
IMPORT Map;

IMPORT RTHisto;
IMPORT RTOSMachine; (* cycle counter *)


TYPE
  Info = RECORD
    module    : RT0.ModulePtr;
    stack_min : ADDRESS;
    stack_max : ADDRESS;
    location  : ADDRESS;
    ref       : ADDRESS;
    n_objects : INTEGER;
    n_bytes   : INTEGER;
  END;

CONST
  MAX_LOCATIONS = 10000;
  MAX_INFO      = 1000;

TYPE
  InfoSet = RECORD
    count : INTEGER;
    info  : ARRAY [0..MAX_INFO-1] OF Info;
  END;

TYPE
  VisitStack = ARRAY [0..10000] OF ADDRESS;

CONST
  MapGrain = 2 * BYTESIZE (RT0.RefHeader);  (* = 1 bit in the map *)
  MapBitsPerHeapPage = RTHeapRep.BytesPerPage DIV MapGrain;
  MapWordsPerHeapPage = MapBitsPerHeapPage DIV BITSIZE (Word.T);

VAR
  units       : InfoSet;
  unit_roots  : InfoSet;
  stacks      : InfoSet;
  stack_roots : InfoSet;
  stack_pages : InfoSet;
  map         : UNTRACED REF ARRAY OF Word.T;
  heap_min    : ADDRESS;
  heap_max    : ADDRESS;
  visit       : Info;
  strongrefs  : BOOLEAN;
  is_registers: BOOLEAN;
  visit_stack : UNTRACED REF VisitStack;
  top_of_stack: INTEGER;
  n_overflows : INTEGER;
  last_alloc  : ADDRESS;
  outerVisitor: RTHeapMap.Visitor := NIL;
  innerVisitor: RTHeapMap.Visitor := NIL;
  rootVisitor : RTHeapMap.Visitor := NIL;

VAR
  locations   : UNTRACED REF ARRAY OF ADDRESS;
  referents   : UNTRACED REF ARRAY OF ADDRESS;
  loc_cnt     : INTEGER;
  curr_tc     : INTEGER;
  do_pointers       : BOOLEAN;
  do_types          : BOOLEAN;
  get_locations     : BOOLEAN;
  get_all_locations : BOOLEAN;
  curr_ref          : ADDRESS;

VAR
  lock : BOOLEAN := FALSE; 

PROCEDURE ReportReachable (p: RTIO.SimplePutter; 
                           tc: RT0.Typecode := 0;
                           ref: INTEGER := 0;
                           detailed: BOOLEAN := FALSE;
                           types: BOOLEAN := FALSE;
                           pointers_only: BOOLEAN := FALSE;
                           all_pointers: BOOLEAN := FALSE) =
  CONST MByte = 1024 * 1024;
  BEGIN
    (* lock the service *)
    RTOS.LockHeap();
    IF lock THEN
      RTIO.PutText("ERROR >> RTHeapStats entered recursively (aborting)\n");
      RTOS.UnlockHeap();
      RETURN;
    END;
    lock := TRUE;
    RTOS.UnlockHeap();

    (*
     * prepare for the collecting the data
     *)

    (* initialize the globals *)
    units.count       := 0;
    unit_roots.count  := 0;
    stacks.count      := 0;
    stack_roots.count := 0;
    stack_pages.count := 0;
    top_of_stack      := 0;
    n_overflows       := 0;
    curr_tc           := tc;
    curr_ref          := LOOPHOLE(ref, ADDRESS);

    (* allocate space for the stats *)
    outerVisitor := NEW (RTHeapMap.Visitor, apply := Visit);
    innerVisitor := NEW (RTHeapMap.Visitor, apply := InnerVisit);
    rootVisitor  := NEW (RTHeapMap.Visitor, apply := VisitRoot);
    visit_stack  := NEW (UNTRACED REF VisitStack);
    map := NEW (UNTRACED REF ARRAY OF Word.T,
                 (RTHeapRep.p1 - RTHeapRep.p0) * MapWordsPerHeapPage);

    do_pointers  := pointers_only OR curr_ref # NIL;
    DISPOSE (objects);
    IF pointers_only THEN
      locations := NEW (UNTRACED REF ARRAY OF ADDRESS, MAX_LOCATIONS);
      referents := NEW (UNTRACED REF ARRAY OF ADDRESS, MAX_LOCATIONS);
      loc_cnt := 0;
    END;
    objects   := NEW (UNTRACED REF ARRAY OF ADDRESS, MAX_LOCATIONS);
    obj_cnt := 0;

    do_types := types;
    IF do_types THEN
      typeCounts := NEW (R, RTType.MaxTypecode()+1);
      ResetTypeCounts();
    END;

    (*
     * get the data
     *)

    (* freeze the world *)
    RTCollector.Disable ();
    RTOS.LockHeap (); (* freeze the heap *)

    (* disable allocations *)
    (* RTCollectorSRC.AllertAllocation(TRUE);*)

    (* capture the heap limits *)
    heap_min  := RTHeapRep.MinAddress();
    heap_max  := RTHeapRep.MaxAddress();

    (* find the edge of the new space *)
    last_alloc := LOOPHOLE (NEW (REF INTEGER), ADDRESS);

    (* get summaries for each module and interface *)
    get_locations       := pointers_only;
    get_all_locations   := pointers_only AND all_pointers;
    FOR i := 0 TO RTModule.Count() - 1 DO GetUnitStats (i); END;

    (* get summaries for each global variable *)
    get_locations       := FALSE;
    get_all_locations   := FALSE;
    IF detailed AND NOT do_types THEN
      FOR i := 0 TO RTModule.Count() - 1 DO GetUnitRootStats (i); END;
    END;

    (* get summary of each stack *)
    is_registers := FALSE;
    strongrefs := FALSE;
    ThreadF.ProcessStacks (GetThreadStats);
    strongrefs := TRUE;
    IF NOT do_types THEN
      ResetVisitCountsForStacks(NIL, NIL);
    END;
    RTStrongRef.ProcessRefs(GetThreadStats);
    AddVisit(stacks);

    (* get stack roots (optimistic) *)
    IF detailed AND NOT do_types THEN
      strongrefs := FALSE;
      ThreadF.ProcessStacks (GetThreadRootStats);
      strongrefs := TRUE;
      ResetVisitCountsForStacks(NIL, NIL);
      RTStrongRef.ProcessRefs(GetThreadRootStats);
      AddVisit(stacks);
    END;

    (* get stack roots (conservative) *)
    IF detailed AND NOT do_types THEN
      strongrefs := FALSE;
      ThreadF.ProcessStacks (GetThreadPageStats);
      strongrefs := TRUE;
      ResetVisitCountsForStacks(NIL, NIL);
      RTStrongRef.ProcessRefs(GetThreadPageStats);
      AddVisit(stacks);
    END;

    (* it's OK to allocate now because the collection is disabled so
       we won't disturbed the data we already collected *)
    (* RTCollectorSRC.AllertAllocation(FALSE);*)

    (*
     * generate the report (we can do it without locking the heap
     * because we captured all the information)
     *)

    p.putText ("\nSummary for ");
    IF curr_tc # 0 THEN
      p.putText("type index "); p.putInt(curr_tc);
      IF pointers_only AND all_pointers THEN
        p.putText(" (all pointers from the traced heap)");
      ELSE
        p.putText(" (single pointer from the traced heap)");
      END;
    ELSIF curr_ref # NIL THEN
      p.putText("reference "); p.putAddr(curr_ref);
    ELSIF do_types THEN
      p.putText("reachability counts per type");
    ELSE
      p.putText("full reachability");
    END;

    p.putText ("\nHEAP: ");
    p.putAddr (heap_min);
    p.putText (" .. ");
    p.putAddr (heap_max);
    p.putText (" => ");
    p.putInt ((heap_max - heap_min) DIV MByte);
    p.putText (".");
    p.putInt ((heap_max - heap_min) * 10 DIV MByte MOD 10);
    p.putText (" Mbytes\n");

    IF (n_overflows > 0) THEN
      p.putText ("  ** warning: ");
      p.putInt (n_overflows);
      p.putText (" paths, longer than ");
      p.putInt (NUMBER (VisitStack));
      p.putText (" REFs, were truncated.\n");
    END;

    IF NOT do_types THEN
      ReportObjects (p);
      IF pointers_only THEN
        ReportLocationsOnHeap (p);
      END;

      IF NOT pointers_only THEN
        ReportUnits (p);
      END;
    
      IF detailed THEN ReportUnitRoots (p); END;
      ReportStacks (p);
      IF detailed THEN ReportStackRoots (p); END;
      IF detailed THEN ReportStackPages (p); END;
      
      IF tc = 0 THEN
        IF detailed THEN
          ReportStackPCs (p);
          ReportStackRootPCs (p);
          ReportStackPagePCs (p);
        END;
      END;
    ELSE
      ReportTypes (p, TRUE, HeapPresentation.ByByteCount, LAST(INTEGER));
    END;


    p.flush ();

    (*
     * thaw the world
     *)

    RTOS.UnlockHeap (); (* unfreeze the heap *)
    RTCollector.Enable ();

    (*
     * clean up
     *)

    DISPOSE (visit_stack);
    DISPOSE (map);

    IF pointers_only THEN
      DISPOSE (locations);
      DISPOSE (referents);
      DISPOSE (objects);
    END;
    IF do_types THEN
      DISPOSE (typeCounts);
    END;

    (* unlock the service *)
    RTOS.LockHeap();
    lock := FALSE;
    RTOS.UnlockHeap();
  END ReportReachable;

(*------------------------------------------------------------ REF visits ---*)

PROCEDURE ResetVisitCounts () =
  BEGIN
    visit.n_objects := 0;
    visit.n_bytes   := 0;
    top_of_stack    := 0;
    RTMisc.Zero (ADR (map[0]), BYTESIZE (map^));
  END ResetVisitCounts;

PROCEDURE ResetVisitCountsForStacks (start, stop: ADDRESS) =
  BEGIN
    visit.module     := NIL;
    visit.stack_min  := start;
    visit.stack_max  := stop;
    visit.location   := NIL;
    visit.ref        := NIL;
    ResetVisitCounts ();
  END ResetVisitCountsForStacks;

PROCEDURE AddVisit (VAR s: InfoSet) =
  VAR n: INTEGER;
  BEGIN
    (* if the set isn't full, make room for this visit *)
    IF (s.count < NUMBER (s.info)) THEN
      s.info[s.count].n_bytes := -1;
      INC (s.count);
    END;

    (* find where to insert this visit *)
    n := s.count-1;
    WHILE (n >= 0) AND (s.info[n].n_bytes < visit.n_bytes) DO
      IF (n < LAST(s.info)) THEN s.info[n+1] := s.info[n]; END;
      DEC (n);
    END;
    INC (n);

    (* insert the new root *)
    IF (n < s.count) THEN  s.info[n] := visit;  END;
  END AddVisit;

PROCEDURE Visit (<*UNUSED*> self: RTHeapMap.Visitor;  loc: ADDRESS) =
  BEGIN
    InnerVisit (NIL, loc);
    WHILE (top_of_stack > 0) DO
      DEC (top_of_stack);
      RTHeapMap.WalkRef (visit_stack[top_of_stack], innerVisitor);
    END;
  END Visit;

PROCEDURE InnerVisit (<*UNUSED*> self: RTHeapMap.Visitor;  loc: ADDRESS) =
  CONST Mask = ADRSIZE (RT0.RefHeader) - 1; (* assume it's 2^k-1 for some k *)
  VAR ptr : UNTRACED REF ADDRESS := loc;
  VAR ref : ADDRESS := ptr^;
  VAR header: RTHeapMap.ObjectPtr;
  VAR cell, word, bit, mask, typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (heap_min <= ref) AND (ref < heap_max) AND
       (Word.And (LOOPHOLE(ref, INTEGER), Mask) = 0) AND
       RTType.IsValid(header.typecode) 
     THEN
      typecode := header.typecode;
      IF (0 < typecode) AND (typecode < RT0u.nTypes) THEN
        cell := (ref - heap_min) DIV MapGrain;
        word := cell DIV BITSIZE (Word.T);
        bit  := cell - word * BITSIZE (Word.T);
        mask := Word.LeftShift (1, bit);
        IF (get_all_locations AND (typecode = curr_tc OR curr_ref = ref)) THEN
          AddLocation(loc, ref); 
        END;
        IF (Word.And (mask, map[word]) = 0) THEN
          (* this is a new ref... *)
          IF do_types THEN
            AddToTypeCounts(header);
          END;
          map[word] := Word.Or (mask, map[word]);
          IF (curr_tc = 0 AND curr_ref = NIL) OR
            (typecode = curr_tc OR ref = curr_ref) 
           THEN
            INC (visit.n_objects);
            INC (visit.n_bytes, DataSize (header) + BYTESIZE (RT0.RefHeader));
            IF get_locations AND NOT get_all_locations THEN
              AddLocation(loc, ref); 
            END;
          END;
          IF (top_of_stack < NUMBER (VisitStack)) THEN
            visit_stack [top_of_stack] := header;
            INC (top_of_stack);
          ELSE
            INC (n_overflows);
          END;
        END;
      ELSE
        (* <* ASSERT FALSE *> *)
      END;
    END;
  END InnerVisit;

(*
 * counts and total sizes per types
 *)

TYPE
  TypeDesc = RECORD
    count : INTEGER := 0;
    size  : INTEGER := 0;
  END;

VAR
  countSum   : INTEGER;
  sizeSum    : INTEGER;
  typeCounts : R;

PROCEDURE ResetTypeCounts () =
  BEGIN
    countSum := 0;
    sizeSum  := 0;
    FOR i := FIRST(typeCounts^) TO LAST (typeCounts^) DO
      typeCounts[i].count := 0;
      typeCounts[i].size  := 0;
    END;
  END ResetTypeCounts;

PROCEDURE AddToTypeCounts(header: RTHeapMap.ObjectPtr) =
  VAR
    tc     := header.typecode;
    size   := RTHeapRep.ReferentSize(header);
  BEGIN
    INC (typeCounts[tc].count);
    INC (typeCounts[tc].size, size);
    INC (countSum);
    INC (sizeSum, size);
  END AddToTypeCounts;

TYPE
  R = REF ARRAY OF TypeDesc;
  RMap = REF ARRAY OF INTEGER;
            
PROCEDURE ReportTypes (p: RTIO.SimplePutter;
                       suppressZeros: BOOLEAN;
                       presentation: HeapPresentation;
                       window: INTEGER) =
  VAR
    nPrinted := 0;
    map := NEW (RMap, NUMBER (typeCounts^));
  BEGIN
    FOR i := 0 TO LAST (map^) DO map[i] := i; END;
    CASE presentation OF
    | HeapPresentation.ByTypecode  => (*SKIP*)
    | HeapPresentation.ByNumber    => Sort (map, typeCounts, CompareCount)
    | HeapPresentation.ByByteCount => Sort (map, typeCounts, CompareSize)
    END;
    p.putText (
      (* 012345678901234567890123456789012345678901234567890 *)
        "Code   Count   TotalSize  AvgSize  Name\n"
      & "---- --------- --------- --------- --------------------------\n");
    FOR i := 0 TO LAST (typeCounts^) DO
      IF (nPrinted >= window) THEN EXIT; END;
      WITH tc = map[i], zz = typeCounts[tc] DO
        IF (zz.count > 0) OR (NOT suppressZeros) THEN
          p.putInt (tc, 4);
          p.putInt (zz.count, 10);
          p.putInt (zz.size, 10);
          IF (zz.count = 0)
            THEN p.putText ("         0");
            ELSE p.putInt  (zz.size DIV zz.count, 10);
          END;

          p.putChar (' ');
          p.putText (RTTypeSRC.TypecodeName (tc));
          p.putChar ('\n');
          INC(nPrinted);
        END
      END;
    END;
    p.putText ("     --------- ---------\n    ");
    p.putInt  (countSum, 10);
    p.putInt  (sizeSum, 10);
    p.putChar ('\n');
    p.flush ();
    map := NIL;
  END ReportTypes;

(*--------------------------------------------------------------- sorting ---*)

PROCEDURE Sort (map: RMap;  r: R;  cmp := CompareCount) =
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

(*
 *
 *)

PROCEDURE AddLocation(loc: ADDRESS; ref: ADDRESS) =
  BEGIN
    IF loc_cnt = MAX_LOCATIONS THEN RETURN; END;
    FOR i := 0 TO loc_cnt-1 DO
      IF locations[i] = loc THEN
        RETURN;
      END;
    END;
    locations[loc_cnt] := loc;
    referents[loc_cnt] := ref;

    INC(loc_cnt);
  END AddLocation;

PROCEDURE DataSize (h: RTHeapMap.ObjectPtr): CARDINAL =
  VAR
    res : INTEGER;
    tc  : RT0.Typecode := h.typecode;
    def : RT0.TypeDefn;
  BEGIN
    IF tc = RTHeapRep.Fill_1_type THEN RETURN 0; END;
    IF tc = RTHeapRep.Fill_N_type THEN
      res := LOOPHOLE(h + ADRSIZE(RT0.RefHeader), UNTRACED REF INTEGER)^;
      RETURN res - BYTESIZE(RT0.RefHeader);
    END;
    def := RTType.Get (tc);
    IF def.nDimensions = 0 THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize;
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
      sizes: UNTRACED REF INTEGER := h + ADRSIZE(RT0.RefHeader)
                                       + ADRSIZE(ADDRESS);  (* ^ elt pointer*)
    BEGIN
      res := 1;
      FOR i := 0 TO def.nDimensions - 1 DO
        res := res * sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
      res := res * def.elementSize;
    END;
    res := RTMisc.Upper(res + def.dataSize, BYTESIZE(RT0.RefHeader));
    RETURN res;
  END DataSize;

PROCEDURE TypeName (ref: ADDRESS): TEXT =
  VAR header: RTHeapMap.ObjectPtr;
  VAR tc: RT0.Typecode;
  BEGIN
    IF heap_min <= ref AND heap_max >= ref THEN
      ref := LOOPHOLE(RTCollectorSRC.GetReference(ref), ADDRESS);
      IF ref # NIL THEN
        header := ref - ADRSIZE (RT0.RefHeader);
        tc := header.typecode;
        IF RTType.IsValid(tc) THEN
          IF (0 < tc) AND (tc < RT0u.nTypes) THEN
            WITH tname = RTTypeSRC.TypecodeName(tc) DO
              IF Text.Equal(tname, "<anon type>") THEN
                RETURN "Anonymous type";
              ELSE
                RETURN tname;
              END;
            END;
          END;
        END;
      END;
    END;
    RETURN "?";
  END TypeName;

(*----------------------------------------------------------------- units ---*)

PROCEDURE GetUnitStats (n: CARDINAL) =
  BEGIN
    visit.module     := RTModule.Get (n);
    visit.stack_min  := NIL;
    visit.stack_max  := NIL;
    visit.location   := NIL;
    visit.ref        := NIL;
    IF NOT do_types THEN
      ResetVisitCounts ();
    END;
    RTHeapMap.WalkModuleGlobals (outerVisitor, n);
    AddVisit (units);
  END GetUnitStats;

PROCEDURE GetUnitRootStats (n: CARDINAL) =
  BEGIN
    visit.module     := RTModule.Get (n);
    visit.stack_min  := NIL;
    visit.stack_max  := NIL;
    visit.location   := NIL;
    visit.ref        := NIL;
    RTHeapMap.WalkModuleGlobals (rootVisitor, n);
  END GetUnitRootStats;

PROCEDURE VisitRoot (<*UNUSED*> self: RTHeapMap.Visitor;  root: ADDRESS) =
  VAR p: UNTRACED REF ADDRESS := root;
  BEGIN
    visit.location := root;
    visit.ref      := p^;
    ResetVisitCounts ();
    IF do_pointers THEN
      InnerVisit (NIL, root);
    ELSE
      Visit (NIL, root);
    END;
    AddVisit (unit_roots);
  END VisitRoot;

(*--------------------------------------------------------------- threads ---*)

PROCEDURE GetThreadStats (start, stop: ADDRESS (*; th: REFANY*)) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    IF NOT (do_types OR strongrefs OR is_registers) THEN
      ResetVisitCountsForStacks (start, stop);
    END;

    (* scan the stack or registers *)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          VisitPage (page);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;

    IF NOT (strongrefs) THEN
      AddVisit (stacks);
    END;

    (* SPIN - disable alternation between saved registers and stack *)
    (* because in SPIN registers are also on stack *)
    (*
      IF (is_registers) THEN AddVisit (stacks); END;
      is_registers := NOT is_registers;
    *)
  END GetThreadStats;

PROCEDURE GetThreadRootStats (start, stop: ADDRESS (*; th: REFANY*)) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    IF (is_registers) THEN
      visit.location   := NIL;
    ELSE
      visit.module     := NIL;
      visit.stack_min  := start;
      visit.stack_max  := stop;
    END;

    (* scan the stack *)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          IF (NOT is_registers) THEN visit.location := fp; END;
          visit.ref      := p;
          ResetVisitCounts ();
          IF do_pointers THEN
            InnerVisit (NIL, fp);
          ELSE
            Visit (NIL, fp);
          END;
          AddVisit (stack_roots);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;

    (* SPIN - disable alternation between saved registers and stack *)
    (* in SPIN registers are also on stack because *)
    (*
      is_registers := NOT is_registers;
    *)
  END GetThreadRootStats;

PROCEDURE GetThreadPageStats (start, stop: ADDRESS (*; th: REFANY*)) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    IF (is_registers) THEN
      visit.location   := NIL;
    ELSE
      visit.module     := NIL;
      visit.stack_min  := start;
      visit.stack_max  := stop;
    END;

    (* scan the stack *)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF heap_min <= p AND p < heap_max THEN
        page := (p - heap_min) DIV RTHeapRep.BytesPerPage;
        IF RTHeapRep.desc[page].space = RTHeapRep.Space.Current THEN
          IF (NOT is_registers) THEN visit.location := fp; END;
          visit.ref      := p;
          ResetVisitCounts ();
          VisitPage (page);
          AddVisit (stack_pages);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;

    (* SPIN - disable alternation between saved registers and stack *)
    (* in SPIN registers are also on stack because *)
    (*
      is_registers := NOT is_registers;
    *)
  END GetThreadPageStats;

PROCEDURE VisitPage (page: INTEGER) =
  VAR start, stop: ADDRESS;  h: RTHeapMap.ObjectPtr;  ref: ADDRESS;
  BEGIN
    (* find the address limits of this "page" *)
    WHILE (page > 0)
      AND (RTHeapRep.desc[page].space = RTHeapRep.Space.Current)
      AND (RTHeapRep.desc[page].continued) DO
      DEC (page);
    END;
    start := heap_min + page * RTHeapRep.BytesPerPage;
    REPEAT
      INC (page);
    UNTIL (page >= RTHeapRep.p1-RTHeapRep.p0)
       OR (RTHeapRep.desc[page].space # RTHeapRep.Space.Current)
       OR (NOT RTHeapRep.desc[page].continued);
    stop := heap_min + page * RTHeapRep.BytesPerPage;

    IF (start <= last_alloc) AND (last_alloc < stop) THEN
      (* we're on the allocator's partial page... *)
      stop := last_alloc;
    END;

    (* visit each object on the page *)
    h := start;
    WHILE (h < stop) AND RTType.IsValid(h.typecode) DO
      ref := h + ADRSIZE (RT0.RefHeader);
      IF do_pointers THEN
        IF RTType.Get(h.typecode).typecode = curr_tc OR curr_ref = ref THEN 
          InnerVisit (NIL, ADR (ref));
        END;
      ELSE
        Visit (NIL, ADR (ref));
      END;
      INC (h, DataSize (h) + ADRSIZE (RT0.RefHeader));
    END;
  END VisitPage;

(*--------------------------------------------------------------- reports ---*)

(* the pointers to objects of a given type found in the traced heap *)
PROCEDURE ReportLocationsOnHeap (p: RTIO.SimplePutter) =
  VAR
    loc: ADDRESS;
    addr: ADDRESS;
    ref: REFANY;
  BEGIN
    IF loc_cnt = MAX_LOCATIONS THEN
      p.putText("\nERROR >> this report incomplete (ran out of space):\n");
    END;
    p.putText("\nLocations on the traced heap:\n");
    p.putText("reference:          #bytes:  at location:       inside of (+ offset):\n");
    p.putText("-----------------------------------------------------------\n");
    FOR i := 0 TO loc_cnt-1 DO
      loc := locations[i];
      addr := referents[i];

      IF (heap_min <= loc) AND (loc < heap_max) THEN
        p.putText("0x"); p.putHex(LOOPHOLE(addr, INTEGER)); 
        ref := RTCollectorSRC.GetReference(addr);
        p.putText("  ");
        (* XXX This is machine dependent!! *)
        p.putInt(RTHeapRep.ReferentSize(
                     LOOPHOLE((LOOPHOLE(ref, ADDRESS) - 8), 
                              UNTRACED REF RT0.RefHeader)), 8);
        p.putText(" 0x"); p.putHex(LOOPHOLE(loc, INTEGER)); p.putText(" "); 
        ref := RTCollectorSRC.GetReference(loc);
        p.putText(" 0x"); p.putHex(LOOPHOLE(ref, INTEGER)); p.putText(" "); 
        IF ref = NIL THEN 
          p.putText("ERROR >> could not find type for the reference");
        ELSE
          p.putString(RTType.Get(TYPECODE(ref)).name);
        END;
        p.putText(" + "); 
        p.putInt(loc - LOOPHOLE(ref, ADDRESS)); 
        p.putText("\n"); 
      END;
    END;
  END ReportLocationsOnHeap;

VAR
  verbose: BOOLEAN := FALSE;

(* the pointers to objects of a given type found in the traced heap *)
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
      (* XXX Machine dependent! *)
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

(* objects reachable from each unit *)
PROCEDURE ReportUnits (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nModule globals:\n");
    p.putText (" # objects   # bytes  unit\n");
    p.putText (" ---------  --------  -----------------\n");
    FOR i := 0 TO units.count-1 DO
      WITH m = units.info[i] DO
        IF (m.n_bytes > 0) THEN
          p.putInt (m.n_objects, 10);
          p.putInt (m.n_bytes, 10);
          p.putText ("  ");
          p.putString  (PathTail (m.module.file));
          p.putText ("\n");
        END;
      END;
    END;
  END ReportUnits;


(* objects reachable from each global variable *)
PROCEDURE ReportUnitRoots (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nGlobal variable roots:\n");
    p.putText (" # objects   # bytes                 ref type                location\n");
    p.putText (" ---------  --------  ------------------ -----------------   ------------------------\n");
    FOR i := 0 TO unit_roots.count-1 DO
      WITH r = unit_roots.info[i] DO
        IF (r.n_bytes > 0) THEN
          p.putInt  (r.n_objects, 10);
          p.putInt  (r.n_bytes, 10);
          p.putText ("  ");
          p.putAddr (r.ref);
          p.putText (" ");
          PadText (p, TypeName (r.ref), 18);
          p.putText ("  ");
          p.putString  (PathTail (r.module.file));
          p.putText (" + ");
          p.putInt (r.location - r.module);
          p.putText ("\n");
        END;
      END;
    END;
  END ReportUnitRoots;


(* objects reachable conservatively from stacks *)
(* located on pages pointed to from the stacks *)
PROCEDURE ReportStacks (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nThread stacks (conservative page scan):\n");
    p.putText (" # objects   # bytes  stack bounds\n");
    p.putText (" ---------  --------  ------------------------\n");
    FOR i := 0 TO stacks.count-1 DO
      WITH t = stacks.info[i] DO
        IF (t.n_bytes > 0) THEN
          p.putInt (t.n_objects, 10);
          p.putInt (t.n_bytes, 10);
          p.putText ("  [");
          p.putAddr (t.stack_min);
          p.putText ("..");
          p.putAddr (t.stack_max);
          p.putText ("]");
          IF t.stack_min = NIL AND t.stack_max = NIL THEN
            p.putText (" (strongrefs)");
          END;
          p.putText ("\n");
        END;
      END;
    END;
  END ReportStacks;


PROCEDURE ReportStackPCs (p: RTIO.SimplePutter) =
  VAR stack_mid: ADDRESS;
  BEGIN
    p.putText ("\nThread stack PCs:\n");
    p.putText ("     SP         PC     procedure\n");
    p.putText ("----------  ---------  ------------------------\n");
    FOR i := 0 TO stacks.count-1 DO
      WITH t= stacks.info[i] DO
        IF (t.n_bytes > 0) THEN
          p.putText ("                          [");
          p.putAddr (t.stack_min);
          p.putText ("..");
          p.putAddr (t.stack_max);
          p.putText ("]\n");
          stack_mid := t.stack_min + (t.stack_max - t.stack_min) DIV 2;
          ReportPCs (p, t.stack_min, stack_mid, 5, +1);
          p.putText ("  ...\n");
          ReportPCs (p, t.stack_max, stack_mid, 5, -1);
          p.putText ("\n");
        END;
      END;
    END;
  END ReportStackPCs;

(* objects directly (optimistically) reachable from stacks *)
PROCEDURE ReportStackRoots (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nThread stack roots (optimistic):\n");
    ReportStackInfo (p, stack_roots);
  END ReportStackRoots;


PROCEDURE ReportStackPages (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nThread stack roots (conservative page scan):\n");
    ReportStackInfo (p, stack_pages);
  END ReportStackPages;


PROCEDURE ReportStackInfo (p: RTIO.SimplePutter; READONLY s: InfoSet) =
  BEGIN
    p.putText (" # objects   # bytes                 ref type                location\n");
    p.putText (" ---------  --------  ------------------ -----------------   ------------------------\n");
    FOR i := 0 TO s.count-1 DO
      WITH r = s.info[i] DO
        IF (r.n_bytes > 0) THEN
          p.putInt  (r.n_objects, 10);
          p.putInt  (r.n_bytes, 10);
          p.putText ("  ");
          p.putAddr (r.ref);
          p.putText (" ");
          PadText (p, TypeName (r.ref), 18);
          p.putText ("  ");
          IF (r.location # NIL) THEN
            p.putText ("sp+");
            p.putInt  (r.location - r.stack_min);
          ELSE
            p.putText ("register");
          END;
          p.putText (" in [");
          p.putAddr (r.stack_min);
          p.putText ("..");
          p.putAddr (r.stack_max);
          p.putText ("]");
          p.putText ("\n");
        END;
      END;
    END;
  END ReportStackInfo;

PROCEDURE ReportStackRootPCs (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nThread stack root PCs (optimistic):\n");
    ReportStackInfoPCs (p, stack_roots);
  END ReportStackRootPCs;


PROCEDURE ReportStackPagePCs (p: RTIO.SimplePutter) =
  BEGIN
    p.putText ("\nThread stack root PCs (conservative page scan):\n");
    ReportStackInfoPCs (p, stack_pages);
  END ReportStackPagePCs;


PROCEDURE ReportStackInfoPCs (p: RTIO.SimplePutter; READONLY s: InfoSet) =
  VAR m: ARRAY [0..LAST(s.info)] OF INTEGER;  xx, xy: ADDRESS;
  BEGIN
    (* first, sort the set by location *)
    m[0] := 0;
    FOR i := 1 TO s.count-1 DO
      VAR j := i-1; key := s.info[i].location;  BEGIN
        WHILE (j >= 0) AND (s.info[m[j]].location > key) DO
          m[j+1] := m[j];
          DEC (j);
        END;
        m[j+1] := i;
      END;
    END;

    p.putText ("     SP                 PC             location\n");
    p.putText ("------------------ ------------------  ------------------------\n");
    FOR i := 0 TO s.count-1 DO
      WITH r = s.info[m[i]] DO
        IF (r.n_bytes > 0) AND (r.location # NIL) THEN
          xy := r.location - RTMachine.PointerAlignment;
          IF (i = 0) OR (s.info[m[i-1]].location = NIL) OR
            (s.info[m[i-1]].stack_min # r.stack_min) THEN
            (* this is the first entry on this stack *)
            xx := xy + (r.stack_min - xy) DIV 2;
            p.putAddr (r.stack_min, 10);
            p.putText ("              [");
            p.putAddr (r.stack_min);
            p.putText ("..");
            p.putAddr (r.stack_max);
            p.putText ("]\n");
            ReportPCs (p, r.stack_min, xx, 3, +1);
          ELSE
            xx := xy + (s.info[m[i-1]].location - xy) DIV 2;
          END;
          p.putText ("...\n");
          ReportPCs (p, xy, xx, 3, -1);
          p.putAddr (r.location, 10);
          p.putText ("                ");
          p.putText ("sp+");
          p.putInt (r.location - r.stack_min);
          p.putText ("\n");
          xy := r.location + RTMachine.PointerAlignment;
          IF (i = s.count-1) OR (s.info[m[i+1]].location = NIL) OR
            (s.info[m[i+1]].stack_min # r.stack_min) THEN
            (* this is the last entry on this stack *)
            xx := xy + (r.stack_max - xy) DIV 2;
            ReportPCs (p, xy, xx, 3, +1);
            p.putText ("...\n");
            ReportPCs (p, r.stack_max, xx, 3, -1);
            p.putAddr (r.stack_max, 10);
            p.putText ("\n\n");
          ELSE
            xx := xy + (s.info[m[i+1]].location - xy) DIV 2;
            ReportPCs (p, xy, xx, 3, +1);
          END;
        END;
      END;
    END;
  END ReportStackInfoPCs;


(*--------------------------------------------------------------- PC info ---*)

CONST
  Max_proc = 4096;  (* good enough for 99% of the procedures *)

TYPE
  PCInfo = RECORD
    loc  : ADDRESS;
    pc   : ADDRESS;
    proc : RTProcedure.Proc;
    file : RTProcedureSRC.Name;
    name : RTProcedureSRC.Name;
  END;


PROCEDURE ReportPCs (p          : RTIO.SimplePutter;
                     start, stop: ADDRESS;
                     max, dir   : INTEGER            ) =
  VAR
    x: ARRAY [0 .. 9] OF PCInfo;
    cnt := FindPCs(start, stop, SUBARRAY(x, 0, MIN(max, NUMBER(x))));
    a, b: INTEGER;
  BEGIN
    a := 0;
    b := cnt - 1;
    IF (dir < 0) THEN a := b; b := 0; END;
    FOR i := a TO b BY dir DO
      WITH pc = x[i] DO
        p.putAddr(pc.loc, 10);
        p.putText(" ");
        p.putAddr(pc.pc, 10);
        p.putText("  ");
        p.putString(pc.name);
        IF (pc.pc # pc.proc) THEN
          p.putText(" + ");
          p.putInt(pc.pc - pc.proc);
        END;
        IF (pc.file # NIL) THEN
          p.putText(" in ");
          p.putString(PathTail(pc.file));
        END;
        p.putText("\n");
      END;
    END;
  END ReportPCs;


PROCEDURE FindPCs (start, stop: ADDRESS;  VAR x: ARRAY OF PCInfo): INTEGER =
  VAR
    n := 0;  
    fp: UNTRACED REF ADDRESS := start;
    proc: RTProcedureSRC.ProcInfo; 
    unit: RTProcedureSRC.UnitInfo;
    ignore: ADDRESS;
  BEGIN
    IF (start < stop) THEN
      WHILE (fp < stop) AND (n < NUMBER (x)) DO
        WITH p = x[n] DO
          p.loc := fp;
          p.pc  := fp^;
          RTProcedureSRC.FromPC (p.pc, p.proc, ignore, proc,
                                 unit, unit, p.file, p.name);
          IF (p.proc # NIL) AND (p.pc - p.proc < Max_proc) THEN INC (n) END;
        END;
        INC (fp, RTMachine.PointerAlignment);
      END;
    ELSE
      WHILE (fp > stop) AND (n < NUMBER (x)) DO
        WITH p = x[n] DO
          p.loc := fp;
          p.pc := fp^;
          RTProcedureSRC.FromPC (p.pc, p.proc, ignore, proc, unit,
                                 unit, p.file, p.name);
          IF (p.proc # NIL) AND (p.pc - p.proc < Max_proc) THEN INC (n) END;
        END;
        DEC (fp, RTMachine.PointerAlignment);
      END;
    END;
    RETURN n;
  END FindPCs;
     
(*--------------------------------------------------------- low-level I/O ---*)

PROCEDURE PathTail (a: ADDRESS): ADDRESS =
  VAR p0 : UNTRACED REF CHAR := a;  p := p0;
  BEGIN
    IF (p0 = NIL) THEN RETURN NIL END;
    WHILE (p^ # '\000') DO
      IF (p^ = '/') THEN p0 := p + ADRSIZE (p^); END;
      INC (p, ADRSIZE (p^));
    END;
    RETURN p0;
  END PathTail;


PROCEDURE PadText (p: RTIO.SimplePutter; t: TEXT;  width := 0) =
  VAR len := Text.Length (t);
  BEGIN
    p.putText (t);
    WHILE (len < width) DO
      p.putChar (' ');
      INC (len);
    END;
  END PadText;

(****************************** reference stats ******************************)


CONST
  MaintainTypecode = DoTracing AND FALSE;
  MaintainAddr = DoTracing AND FALSE;
  MaintainPC = DoTracing AND FALSE;
  MaintainConnectivity = DoTracing AND TRUE;


TYPE
  MapperType = RTTypeMap.Visitor OBJECT
    old, new: ADDRESS;
  OVERRIDES
    apply := Apply;
  END;

VAR
  TCCounts: TCDataUtbl.Default;     (* map of typecode info *)
  AddrMap: AddrDataUtbl.Default;    (* map of addr info     *)
  PCMap: PCDataUtbl.Default;        (* map of pc info       *)
  GlobalMap: AddrMapUtbl.Default;   (* map of connectivity  *)

  SaveDeallocation, SaveClock: Word.T := 0;

  DeallocationTotal : Word.T := 0;

  CollectionCount: Word.T := 0;

  Mapper: MapperType;
  MapperMask: RTTypeMap.Mask;

  MapLock: MUTEX := NIL;

PROCEDURE Init () =
  BEGIN
    TCCounts := NEW (TCDataUtbl.Default).init (10000);
    AddrMap := NEW (AddrDataUtbl.Default).init (100000);
    PCMap := NEW (PCDataUtbl.Default).init (100000);
    GlobalMap := NEW (AddrMapUtbl.Default).init (10000);

    (* necessary to avoid invoking ref counting/tracing *)
    (* FIXME!!! but AssignKnown happens anyway!!!! *)
    LOOPHOLE (ADR(MapLock), UNTRACED REF ADDRESS)^ :=
        LOOPHOLE (NEW (MUTEX), ADDRESS);

    Mapper := NEW (MapperType);
    MapperMask := RTTypeMap.Mask {RTTypeMap.Kind.Ref};
    
  END Init;

PROCEDURE Ptrs (addr, old, new: ADDRESS) =
  VAR
    localMap: Map.T;
    tmp, tmp2: BOOLEAN;
  BEGIN
    IF MaintainConnectivity THEN
      IF old # NIL THEN
        LOCK MapLock DO
          tmp := GlobalMap.get (old, localMap);
        END;
        IF tmp THEN
          LOCK MapLock DO
            tmp2 := Map.Remove (localMap, addr);
          END;
          IF NOT tmp2 THEN
            IF RTHeapRep.MinAddress () <= addr AND
               addr <= RTHeapRep.MaxAddress () THEN
              RTIO.PutText ("Remove could not find a heap addr for old=");
              RTIO.PutAddr (old);
              RTIO.PutText (", new=");
              RTIO.PutAddr (new);
              RTIO.PutText (", addr=");
              RTIO.PutAddr (addr);
              RTIO.PutText ("\n");
              RTOS.Crash ();
            ELSE
              RTIO.PutText ("Remove could not find a global addr for old=");
              RTIO.PutAddr (old);
              RTIO.PutText (", new=");
              RTIO.PutAddr (new);
              RTIO.PutText (", addr=");
              RTIO.PutAddr (addr);
              RTIO.PutText ("\n");
              RTOS.Crash ();
            END;
          END;
        ELSE
          RTIO.PutText ("RTHeapStats.Ptrs called with addr=");
          RTIO.PutAddr (addr);
          RTIO.PutText (", old=");
          RTIO.PutAddr (old);
          RTIO.PutText (", new= ");
          RTIO.PutAddr (new);
          RTIO.PutText (", addr not found in old's points-to set\n");
          RTOS.Crash ();
        END;
      END;

      IF new # NIL THEN
        LOCK MapLock DO
          tmp := GlobalMap.get (new, localMap);
          IF tmp THEN
            Map.Add (localMap, addr);
          ELSE
            localMap := NEW (Map.T);
            Map.Add (localMap, addr);
            tmp := GlobalMap.put (new, localMap);
            <* ASSERT tmp = FALSE *>
          END;
        END;
      END;
    END;
  END Ptrs;

(* typecode-based procedures *)

PROCEDURE CountTC (addr: ADDRESS; tc: RT0.Typecode) =
  VAR
    value: HeapData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainTypecode THEN
      tmp1 := TCCounts.get (tc, value);
      IF ThreadF.IsOnCurrentStack (addr) THEN
        INC (value.stack);
      ELSIF RTHeapRep.MinAddress () <= addr AND
        addr <= RTHeapRep.MaxAddress () THEN
        INC (value.heap);
      ELSE
        INC (value.global);
      END;
      
      tmp2 := TCCounts.put (tc, value);
      <* ASSERT tmp1 = tmp2 *>
    END;
  END CountTC;

PROCEDURE AllocateTC (tc: RT0.Typecode; size: Word.T) =
  VAR
    value: HeapData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainTypecode THEN
      tmp1 := TCCounts.get (tc, value);
      INC (value.alloc);
      INC (value.alloc_size, size);
      tmp2 := TCCounts.put (tc, value);
      <* ASSERT tmp1 = tmp2 *>
    END;
  END AllocateTC;

PROCEDURE DeallocateTC (tc: RT0.Typecode; size: Word.T) =
  VAR
    value: HeapData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainTypecode THEN
      tmp1 := TCCounts.get (tc, value);
      INC (value.dealloc);
      INC (value.dealloc_size, size);
      tmp2 := TCCounts.put (tc, value);
      <* ASSERT tmp1 = tmp2 *>
    END;
  END DeallocateTC;

PROCEDURE ResetTC () =
  VAR
    iter: TCDataUtbl.Iterator;
    key: INTEGER;
    value: HeapData.T;
    nulled: HeapData.T;
    tmp: BOOLEAN;
  BEGIN
    IF MaintainTypecode THEN
      iter := TCCounts.iterate ();
      
      WHILE iter.next (key, value) DO
        tmp := TCCounts.put (key, nulled);
        <* ASSERT tmp *>
      END;
      
      DISPOSE (iter);
    END;
  END ResetTC;

(* print typecode-based information *)
PROCEDURE PrintTC (p: RTIO.SimplePutter; number: CARDINAL) =
  VAR
    size := TCCounts.size ();
    tmptbl := NEW (UNTRACED REF ARRAY OF IntAddr.T, size);
    iter: TCDataUtbl.Iterator;
    tc: INTEGER;
    value: HeapData.T;
    name: TEXT;
    cnt: Word.T;
  BEGIN
    IF NOT MaintainTypecode THEN
      p.putText ("Typecode info not maintained by kernel\n");
      RETURN;
    END;

    iter := TCCounts.iterate ();

    cnt := 0;
    WHILE iter.next (tc, value) DO
      IF RTType.IsValid(tc) AND (0 < tc) AND (tc < RT0u.nTypes) THEN
        tmptbl[cnt].primary := value.alloc_size;
        tmptbl[cnt].addr := LOOPHOLE (tc, ADDRESS);
        INC (cnt);
      ELSE
        p.putText ("iterator in RTHeapStats.PrintTC sees bad typecode:");
        p.putInt (tc);
        p.putText ("\n");
      END;
    END;
    <* ASSERT cnt = size *>

    IntAddrArraySort.Sort (tmptbl^);

    p.putText ("\nTypecode assignment counts\n");
    p.putText ("Name: heap stack global\n");
    FOR i := size-1 TO size-number BY -1 DO
      tc := LOOPHOLE (tmptbl[i].addr, Word.T);
      EVAL TCCounts.get (tc, value);

      name := RTTypeSRC.TypecodeName(tc);
      p.putText (name & ":");
      p.putWord (value.heap, 10);
      p.putText (" ");
      p.putWord (value.stack, 10);
      p.putText (" ");
      p.putWord (value.global, 10);
      p.putText ("\n");
      
      p.putWord (value.alloc, 10);
      p.putText (" have been allocated\n");
      p.putWord (value.dealloc, 10);
      p.putText (" have been deallocated\n");
      
      p.putWord (value.alloc_size, 10);
      p.putText (" bytes have been allocated\n");
      p.putWord (value.dealloc_size, 10);
      p.putText (" bytes have been deallocated\n");
    END;
    p.putText ("\nDone with typecode assignment counts\n");

    DISPOSE (tmptbl);
    DISPOSE (iter);
  END PrintTC;


(* tracing statistics *)

PROCEDURE PrintTraceInfo (p: RTIO.SimplePutter) =
  VAR
    HeapAvg :=
        RTMachineCollectorExtern.HeapTotal DIV RTMachineCollectorExtern.HeapCount;
    HeapVar := (RTMachineCollectorExtern.HeapTotal2 -
                ((RTMachineCollectorExtern.HeapTotal *
                  RTMachineCollectorExtern.HeapTotal) DIV
                 RTMachineCollectorExtern.HeapCount)) DIV
                (RTMachineCollectorExtern.HeapCount - 1);
    GlobalAvg :=
        RTMachineCollectorExtern.GlobalTotal DIV RTMachineCollectorExtern.GlobalCount;
    GlobalVar := (RTMachineCollectorExtern.GlobalTotal2 -
                ((RTMachineCollectorExtern.GlobalTotal *
                  RTMachineCollectorExtern.GlobalTotal) DIV
                 RTMachineCollectorExtern.GlobalCount)) DIV
                (RTMachineCollectorExtern.GlobalCount - 1);
    LocalAvg :=
        RTMachineCollectorExtern.LocalTotal DIV RTMachineCollectorExtern.LocalCount;
    LocalVar := (RTMachineCollectorExtern.LocalTotal2 -
                ((RTMachineCollectorExtern.LocalTotal *
                  RTMachineCollectorExtern.LocalTotal) DIV
                 RTMachineCollectorExtern.LocalCount)) DIV
                (RTMachineCollectorExtern.LocalCount - 1);
  BEGIN
    p.putText ("\nTotal heap writes (runtime-determined): ");
    p.putWord (RTMachineCollectorExtern.HeapCount);
    p.putText (" (");
    p.putWord (RTMachineCollectorExtern.PossibleHeap);
    p.putText (")");
    p.putText ("\nAverage bytes allocated between heap REF writes: ");
    p.putWord (HeapAvg);
    p.putText ("\nStd in bytes allocated between heap REF writes: ");
    p.putWord (Sqrt (HeapVar));
    p.putText ("\nVar in bytes allocated between heap REF writes: ");
    p.putWord (HeapVar);
    p.putText ("\nTotal local writes (runtime-determined): ");
    p.putWord (RTMachineCollectorExtern.LocalCount);
    p.putText (" (");
    p.putWord (RTMachineCollectorExtern.PossibleLocal);
    p.putText (")");
    p.putText ("\nAverage bytes allocated between local REF writes: ");
    p.putWord (LocalAvg);
    p.putText ("\nStd in bytes allocated between local REF writes: ");
    p.putWord (Sqrt (LocalVar));
    p.putText ("\nVar in bytes allocated between local REF writes: ");
    p.putWord (LocalVar);
    p.putText ("\nTotal global writes (runtime-determined): ");
    p.putWord (RTMachineCollectorExtern.GlobalCount);
    p.putText (" (");
    p.putWord (RTMachineCollectorExtern.PossibleGlobal);
    p.putText (")");
    p.putText ("\nAverage bytes allocated between global REF writes: ");
    p.putWord (GlobalAvg);
    p.putText ("\nStd in bytes allocated between global REF writes: ");
    p.putWord (Sqrt (GlobalVar));
    p.putText ("\nVar in bytes allocated between global REF writes: ");
    p.putWord (GlobalVar);
    p.putText ("\nGlobal/heap writes with NIL old value: ");
    p.putWord (RTMachineCollectorExtern.NilOld);
    p.putText ("\nGlobal/heap writes with NIL new value: ");
    p.putWord (RTMachineCollectorExtern.NilNew);
    p.putText ("\nGlobal/heap writes with old value = new value: ");
    p.putWord (RTMachineCollectorExtern.OldEqualNew);
    p.putText ("\n");
  END PrintTraceInfo;


(* addr-based statistics *)

PROCEDURE AllocateAddr (addr: ADDRESS; tc: RT0.Typecode; size: CARDINAL) =
  VAR
    value: AddrData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainAddr THEN
      tmp1 := AddrMap.get (addr, value);
      IF tmp1 THEN
        RTIO.PutText ("RTHeapStats.AllocateAddr called on an already allocated address:");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      END;
      value.tc := tc;
      tmp2 := AddrMap.put (addr, value);
      <* ASSERT tmp1 = tmp2 *>
    END;
    AllocateTC (tc, size);
  END AllocateAddr;

PROCEDURE DeallocateAddr (addr: ADDRESS; tc: RT0.Typecode; size: CARDINAL) =
  VAR
    value: AddrData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainAddr THEN
      tmp1 := AddrMap.get (addr, value);
      IF NOT tmp1 THEN
        RTIO.PutText ("RTHeapStats.DeallocateAddr called on an unallocated address:");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      ELSE
        <* ASSERT tc = value.tc *>
      END;
      
      tmp2 := AddrMap.delete (addr, value);
      <* ASSERT tmp1 = tmp2 *>
    END;

    DeallocateTC (tc, size);

    (* deallocation stats *)
    INC (DeallocationTotal, size);
  END DeallocateAddr;


PROCEDURE MoveAddr (old, new: ADDRESS; tc: RT0.Typecode) =
  VAR
    value: AddrData.T;
    tmp1, tmp2, tmp3, tmp4: BOOLEAN;
    localMap: Map.T;
  BEGIN
    IF MaintainAddr THEN
      tmp1 := AddrMap.get (old, value);
      IF NOT tmp1 THEN
        RTIO.PutText ("RTHeapStats.MoveAddr called on an unallocated address:");
        RTIO.PutAddr (old);
        RTIO.PutText ("\n");
      ELSE
        <* ASSERT tc = value.tc *>
      END;
      INC (value.move);
      tmp2 := AddrMap.put (new, value);
      IF tmp2 THEN
        RTIO.PutText ("RTHeapStats.MoveAddr called on an already allocated address:");
        RTIO.PutAddr (new);
        RTIO.PutText ("\n");
      END;
      tmp3 := AddrMap.delete (old, value);
      <* ASSERT tmp3 = tmp1 *>
    END;      
    
    IF MaintainConnectivity THEN
      IF LOOPHOLE (new-ADRSIZE(RT0.RefHeader), UNTRACED REF RT0.RefHeader).typecode = 56 THEN
        RTIO.PutText ("mutex ");
        RTIO.PutAddr (old);
        RTIO.PutText (" moved to ");
        RTIO.PutAddr (new);
        RTIO.PutText ("\n");
      END;
      (* update moved object *)
      LOCK MapLock DO
        tmp4 := GlobalMap.delete (old, localMap);
        IF NOT tmp4 THEN
          RTIO.PutText ("old value not found in MoveAddr: ");
          RTIO.PutAddr (old);
          RTIO.PutText ("\n");
          RTOS.Crash ();
        END;
        tmp4 := GlobalMap.put (new, localMap);
        IF tmp4 THEN
          RTIO.PutText ("new value duplicated in MoveAddr: ");
          RTIO.PutAddr (new);
          RTIO.PutText ("\n");
          RTOS.Crash ();
        END;
      END;

      (* walk the moved object, updating any of its fields *)
      LOCK MapLock DO
        Mapper.old := old;
        Mapper.new := new;
        RTTypeMap.WalkRef (LOOPHOLE (new, REFANY), MapperMask, Mapper);
      END;
    END;
  END MoveAddr;

(* apply method for MapperType *)
PROCEDURE Apply (mt: MapperType; field: ADDRESS; k: RTTypeMap.Kind) =
  VAR
    oldfield: ADDRESS := mt.old + (field-mt.new);
    addr: ADDRESS;
    localMap: Map.T := NIL;
    tmp, tmp2: BOOLEAN;
    p: RTHeapRep.Page;
   (* tc1, tc2: RT0.Typecode; *)
  BEGIN
    <* ASSERT k = RTTypeMap.Kind.Ref *>

(*  testing code
    p := RTHeapRep.AddressToPage (field);
    IF p = RTHeapRep.Nil THEN
      RTIO.PutText ("field on bad page: ");
      RTIO.PutAddr (field);
      RTIO.PutText ("\n");
      RTOS.Crash ();
    ELSIF RTHeapRep.desc[p - RTHeapRep.p0].protected THEN
      RTIO.PutText ("crap\n");
      RTOS.Crash ();
    END;
*)
    addr := LOOPHOLE (field, UNTRACED REF ADDRESS)^;
    IF addr = NIL THEN RETURN; END;

    p := RTHeapRep.AddressToPage (addr);
    IF p = RTHeapRep.Nil THEN
      (* thing had better be a text *)
      (*
        IF LOOPHOLE (addr-BYTESIZE(RT0.RefHeader),
        UNTRACED REF RT0.RefHeader).typecode # 1 THEN
        RTIO.PutText ("bad pointer in RTHeapStats.Apply\n");
        RTOS.Crash ();
        END;
      *)        
      tmp := GlobalMap.get (addr, localMap);
      IF NOT tmp THEN
        localMap := NEW (Map.T);
        Map.Add (localMap, addr);
        tmp := GlobalMap.put (addr, localMap);
        <* ASSERT NOT tmp *>
        RETURN;
      END;
      
    ELSIF NOT RTHeapRep.desc[p - RTHeapRep.p0].protected THEN
      (*
        RTIO.PutText ("calling GetRealPointer on ");
        RTIO.PutAddr (addr);
      *)
      addr := RTHeapRep.GetRealPointer (addr);
      (*
        RTIO.PutText (", new addr is ");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      *)
      tmp := GlobalMap.get (addr, localMap);
    ELSE
      tmp := GlobalMap.get (addr, localMap);
    END;
    
    IF tmp THEN
      tmp2 := Map.Move (localMap, oldfield, field);
      IF NOT tmp2 THEN
(*
        tc1 := LOOPHOLE (mt.new-BYTESIZE (RT0.RefHeader),
                         UNTRACED REF RT0.RefHeader).typecode;
        tc2 := LOOPHOLE (addr-BYTESIZE (RT0.RefHeader),
                         UNTRACED REF RT0.RefHeader).typecode;
*)
        IF RTHeapRep.MinAddress () <= oldfield AND
          oldfield <= RTHeapRep.MaxAddress () THEN
          (*
            RTIO.PutText ("Move could not find a heap addr, addr=");
            RTIO.PutAddr (addr);
            RTIO.PutText (", oldfield=");
            RTIO.PutAddr (oldfield);
            RTIO.PutText (", field=");
            RTIO.PutAddr (field);
            RTIO.PutText (", mt.old=");
            RTIO.PutAddr (mt.old);
            RTIO.PutText (", mt.new=");
            RTIO.PutAddr (mt.new);
            RTIO.PutText (", typecode of main obj:");
            RTIO.PutHex (tc1);
            RTIO.PutText (", typecode of addr:");
            RTIO.PutHex (tc2);
            RTIO.PutText ("\n");
            RTOS.Crash ();
          *)
        ELSE
          (*
            RTIO.PutText ("Move could not find a global addr, addr=");
            RTIO.PutAddr (addr);
            RTIO.PutText (", oldfield=");
            RTIO.PutAddr (oldfield);
            RTIO.PutText (", field=");
            RTIO.PutAddr (field);
            RTIO.PutText (", mt.old=");
            RTIO.PutAddr (mt.old);
            RTIO.PutText (", mt.new=");
            RTIO.PutAddr (mt.new);
            RTIO.PutText (", typecode of main obj:");
            RTIO.PutHex (tc1);
            RTIO.PutText (", typecode of addr:");
            RTIO.PutHex (tc2);
            RTIO.PutText ("\n");
            RTOS.Crash ();
          *)
        END;
      END;
    ELSE
      (*
        RTIO.PutText ("Bad addr in RTHeapStats.Apply: ");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      *)
    END;
  END Apply;


PROCEDURE ImplicitMoveAddr (addr: ADDRESS; tc: RT0.Typecode) =
  VAR
    value: AddrData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainAddr THEN
      tmp1 := AddrMap.get (addr, value);
      IF NOT tmp1 THEN
        RTIO.PutText ("RTHeapStats.ImplicitMoveAddr called on an unallocated address:");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      ELSE
        <* ASSERT tc = value.tc *>
      END;
      INC (value.implicitMove);
      tmp2 := AddrMap.put (addr, value);
      <* ASSERT tmp2 = tmp1 *>
    END;
  END ImplicitMoveAddr;

PROCEDURE TenureAddr (addr: ADDRESS; tc: RT0.Typecode) =
  VAR
    value: AddrData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainAddr THEN
      tmp1 := AddrMap.get (addr, value);
      IF NOT tmp1 THEN
        RTIO.PutText ("RTHeapStats.PromoteAddr called on an unallocated address:");
        RTIO.PutAddr (addr);
        RTIO.PutText ("\n");
      ELSE
        <* ASSERT tc = value.tc *>
      END;
      INC (value.tenure);
      tmp2 := AddrMap.put (addr, value);
      <* ASSERT tmp2 = tmp1 *>
    END;
  END TenureAddr;

PROCEDURE ResetAddr () =
  VAR
    iter: AddrDataUtbl.Iterator;
    addr: ADDRESS;
    value, nulled: AddrData.T;
    tmp: BOOLEAN;
  BEGIN
    IF MaintainAddr THEN
      iter := AddrMap.iterate ();
      
      WHILE iter.next (addr, value) DO
        tmp := AddrMap.put (addr, nulled);
        <* ASSERT tmp *>
      END;
      
      DISPOSE (iter);
    END;
  END ResetAddr;


(* upon collection, update certain values *)
PROCEDURE Collection () =
  VAR
    iter: AddrDataUtbl.Iterator;
    addr: ADDRESS;
    value: AddrData.T;
    tmp: BOOLEAN;
  BEGIN
    INC (CollectionCount);

    RTIO.PutText ("\nCOLLECTION #");
    RTIO.PutWord (CollectionCount);
    RTIO.PutText ("\n");
    RTIO.PutWord (DeallocatedBytes ());
    RTIO.PutText (" bytes collected\n");
    RTIO.PutWord (AllocatedBytes ());
    RTIO.PutText (" bytes allocated\n");

    IF MaintainAddr THEN
      (* update collection information *)
      iter := AddrMap.iterate ();
      WHILE iter.next (addr, value) DO
        INC (value.collection);
        tmp := AddrMap.put (addr, value);
        <* ASSERT tmp *>
      END;
      DISPOSE (iter);
    END;

    RTIO.PutWord (DeallocationTotal DIV CollectionCount);
    RTIO.PutText (" avg bytes collected\n");
    RTIO.PutWord (RTMachineCollectorExtern.AllocationClock DIV CollectionCount);
    RTIO.PutText (" avg bytes allocated\n");

    SaveClock := RTMachineCollectorExtern.AllocationClock;
  END Collection;
 

PROCEDURE PrintAddr (p: RTIO.SimplePutter; number: CARDINAL) =
  VAR
    size := AddrMap.size ();
    tmptbl := NEW (UNTRACED REF ARRAY OF Int2Addr.T, size);
    iter: AddrDataUtbl.Iterator;
    addr: ADDRESS;
    value: AddrData.T;
    cnt: Word.T;
    tc: RT0.Typecode;
    name: TEXT;
    min := RTHeapRep.MinAddress ();
    max := RTHeapRep.MaxAddress ();
  BEGIN
    IF NOT MaintainAddr THEN
      p.putText ("Addr info not maintained by kernel\n");
      RETURN;
    END;
    iter := AddrMap.iterate ();

    cnt := 0;
    WHILE iter.next (addr, value) DO
      IF min <= addr AND addr <= max THEN
        tmptbl[cnt].primary := value.collection;
        tmptbl[cnt].secondary :=
            RTHeapRep.ReferentSize (LOOPHOLE (addr-BYTESIZE (RT0.RefHeader),
                                              UNTRACED REF RT0.RefHeader));
        tmptbl[cnt].addr := addr;
      ELSE
        tmptbl[cnt].primary := 0;
      END;
      INC (cnt);
    END;
    <* ASSERT cnt = size *>

    Int2AddrArraySort.Sort (tmptbl^);

    p.putText ("\nAddr summary\n");
    p.putText ("Addr: size tenure moves (implicit moves) collections\n");
    FOR i := size-1 TO size-number BY -1 DO
      addr := tmptbl[i].addr;
      tc := LOOPHOLE (addr-BYTESIZE (RT0.RefHeader),
                      UNTRACED REF RT0.RefHeader).typecode;
      name := RTTypeSRC.TypecodeName(tc);
      EVAL AddrMap.get (addr, value);

      p.putAddr (addr);
      p.putText (" (" & name & "): ");

      p.putWord (RTHeapRep.ReferentSize (LOOPHOLE (addr-BYTESIZE (RT0.RefHeader),
                                                  UNTRACED REF RT0.RefHeader)),
                6);
      p.putText (" ");

      p.putWord (value.tenure, 4);
      p.putText (" ");
      p.putWord (value.move, 4);
      p.putText ("(");
      p.putWord (value.implicitMove, 4);
      p.putText (") ");
      p.putWord (value.collection, 4);
      p.putText ("\n");
    END;
    p.putText ("\nDone with addr summary\n");

    DISPOSE (iter);
    DISPOSE (tmptbl);
  END PrintAddr;


(* pc-based statistics *)

PROCEDURE AllocatePC (pc: ADDRESS; size: Word.T) =
  VAR
    value: PCData.T;
    tmp1, tmp2: BOOLEAN;
  BEGIN
    IF MaintainPC THEN
      IF MaintainPC THEN
        tmp1 := PCMap.get (pc, value);
        INC (value.count);
        INC (value.size, size);
        tmp2 := PCMap.put (pc, value);
        <* ASSERT tmp1 = tmp2 *>
      END;
    END;
  END AllocatePC;

PROCEDURE PrintPC (p: RTIO.SimplePutter; number: CARDINAL) =
  VAR
    size := PCMap.size ();
    tmptbl := NEW (UNTRACED REF ARRAY OF IntAddr.T, size);
    iter: PCDataUtbl.Iterator;
    key: ADDRESS; value: PCData.T;
    cnt : Word.T;
    file, name: ADDRESS;
    ignore1: PROCANY;
    ignore2: RTProcedureSRC.ProcInfo; 
    ignore3: RTProcedureSRC.UnitInfo; 
    ignore4: ADDRESS;
  BEGIN
    IF NOT MaintainPC THEN
      p.putText ("pc info not maintained by kernel\n");
      RETURN;
    END;
    iter := PCMap.iterate ();

    (* info in terms of byte size *)

    cnt := 0;
    WHILE iter.next (key, value) DO
      tmptbl[cnt].primary := value.size;
      tmptbl[cnt].addr := key;
      INC (cnt);
    END;
    <* ASSERT cnt = size *>

    IntAddrArraySort.Sort (tmptbl^);

    p.putText ("\nAllocation information by byte volume\n");
    FOR cnt := size-1 TO size-number BY -1 DO
      WITH r = tmptbl[cnt] DO
        RTProcedureSRC.FromPC (r.addr, ignore1, ignore4, ignore2, ignore3,
                               ignore3, file, name);
        p.putString (file);
        p.putText (", ");
        p.putString (name);
        p.putText (" (");
        p.putAddr (r.addr);
        p.putText ("): ");
        p.putWord (r.primary);
        p.putText (" bytes\n");
      END;
    END;

    (* info in terms of allocation count *)

    cnt := 0;
    iter.reset ();
    WHILE iter.next (key, value) DO
      tmptbl[cnt].primary := value.count;
      tmptbl[cnt].addr := key;
      INC (cnt);
    END;
    <* ASSERT cnt = size *>

    IntAddrArraySort.Sort (tmptbl^);

    p.putText ("\nAllocation information by # allocations\n");
    FOR cnt := size-1 TO size-number BY -1 DO
      WITH r = tmptbl[cnt] DO
        RTProcedureSRC.FromPC (r.addr, ignore1, ignore4, ignore2,
                               ignore3, ignore3, file, name);
        p.putString (file);
        p.putText (",");
        p.putString (name);
        p.putText (" (");
        p.putAddr (r.addr);
        p.putText ("): ");
        p.putWord (r.primary);
        p.putText (" allocations\n");
      END;
    END;

    DISPOSE (tmptbl);
    DISPOSE (iter);
  END PrintPC;


PROCEDURE ResetPC () =
  VAR
    iter: PCDataUtbl.Iterator;
    addr: ADDRESS;
    value, nulled: PCData.T;
    tmp: BOOLEAN;
  BEGIN
    IF MaintainPC THEN
      iter := PCMap.iterate ();

      WHILE iter.next (addr, value) DO
        tmp := PCMap.put (addr, nulled);
        <* ASSERT tmp *>
      END;

      DISPOSE (iter);
    END;
  END ResetPC;

(* other functions *)

PROCEDURE ResetDeallocation () =
  BEGIN
    SaveDeallocation := DeallocationTotal;
  END ResetDeallocation;

PROCEDURE UpdateClock (size: Word.T) =
  BEGIN
    INC (RTMachineCollectorExtern.AllocationClock, size);
  END UpdateClock;

(* data on bytes manipulated *)

PROCEDURE DeallocatedBytes () : Word.T =
  BEGIN
    RETURN DeallocationTotal - SaveDeallocation;
  END DeallocatedBytes;

PROCEDURE AllocatedBytes () : Word.T =
  BEGIN
    RETURN RTMachineCollectorExtern.AllocationClock - SaveClock;
  END AllocatedBytes;

(* misc *)

PROCEDURE Sqrt(x: Word.T): Word.T =
  (* adapted from Measure.m3 *)
  BEGIN
    IF x < 0 THEN
      RETURN 0;
    END;

    (* x is between 0 and 2^(BITSIZE(INTEGER)) - 1, i.e. 2^64 - 1 *)
    (* so its square root is between 0 and 2^(BITSIZE(INTEGER)/2) - 1, 
       i.e. 2^32 - 1 *)

    VAR
      root := 0;
    BEGIN
      FOR bit := 31 TO 0 BY -1 DO
        WITH larger = Word.Or(root, Word.Shift(1, bit)) DO
          WITH LargeSq = larger * larger DO
            IF LargeSq > 0 AND
               LargeSq < x THEN
              root := larger;
            END;
          END;
        END;
      END;
      RETURN root;
    END;
  END Sqrt;


(* ------------------------------------- Fragmentation reporting *)


CONST
  SampleAtIntervals = FALSE; (* use wall clock and/or allocation 
                                clock to sample at regular intervals  *)
  WallClock = TRUE;
  NumSamples = 2500;
  SampleFreqAlloc = 1800000; (* number of bytes to sample at  *)
  SampleFreqTime  = 1000000;  (* us to sample at *) 
  
  
TYPE Sample = RECORD
  timeClock    : INTEGER; (* cycle counter *)
  allocClock   : INTEGER; 
  activeMem    : INTEGER; (* active bytes *)
  allocMem     : INTEGER; (* allocated bytes, not all of which may be useful *)
  collectStart : BOOLEAN := FALSE;
  collectEnd   : BOOLEAN := FALSE;
END;

VAR
  memSamples : UNTRACED REF ARRAY OF Sample;
  SampleByteCount : INTEGER; (* count # of bytes to sample at *)
  SampleIndex : INTEGER;
  heapSize : INTEGER;
  startTime: INTEGER; (* when we last gc reset *)
  lastTime : INTEGER;
 
PROCEDURE InitSampleArray (heap : INTEGER)=
  VAR
  BEGIN
    IF memSamples = NIL THEN
      memSamples := NEW(UNTRACED REF ARRAY OF Sample, NumSamples);
    END;
    SampleIndex := 0;
    heapSize := heap;
    lastTime := RTOSMachine.TimeOfDay();
    startTime := lastTime;
  END InitSampleArray;

(* Increment allocation clock and indicate if a sample should be taken *)
(* TRUE if a sample should be taken.  The sample is taken by the collector,
   since we cannot measure it from here. *)
PROCEDURE SampleAlloc (allocSize : INTEGER) : BOOLEAN =
  VAR
    delta : INTEGER;
    currentTime : INTEGER;
  BEGIN
    IF NOT SampleAtIntervals THEN
      RETURN FALSE;
    END;
    IF NOT ReportFragmentation THEN 
      RETURN FALSE;
    END;
    INC(SampleByteCount, allocSize);

    IF WallClock THEN
      currentTime := RTOSMachine.TimeOfDay();
      delta := currentTime - lastTime;

      IF SampleByteCount > SampleFreqAlloc OR
       delta > SampleFreqTime THEN
        SampleByteCount := 0;
        lastTime := currentTime;
        RETURN TRUE;
      END;
      RETURN FALSE;
    END;

    (* ignores clock *)
    IF SampleByteCount > SampleFreqAlloc THEN
      SampleByteCount := 0;
      RETURN TRUE;
    END;
    RETURN FALSE;

  END SampleAlloc;

(* add a frag sample to the array *)
PROCEDURE SampleFrag(activeBytes : INTEGER;
                     allocBytes : INTEGER;
                     start : BOOLEAN := FALSE;
                     end   : BOOLEAN := FALSE) =
  VAR
    now : INTEGER;
  BEGIN
    IF NOT ReportFragmentation THEN 
      RETURN;
    END;
    memSamples[SampleIndex].allocClock := RT0u.total_traced_bytes;
    IF WallClock THEN
      (* compute elapsed time *)
      now := RTOSMachine.TimeOfDay();
      memSamples[SampleIndex].timeClock := now- startTime;
    END;
    memSamples[SampleIndex].activeMem := activeBytes;
    memSamples[SampleIndex].allocMem := allocBytes;
    memSamples[SampleIndex].collectStart := start;
    memSamples[SampleIndex].collectEnd := end;

    INC(SampleIndex);
    IF SampleIndex > LAST(memSamples^) THEN
      (* grow array *)
      (* Note : in order not to mess up timing measurement too much, really
         should use a linked list of arrays *)
      RTIO.PutText("SAMPLE ARRAY GROWING...\n");
      VAR
        newarray : UNTRACED REF ARRAY OF Sample;
      BEGIN
        newarray := NEW(UNTRACED REF ARRAY OF Sample, NUMBER(memSamples^)*2);
        FOR i := 0 TO LAST(memSamples^) DO
          newarray[i] := memSamples[i];
        END;
        DISPOSE(memSamples);
        memSamples := newarray;
      END;
    END;
  END SampleFrag;
 
(* Make sure the caller of this (Reset stats) forces an initial sample after
   calling this *)
PROCEDURE ResetSamples () = 
  BEGIN
    SampleIndex := 0;
    SampleByteCount := 0;
    lastTime:=RTOSMachine.TimeOfDay();
    startTime := lastTime;
  END ResetSamples;

(* make sure the caller of this (GetStatistics) gets one last sample
*)
PROCEDURE DumpSamples () =
  VAR
  BEGIN
    IF NOT ReportFragmentation THEN 
      RTIO.PutText("WARNING : Fragmentation reporting not enabled!\n");
      RETURN;
    END;
    (* Force a frag sample *)
    RTCollectorSRC.ForceFragSample();
    RTIO.PutText("\n-WallClock(us)-\t-bytes alloced-\t-useful mem-\t-allocated mem-\tSEX\n");
    RTIO.PutText("### BEGIN UTILIZATION\n");
    RTIO.PutText("HEAPSIZE ");
    RTIO.PutInt(heapSize);
    RTIO.PutText("\n");
    FOR i := 0 TO SampleIndex-1 DO
      RTIO.PutInt(memSamples[i].timeClock,12);
      RTIO.PutInt(memSamples[i].allocClock,12);
      RTIO.PutText("\t");
      RTIO.PutInt(memSamples[i].activeMem,9);
      RTIO.PutText("\t");
      RTIO.PutInt(memSamples[i].allocMem,9);
      RTIO.PutText("\t");
      IF memSamples[i].collectStart THEN
        RTIO.PutText("S");
      ELSIF memSamples[i].collectEnd THEN
        RTIO.PutText("E");
      ELSE
        RTIO.PutText("X");
      END;
      RTIO.PutText("\n");
    END;
    RTIO.PutText("### END UTILIZATION\n");
  END DumpSamples;


PROCEDURE DumpHisto() = 
  BEGIN
    RTHisto.DumpHistogram();
  END DumpHisto;

BEGIN
END RTHeapStats.
   
   
