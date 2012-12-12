(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Wed Dec 21 14:57:09 PST 1994 by kalsow     *)

(* The code below makes the following NASTY assumption:
      ThreadF.ProcessStacks calls its argument twice for
      each thread -- the first time for the stack, the
      second time for its registers. *)

UNSAFE MODULE RTHeapStats;

IMPORT RT0, RT0u, RTCollector, RTModule, RTIO, RTHeapMap, RTHeapRep, RTMisc;
IMPORT RTOS, RTType, RTTypeSRC, RTProcedure, RTProcedureSRC, RTMachine; 
IMPORT ThreadF, Word, Text;
FROM RTIO IMPORT PutInt, PutAddr, PutText;

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

TYPE
  InfoSet = RECORD
    count : INTEGER;
    info  : ARRAY [0..19] OF Info;
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
  is_registers: BOOLEAN;
  visit_stack : UNTRACED REF VisitStack;
  top_of_stack: INTEGER;
  n_overflows : INTEGER;
  last_alloc  : ADDRESS;
  outerVisitor: RTHeapMap.Visitor := NIL;
  innerVisitor: RTHeapMap.Visitor := NIL;
  rootVisitor : RTHeapMap.Visitor := NIL;

PROCEDURE ReportReachable () =
  CONST MByte = 1024 * 1024;
  BEGIN
    (* allocate space for the stats *)
    outerVisitor := NEW (RTHeapMap.Visitor, apply := Visit);
    innerVisitor := NEW (RTHeapMap.Visitor, apply := InnerVisit);
    rootVisitor  := NEW (RTHeapMap.Visitor, apply := VisitRoot);
    visit_stack  := NEW (UNTRACED REF VisitStack);
    map := NEW (UNTRACED REF ARRAY OF Word.T,
                 (RTHeapRep.p1 - RTHeapRep.p0) * MapWordsPerHeapPage);

    (* initialize the globals *)
    units.count       := 0;
    unit_roots.count  := 0;
    stacks.count      := 0;
    stack_roots.count := 0;
    stack_pages.count := 0;
    top_of_stack      := 0;
    n_overflows       := 0;

    (* freeze the world *)
    RTCollector.Disable ();
    RTOS.LockHeap (); (* freeze the heap *)

    (* capture the heap limits *)
    heap_min  := LOOPHOLE (RTHeapRep.p0 * RTHeapRep.BytesPerPage, ADDRESS);
    heap_max  := LOOPHOLE (RTHeapRep.p1 * RTHeapRep.BytesPerPage, ADDRESS);

    PutText ("\nHEAP: ");
    PutAddr (heap_min);
    PutText (" .. ");
    PutAddr (heap_max);
    PutText (" => ");
    PutInt ((heap_max - heap_min) DIV MByte);
    PutText (".");
    PutInt ((heap_max - heap_min) * 10 DIV MByte MOD 10);
    PutText (" Mbytes\n");

    (* find the edge of the new space *)
    last_alloc := LOOPHOLE (NEW (REF INTEGER), ADDRESS);

    FOR i := 0 TO RTModule.Count() - 1 DO GetUnitStats (i); END;
    FOR i := 0 TO RTModule.Count() - 1 DO GetUnitRootStats (i); END;
    is_registers := FALSE;
    ThreadF.ProcessStacks (GetThreadStats);
    is_registers := FALSE;
    ThreadF.ProcessStacks (GetThreadRootStats);
    is_registers := FALSE;
    ThreadF.ProcessStacks (GetThreadPageStats);

    IF (n_overflows > 0) THEN
      PutText ("  ** warning: ");
      PutInt (n_overflows);
      PutText (" paths, longer than ");
      PutInt (NUMBER (VisitStack));
      PutText (" REFs, were truncated.\n");
    END;

    ReportUnits ();
    ReportUnitRoots ();

    ReportStacks ();
    ReportStackRoots ();
    ReportStackPages ();

    ReportStackPCs ();
    ReportStackRootPCs ();
    ReportStackPagePCs ();
    RTIO.Flush ();

    (* thaw the world *)
    DISPOSE (visit_stack);
    DISPOSE (map);
    RTOS.UnlockHeap (); (* unfreeze the heap *)
    RTCollector.Enable ();
  END ReportReachable;

(*------------------------------------------------------------ REF visits ---*)

PROCEDURE ResetVisitCounts () =
  BEGIN
    visit.n_objects := 0;
    visit.n_bytes   := 0;
    top_of_stack    := 0;
    RTMisc.Zero (ADR (map[0]), BYTESIZE (map^));
  END ResetVisitCounts;

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
    IF (heap_min <= ref) AND (ref < heap_max)
      AND (Word.And (LOOPHOLE(ref, INTEGER), Mask) = 0) THEN
      typecode := header.typecode;
      IF (0 < typecode) AND (typecode < RT0u.nTypes) THEN
        cell := (ref - heap_min) DIV MapGrain;
        word := cell DIV BITSIZE (Word.T);
        bit  := cell - word * BITSIZE (Word.T);
        mask := Word.LeftShift (1, bit);
        IF (Word.And (mask, map[word]) = 0) THEN
          (* this is a new ref... *)
          map[word] := Word.Or (mask, map[word]);
          INC (visit.n_objects);
          INC (visit.n_bytes, DataSize (header) + BYTESIZE (RT0.RefHeader));
          IF (top_of_stack < NUMBER (VisitStack)) THEN
            visit_stack [top_of_stack] := header;
            INC (top_of_stack);
          ELSE
            INC (n_overflows);
          END;
        END;
      END;
    END;
  END InnerVisit;

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
  VAR typecode: INTEGER;
  BEGIN
    header := ref - ADRSIZE (RT0.RefHeader);
    IF (heap_min <= ref) AND (ref < heap_max) THEN
      typecode := header.typecode;
      IF (0 < typecode) AND (typecode < RT0u.nTypes) THEN
        RETURN RTTypeSRC.TypecodeName (typecode);
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
    ResetVisitCounts ();
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
    Visit (NIL, root);
    AddVisit (unit_roots);
  END VisitRoot;

(*--------------------------------------------------------------- threads ---*)

PROCEDURE GetThreadStats (start, stop: ADDRESS) =
  VAR fp := start;  p: ADDRESS;  page: INTEGER;
  BEGIN
    IF (NOT is_registers) THEN
      visit.module     := NIL;
      visit.stack_min  := start;
      visit.stack_max  := stop;
      visit.location   := NIL;
      visit.ref        := NIL;
      ResetVisitCounts ();
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

    IF (is_registers) THEN AddVisit (stacks); END;
    is_registers := NOT is_registers;
  END GetThreadStats;

PROCEDURE GetThreadRootStats (start, stop: ADDRESS) =
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
          Visit (NIL, fp);
          AddVisit (stack_roots);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;

    is_registers := NOT is_registers;
  END GetThreadRootStats;

PROCEDURE GetThreadPageStats (start, stop: ADDRESS) =
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

    is_registers := NOT is_registers;
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
    WHILE (h < stop) AND (h.typecode # 0) DO
      ref := h + ADRSIZE (RT0.RefHeader);
      Visit (NIL, ADR (ref));
      INC (h, DataSize (h) + ADRSIZE (RT0.RefHeader));
    END;
  END VisitPage;

(*--------------------------------------------------------------- reports ---*)

PROCEDURE ReportUnits () =
  BEGIN
    PutText ("\nModule globals:\n");
    PutText (" # objects   # bytes  unit\n");
    PutText (" ---------  --------  -----------------\n");
    FOR i := 0 TO units.count-1 DO
      WITH m = units.info[i] DO
        IF (m.n_bytes > 0) THEN
          PutInt (m.n_objects, 10);
          PutInt (m.n_bytes, 10);
          PutText ("  ");
          PutStr  (PathTail (m.module.file));
          PutText ("\n");
        END;
      END;
    END;
  END ReportUnits;

PROCEDURE ReportUnitRoots () =
  BEGIN
    PutText ("\nGlobal variable roots:\n");
    PutText (" # objects   # bytes         ref type                location\n");
    PutText (" ---------  --------  ---------- -----------------   ------------------------\n");
    FOR i := 0 TO unit_roots.count-1 DO
      WITH r = unit_roots.info[i] DO
        IF (r.n_bytes > 0) THEN
          PutInt  (r.n_objects, 10);
          PutInt  (r.n_bytes, 10);
          PutText ("  ");
          PutAddr (r.ref);
          PutText (" ");
          PadText (TypeName (r.ref), 18);
          PutText ("  ");
          PutStr  (PathTail (r.module.file));
          PutText (" + ");
          PutInt (r.location - r.module);
          PutText ("\n");
        END;
      END;
    END;
  END ReportUnitRoots;

PROCEDURE ReportStacks () =
  BEGIN
    PutText ("\nThread stacks (conservative page scan):\n");
    PutText (" # objects   # bytes  stack bounds\n");
    PutText (" ---------  --------  ------------------------\n");
    FOR i := 0 TO stacks.count-1 DO
      WITH t = stacks.info[i] DO
        IF (t.n_bytes > 0) THEN
          PutInt (t.n_objects, 10);
          PutInt (t.n_bytes, 10);
          PutText ("  [");
          PutAddr (t.stack_min);
          PutText ("..");
          PutAddr (t.stack_max);
          PutText ("]\n");
        END;
      END;
    END;
  END ReportStacks;

PROCEDURE ReportStackPCs () =
  VAR stack_mid: ADDRESS;
  BEGIN
    PutText ("\nThread stack PCs:\n");
    PutText ("     SP         PC     procedure\n");
    PutText ("----------  ---------  ------------------------\n");
    FOR i := 0 TO stacks.count-1 DO
      WITH t = stacks.info[i] DO
        IF (t.n_bytes > 0) THEN
          PutText ("                          [");
          PutAddr (t.stack_min);
          PutText ("..");
          PutAddr (t.stack_max);
          PutText ("]\n");
          stack_mid := t.stack_min + (t.stack_max - t.stack_min) DIV 2;
          ReportPCs (t.stack_min, stack_mid, 5, +1);
          PutText ("  ...\n");
          ReportPCs (t.stack_max, stack_mid, 5, -1);
          PutText ("\n");
        END;
      END;
    END;
  END ReportStackPCs;

PROCEDURE ReportStackRoots () =
  BEGIN
    PutText ("\nThread stack roots (optimistic):\n");
    ReportStackInfo (stack_roots);
  END ReportStackRoots;

PROCEDURE ReportStackPages () =
  BEGIN
    PutText ("\nThread stack roots (conservative page scan):\n");
    ReportStackInfo (stack_pages);
  END ReportStackPages;

PROCEDURE ReportStackInfo (READONLY s: InfoSet) =
  BEGIN
    PutText (" # objects   # bytes         ref type                location\n");
    PutText (" ---------  --------  ---------- -----------------   ------------------------\n");
    FOR i := 0 TO s.count-1 DO
      WITH r = s.info[i] DO
        IF (r.n_bytes > 0) THEN
          PutInt  (r.n_objects, 10);
          PutInt  (r.n_bytes, 10);
          PutText ("  ");
          PutAddr (r.ref);
          PutText (" ");
          PadText (TypeName (r.ref), 18);
          PutText ("  ");
          IF (r.location # NIL) THEN
            PutText ("sp+");
            PutInt  (r.location - r.stack_min);
          ELSE
            PutText ("register");
          END;
          PutText (" in [");
          PutAddr (r.stack_min);
          PutText ("..");
          PutAddr (r.stack_max);
          PutText ("]");
          PutText ("\n");
        END;
      END;
    END;
  END ReportStackInfo;

PROCEDURE ReportStackRootPCs () =
  BEGIN
    PutText ("\nThread stack root PCs (optimistic):\n");
    ReportStackInfoPCs (stack_roots);
  END ReportStackRootPCs;

PROCEDURE ReportStackPagePCs () =
  BEGIN
    PutText ("\nThread stack root PCs (conservative page scan):\n");
    ReportStackInfoPCs (stack_pages);
  END ReportStackPagePCs;

PROCEDURE ReportStackInfoPCs (READONLY s: InfoSet) =
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

    PutText ("     SP         PC     location\n");
    PutText ("----------  ---------  ------------------------\n");
    FOR i := 0 TO s.count-1 DO
      WITH r = s.info[m[i]] DO
        IF (r.n_bytes > 0) AND (r.location # NIL) THEN
          xy := r.location - RTMachine.PointerAlignment;
          IF (i = 0) OR (s.info[m[i-1]].location = NIL) OR
            (s.info[m[i-1]].stack_min # r.stack_min) THEN
            (* this is the first entry on this stack *)
            xx := xy + (r.stack_min - xy) DIV 2;
            PutAddr (r.stack_min, 10);
            PutText ("              [");
            PutAddr (r.stack_min);
            PutText ("..");
            PutAddr (r.stack_max);
            PutText ("]\n");
            ReportPCs (r.stack_min, xx, 3, +1);
          ELSE
            xx := xy + (s.info[m[i-1]].location - xy) DIV 2;
          END;
          PutText ("...\n");
          ReportPCs (xy, xx, 3, -1);
          PutAddr (r.location, 10);
          PutText ("                ");
          PutText ("sp+");
          PutInt (r.location - r.stack_min);
          PutText ("\n");
          xy := r.location + RTMachine.PointerAlignment;
          IF (i = s.count-1) OR (s.info[m[i+1]].location = NIL) OR
            (s.info[m[i+1]].stack_min # r.stack_min) THEN
            (* this is the last entry on this stack *)
            xx := xy + (r.stack_max - xy) DIV 2;
            ReportPCs (xy, xx, 3, +1);
            PutText ("...\n");
            ReportPCs (r.stack_max, xx, 3, -1);
            PutAddr (r.stack_max, 10);
            PutText ("\n\n");
          ELSE
            xx := xy + (s.info[m[i+1]].location - xy) DIV 2;
            ReportPCs (xy, xx, 3, +1);
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

PROCEDURE ReportPCs (start, stop: ADDRESS;  max, dir: INTEGER) =
  VAR
    x   : ARRAY [0..9] OF PCInfo;
    cnt := FindPCs (start, stop, SUBARRAY (x, 0, MIN (max, NUMBER(x))));
    a, b: INTEGER;
  BEGIN
    a := 0;
    b := cnt-1;
    IF (dir < 0) THEN a := b; b := 0; END;
    FOR i := a TO b BY dir DO
      WITH p = x[i] DO
        PutAddr (p.loc, 10);
        PutText (" ");
        PutAddr (p.pc, 10);
        PutText ("  ");
        PutStr  (p.name);
        IF (p.pc # p.proc) THEN
          PutText (" + ");
          PutInt (p.pc - p.proc);
        END;
        IF (p.file # NIL) THEN
          PutText (" in ");
          PutStr  (PathTail (p.file));
        END;
        PutText ("\n");
      END;
    END;
  END ReportPCs;

PROCEDURE FindPCs (start, stop: ADDRESS;  VAR x: ARRAY OF PCInfo): INTEGER =
  VAR n := 0;  fp: UNTRACED REF ADDRESS := start;
  BEGIN
    IF (start < stop) THEN
      WHILE (fp < stop) AND (n < NUMBER (x)) DO
        WITH p = x[n] DO
          p.loc := fp;
          p.pc  := fp^;
          RTProcedureSRC.FromPC (p.pc, p.proc, p.file, p.name);
          IF (p.proc # NIL) AND (p.pc - p.proc < Max_proc) THEN INC (n) END;
        END;
        INC (fp, RTMachine.PointerAlignment);
      END;
    ELSE
      WHILE (fp > stop) AND (n < NUMBER (x)) DO
        WITH p = x[n] DO
          p.loc := fp;
          p.pc := fp^;
          RTProcedureSRC.FromPC (p.pc, p.proc, p.file, p.name);
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

PROCEDURE PutStr (s: ADDRESS) =
  BEGIN
    IF (s = NIL) THEN RETURN END;
    RTIO.PutString (s);
  END PutStr;

PROCEDURE PadText (t: TEXT;  width := 0) =
  VAR len := Text.Length (t);
  BEGIN
    RTIO.PutText (t);
    WHILE (len < width) DO
      RTIO.PutChar (' ');
      INC (len);
    END;
  END PadText;

BEGIN
END RTHeapStats.
   
   
