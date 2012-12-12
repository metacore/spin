(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Use GetPC() instead of GetRef() for PC values.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned-up.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Split out machine-dependent portions of this file into 
 *      RTMachineHeapTrace.m3
 *
 *)

UNSAFE MODULE RTHeapTrace;
IMPORT Word, RTIO, RT0, RTType, RTHeapRep, RTOS;
IMPORT RTOSMachine, RTHeapStats, RTCollector, RTCollectorSRC; <* NOWARN *>
IMPORT RTMachineHeapTrace;

VAR
  Debug : BOOLEAN := FALSE;
  verbose: BOOLEAN := FALSE;

TYPE
  ObjDesc = RTMachineHeapTrace.ObjDesc;
  RefT = RTMachineHeapTrace.RefT;
  PCT = RTMachineHeapTrace.PCT;
  ObjDescT = RTMachineHeapTrace.ObjDescT;
  ObjState = RTMachineHeapTrace.ObjState;

CONST
  MaxObjDesc = RTMachineHeapTrace.MaxObjDesc;

VAR
  curr_tc    : INTEGER := -1;
  currActive : INTEGER := 0;

VAR
  allocCnt   : INTEGER := 0;
  deallocCnt : INTEGER := 0;
  promoteCnt : INTEGER := 0;
  moveCnt    : INTEGER := 0;

VAR
  AllObjDesc: ARRAY [0..MaxObjDesc] OF ObjDescT;
  AllObjCnt: ObjDesc := 1;
  max := 0;

(* 
 * finds either the latest object with reference ref
 * or first available empty slot for such object
 *)

PROCEDURE FindObjDesc(ref: RefT): ObjDesc =
  VAR 
    tc := RTMachineHeapTrace.Hash(ref);
    i  := 0;
  BEGIN
    WHILE AllObjDesc[tc].ref # 0 DO
      IF AllObjDesc[tc].ref = ref THEN
        IF i > max THEN max := i; END;
        RETURN tc;
      END;
      INC(tc); 
      INC(i);
      IF tc = MaxObjDesc THEN tc := 1; END;
    END;
    IF i > max THEN max := i; END;
    RETURN tc;
  END FindObjDesc;

(*
 * creates a new descriptor and puts it under tc if it is empty
 * or first available empty slot after tc
 *)

PROCEDURE Create(desc: ObjDesc; ref: RefT; pc: PCT) =
  VAR tc: RT0.Typecode := TYPECODE(RTMachineHeapTrace.GetRef(ref));
  BEGIN
    IF tc >= 16_3fff THEN
      RTIO.PutText("ERROR >> Heap Tracer:: type index too big\n");
      <* ASSERT FALSE *>
    END;

    IF AllObjDesc[desc].ref = 0 THEN
      IF AllObjCnt = MaxObjDesc THEN
        RTIO.PutText("ERROR >> Heap Tracer:: ran out of space\n");
        Dump();
        RTOS.Crash ();
      END;
      INC(AllObjCnt);

      IF verbose THEN
        IF AllObjCnt MOD 10000 = 0 AND max # 0THEN
          RTIO.PutText("("); RTIO.PutInt(AllObjCnt); RTIO.PutText(" ");
          RTIO.PutInt(max); RTIO.PutText(")");
        END;
      END;
    END;

    WHILE AllObjDesc[desc].ref # 0 AND AllObjDesc[desc].ref # ref DO
      INC(desc); 
      IF desc = MaxObjDesc THEN desc := 1; END;
    END;

    IF AllObjDesc[desc].ref = 0 THEN
      INC(AllObjCnt);
    END;
    AllObjDesc[desc].ref := ref;
    AllObjDesc[desc].pc := pc;
    AllObjDesc[desc].state := ObjState.Active;
    AllObjDesc[desc].tc := tc;
  END Create;

(*
 * handlers
 *)

PROCEDURE ObjectAllocated(r: REFANY; pcAddr: ADDRESS) =
  VAR ref  : RefT;
      pc   : PCT;
      desc : ObjDesc;
  BEGIN
    IF Debug THEN
      RTIO.PutText("ObjectAllocated : "); RTIO.PutRef(r); 
      RTIO.PutText(" @ "); RTIO.PutAddr(pcAddr); RTIO.PutText("\n");
    END;
    
    ref  := RTMachineHeapTrace.RefAnyToRefT(r);
    pc   := RTMachineHeapTrace.AddrToPCT(pcAddr);
    desc := FindObjDesc(ref);

    IF curr_tc = -1 OR TYPECODE(RTMachineHeapTrace.GetRef(ref)) = curr_tc 
     THEN
      INC(currActive);
      IF verbose THEN
        RTIO.PutText("<A "); RTIO.PutHex(ref); RTIO.PutText(" ");
        RTIO.PutInt(currActive); RTIO.PutText(">");
      END;
      IF currActive MOD 100 = 0 THEN
        IF verbose THEN
          RTIO.PutText("<A "); RTIO.PutInt(currActive); RTIO.PutText(">");
        END;
        (*
        IF currActive MOD 1000 = 0 THEN 
          RTIO.PutText("COLLECT\n");
          RTCollector.Collect();
          RTCollector.Collect();
          RTIO.PutText("DONE\n");
        END;
        *)
        (*
        IF currActive = 6000 THEN 
          RTIO.PutText("COLLECT 1\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 2\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 3\n");
          RTCollector.Collect();
          RTCollectorSRC.gcRatio := 20000;
          RTIO.PutText("COLLECT 4\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 5\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 6\n");
          RTCollector.Collect();
        END;
        *)
      END;
    END;
    IF AllObjDesc[desc].ref # 0 AND
      AllObjDesc[desc].state = ObjState.Active
     THEN
      RTIO.PutText("ERROR >> Heap Tracer:: an active object was reallocated ");
      PutDesc(desc); RTIO.PutText("\n");
    END;

    INC(allocCnt);
    Create(desc, ref, pc);
  END ObjectAllocated;

PROCEDURE ObjectDeallocated(r: Word.T) =
  VAR ref  : RefT;
      desc : ObjDesc;
  BEGIN
    IF Debug THEN
      RTIO.PutText("ObjectDeallocated : "); 
      RTIO.PutHex(ref); 
      RTIO.PutText("\n");
    END;

    ref := RTMachineHeapTrace.WordToRefT(r);
    desc := FindObjDesc(ref);

    IF curr_tc = -1 OR TYPECODE(RTMachineHeapTrace.GetRef(ref)) = curr_tc
     THEN
      DEC(currActive);
      INC(deallocated_cnt);
      IF verbose THEN
        RTIO.PutText("<D "); RTIO.PutHex(ref); RTIO.PutText(" ");
        RTIO.PutInt(currActive); RTIO.PutText(">");
      END;
      IF currActive MOD 100 = 0 THEN
        IF verbose THEN
          RTIO.PutText("<D "); RTIO.PutInt(currActive); RTIO.PutText(">");
        END;
        (*
        IF currActive MOD 1000 = 0 THEN 
          RTIO.PutText("COLLECT\n");
          RTCollector.Collect();
          RTCollector.Collect();
          RTIO.PutText("DONE\n");
        END;
        *)
        (*
        IF currActive = 6000 THEN 
          RTIO.PutText("COLLECT 1\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 2\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 3\n");
          RTCollector.Collect();
          RTCollectorSRC.gcRatio := 20000;
          RTIO.PutText("COLLECT 4\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 5\n");
          RTCollector.Collect();
          RTIO.PutText("COLLECT 6\n");
          RTCollector.Collect();
        END;
        *)
      END;
    END;

    IF AllObjDesc[desc].ref = 0 THEN
      RTIO.PutText("ERROR >> Heap Tracer:: deallocated unknown object ");
      PutDesc(desc); RTIO.PutText("\n");
      RETURN;
    END;

    IF AllObjDesc[desc].state # ObjState.Active THEN
      IF AllObjDesc[desc].state = ObjState.Deallocated THEN
        RTIO.PutText(
            "ERROR >> Heap Tracer:: deallocated a deallocated object ");
        PutDesc(desc); RTIO.PutText("\n");
      END;
    END;

    AllObjDesc[desc].state := ObjState.Deallocated;
    INC(deallocCnt);
  END ObjectDeallocated;

PROCEDURE ObjectPromoted(r: REFANY; stage: INTEGER; 
                         ptr: ADDRESS; loc: ADDRESS; src: REFANY) = 
  VAR ref  := RTMachineHeapTrace.RefAnyToRefT(r);
      desc : ObjDesc;
  BEGIN
    IF Debug THEN
      RTIO.PutText("ObjectPromoted : "); 
      RTIO.PutHex(ref); 
      RTIO.PutText("\n");
    END;

    desc := FindObjDesc(ref);

    IF TYPECODE(RTMachineHeapTrace.GetRef(ref)) = curr_tc THEN
      IF NOT FindObj(r) THEN
        RTIO.PutText("<?P "); RTIO.PutHex(ref); RTIO.PutText(" "); 
        RTIO.PutAddr(ptr); RTIO.PutText(" ");
        RTIO.PutAddr(loc); RTIO.PutText(" ");
        RTIO.PutRef(src); RTIO.PutText(" ");
        RTIO.PutInt(stage); RTIO.PutText(">");
      END;
    END;

    IF curr_tc = -1 OR TYPECODE(RTMachineHeapTrace.GetRef(ref)) = curr_tc THEN
      INC(promoted_cnt[stage]);
      IF verbose THEN
        RTIO.PutText("<P "); RTIO.PutHex(ref); RTIO.PutText(" "); 
        RTIO.PutAddr(ptr); RTIO.PutText(" ");
        RTIO.PutAddr(loc); RTIO.PutText(" ");
        RTIO.PutRef(src); RTIO.PutText(" ");
        RTIO.PutInt(stage); RTIO.PutText(">");
      END;

      IF verbose THEN RTIO.PutText("<");  END;
      IF r = src THEN
        IF verbose THEN RTIO.PutText("<SRC>");  END;
        INC(src_cnt);
      END;
      IF src # NIL THEN
        IF TYPECODE(r) = TYPECODE(src) THEN
          IF verbose THEN RTIO.PutText("<TH>");  END;
          INC(th_cnt);
        END;
      END;
      WITH rr = LOOPHOLE(r, ADDRESS) - 8 DO
        IF ptr >= rr AND 
          ptr < rr + 8 + RTHeapRep.ReferentSize(
                             LOOPHOLE(rr, UNTRACED REF RT0.RefHeader))
         THEN
          IF verbose THEN RTIO.PutText("<DIR>");  END;
          INC(dir_cnt);
        END;
      END;
      IF verbose THEN RTIO.PutText(">"); END;
    END;

    IF AllObjDesc[desc].ref = 0 THEN
      RTIO.PutText("ERROR >> Heap Tracer:: promoted unknown object ");
      PutDesc(desc); RTIO.PutText("\n");
      RETURN;
    END;

    IF AllObjDesc[desc].state # ObjState.Active THEN
      RTIO.PutText("ERROR >> Heap Tracer:: non-active object promoted ");
      PutDesc(desc); RTIO.PutText("\n");
    END;

    INC(promoteCnt);
  END ObjectPromoted;

PROCEDURE ObjectMoved(before: Word.T; after: REFANY; state: INTEGER) =
  VAR refBefore := RTMachineHeapTrace.WordToRefT(before);
      refAfter  := RTMachineHeapTrace.RefAnyToRefT(after);
      desc : ObjDesc;
  BEGIN
    IF Debug THEN
      RTIO.PutText("ObjectMoved : "); RTIO.PutHex(refBefore);
      RTIO.PutText(" "); RTIO.PutHex(refAfter); RTIO.PutText("\n");
    END;

    desc := FindObjDesc(refBefore);

    IF TYPECODE(RTMachineHeapTrace.GetRef(refBefore)) = curr_tc THEN
      IF NOT FindObj(LOOPHOLE(before, REFANY)) THEN
        RTIO.PutText("<?M "); RTIO.PutHex(refBefore); RTIO.PutText(" ");
        RTIO.PutHex(refAfter); RTIO.PutText(" "); RTIO.PutInt(state);
        RTIO.PutText(">");
      END;
    END;

    IF curr_tc = -1 OR TYPECODE(RTMachineHeapTrace.GetRef(refBefore)) = curr_tc
     THEN
      INC(moved_cnt[state]);
      IF verbose THEN
        RTIO.PutText("<M "); RTIO.PutHex(refBefore); RTIO.PutText(" ");
        RTIO.PutHex(refAfter); RTIO.PutText(">");
      END;
    END;

    IF AllObjDesc[desc].ref = 0 THEN
      RTIO.PutText("ERROR >> Heap Tracer:: moved unknown object ");
      PutDesc(desc); RTIO.PutText("\n");
      RETURN;
    END;

    IF AllObjDesc[desc].state # ObjState.Active THEN
      RTIO.PutText("ERROR >> Heap Tracer:: non-active object moved ");
      PutDesc(desc); RTIO.PutText("\n");
      IF AllObjDesc[desc].state = ObjState.Moved AND
        NOT RTCollectorSRC.Moved(
                LOOPHOLE(RTMachineHeapTrace.GetRef(AllObjDesc[desc].ref),
                         ADDRESS)) 
       THEN
        RTIO.PutText("ERROR >> Heap Tracer:: incorrectly moved an object ");
        PutDesc(desc); RTIO.PutText("\n");
      END;
    END;

    INC(moveCnt);
    Create(FindObjDesc(refAfter), refAfter, AllObjDesc[desc].pc);
    AllObjDesc[desc].state := ObjState.Moved;
  END ObjectMoved;

PROCEDURE FindObj(r: REFANY): BOOLEAN =
  VAR
    addr := LOOPHOLE(r, ADDRESS);
  BEGIN
    IF RTHeapStats.obj_cnt = -1 THEN RETURN TRUE; END;
    FOR i := 0 TO RTHeapStats.obj_cnt-1 DO
      IF RTHeapStats.objects[i] = addr THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END FindObj;

VAR
  gc_epoch : INTEGER := 0;
  moved_cnt       : ARRAY [0..5] OF INTEGER;
  promoted_cnt    : ARRAY [1..3] OF INTEGER;
  deallocated_cnt : INTEGER;
  src_cnt         : INTEGER;
  th_cnt          : INTEGER;
  dir_cnt         : INTEGER;

PROCEDURE GCStarted () =
  BEGIN
    INC(gc_epoch);
    FOR i := FIRST(moved_cnt) TO LAST(moved_cnt) DO
      moved_cnt[i] := 0;
    END;
    FOR i := FIRST(promoted_cnt) TO LAST(promoted_cnt) DO
      promoted_cnt[i] := 0;
    END;
    deallocated_cnt := 0;
    src_cnt := 0;
    dir_cnt := 0;
    th_cnt := 0;
  END GCStarted;

PROCEDURE GCDone () = 
  BEGIN
    IF verbose THEN
      RTIO.PutText("\nactive : "); RTIO.PutInt(currActive);
      RTIO.PutText("; moved  : "); 
      FOR i := FIRST(moved_cnt) TO LAST(moved_cnt) DO
        RTIO.PutText(" - "); RTIO.PutInt(moved_cnt[i]);
      END;
      RTIO.PutText("; promoted  : "); 
      FOR i := FIRST(promoted_cnt) TO LAST(promoted_cnt) DO
        RTIO.PutText(" - "); RTIO.PutInt(promoted_cnt[i]);
      END;
      RTIO.PutText("; self  : "); RTIO.PutInt(src_cnt);
      RTIO.PutText("; thread  : "); RTIO.PutInt(th_cnt);
      RTIO.PutText("; direct  : "); RTIO.PutInt(dir_cnt);
      RTIO.PutText("; deallocated  : "); RTIO.PutInt(deallocated_cnt);
      RTIO.PutText("\n");
    END;
  END GCDone;

(*
 * printing
 *)

PROCEDURE PutRefDesc(r: REFANY) =
  VAR ref := RTMachineHeapTrace.RefAnyToRefT(r);
      desc := FindObjDesc(ref);
  BEGIN
    PutDesc(desc);
  END PutRefDesc;

PROCEDURE PutDesc(desc: ObjDesc) =
  BEGIN
    RTIO.PutText("[");
    RTIO.PutHex(AllObjDesc[desc].ref); RTIO.PutText(" "); 
    RTIO.PutInt(AllObjDesc[desc].tc); RTIO.PutText(" "); 
    RTIO.PutString(RTType.Get(AllObjDesc[desc].tc).name); 
    RTIO.PutText(" "); PutState(desc); RTIO.PutText("]");
  END PutDesc;

PROCEDURE PutState(desc: ObjDesc) =
  VAR msg: TEXT;
  BEGIN
    CASE AllObjDesc[desc].state OF
    | ObjState.Active => msg := "Active";
    | ObjState.Moved => msg := "Moved";
    | ObjState.Deallocated => msg := "Deallocated";
    END;
    RTIO.PutText(msg);
  END PutState;

PROCEDURE TraceTypeOn(tc: RT0.Typecode) =
  BEGIN
    curr_tc := tc;
  END TraceTypeOn; 

PROCEDURE TraceTypeOff() =
  BEGIN
    curr_tc := 0;
  END TraceTypeOff;

PROCEDURE TraceAllOn() =
  BEGIN
    curr_tc := -1;
  END TraceAllOn;

PROCEDURE TraceAllOff() =
  BEGIN
    curr_tc := 0;
  END TraceAllOff;

PROCEDURE GetAllocationPC(r: REFANY): ADDRESS =
  VAR 
    ref := RTMachineHeapTrace.RefAnyToRefT(r);
    desc := FindObjDesc(ref);
  BEGIN
    RETURN RTMachineHeapTrace.GetPC(AllObjDesc[desc].pc);
  END GetAllocationPC;

PROCEDURE Dump () =
  VAR
    table     : UNTRACED REF ARRAY OF RTMachineHeapTrace.PCCnt;
    pc        : PCT;
    found     : BOOLEAN;
    addr      : ADDRESS;
    totalSize : INTEGER := 0;
    nextPC    : INTEGER := 0;
    cnt       : INTEGER := 0;
    size, tc  : INTEGER;
  BEGIN
    RTIO.PutText("\nTraced heap usage by allocation call sites:\n");
    RTIO.PutText("\t"); RTIO.PutInt(AllObjCnt); RTIO.PutText("\n");

    (*
    table    := NEW(UNTRACED REF ARRAY OF RTMachineHeapTrace.PCCnt, AllObjCnt);
    *)

    table    := NEW(UNTRACED REF ARRAY OF RTMachineHeapTrace.PCCnt, 2000);

    FOR i := 0 TO MaxObjDesc DO
      IF AllObjDesc[i].ref # 0 AND AllObjDesc[i].state = ObjState.Active THEN
        INC(cnt);
        pc := AllObjDesc[i].pc;
        addr := LOOPHOLE(RTMachineHeapTrace.GetRef(AllObjDesc[i].ref), ADDRESS);
        size := RTHeapRep.ReferentSize(addr - ADRSIZE(RTHeapRep.Header));
        tc   := AllObjDesc[i].tc;
        INC(totalSize, size);
        found := FALSE;
        FOR j := 0 TO nextPC DO
          IF table[j].pc = pc THEN 
            INC(table[j].cnt);
            INC(table[j].size, size);
            IF table[j].tc # tc THEN
              RTIO.PutText("ERROR >> type mismatch in the allocation data\n");
            END;
            found := TRUE;
            EXIT;
          END;
        END;
        IF NOT found THEN
          table[nextPC].pc := pc;
          table[nextPC].cnt := 1;
          table[nextPC].size := size;
          table[nextPC].tc := tc;
          INC(nextPC);
        END;
      END;
    END;

    RTIO.PutText("\nNumber of active objects: ");
    RTIO.PutInt(cnt);
    RTIO.PutText("\nTotal size of active objects: ");
    RTIO.PutInt(totalSize);
    RTIO.PutText("\nNumber of active PC values: ");
    RTIO.PutInt(nextPC);
    RTIO.PutText("\n");

    FOR i := 0 TO nextPC-1 DO
      RTIO.PutInt(i, 5);
      RTIO.PutText(" ");
      RTIO.PutHex(LOOPHOLE(RTMachineHeapTrace.GetPC(table[i].pc), INTEGER));
      RTIO.PutText(" ");
      RTIO.PutInt(table[i].cnt, 6);
      RTIO.PutText(" ");
      RTIO.PutInt(table[i].size, 10);
      RTIO.PutText(" ");
      RTIO.PutString(RTType.Get(table[i].tc).name);
      RTIO.PutText("\n");
    END;

    DISPOSE (table);
  END Dump;

PROCEDURE Reset () =
  BEGIN
  END Reset;

(*
 * initialization
 *)

PROCEDURE Init() =
  BEGIN
    IF RTHeapRep.verbose > 0 THEN
      RTIO.PutText("Tracer initialized...\n");
    END;
  END Init;

BEGIN
END RTHeapTrace.
