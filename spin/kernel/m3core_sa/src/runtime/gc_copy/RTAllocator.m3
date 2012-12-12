(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(*| Last modified on Wed Oct 12 14:46:56 PDT 1994 by kalsow  *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)
(*|      modified on Tue Mar  9 08:45:18 PST 1993 by jdd     *)

(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled caching of security information.  Call FlushInitCache()
 *      to flush the cache when security information changes.
 *
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Integrated pause histogram generation.
 *
 * 02-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *      Fixed bugs in type security support. Need to add interface
 *      to type cache, move RTTypeSecurity back into run-time and
 *      make RTTypeSecurity thread-safe.
 *
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added runtime type security support.  Currently uses Z field in
 *	header to denote a secure object.  This should be fixed.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Pzemek says Allocate no longer needs to set spl
 *
 * 30-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	call into ref counting code
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added spies to account for allocation time.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed to new interrupt scheme including both classes and levels.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Switched back to 20 bit Typecodes.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added tracing and statistics of collector operations.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Modified to accomodate the split of RT0.Typecode into
 *	RT0.Typecode and RT0.Typeidx
 *)

UNSAFE MODULE RTAllocator EXPORTS RTAllocator, RTHooks;

IMPORT Cstdlib, RT0, RT0u, RTMisc, RTOS, RTType, Word, RTOSMachine;
IMPORT RTSynch;
FROM RTType IMPORT Typecode;
FROM RTMisc IMPORT FatalErrorI;
FROM RTHeapRep IMPORT Header, RefHeader, AllocForNew, Malloc;
IMPORT RTHeapRep;

FROM RTCollectorSRC IMPORT DoTimings, nestedAlloc;
FROM RTCollectorSRC IMPORT allocTraced_timer, allocUntraced_timer, allocInit_timer;
IMPORT Spy;

IMPORT RTMachineCollectorExtern;

IMPORT RTTypeSecurity;
IMPORT RTHisto;
FROM RTHisto IMPORT pauseStart;

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

(*----------------------------------------------------------- RTAllocator ---*)

(* NewTraced returns a reference to a freshly allocated and initialized,
   traced referent with typecode "tc".  It is a checked runtime error if
   "tc" does not name a traced reference type other than REFANY, or if
   its referent is an open array. *)

PROCEDURE NewTraced(tc: Typecode): REFANY =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced = 0 OR def.nDimensions # 0 THEN
      FatalErrorI("RTAllocator.NewTraced: improper typecode: ", tc);
    END;
    RETURN Allocate(def);
  END NewTraced;

(* NewUntraced returns a reference to a freshly allocated and initialized,
   untraced referent with typecode "tc".  It is a checked runtime error if
   "tc" does not name an untraced reference type other than ADDRESS, or if
   it names an untraced object type, or if its referent is an open
   array. *)

PROCEDURE NewUntraced(tc: Typecode): ADDRESS =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.defaultMethods # NIL
         OR def.nDimensions # 0 THEN
      FatalErrorI("RTAllocator.NewUntraced: improper typecode: ", tc);
    END;
    RETURN AllocateUntracedRef(def);
  END NewUntraced;

(* NewUntracedObject returns a freshly allocated and initialized, untraced
   object with typecode "tc".  It is a checked runtime error if "tc" does
   not name an untraced object type. *)

PROCEDURE NewUntracedObject(tc: Typecode): UNTRACED ROOT =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.defaultMethods = NIL THEN
      FatalErrorI("RTAllocator.NewUntracedObject: improper typecode:", tc);
    END;
    RETURN AllocateUntracedObj(def);
  END NewUntracedObject;

(* NewTracedArray returns a reference to a freshly allocated and
   initialized, traced open array referent with typecode "tc" and sizes
   "s[0]", ..., "s[LAST(s)]".  It is a checked runtime error if "tc" does
   not name a traced reference to an open array, if any s[i] is negative,
   or if "NUMBER(s)" does not equal the number of open dimensions of the
   array. *)

PROCEDURE NewTracedArray(tc: Typecode; READONLY s: Shape): REFANY =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced = 0 OR def.nDimensions = 0 THEN
      FatalErrorI("RTAllocator.NewTracedArray: improper typecode: ", tc);
    END;
    IF NUMBER(s) # def.nDimensions THEN
      FatalErrorI("RTAllocator.NewTracedArray: bad NUMBER(s): ", NUMBER(s));
    END;
    RETURN AllocateOpenArray(def, s);
  END NewTracedArray;

(* NewUntracedArray returns a reference to a freshly allocated and
   initialized, untraced open array referent with typecode "tc" and sizes
   "s[0]", ..., "s[LAST(s)]".  It is a checked runtime error if "tc" does
   not name an untraced reference to an open array, if any s[i] is
   negative, or if "NUMBER(s)" does not equal the number of open
   dimensions of the array. *)

PROCEDURE NewUntracedArray(tc: Typecode; READONLY s: Shape): ADDRESS =
  VAR def := RTType.Get(tc);
  BEGIN
    IF tc = 0 OR def.traced # 0 OR def.nDimensions = 0 THEN
      FatalErrorI("RTAllocator.NewUntracedArray: improper typecode: ", tc);
    END;
    IF NUMBER(s) # def.nDimensions THEN
      FatalErrorI("RTAllocator.NewUntracedArray: bad NUMBER(s): ", NUMBER(s));
    END;
    RETURN AllocateUntracedOpenArray(def, s);
  END NewUntracedArray;

(*--------------------------------------------------------------- RTHooks ---*)

VAR
  initCache: ARRAY [0 .. 4095] OF ADDRESS; (* initialized contents for
                                              freshly allocated objects *)
PROCEDURE Allocate (defn: ADDRESS): REFANY =
  VAR
    h   : RefHeader; 
    def : RT0.TypeDefn := defn;
    tc  : Typecode := def.typecode;
    res : ADDRESS;
    secure : BOOLEAN := FALSE; (* is this allocation secure? *)
    allocSize := def.dataSize;
  BEGIN
    RTOS.LockHeap();
    IF RTHisto.Histogram THEN
      pauseStart := RTOSMachine.CycleCounter();
    END;

    IF DoTimings AND NOT nestedAlloc THEN Spy.Enter(allocTraced_timer); END;

    (* check if secure.  if so, add to size, and set bit *)
    IF RTHeapRep.TypeSecurityOn AND
      RTTypeSecurity.typeSecurityVector[tc]
     THEN
      secure := TRUE;
      INC(allocSize, RTTypeSecurity.RecordSize); 
    END;

    BEGIN
      WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
      RT0u.total_traced_bytes := 
          Word.Plus(RT0u.total_traced_bytes, def.dataSize);
      res := AllocForNew(allocSize, def.dataAlignment);
      IF DoTimings AND NOT nestedAlloc THEN Spy.Enter(allocInit_timer); END;
      h := LOOPHOLE(res - ADRSIZE(Header), RefHeader);
      IF (tc <= LAST (initCache)) AND (initCache[tc] # NIL) THEN
        RTMisc.Copy(initCache[tc], res - ADRSIZE(Header),
                    allocSize + BYTESIZE(Header));
      ELSE
        (* XXX FIXME - using Z for security bit *)
        h^ := Header{typecode := tc, forwarded := FALSE,
                 (* FIXME: is this dealt correctly with in all places ?*)
                 size := MIN(allocSize, RT0.MaxReferentSize),
                 Z := secure, V := FALSE, refcount := 1};
        RTMisc.Zero(res, def.dataSize);
        IF def.defaultMethods # NIL THEN
          LOOPHOLE(res, UNTRACED REF ADDRESS)^ := def.defaultMethods;
        END;
        VAR d := def;
        BEGIN
          WHILE d # NIL DO
            IF d.initProc # NIL THEN d.initProc(res); END;
            d := d.parent;
          END;
        END;

        IF (def.dataSize <= BYTESIZE(def^)) AND (tc <= LAST (initCache)) THEN
          initCache[tc] := Malloc(allocSize + BYTESIZE(Header));
          RTMisc.Copy(res - ADRSIZE(Header), initCache[tc],
                      allocSize + BYTESIZE(Header));
        END;
      END;

      IF RTHeapRep.TypeSecurityOn AND secure THEN
        RTTypeSecurity.InitRecord(
            res + h.size - RTTypeSecurity.RecordSize);
      END;

      IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocInit_timer); END;
    END;
    IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocTraced_timer); END;

    IF RTHisto.Histogram THEN
      RTHisto.HistogramEnd();
    END;
    RTOS.UnlockHeap();

    (* for reference counting *)
    IF RTHeapRep.RefCount THEN
      RTMachineCollectorExtern.AssignKnown (res, NIL);
    END;
    RETURN LOOPHOLE(res, REFANY);
  END Allocate;

PROCEDURE FlushInitCache (tc: Typecode) =
  BEGIN
    DisposeUntracedRef(initCache[tc]);
  END FlushInitCache;

PROCEDURE AllocateUntracedRef (defn: ADDRESS): ADDRESS =
  VAR
    def : RT0.TypeDefn := defn;
    tc  : Typecode := def.typecode;
    res := Malloc(def.dataSize);
  BEGIN
    IF DoTimings THEN Spy.Enter(allocUntraced_timer); END;
    WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
    RT0u.total_untraced_bytes := 
        Word.Plus(RT0u.total_untraced_bytes,def.dataSize);
    IF def.initProc # NIL THEN def.initProc(res); END;
    IF DoTimings THEN Spy.Exit(allocUntraced_timer); END;
    RETURN res;
  END AllocateUntracedRef;

PROCEDURE AllocateUntracedObj (defn: ADDRESS): UNTRACED ROOT =
  VAR
    def     : RT0.TypeDefn := defn;
    hdrSize := MAX(BYTESIZE(Header), def.dataAlignment);
    tc      : RTType.Typecode := def.typecode;
    res     := Malloc(hdrSize + def.dataSize) + hdrSize;
    (* res requires special treatment by DisposeUntracedObj *)
  BEGIN
    IF DoTimings THEN Spy.Enter(allocUntraced_timer); END;
    WITH z = RT0u.alloc_cnts[tc] DO z := Word.Plus (z, 1) END;
    RT0u.total_untraced_bytes := 
        Word.Plus(RT0u.total_untraced_bytes,def.dataSize);
    LOOPHOLE(res - ADRSIZE(Header), RefHeader)^ :=
      Header{typecode := tc, forwarded := FALSE};
    IF def.defaultMethods # NIL THEN
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := def.defaultMethods;
    END;
    WHILE def # NIL DO
      IF def.initProc # NIL THEN def.initProc(res); END;
      def := def.parent;
    END;
    IF DoTimings THEN Spy.Exit(allocUntraced_timer); END;
    RETURN res;
  END AllocateUntracedObj;

PROCEDURE AllocateOpenArray (defn: ADDRESS; READONLY s: Shape): REFANY =
  VAR
    h   : RefHeader; 
    def     : RT0.TypeDefn := defn;
    res     : ADDRESS;
    nbElems := OpenArrayCount(s);
    nBytes  := def.dataSize + nbElems * def.elementSize;
    tc      : RTType.Typecode := def.typecode;
    secure : BOOLEAN := FALSE; (* secure? *)
    allocSize := nBytes;
  BEGIN
    RTOS.LockHeap();

    IF RTHisto.Histogram THEN
      pauseStart := RTOSMachine.CycleCounter();
    END;
    IF DoTimings AND NOT nestedAlloc THEN Spy.Enter(allocTraced_timer); END;

    (* check if secure.  if so, add to size, and set bit *)
    IF RTHeapRep.TypeSecurityOn AND
      RTTypeSecurity.typeSecurityVector[tc] THEN
      secure := TRUE;
      INC(allocSize, RTTypeSecurity.RecordSize);
    END;

    BEGIN
      WITH z = RT0u.alloc_cnts[tc]  DO EVAL RTSynch.Inc(z) END;
      WITH z = RT0u.alloc_bytes[tc] DO EVAL RTSynch.Inc(z, nBytes) END;
      EVAL RTSynch.Inc(RT0u.total_traced_bytes, nBytes);

      allocSize := RTMisc.Upper(allocSize, BYTESIZE(Header));
      res := AllocForNew(allocSize, def.dataAlignment);

      IF DoTimings AND NOT nestedAlloc THEN Spy.Enter(allocInit_timer); END;

      (* XXX FIXME - using Z for security bit *)
      h := LOOPHOLE(res - ADRSIZE(Header), RefHeader);
      h^ := Header{typecode := tc, forwarded := FALSE, 
               size := MIN(allocSize, RT0.MaxReferentSize),
               Z := secure, V := FALSE, refcount := 1};
      LOOPHOLE(res, UNTRACED REF ADDRESS)^ := res + def.dataSize;
      FOR i := 0 TO NUMBER(s) - 1 DO
        LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
                 UNTRACED REF INTEGER)^ := s[i];
      END;
      RTMisc.Zero(res + def.dataSize, nbElems * def.elementSize);
      WHILE def # NIL DO
        IF def.initProc # NIL THEN def.initProc(res); END;
        def := def.parent;
      END;

      IF RTHeapRep.TypeSecurityOn THEN
        h^ := Header{typecode := tc, forwarded := FALSE, 
               size := MIN(allocSize, RT0.MaxReferentSize),
               Z := secure, V := FALSE, refcount := 1};
        IF secure THEN
          RTTypeSecurity.InitRecord(res+h.size-RTTypeSecurity.RecordSize);
        END;
      END;

      IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocInit_timer); END;
    END;
    IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocTraced_timer); END;

    IF RTHisto.Histogram THEN
      RTHisto.HistogramEnd();
    END;
    RTOS.UnlockHeap();

    (* for reference counting *)
    IF RTHeapRep.RefCount THEN
      RTMachineCollectorExtern.AssignKnown (res, NIL);
    END;
    RETURN LOOPHOLE(res, REFANY);
  END AllocateOpenArray;

PROCEDURE AllocateUntracedOpenArray (defn : ADDRESS;
                            READONLY s    : Shape): ADDRESS =
  VAR
    def     : RT0.TypeDefn := defn;
    tc      : RTType.Typecode := def.typecode;
    nbElems := OpenArrayCount(s);
    nBytes  := def.dataSize + nbElems * def.elementSize;
    res     := Malloc(nBytes);
  BEGIN
    IF DoTimings THEN Spy.Enter(allocUntraced_timer); END;
    WITH z = RT0u.alloc_cnts[tc]  DO EVAL RTSynch.Inc(z) END;
    WITH z = RT0u.alloc_bytes[tc] DO EVAL RTSynch.Inc(z, nBytes) END;
    EVAL RTSynch.Inc(RT0u.total_untraced_bytes, nBytes);

    LOOPHOLE(res, UNTRACED REF ADDRESS)^ := res + def.dataSize;
    FOR i := 0 TO NUMBER(s) - 1 DO
      LOOPHOLE(res + ADRSIZE(ADDRESS) + i * ADRSIZE(INTEGER),
               UNTRACED REF INTEGER)^ := s[i];
    END;
    WHILE def # NIL DO
      IF def.initProc # NIL THEN def.initProc(res); END;
      def := def.parent;
    END;
    IF DoTimings THEN Spy.Exit(allocUntraced_timer); END;
    RETURN res;
  END AllocateUntracedOpenArray;

PROCEDURE DisposeUntracedRef (VAR a: ADDRESS) =
  BEGIN
    IF a # NIL THEN Cstdlib.free(a); a := NIL; END;
  END DisposeUntracedRef;

PROCEDURE DisposeUntracedObj (VAR a: UNTRACED ROOT) =
  VAR def: RT0.TypeDefn;
  BEGIN
    IF a # NIL THEN
      def := RTType.Get (TYPECODE (a));
      Cstdlib.free (a - MAX(BYTESIZE(Header), def.dataAlignment));
      a := NIL;
    END;
  END DisposeUntracedObj;

(*-------------------------------------------------------------- internal ---*)

(* OpenArrayCount computes the number of elements given by a Shape.  It
   also checks that all bounds are non-negative. *)

PROCEDURE OpenArrayCount (READONLY s: Shape): CARDINAL =
  VAR n := 1;
  BEGIN
    FOR i := 0 TO NUMBER(s) - 1 DO
      WITH si = s[i] DO
        IF (si < 0) THEN
          RTMisc.FatalErrorI("negative size passed to NEW (open array): ", si);
        END;
        n := si * n;
      END;
    END;
    RETURN n;
  END OpenArrayCount;

VAR
  NewAllocate : PROCEDURE (defn: ADDRESS): REFANY;
  NewAllocateOpenArray : PROCEDURE (defn: ADDRESS; READONLY s: Shape): REFANY;
  NewAllocateUntracedRef : PROCEDURE (defn: ADDRESS): ADDRESS;
  NewAllocateUntracedObj : PROCEDURE (defn: ADDRESS): UNTRACED ROOT;
  NewAllocateUntracedOpenArray : PROCEDURE (defn : ADDRESS;
                                            READONLY s: Shape): ADDRESS;

PROCEDURE AllocateWithPC (defn: ADDRESS; pc: ADDRESS): REFANY =
  VAR
    res: REFANY;
  BEGIN
    RTOS.LockHeap();
    res := NewAllocate(defn);
    IF RTHeapRep.traceOn THEN 
      RTHeapRep.ObjectAllocated(LOOPHOLE(res, REFANY), pc); 
    END;
    RTOS.UnlockHeap();
    RETURN res;
  END AllocateWithPC;

PROCEDURE AllocateOpenArrayWithPC (defn: ADDRESS; READONLY s: Shape; 
                                   pc: ADDRESS): REFANY =
  VAR
    res: REFANY;
  BEGIN
    RTOS.LockHeap();
    res := NewAllocateOpenArray(defn, s);
    IF RTHeapRep.traceOn THEN 
      RTHeapRep.ObjectAllocated(LOOPHOLE(res, REFANY), pc);
    END;
    RTOS.UnlockHeap();
    RETURN res;
  END AllocateOpenArrayWithPC; 

PROCEDURE AllocateUntracedRefWithPC (defn: ADDRESS; pc: ADDRESS): ADDRESS =
  VAR
    res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
    res := NewAllocateUntracedRef(defn);
    IF RTHeapRep.traceOn THEN 
      RTHeapRep.ObjectAllocated(LOOPHOLE(res, REFANY), pc);
    END;
    RTOS.UnlockHeap();
    RETURN res;
  END AllocateUntracedRefWithPC; 

PROCEDURE AllocateUntracedObjWithPC (defn: ADDRESS; pc: ADDRESS): ADDRESS =
  VAR
    res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
    res := NewAllocateUntracedObj(defn);
    IF RTHeapRep.traceOn THEN 
      RTHeapRep.ObjectAllocated(LOOPHOLE(res, REFANY), pc); 
    END;
    RTOS.UnlockHeap();
    RETURN res;
  END AllocateUntracedObjWithPC; 

PROCEDURE AllocateUntracedOpenArrayWithPC (defn: ADDRESS; READONLY s: Shape; 
                                          pc: ADDRESS): ADDRESS =
  VAR
    res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
    res := NewAllocateUntracedOpenArray(defn, s);
    IF RTHeapRep.traceOn THEN 
      RTHeapRep.ObjectAllocated(LOOPHOLE(res, REFANY), pc);
    END;
    RTOS.UnlockHeap();
    RETURN res;
  END AllocateUntracedOpenArrayWithPC; 

PROCEDURE Init () =
  VAR spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
  BEGIN
    IF RTHeapRep.traceOn THEN
      NewAllocate := 
          LOOPHOLE(RTOSMachine.AddPCBypass(
                       LOOPHOLE(Allocate, ADDRESS),
                       LOOPHOLE(AllocateWithPC, ADDRESS), 1),
                   PROCEDURE (defn: ADDRESS): REFANY);
      NewAllocateOpenArray := 
          LOOPHOLE(RTOSMachine.AddPCBypass(
                       LOOPHOLE(AllocateOpenArray, ADDRESS), 
                       LOOPHOLE(AllocateOpenArrayWithPC, ADDRESS), 2),
                   PROCEDURE (defn: ADDRESS; READONLY s: Shape): REFANY);

      NewAllocateUntracedRef := 
          LOOPHOLE(RTOSMachine.AddPCBypass(
                       LOOPHOLE(AllocateUntracedRef, ADDRESS),
                       LOOPHOLE(AllocateUntracedRefWithPC, ADDRESS), 1),
                   PROCEDURE (defn: ADDRESS): ADDRESS);
      NewAllocateUntracedObj := 
          LOOPHOLE(RTOSMachine.AddPCBypass(
                       LOOPHOLE(AllocateUntracedObj, ADDRESS),
                       LOOPHOLE(AllocateUntracedObjWithPC, ADDRESS), 1),
                   PROCEDURE (defn: ADDRESS): UNTRACED ROOT);
      NewAllocateUntracedOpenArray := 
          LOOPHOLE(RTOSMachine.AddPCBypass(
                       LOOPHOLE(AllocateUntracedOpenArray, ADDRESS), 
                       LOOPHOLE(AllocateUntracedOpenArrayWithPC, ADDRESS), 2),
                   PROCEDURE (defn: ADDRESS; READONLY s: Shape): ADDRESS);
    END;

    RTHisto.Init();

    RTOSMachine.RestoreInterruptMask(spl);
  END Init;

BEGIN
END RTAllocator.
