(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)

UNSAFE MODULE Heap;

IMPORT RTType, RTTypeSRC;
IMPORT RTMisc, RTHeapRep, RTHeapMap, RT0, RT0u, RTOS, RTCollector;
IMPORT Domain, DomainRep, DomainPrivate, IO, Fmt, ParseParams, Mx;
IMPORT RTIO, RTHeapStats;

(* FIXME need a general use IO putter *)

(*** ITERATION ***************************************************************)

TYPE
  IterClosure = Domain.Closure OBJECT
  METHODS
    iterate(domain: Domain.T) := IterateOverTypecells;
    start() := DoNothing;
    typecell(t: RT0.TypeDefn) := NIL;
    stop() := DoNothing;
    noM3() := DoNothing;
  OVERRIDES
    apply := CallIterate;
  END;

PROCEDURE CallIterate (cl: IterClosure; domain: Domain.T): BOOLEAN =
  BEGIN
    cl.iterate(domain);
    RETURN TRUE;
  END CallIterate; 

PROCEDURE DoNothing (<* UNUSED *>cl: IterClosure) =
  BEGIN
  END DoNothing;

PROCEDURE IterateOverTypecells (cl: IterClosure; domain: Domain.T) =
  VAR
    name     : TEXT;
    trusted  : BOOLEAN;
    dynamic  : BOOLEAN;
    linkBase : Mx.LinkSet := NIL;
  BEGIN
    IF domain = NIL THEN
      linkBase := DomainPrivate.staticLinkBase;
      name := "static kernel";
    ELSE
      DomainPrivate.GetState(domain, name, trusted, dynamic);
      IF domain.parent = NIL THEN
        linkBase := domain.linkBase;
      END;
    END;
    IF domain = NIL OR domain.parent = NIL THEN
      IF linkBase # NIL THEN
        (*
         * iterate over all typecodes
         *)
        VAR
          modules     : DomainPrivate.ModulePtrs;
          linkInfo    : DomainPrivate.LinkInfoPtrs;
          t           : RT0.TypeDefn;
          first, last : INTEGER;
        BEGIN
          IF domain = NIL THEN
            modules := RT0u.modules_array;
            first := 0;
            last  := DomainPrivate.staticModuleCnt;
          ELSE
            DomainPrivate.CollectM3Modules(domain, modules, linkInfo);
            first := FIRST(modules^);
            last  := LAST(modules^);
          END;
          IF modules # NIL THEN
            cl.start();
            FOR i := first TO last DO
              (* RTIO.PutString(modules[i].file); RTIO.PutText("\n");*)
              t := LOOPHOLE(modules[i].type_cells, RT0.TypeDefn);
              WHILE (t # NIL) DO 
                cl.typecell(t);
                t := t.next; 
              END;
            END;
            cl.stop();
          ELSE
            cl.noM3();
          END;
        END;
      END;
    END;
  END IterateOverTypecells;

(*** TABLE 1 *****************************************************************)
(*
TYPE
  Class = { Error, Named, Integer, Real, Longreal, Extended,
            Array, Enum, Object, Opaque, OpenArray, Packed,
            Procedure, Record, Ref, Set, Subrange, Aligned };
*)

TYPE
  T1Closure = IterClosure OBJECT
    ob: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    rc: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    oa: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    ar: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    rf: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    st: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    pr: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    sc: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    mo: ARRAY [0..3] OF INTEGER := ARRAY [0..3] OF INTEGER{ 0, ..};
    total: INTEGER := 0;
  OVERRIDES
    typecell := T1Count;
  END;

PROCEDURE T1Count (cl: T1Closure; <*UNUSED*>t: RT0.TypeDefn) =
  BEGIN
    INC(cl.total);
    (* IO.Put(Fmt.Int(t.class) & " ");*)
    (* FIXME
    IF t.class = -1 OR t.class = -2 THEN
      INC(cl.mo[t.traced]);
    ELSE
      CASE VAL(t.class, Class) OF
      | Class.Object    => INC(cl.ob[t.traced]);
      | Class.Record    => INC(cl.rc[t.traced]);
      | Class.OpenArray => INC(cl.oa[t.traced]);
      | Class.Array     => INC(cl.ar[t.traced]);
      | Class.Ref       => INC(cl.rf[t.traced]);
      | Class.Set       => INC(cl.st[t.traced]);
      | Class.Procedure => INC(cl.pr[t.traced]);
      | Class.Integer, Class.Real, Class.Longreal,
        Class.Extended, Class.Enum => INC(cl.sc[t.traced]);

        (* these should have been filtered out by the compiler *)
      | Class.Opaque => (*IO.PutError("Opaque found\n");*)INC(cl.mo[t.traced]);
      | Class.Packed =>  IO.PutError("Packed found\n");
      | Class.Subrange => IO.PutError("Subrange found\n");
      | Class.Aligned => IO.PutError("Aligned found\n");
      | Class.Named => IO.PutError("Named found\n");
      | Class.Error => IO.PutError("Error found\n");
      END;
    END;
    *)
  END T1Count;

PROCEDURE Table1 () =
  VAR
    cl := NEW(T1Closure);
    t  : INTEGER;
  BEGIN
    IO.Put("|----------------|-------|----|---------|----|\n");
    IO.Put("| referent class | traced (R) | untraced (R) |\n");
    IO.Put("|                |     # |  % |       # |  % |\n");
    IO.Put("|----------------|-------|----|---------|----|\n");
    
    EVAL cl.apply(NIL);
    EVAL DomainPrivate.ApplyToAll(cl);

    t := cl.total;
    IO.Put(Fmt.F("| object         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.ob[1]), Fmt.Int(cl.ob[1] * 100 DIV t),
                 Fmt.Int(cl.ob[0]), Fmt.Int(cl.ob[0] * 100 DIV t)));
    IO.Put(Fmt.F("| record         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.rc[1]), Fmt.Int(cl.rc[1] * 100 DIV t),
                 Fmt.Int(cl.rc[0]), Fmt.Int(cl.rc[0] * 100 DIV t)));
    IO.Put(Fmt.F("| open array     |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.oa[1]), Fmt.Int(cl.oa[1] * 100 DIV t),
                 Fmt.Int(cl.oa[0]), Fmt.Int(cl.oa[0] * 100 DIV t)));
    IO.Put(Fmt.F("| array          |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.ar[1]), Fmt.Int(cl.ar[1] * 100 DIV t),
                 Fmt.Int(cl.ar[0]), Fmt.Int(cl.ar[0] * 100 DIV t)));
    IO.Put(Fmt.F("| ref            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.rf[1]), Fmt.Int(cl.rf[1] * 100 DIV t),
                 Fmt.Int(cl.rf[0]), Fmt.Int(cl.rf[0] * 100 DIV t)));
    (* FIXME *)
    IO.Put(Fmt.F("| untraced ref   |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(-1), Fmt.Int(-1),
                 Fmt.Int(-1), Fmt.Int(-1)));
    IO.Put(Fmt.F("| set            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.st[1]), Fmt.Int(cl.st[1] * 100 DIV t),
                 Fmt.Int(cl.st[0]), Fmt.Int(cl.st[0] * 100 DIV t)));
    IO.Put(Fmt.F("| procedure      |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.pr[1]), Fmt.Int(cl.pr[1] * 100 DIV t),
                 Fmt.Int(cl.pr[0]), Fmt.Int(cl.pr[0] * 100 DIV t)));
    IO.Put(Fmt.F("| scalar         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.sc[1]), Fmt.Int(cl.sc[1] * 100 DIV t),
                 Fmt.Int(cl.sc[0]), Fmt.Int(cl.sc[0] * 100 DIV t)));
    (* FIXME *)
    IO.Put(Fmt.F("| ???            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.mo[1]), Fmt.Int(cl.mo[1] * 100 DIV t),
                 Fmt.Int(cl.mo[0]), Fmt.Int(cl.mo[0] * 100 DIV t)));
    IO.Put("|----------------|-------|----|---------|----|\n\n");

    IO.Put("|----------------|-------|----|---------|----|\n");
    IO.Put("| referent class | traced (A) | untraced (A) |\n");
    IO.Put("|                |     # |  % |       # |  % |\n");
    IO.Put("|----------------|-------|----|---------|----|\n");
    IO.Put(Fmt.F("| object         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.ob[3]), Fmt.Int(cl.ob[3] * 100 DIV t),
                 Fmt.Int(cl.ob[2]), Fmt.Int(cl.ob[2] * 100 DIV t)));
    IO.Put(Fmt.F("| record         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.rc[3]), Fmt.Int(cl.rc[3] * 100 DIV t),
                 Fmt.Int(cl.rc[2]), Fmt.Int(cl.rc[2] * 100 DIV t)));
    IO.Put(Fmt.F("| open array     |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.oa[3]), Fmt.Int(cl.oa[3] * 100 DIV t),
                 Fmt.Int(cl.oa[2]), Fmt.Int(cl.oa[2] * 100 DIV t)));
    IO.Put(Fmt.F("| array          |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.ar[3]), Fmt.Int(cl.ar[3] * 100 DIV t),
                 Fmt.Int(cl.ar[2]), Fmt.Int(cl.ar[2] * 100 DIV t)));
    IO.Put(Fmt.F("| ref            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.rf[3]), Fmt.Int(cl.rf[3] * 100 DIV t),
                 Fmt.Int(cl.rf[2]), Fmt.Int(cl.rf[2] * 100 DIV t)));
    (* FIXME *)
    IO.Put(Fmt.F("| untraced ref   |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(-1), Fmt.Int(-1),
                 Fmt.Int(-1), Fmt.Int(-1)));
    IO.Put(Fmt.F("| set            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.st[3]), Fmt.Int(cl.st[3] * 100 DIV t),
                 Fmt.Int(cl.st[2]), Fmt.Int(cl.st[2] * 100 DIV t)));
    IO.Put(Fmt.F("| procedure      |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.pr[3]), Fmt.Int(cl.pr[3] * 100 DIV t),
                 Fmt.Int(cl.pr[2]), Fmt.Int(cl.pr[2] * 100 DIV t)));
    IO.Put(Fmt.F("| scalar         |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.sc[3]), Fmt.Int(cl.sc[3] * 100 DIV t),
                 Fmt.Int(cl.sc[2]), Fmt.Int(cl.sc[2] * 100 DIV t)));
    (* FIXME *)
    IO.Put(Fmt.F("| ???            |  %4s | %2s |    %4s | %2s |\n",
                 Fmt.Int(cl.mo[3]), Fmt.Int(cl.mo[3] * 100 DIV t),
                 Fmt.Int(cl.mo[2]), Fmt.Int(cl.mo[2] * 100 DIV t)));
    IO.Put("|----------------|-------|----|---------|----|\n");
  END Table1;

(*** TABLE A *****************************************************************)

TYPE
  TAClosure = IterClosure OBJECT
    sub: REF ARRAY OF INTEGER;
    dep: REF ARRAY OF INTEGER;
  OVERRIDES
    typecell := TACount;
  END;

PROCEDURE TACount (cl: TAClosure; t: RT0.TypeDefn) =
  BEGIN
    (*IO.Put(Fmt.Int(t.lastSubTypeCode) & " " & Fmt.Int(t.subTypeCode) & "\n");*)
    INC(cl.sub[t.lastSubTypeCode - t.subTypeCode]);

    VAR depth := -1;
    BEGIN
      WHILE t # NIL DO
        INC(depth);
        t := t.parent;
      END;
      INC(cl.dep[depth]);
    END;
  END TACount;

PROCEDURE TableA () =
  VAR
    cl := NEW (TAClosure);
    nTypes := RT0u.nTypes;
  BEGIN
    cl.sub := NEW (REF ARRAY OF INTEGER, nTypes);
    cl.dep := NEW (REF ARRAY OF INTEGER, nTypes);

    EVAL cl.apply(NIL);
    EVAL DomainPrivate.ApplyToAll(cl);

    IO.Put("|-----------|--------|\n");
    IO.Put("| #subtypes | #types |\n");
    IO.Put("|-----------|--------|\n");
    FOR i := FIRST (cl.sub^) TO LAST (cl.sub^) DO
      IF cl.sub[i] # 0 THEN
        IO.Put(Fmt.F("|      %4s |   %4s |\n", Fmt.Int(i), Fmt.Int(cl.sub[i])));
      END;
    END;
    IO.Put("|-----------|--------|\n");
    IO.Put("\n");

    IO.Put("|-------|--------|\n");
    IO.Put("| depth | #types |\n");
    IO.Put("|-------|--------|\n");
    FOR i := FIRST (cl.dep^) TO LAST (cl.dep^) DO
      IF cl.dep[i] # 0 THEN
        IO.Put(Fmt.F("|  %4s |   %4s |\n", Fmt.Int(i), Fmt.Int(cl.dep[i])));
      END;
    END;
    IO.Put("|-------|--------|\n");
  END TableA;


(*** TABLE 2 and 3 ***********************************************************)

TYPE
  SizeDesc = RECORD
    size : INTEGER := 0;
    tcnt : INTEGER := 0;
    ucnt : INTEGER := 0;
  END;

TYPE
  T2Closure = IterClosure OBJECT
    exact : REF ARRAY OF SizeDesc;
    real  : REF ARRAY OF SizeDesc;
    elem  : REF ARRAY OF SizeDesc;
    max   : INTEGER := 0;
  OVERRIDES
    typecell := T2Count;
  END;

(* FIXME: let the array grow inside *)
PROCEDURE IncSizeCnt (sizes: REF ARRAY OF SizeDesc; 
                      size: CARDINAL; traced: BOOLEAN; cnt: CARDINAL) =
  VAR
    i: INTEGER;
  BEGIN
    (* find the right spot *)
    i := FIRST (sizes^);
    WHILE i < LAST (sizes^) AND size < sizes[i].size DO
      INC(i);
    END;

    IF sizes[i].size = size THEN
      (* increment for the existing size*)
      IF traced THEN
        INC(sizes[i].tcnt, cnt);
      ELSE
        INC(sizes[i].ucnt, cnt);
      END;
    ELSE
      (* move the rest *)
      FOR j := LAST (sizes^) TO i+1 BY -1 DO
        sizes[j] := sizes[j-1];
      END;

      (* insert new size *)
      sizes[i].size := size;
      IF traced THEN
        sizes[i].tcnt  := cnt;
        sizes[i].ucnt  := 0;
      ELSE
        sizes[i].ucnt  := cnt;
        sizes[i].tcnt  := 0;
      END;
    END;
  END IncSizeCnt;

PROCEDURE T2Count (cl: T2Closure; t: RT0.TypeDefn) =
  BEGIN
    IF t.nDimensions = 0 THEN
      IF t.dataSize > cl.max THEN
        cl.max := t.dataSize;
      END;
      IncSizeCnt(cl.exact, t.dataSize, t.traced = 1, 1);
      (* FIXME -- collector dependent *)
      IncSizeCnt(cl.real,
                 RTMisc.Upper(t.dataSize, BYTESIZE(RTHeapRep.Header)) +
                 BYTESIZE(RTHeapRep.Header),
                 t.traced = 1,
                 1);
    ELSE
      IncSizeCnt(cl.elem, t.elementSize, t.traced = 1, 1);
    END;
  END T2Count;

PROCEDURE Table2and3 () =
  VAR
    cl := NEW (T2Closure);
    nTypes := RT0u.nTypes;
  BEGIN
    cl.exact := NEW (REF ARRAY OF SizeDesc, nTypes);
    cl.real  := NEW (REF ARRAY OF SizeDesc, nTypes);
    cl.elem  := NEW (REF ARRAY OF SizeDesc, nTypes);
    FOR i := 0 TO nTypes - 1  DO
      cl.exact[i].size := -1;
      cl.real[i].size := -1;
      cl.elem[i].size := -1;
    END;

    EVAL cl.apply(NIL);
    EVAL DomainPrivate.ApplyToAll(cl);

    IO.Put("|------------|---------|-----------|\n");
    IO.Put("|      exact | #traced | #untraced |\n");
    IO.Put("|------------|---------|-----------|\n");
    FOR i := 0 TO nTypes - 1  DO
      IF cl.exact[i].size # -1 THEN
        IO.Put(Fmt.F("| %10s |    %4s |      %4s |\n", 
                     Fmt.Int(cl.exact[i].size), 
                     Fmt.Int(cl.exact[i].tcnt),
                     Fmt.Int(cl.exact[i].ucnt)));
      END;
    END;
    IO.Put("|------------|---------|-----------|\n");
    IO.Put("\n");

    IO.Put("|------------|---------|-----------|\n");
    IO.Put("|       real | #traced | #untraced |\n");
    IO.Put("|------------|---------|-----------|\n");
    FOR i := 0 TO nTypes - 1 DO
      IF cl.real[i].size # -1 THEN
        IO.Put(Fmt.F("| %10s |    %4s |      %4s |\n", 
                     Fmt.Int(cl.real[i].size), 
                     Fmt.Int(cl.real[i].tcnt),
                     Fmt.Int(cl.real[i].ucnt)));
      END;
    END;
    IO.Put("|------------|---------|-----------|\n");
    IO.Put("\n");

    IO.Put("|------------|---------|-----------|\n");
    IO.Put("|       elem | #traced | #untraced |\n");
    IO.Put("|------------|---------|-----------|\n");
    FOR i := 0 TO nTypes - 1 DO
      IF cl.elem[i].size # -1 THEN
        IO.Put(Fmt.F("| %10s |    %4s |      %4s |\n", 
                     Fmt.Int(cl.elem[i].size), 
                     Fmt.Int(cl.elem[i].tcnt),
                     Fmt.Int(cl.elem[i].ucnt)));
      END;
    END;
    IO.Put("|------------|---------|-----------|\n");
  END Table2and3;

(*** TABLE F *****************************************************************)

TYPE
  TFClosure = Domain.Closure OBJECT
    abcqpa: BOOLEAN;
    i         := 0;
    intCntT   := 0;
    modCntT   := 0;
    intUnCntT := 0;
    modUnCntT := 0;
    trCntT    := 0;
    untrCntT  := 0;
    mapCntT   := 0;
    brandCntT := 0;
  OVERRIDES
    apply := TFCount;
  END;

PROCEDURE TFCount (<*UNUSED*>cl: TFClosure; <*UNUSED*>domain: Domain.T): BOOLEAN =
(*
  VAR
    name     : TEXT;
    trusted  : BOOLEAN;
    dynamic  : BOOLEAN;
    linkBase : Mx.LinkSet := NIL;
*)
  BEGIN
(* FIXME
    IF domain = NIL THEN
      linkBase := DomainPrivate.staticLinkBase;
      name := "static kernel";
    ELSE
      DomainPrivate.GetState(domain, name, trusted, dynamic);
      IF domain.parent = NIL THEN
        linkBase := domain.linkBase;
      END;
    END;
    IF domain = NIL OR domain.parent = NIL THEN
      INC(cl.i);
      IO.Put(Fmt.F("|%4s | %-15s |", Fmt.Int(cl.i), name));
      IF linkBase # NIL THEN
        VAR 
          units  := Mx.Contents(linkBase);
          intCnt := 0;
          modCnt := 0;
          intUnCnt := 0;
          modUnCnt := 0;
        BEGIN
          WHILE units # NIL DO
            IF units.unit.interface THEN
              INC(intCnt); 
              IF NOT units.unit.safe THEN
                INC(intUnCnt);
              END;
            ELSE
              INC(modCnt);
              IF NOT units.unit.safe THEN
                INC(modUnCnt);
              END;
            END;
            units := units.next;
          END;
          IO.Put(Fmt.F("%4s |%4s |%4s |%4s |", 
                       Fmt.Int(intCnt), Fmt.Int(modCnt), 
                       Fmt.Int(intUnCnt), Fmt.Int(modUnCnt)));
          INC(cl.intCntT, intCnt);
          INC(cl.intUnCntT, intUnCnt);
          INC(cl.modCntT, modCnt);
          INC(cl.modUnCntT, modUnCnt);
        END;
        VAR
          modules  : DomainPrivate.ModulePtrs;
          linkInfo : DomainPrivate.LinkInfoPtrs;
          t        : RT0.TypeDefn;
          trCnt    := 0;
          untrCnt  := 0;
          mapCnt   := 0;
          brandCnt := 0;
          first, last: INTEGER;
        BEGIN
          IF domain = NIL THEN
            modules := RT0u.modules_array;
            first := 0;
            last  := DomainPrivate.staticModuleCnt;
          ELSE
            DomainPrivate.CollectM3Modules(domain, modules, linkInfo);
            first := FIRST(modules^);
            last  := LAST(modules^);
          END;
          IF modules # NIL THEN
            FOR i := first TO last DO
              t := LOOPHOLE(modules[i].type_cells, RT0.TypeDefn);
              WHILE (t # NIL) DO 
                IF t.traced # 0 THEN
                  INC(trCnt);
                ELSE
                  INC(untrCnt);
                END;
                IF t.gc_map # NIL THEN
                  INC(mapCnt);
                END;
                IF t.brand # NIL THEN
                  INC(brandCnt);
                END;
                t := t.next; 
              END;
            END;
            IO.Put(Fmt.F("%4s |%4s |%4s |%4s |",
                         Fmt.Int(trCnt), Fmt.Int(untrCnt),
                         Fmt.Int(mapCnt), Fmt.Int(brandCnt)));
            INC(cl.trCntT, trCnt);
            INC(cl.untrCntT, untrCnt);
            INC(cl.brandCntT, brandCnt);
            INC(cl.mapCntT, mapCnt);
          ELSE
            IO.PutError("M3 but no modules\n");
          END;
        END;
      ELSE
        IO.Put("  -- |  -- |  -- |  -- |  -- |  -- |  -- |  -- |");
      END;
      IO.Put("\n");
    END;
*)
    RETURN TRUE;
  END TFCount; 

PROCEDURE TableF () =
  VAR
    cl := NEW(TFClosure);
  BEGIN
    (* print the header *)
    IO.Put("|-----|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|\n");
    IO.Put("|  no | name            | #int| #mod|#uint|#umod| #trd|#utrd| #brd| #gcm|\n");
    IO.Put("|-----|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|\n");

    (* print information for the static kernel *)
    EVAL cl.apply(NIL);
    
    (* print information for all extensions *)
    EVAL DomainPrivate.ApplyToAll(cl);
    IO.Put("|-----|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|\n");
    IO.Put(Fmt.F("|     | total           |%4s |%4s |%4s |%4s |",
                 Fmt.Int(cl.intCntT), Fmt.Int(cl.modCntT),
                 Fmt.Int(cl.intUnCntT), Fmt.Int(cl.modUnCntT)));
    IO.Put(Fmt.F("%4s |%4s |%4s |%4s |\n", 
                 Fmt.Int(cl.trCntT), Fmt.Int(cl.untrCntT),
                 Fmt.Int(cl.brandCntT), Fmt.Int(cl.mapCntT)));
    IO.Put("|-----|-----------------|-----|-----|-----|-----|-----|-----|-----|-----|\n");
  END TableF;

(*** TABLE 4 *****************************************************************)

(* FIXME: add number/size of whole global area *)

CONST
  MAX_CNT = 1000;

TYPE
  T4Counts = ARRAY [0..MAX_CNT] OF INTEGER;

  T4Closure = Domain.Closure OBJECT
    ba: BOOLEAN;
    counts   : T4Counts := T4Counts{0, ..};
  OVERRIDES
    apply := T4Count;
  END;

  T4Visitor = RTHeapMap.Visitor OBJECT
    cnt: INTEGER := 0;
  OVERRIDES
    apply := T4Inc;
  END;

PROCEDURE T4Inc(self: T4Visitor; <*UNUSED*>a: ADDRESS) =
  BEGIN
    INC(self.cnt);
  END T4Inc;

(* FIXME: this is per unit, make it per variable *)
PROCEDURE T4Count (<*UNUSED*>cl: T4Closure; <*UNUSED*>domain: Domain.T): BOOLEAN =
(*
  VAR
    name     : TEXT;
    trusted  : BOOLEAN;
    dynamic  : BOOLEAN;
    linkBase : Mx.LinkSet := NIL;
    v        := NEW(T4Visitor);
    first, last : INTEGER;
*)
  BEGIN
(* FIXME
    IF domain = NIL THEN
      linkBase := DomainPrivate.staticLinkBase;
      name := "static kernel";
    ELSE
      DomainPrivate.GetState(domain, name, trusted, dynamic);
      IF domain.parent = NIL THEN
        linkBase := domain.linkBase;
      END;
    END;
    IF (domain = NIL OR domain.parent = NIL) AND linkBase # NIL THEN
      VAR
        modules  : DomainPrivate.ModulePtrs;
        linkInfo : DomainPrivate.LinkInfoPtrs;
      BEGIN
        IF domain = NIL THEN
          modules := RT0u.modules_array;
          first := 0;
          last  := DomainPrivate.staticModuleCnt;
        ELSE
          DomainPrivate.CollectM3Modules(domain, modules, linkInfo);
          IF modules # NIL THEN
            first := FIRST(modules^);
            last  := LAST(modules^);
          END;
        END;
        IF modules # NIL THEN
          FOR i := first TO last DO
            WITH m = modules[i],
                 map = m.gc_map 
             DO
              IF map # NIL THEN
                v.cnt := 0;
                RTHeapMap.Walk(m, map, v);
                IF v.cnt < NUMBER(cl.counts) THEN
                  INC(cl.counts[v.cnt]);
                ELSE
                  IO.Put("ERROR >> overflow in Table4\n");
                END;
              ELSE
                INC(cl.counts[0]);
              END;
            END;
          END;
        ELSE
          IO.PutError("M3 but no modules\n");
        END;
      END;
    END;
*)
    RETURN TRUE;
  END T4Count; 

PROCEDURE Table4 () =
  VAR
    cl := NEW(T4Closure);
  BEGIN
    EVAL cl.apply(NIL);
    EVAL DomainPrivate.ApplyToAll(cl);

    IO.Put("|------------|--------|\n");
    IO.Put("| #REF slots | #units |\n");
    IO.Put("|------------|--------|\n");
    FOR i := FIRST (cl.counts) TO LAST (cl.counts) DO
      IF cl.counts[i] # 0 THEN
        IO.Put(Fmt.F("|       %4s |   %4s |\n", 
                     Fmt.Int(i), Fmt.Int(cl.counts[i])));
      END;
    END;
    IO.Put("|------------|--------|\n");
    IO.Put("\n");
  END Table4;

(*** TABLE 5 and 6, FIGURE 5 and 6 *******************************************)

CONST
  INIT_SIZE_CNT = 1000;

TYPE
  TypeDesc = RECORD
    count : INTEGER := 0;
    size  : INTEGER := 0;
  END;

TYPE
  R = REF ARRAY OF TypeDesc;
  Map = REF ARRAY OF INTEGER;
            
  Visitor = RTHeapRep.RefVisitor OBJECT
    a: BOOLEAN;
              r         : R;
              countSum  := 0;
              sizeSum   := 0;
              sizeCounts : REF ARRAY OF SizeDesc;
              sizeBytes  : REF ARRAY OF SizeDesc;
            OVERRIDES
              visit := Walk
            END;

PROCEDURE Report (map: Map; v: Visitor; counts: BOOLEAN) =
  BEGIN
    IO.Put("|-----|----------|------|-----------------------------------------------------|\n");
    IF counts THEN
      IO.Put("|  tc |    count |    % | Name                                                |\n");
    ELSE
      IO.Put("|  tc |    bytes |    % | Name                                                |\n");
    END;
    IO.Put("|-----|----------|------|-----------------------------------------------------|\n");
                                     
    FOR i := 0 TO LAST (v.r^) DO
      WITH tc = map[i], zz =v.r[tc] DO
        IF (zz.count > 0) THEN
          IF counts THEN
            IO.Put(Fmt.F("|%4s | %8s | %4s |",
                         Fmt.Int(tc), Fmt.Int(zz.count), 
                         Fmt.Int(zz.count * 1000 DIV v.countSum)));
          ELSE
            IO.Put(Fmt.F("|%4s | %8s | %4s |",
                         Fmt.Int(tc), Fmt.Int(zz.count), 
                         Fmt.Int(zz.count * 1000 DIV v.countSum)));
          END;
          IO.Put(Fmt.F(" %-51s |\n", RTTypeSRC.TypecodeName (tc)));
        END
      END;
    END;
    IO.Put("|-----|----------|------|-----------------------------------------------------|\n\n");
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
    IncSizeCnt (v.sizeCounts, size, TRUE, 1);
    IncSizeCnt (v.sizeBytes, size, TRUE, size);
    RETURN TRUE
  END Walk;

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

PROCEDURE Table5and6_Figure5and6 () =
  VAR
    v := NEW (Visitor, 
              r := NEW (R, RTType.MaxTypecode() + 1),
              sizeCounts := NEW (REF ARRAY OF SizeDesc, INIT_SIZE_CNT),
              sizeBytes := NEW (REF ARRAY OF SizeDesc, INIT_SIZE_CNT));
    map := NEW (Map, NUMBER (v.r^));
    c: INTEGER := 0;
  BEGIN
    FOR i := 0 TO INIT_SIZE_CNT - 1  DO
      v.sizeCounts[i].size := -1;
      v.sizeBytes[i].size := -1;
    END;
    v.countSum := 0;
    v.sizeSum  := 0;
    FOR i := FIRST(v.r^) TO LAST (v.r^) DO
      v.r[i].count := 0;
      v.r[i].size  := 0;
    END;

    RTOS.LockHeap ();
    RTCollector.Collect ();
    RTCollector.Collect ();
    RTCollector.Collect ();
    
    RTHeapRep.VisitAllRefs (v, FALSE);

    FOR i := 0 TO LAST (map^) DO map[i] := i; END;
    
    IO.Put("current number of objects per type\n\n");
    Sort (map, v.r, CompareCount);
    Report (map, v, TRUE);

    IO.Put("current allocated bytes per type\n\n");
    Sort (map, v.r, CompareSize);
    Report (map, v, FALSE);

    IO.Put("distribution of objects by object size\n\n");
    IO.Put("|------------|----------|\n");
    IO.Put("|       size | #objects |\n");
    IO.Put("|------------|----------|\n");
    FOR i := FIRST(v.sizeCounts^) TO LAST(v.sizeCounts^) DO
      IF v.sizeCounts[i].size # -1 THEN
        IO.Put(Fmt.F("| %10s |   %6s |\n", 
                     Fmt.Int(v.sizeCounts[i].size), 
                     Fmt.Int(v.sizeCounts[i].tcnt)));
        INC(c, v.sizeCounts[i].tcnt);
      END;
    END;
    IO.Put("|------------|----------|\n");
    IO.PutInt(c); IO.Put(" "); 
    IO.PutInt(v.countSum); IO.Put(" "); 
    IO.Put("\n");

    IO.Put("distribution of object bytes by object size\n\n");
    IO.Put("|------------|----------|\n");
    IO.Put("|       size |   #bytes |\n");
    IO.Put("|------------|----------|\n");
    FOR i := FIRST(v.sizeBytes^) TO LAST(v.sizeBytes^) DO
      IF v.sizeBytes[i].size # -1 THEN
        IO.Put(Fmt.F("| %10s | %8s |\n", 
                     Fmt.Int(v.sizeBytes[i].size), 
                     Fmt.Int(v.sizeBytes[i].tcnt)));
      END;
    END;
    IO.Put("|------------|----------|\n");
    IO.Put("\n");

    RTOS.UnlockHeap ();
  END Table5and6_Figure5and6;

(*****************************************************************************)

VAR
  ok := TRUE;

(*
 * Shell extension
 *)
VAR
  Putter := NEW (RTIO.SimplePutter);

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
    
  BEGIN
    IF pp.keywordPresent("zap") THEN
      ok := FALSE;
    END;
    IF pp.keywordPresent("table") THEN
      IO.Put("\n##### Table F\n\n");
      TableF();
      IO.Put("\n##### Table 1\n\n");
      Table1();
      IO.Put("\n##### Table A\n\n");
      TableA();
      IO.Put("\n##### Table 2 and 3\n\n");
      Table2and3();
      IO.Put("\n##### Table 4\n\n");
      Table4();
      IO.Put("\n##### Table 5 and 6\n\n");
      Table5and6_Figure5and6();
      IO.Put("\n##### done\n\n");
    END;
    IF pp.keywordPresent("addr") THEN
      RTHeapStats.PrintAddr (Putter, 50);
    END;
    IF pp.keywordPresent("tc") THEN
      RTHeapStats.PrintTC (Putter, 20);
    END;
    IF pp.keywordPresent("pc") THEN
      RTHeapStats.PrintPC (Putter, 20);
    END;
    IF pp.keywordPresent("trace") THEN
      RTHeapStats.PrintTraceInfo (Putter);
    END;
    IF pp.keywordPresent("help") THEN
      IO.Put ("heap table | addr | tc | pc | trace\n");
    END;      
    RETURN TRUE;
  END Run;

BEGIN
END Heap. 
