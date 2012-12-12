(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Sat Nov 19 09:26:45 PST 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:22:58 PDT 1993 by muller     *)

(* The linker generates an inital direct call to this module's
   main body.  All Modula-3 code reached from here. *)

(*
 * HISTORY
 * 11-Feb-97  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned-up a bit. Added subtruction of modules.
 *
 * 03-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added reinitailiaztion of RTutils.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Better clean-up of the runtime state.
 *
 * 27-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Only the initial type setup failure causes the system to crash. 
 *	Reinitialization of types does not crash now if inconsistency
 *	is detected.  
 *
 * 17-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 14-Jul-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for initialization of dynamicly linked modules
 *
 *)

UNSAFE MODULE RTLinker;

IMPORT RT0, RT0u, RTParams, RTProcess, RTHeapRep, RTHeapMap, RTProcedureSRC;
IMPORT RTTypeSRC, RTSignal, RTHooks, RTThreadInit, RTMisc, RTIO, Cstdlib;
IMPORT RTCollector, RTException, Spy, SpinStart, Text, RTutils, RTProcDescF; 

VAR init_done := FALSE;

PROCEDURE ExportProcs (first: INTEGER; last: INTEGER): BOOLEAN =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
    p: RT0.ProcPtr;
    v: UNTRACED REF ADDRESS;
    j, k: INTEGER;
  BEGIN
    (* initialize the interface records with the exported procedures *)
    j := 0;
    k := 0;
    INC (m, first * ADRSIZE (m^));
    FOR i := first TO last DO
      n := m^;
      IF (n # NIL) AND (n.proc_info # NIL) THEN
        p := n.proc_info;
        INC(j);
        WHILE (p.proc # NIL) DO
          INC(k);
          v := p.export;
          IF (v # NIL) THEN v^ := p.proc; END;
          INC (p, ADRSIZE (p^));
        END;
      END;
      INC (m, ADRSIZE (m^));
    END;

    (* finally, patch up any "static" procedure constants *)
    m := info.modules + first * ADRSIZE (m^);
    FOR i := first TO last DO
      n := m^;
      IF (n = NIL) THEN
        RTIO.PutText("ERROR >> empty slot in module table ");
        RTIO.PutInt(i);
        RTIO.PutText("\n");
        RETURN FALSE;
      END;
      IF (n.link # NIL) THEN n.link () END;
      INC (m, ADRSIZE (m^));
    END;

    j := 0;
    k := 0;
    m := info.modules + first * ADRSIZE (m^);
    FOR i := first TO last DO
      n := m^;
      IF (n # NIL) AND (n.proc_info # NIL) THEN
        INC(j);
        p := n.proc_info;
        WHILE (p.proc # NIL) DO
          INC(k);
          INC (p, ADRSIZE (p^));
        END;
      END;
      INC (m, ADRSIZE (m^));
    END;
    RETURN TRUE;
  END ExportProcs;

PROCEDURE RunMainBodies (first: INTEGER; last: INTEGER) =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
  BEGIN
    INC(m, first * ADRSIZE (m^));
    FOR i := first TO last DO
      n := m^;
      IF (n # NIL) AND (n.main # NIL) THEN n.main () END;
      INC (m, ADRSIZE (m^));
    END;
  END RunMainBodies;

<* UNUSED *>
PROCEDURE Count (first: INTEGER; last: INTEGER) =
  VAR
    n: RT0.ModulePtr;
    m: UNTRACED REF RT0.ModulePtr := info.modules;
    nilCnt, intCnt, modCnt: INTEGER;
  BEGIN
    nilCnt := 0;
    intCnt := 0;
    modCnt := 0;
    INC(m, first * ADRSIZE (m^));
    FOR i := first TO last DO
      n := m^;
      IF n = NIL THEN 
        INC(nilCnt)
      ELSIF IsInterface(n.file) THEN 
        INC(intCnt);
      ELSE
        INC(modCnt);
      END;
      INC (m, ADRSIZE (m^));
    END;
    RTIO.PutText("\n$$$ ");
    IF nilCnt # 0 THEN RTIO.PutText("NIL: "); RTIO.PutInt(nilCnt); END;
    RTIO.PutInt(intCnt); RTIO.PutText(" "); RTIO.PutInt(modCnt);
    RTIO.PutText("\n");
    RTIO.Flush();
  END Count; 

PROCEDURE IsInterface (name: UNTRACED REF CHAR): BOOLEAN =
  BEGIN
    WHILE name^ # '\000' DO
      INC(name, ADRSIZE (name^));
    END;
    WHILE name^ # '.' DO
      DEC(name, ADRSIZE (name^));
    END;
    INC(name, ADRSIZE (name^));
    RETURN name^ = 'i';
  END IsInterface;

PROCEDURE Reinitialize (modules: UNTRACED REF ARRAY OF RT0.ModulePtr):BOOLEAN =
  VAR
    n_modules: INTEGER := NUMBER(modules^);
    first: INTEGER := n_initialized;
    last: INTEGER := n_modules -1;
  BEGIN
    info.modules := ADR(modules[0]);
    type_info.modules_array := modules;
    info.n_modules := n_modules;
    IF NOT ExportProcs (first, last) THEN
      RETURN FALSE;
    END;
    IF NOT RTTypeSRC.Init () THEN
      RETURN FALSE;
    END;

    SetStuffToFree();

    RETURN TRUE;
  END Reinitialize;

(* return a new list of all modules in the system *)
PROCEDURE SubtractModules (modules: ModulePtrs) : ModulePtrs =
  VAR
    totModules      : ModulePtrs;
    found           : BOOLEAN;
    cnt             : INTEGER;
    module          : RT0.ModulePtr;
    oldTotalModules := RT0u.nModules; (* old total count *)
    newTotalModules := oldTotalModules - NUMBER(modules^);
    nModules        := NUMBER(modules^);
  BEGIN
    (* create new total list of modules *)
    totModules := NEW(ModulePtrs, newTotalModules);
    
    (* copy pointers to old modules skipping the ones being removed *)
    cnt := 0;
    FOR i := 0 TO oldTotalModules - 1 DO
      module := LOOPHOLE(RT0u.modules + i * ADRSIZE(ADDRESS),
                         UNTRACED REF RT0.ModulePtr)^;
      found := FALSE;
      FOR i := 0 TO nModules-1 DO
        IF module = modules[i] THEN
          found := TRUE;
          EXIT;
        END;
      END;
      IF NOT found THEN
        totModules[cnt] := module;
        INC(cnt);
      END;
    END;
    <* ASSERT cnt = newTotalModules *>
    RETURN totModules;
  END SubtractModules; 

PROCEDURE Destroy (newModules: ModulePtrs): BOOLEAN=
  VAR
    n_modules: INTEGER := NUMBER(newModules^);
  BEGIN
    <* ASSERT RT0u.static_reinitialized *>
    info.modules := ADR(newModules[0]);
    type_info.modules_array := newModules;
    info.n_modules := n_modules;

    IF NOT RTTypeSRC.Init () THEN
      RETURN FALSE;
    END;

    SetStuffToFree();

    n_initialized := n_modules;
    RETURN TRUE;
  END Destroy;

PROCEDURE SetStuffToFree () =
  BEGIN
    (* the original list of modules cannot be deallocated because it *)
    (* is created by the compiler, other arrays are allocated by RTType *)
    IF RT0u.static_reinitialized THEN
      RT0u.free_nModules    := RT0u.nModules;
      RT0u.free_modules     := RT0u.modules_array;
    ELSE
      RT0u.static_reinitialized := TRUE;
    END;

    RT0u.free_nTypes          := RT0u.nTypes;
    RT0u.free_types           := RT0u.types;

    (* FIXME: could reallocate
    RT0u.free_alloc_cnts      := RT0u.alloc_cnts;
    RT0u.free_alloc_bytes     := RT0u.alloc_bytes;
    *)

    (* subTypeCode and lastSubTypeCode are used only for reinitialization
       so there is not need to keep them in RT0u except for freeing *)
    RT0u.free_subTypeCode     := type_info.subTypeCode;
    RT0u.free_lastSubTypeCode := type_info.lastSubTypeCode;

    RT0u.free_n_type_ids      := RTTypeSRC.n_type_ids;
    RT0u.free_type_ids        := RTTypeSRC.type_ids;
  END SetStuffToFree;

PROCEDURE Update () =
  BEGIN
    RT0u.modules := info.modules;
    RT0u.modules_array := type_info.modules_array;
    RT0u.nModules := info.n_modules;
    RT0u.types := type_info.types;
    RT0u.nTypes := type_info.nTypes;

    (* FIXME: could reallocate
    RT0u.alloc_cnts := type_info.alloc_cnts;
    RT0u.alloc_bytes := type_info.alloc_bytes;
    *)

    RTTypeSRC.Update ();
    RTHeapMap.Reset ();
    RTProcedureSRC.Reset ();
    RTProcDescF.Reset ();
    RTutils.Reset ();
  END Update;

PROCEDURE CleanUp () =
  BEGIN
    IF RT0u.free_modules # NIL THEN
      RTMisc.Zero(ADR(RT0u.free_modules[0]),
                  RT0u.free_nModules * ADRSIZE(RT0.ModulePtr));
      Cstdlib.free(RT0u.free_modules);
    END;
    RTMisc.Zero(RT0u.free_types, 
                RT0u.free_nTypes * BYTESIZE(RT0.TypeDefn));
    Cstdlib.free(RT0u.free_types);

    (* FIXME: could reallocate
    RTMisc.Zero(RT0u.free_alloc_cnts, 
                RT0u.free_nTypes * BYTESIZE (INTEGER));
    Cstdlib.free(RT0u.free_alloc_cnts);
    RTMisc.Zero(RT0u.free_alloc_bytes, 
                RT0u.free_nTypes * BYTESIZE (INTEGER));
    Cstdlib.free(RT0u.free_alloc_bytes);
    *)

    RTMisc.Zero(RT0u.free_subTypeCode, 
                RT0u.free_nTypes * BYTESIZE (INTEGER));
    Cstdlib.free(RT0u.free_subTypeCode);
    RTMisc.Zero(RT0u.free_lastSubTypeCode, 
                RT0u.free_nTypes * BYTESIZE (INTEGER));
    Cstdlib.free(RT0u.free_lastSubTypeCode);
    RTMisc.Zero(RT0u.free_type_ids, 
                RT0u.free_n_type_ids * BYTESIZE(RTTypeSRC.IDMap));
    Cstdlib.free(RT0u.free_type_ids);
  END CleanUp;

PROCEDURE ExecuteMainBodies () =
  VAR
    first: INTEGER := n_initialized;
  BEGIN
    n_initialized := info.n_modules;
    (* Count(first, n_initialized - 1);*)
    RTutils.Init ();
    RunMainBodies (first, n_initialized - 1);
  END ExecuteMainBodies;

(*
 * Initialization of Modula-3 runtime and SPIN Modula-3 code.
 * Because we may be using reference counting or a read or write barrier,
 * there may not be any traced reference assignement until the traced
 * heap is initialized.
 *)
BEGIN
  IF NOT init_done THEN
    init_done := TRUE;

    RTHooks.bottom_of_stack := info.bottom_of_stack;
    RTHooks.top_of_stack    := info.top_of_stack;

    (* run each of the init procs to initialize the interface records *)
    n_initialized := 0;

    EVAL ExportProcs (0, info.n_modules - 1);

    (* fix the runtime type structures *)
    IF NOT RTTypeSRC.Init () THEN
      RTIO.PutText (
          "HINT >> this error is most likely caused by inconsisntent build\n");
      RTIO.PutText (
          "HINT >> rebuild spin/kernel/start to update unit information\n\n");
      RTMisc.FatalError (NIL, 0, "could not initialize types at boot time");
    END;

    (* spread the linker variables around where they're needed *)
    Update();

    (* initialize the runtime *)
    RTSignal.InstallHandlers ();
    RTParams.Init ();
    RTHeapRep.Init ();
    RTThreadInit.Init ();
    (* RTHeapInfo.Init (); *)
    RTutils.Init ();

    n_initialized := info.n_modules;

    (* SPIN needs the runtime and core spin services available before
       running random mainbodies.  Normal M3 does have a few active
       mainbodies in the runtime.  We change them to Init routines and
       call them here.  Then we call the SPIN Init routine. *)

    Text.Init();
    Spy.Init();
    RTCollector.RTInit();
    RTException.Init();
    SpinStart.Init ();

    (* SPIN - now the runtime and spin core are fully initialized
       so random main bodies can now run *)

    (* run the module main bodies *)
    RunMainBodies (0, info.n_modules - 1);

    (* and be done *)
    RTProcess.Exit (0);
  END;
END RTLinker.
