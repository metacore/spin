(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Fri May  6 09:30:40 PDT 1994 by kalsow     *)
(*      modified on Wed Jun  2 15:34:43 PDT 1993 by muller     *)

(*
 * HISTORY
 * 17-Jun-97  Przemek Pardyak (pardy) at the University of Washington
 *	Unified SPIN M3 runtime: Added GetByName(), GetByPC(), CleanUp()
 *	and IsNested(),	added reinitialization, NIL-out the global map
 *	to make sure that no lingering pointers remain.
 *)

(* FIXME: the data structures need simplification and integration with 
          the dispatcher procedure data. *)

UNSAFE MODULE RTProcedure EXPORTS RTProcedure, RTProcedureSRC;

IMPORT RT0, RTMisc, RTModule, Fingerprint, Word, Text, M3toC, Ctypes, RTOS;

CONST
  Max_procedure_length = 20000; (* more than 99% of procedures are shorter
                                   than this. *)

(* NOTE: since this module may be invoked during a startup
   crash, we don't use a mutex to protect writes to the
   global variables.  Instead, we assume that assigning a
   REF is an atomic operation.  It's possible that a thread
   race duing Init() will cause some garbage memory to be
   permanently allocated.  *)

VAR
  nProcs : INTEGER  := 0;
  min_pc : ADDRESS  := NIL;
  max_pc : ADDRESS  := NIL;

TYPE
  ProcList = UNTRACED REF ARRAY OF ProcInfo;
  UnitList = UNTRACED REF ARRAY OF UnitInfo;
  
(* the following hash tables are indexed by procedure address *)
VAR
  procInfo : ProcList := NIL;  (* list of procedure descriptors *)
  unitInfo : UnitList := NIL;  (* list of module descriptors *)
  expoInfo : UnitList := NIL;  (* list of interface descriptors *)

(*----------------------------------------------------------- RTProcedure ---*)

PROCEDURE ToFingerprint (<*UNUSED*> p: Proc): Fingerprint.T =
  BEGIN
    RTMisc.FatalError (NIL, 0, "RTProcedure.ToFingerprint is not supported");
    RETURN Fingerprint.Zero;
  END ToFingerprint;

PROCEDURE FromFingerprint (<*UNUSED*> READONLY fp: Fingerprint.T): Proc =
  BEGIN
    RTMisc.FatalError (NIL, 0, "RTProcedure.FromFingerprint is not supported");
    RETURN NIL;
  END FromFingerprint;

PROCEDURE IsNested (p: ADDRESS): BOOLEAN =
  VAR closure: UNTRACED REF RT0.ProcedureClosure;
  BEGIN
    IF p = NIL THEN RETURN FALSE; END;

    closure := LOOPHOLE (p, UNTRACED REF RT0.ProcedureClosure);
    (* FIXME: this should actually check the type *)
    IF closure.marker = RT0.ClosureMarker THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END IsNested;

(*-------------------------------------------------------- RTProcedureSRC ---*)

PROCEDURE NumProcedures (): CARDINAL =
  BEGIN
    IF (nProcs = 0) THEN Init() END;
    RETURN nProcs;
  END NumProcedures;

PROCEDURE FromPC (pc: ADDRESS;  VAR p: Proc;  VAR end: ADDRESS;
                  VAR proc: ProcInfo; VAR unit: UnitInfo; VAR expo: UnitInfo;
                  VAR file, name: Name) =
  VAR best, best_diff, diff, best_end_diff: INTEGER;
  BEGIN
    IF (nProcs = 0) THEN Init () END;
    p    := NIL;
    proc := NIL;
    unit := NIL;
    expo := NIL;
    end  := NIL;

    IF (pc < min_pc) OR (max_pc <= pc) THEN 
      (* don't bother *)
      p := NIL;
      RETURN;
    END;

    best := Locate (pc);  (* try the hash table for an exact match *)

    IF (* FIXME *) TRUE OR (best < 0) THEN    (* resort to linear search *)
      best_diff := LAST (INTEGER);
      best_end_diff := FIRST (INTEGER);
      FOR i := 0 TO LAST (procInfo^) DO
        proc := procInfo[i];
        IF (proc # NIL) THEN
          diff := (pc - proc.proc);
          IF (0 <= diff) AND (diff < best_diff) THEN
            best := i;
            best_diff := diff;
          END;
          IF (diff < 0) AND (diff > best_end_diff) THEN
            end := proc.proc;
            best_end_diff := diff;
          END;
        END;
      END;
    END;

    IF (best >= 0) THEN
      proc := procInfo[best];
      unit := unitInfo[best];
      expo := expoInfo[best];
      IF expo = NIL AND proc.export # NIL THEN
        (* the procedure is exported but interface have not been found yet *)
        expo := RTModule.InfoFromAddress(proc.export);
        expoInfo[best] := expo;
      END;
      p    := proc.proc;
      name := proc.name;
      IF unit # NIL THEN
        file := unit.file;
      END;
      end := pc - best_end_diff - 1;
    END;
  END FromPC;

PROCEDURE GetByPC (pc: ADDRESS): REF ProcDesc =
  VAR
    result: REF ProcDesc := NIL;
    p: Proc;
    proc: ProcInfo;
    unit: UnitInfo;
    expo: UnitInfo;
    file, name: Name;
    end: ADDRESS;
  BEGIN
    FromPC(pc, p, end, proc, unit, expo, file, name);
    IF p # NIL THEN
      result := NEW (REF ProcDesc);
      result.proc := p;
      result.end  := end;
      result.name := M3toC.CopyStoT(LOOPHOLE(name, Ctypes.char_star ));
      result.file := GetUnit(file);
      result.info := proc;
      result.unit := unit;
      result.expo := expo;
    END;
    RETURN result;
  END GetByPC;

(* FIXME: this belongs elsewhere *)
PROCEDURE GetUnit (file: Name): TEXT =
  VAR
    unit: TEXT;
    found := FALSE;
    n := -1;
  BEGIN 
    unit := M3toC.StoT(LOOPHOLE(file, Ctypes.char_star));

    <* ASSERT unit # NIL *> 

    (* find and remove the file extension *)
    n := Text.Length(unit) - 3;
    IF Text.GetChar(unit, n) = '.' AND
      Text.GetChar(unit, n+2) = '3' AND
      (Text.GetChar(unit, n+1) = 'i' OR Text.GetChar(unit, n+1) = 'm') 
     THEN
      found := TRUE;
      unit := Text.Sub(unit, 0, n);
    END;

    (* find the last slash and remove everything until it *)
    (* FIXME: won't work on Windows *)
    IF found THEN
      FOR i := Text.Length(unit) - 1 TO 0 BY -1 DO
        IF Text.GetChar(unit, i) = '/' THEN
          unit := Text.Sub(unit, i+1, Text.Length(unit) - i - 1);
          EXIT;
        END;
      END;
    END;

    RETURN unit;
  END GetUnit;
  
PROCEDURE SetDesc(list: REF ARRAY OF ProcDesc := NIL; i, j: INTEGER) =
  BEGIN
    list[i].name := M3toC.CopyStoT(LOOPHOLE(procInfo[j].name,
                                            Ctypes.char_star));
    list[i].file := GetUnit(unitInfo[j].file);
    list[i].proc := procInfo[j].proc;
    list[i].info := procInfo[j];
    list[i].unit := unitInfo[j];
    list[i].expo := expoInfo[j];
  END SetDesc;

PROCEDURE GetByName (unit: TEXT := NIL; 
                     name: TEXT := NIL): REF ARRAY OF ProcDesc =
  VAR
    list     : REF ARRAY OF ProcDesc := NIL;
    new_list : REF ARRAY OF ProcDesc := NIL;
    n        : INTEGER := 0;

  BEGIN
    IF (nProcs = 0) THEN Init () END;
    <* ASSERT procInfo # NIL *>
    <* ASSERT unitInfo # NIL *>
    <* ASSERT expoInfo # NIL *>

    IF unit = NIL AND name = NIL THEN
      list := NEW(REF ARRAY OF ProcDesc, nProcs);
      FOR i := FIRST(procInfo^) TO LAST(procInfo^) DO
        IF procInfo[i] # NIL AND procInfo[i].name # NIL AND unitInfo[i] # NIL
         THEN
          SetDesc(list, n, i);
          INC(n);
        END;
      END;
    ELSE
      FOR i := FIRST(procInfo^) TO LAST(procInfo^) DO
        IF procInfo[i] # NIL AND procInfo[i].name # NIL AND
          unitInfo[i] # NIL AND
          Text.Equal(M3toC.StoT(LOOPHOLE(procInfo[i].name, Ctypes.char_star )),
                     name) AND
          Text.Equal(unit, GetUnit(unitInfo[i].file))
         THEN
          INC(n);
          new_list := NEW(REF ARRAY OF ProcDesc, n);
          IF list # NIL THEN
            SUBARRAY(new_list^, 0, n-1) := SUBARRAY(list^, 0, n-1);
          END;
          list := new_list;
          SetDesc(list, n-1, i);
        END
      END;
    END;

    RETURN list;
  END GetByName; 

(*-------------------------------------------------------------- internal ---*)

PROCEDURE CountProcs () =
  VAR j, n: INTEGER;  p: RT0.ProcPtr;
  BEGIN
    n := 0;
    FOR i := 0 TO RTModule.Count() - 1 DO
      p := RTModule.Get (i).proc_info;
      IF (p # NIL) THEN
        j := 0;
        WHILE (p^.proc # NIL) DO INC (p, ADRSIZE (p^)); INC (j) END;
        INC (n, j);
      END;
    END;
    nProcs := n; (* ... we'll assume that this update is atomic ... *)
  END CountProcs;

(** CONST Multiplier = 1052824; **)
CONST Multiplier = 2 * 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19 * 23 + 1;
(* See Knuth Vol. 2, Theorem A, page 16. *)
PROCEDURE Insert (m: RT0.ModulePtr; xx: RT0.ProcPtr;
                  proc: ProcList;  unit: UnitList; expo: UnitList) =
  (* called while 'mu' is held *)
  VAR x: ProcInfo;  hash, index: INTEGER;
  BEGIN
    hash := LOOPHOLE (xx^.proc, INTEGER);
    LOOP
      index := Word.Mod (hash, NUMBER (proc^));
      x := proc [index];
      IF (x = NIL) THEN
        proc [index] := xx;
        unit [index] := m;
        expo [index] := NIL; (* do this on demand because it's expensive *)
        RETURN;
      END;
      IF (x.proc = xx.proc) THEN RETURN; END;
      hash := Word.Plus (1, Word.Times (hash, Multiplier));
    END;
  END Insert;

PROCEDURE Locate (proc: Proc): INTEGER =
  (* called while 'mu' is held *)
  VAR x: ProcInfo;  hash, index: INTEGER;
  BEGIN
    hash := LOOPHOLE (proc, INTEGER);
    LOOP
      index := Word.Mod (hash, NUMBER (procInfo^));
      x := procInfo [index];
      IF (x = NIL)       THEN RETURN -1 END;
      IF (x.proc = proc) THEN RETURN index   END;
      hash := Word.Plus (1, Word.Times (hash, Multiplier));
    END;
  END Locate; 

PROCEDURE GetExported (interface: RT0.ModulePtr): REF ARRAY OF ProcDesc =
  VAR
    list     : REF ARRAY OF ProcDesc := NIL;
    new_list : REF ARRAY OF ProcDesc := NIL;
    n        : INTEGER := 0;
  BEGIN
    IF (nProcs = 0) THEN Init () END;
    FOR i := FIRST(procInfo^) TO LAST (procInfo^) DO
      IF expoInfo[i] = NIL AND
        procInfo[i] # NIL AND procInfo[i].export # NIL
       THEN
        (* the procedure is exported but interface have not been found yet *)
        expoInfo[i] := RTModule.InfoFromAddress(procInfo[i].export);
      END;
      IF expoInfo[i] = interface THEN
        INC(n);
        new_list := NEW(REF ARRAY OF ProcDesc, n);
        IF list # NIL THEN
          SUBARRAY(new_list^, 0, n-1) := SUBARRAY(list^, 0, n-1);
        END;
        list := new_list;
        SetDesc(list, n-1, i);
      END;
    END;
    RETURN list;
  END GetExported;

(*------------------------------------------------------------------- ??? ---*)

(* FIXME
PROCEDURE PrintProc (proc: PROCANY; p: RTIO.SimplePutter := NIL) =
  VAR
    desc: REF RTProcedureSRC.ProcDesc;
  BEGIN
    IF proc = NIL THEN
      p.putText("NIL");
    ELSE
      p.putText("0x"); p.putAddr(proc); p.putText(" ");
      desc := RTProcedureSRC.GetByPC(proc);
      IF desc # NIL THEN 
        p.putString(desc.unit.file);
        p.putText("." & desc.name);
      ELSE
        p.putText("<UNKNOWN>");
      END;
    END;
  END PrintProc;
*)

(*-------------------------------------------------------- initialization ---*)

PROCEDURE Reset () =
  BEGIN
    (* force recomputation of the procedure table next time it is needed *)
    nProcs := 0;
  END Reset;

PROCEDURE CleanUp () =
  BEGIN
    nProcs := 0;
    IF procInfo # NIL THEN
      FOR i := FIRST(procInfo^) TO LAST(procInfo^) DO
        procInfo[i] := NIL;
        unitInfo[i] := NIL;
        expoInfo[i] := NIL;
      END;
      DISPOSE(procInfo);
      DISPOSE(unitInfo);
      DISPOSE(expoInfo);
    END;
  END CleanUp;

PROCEDURE Init () =
  VAR
    p: RT0.ProcPtr;
    m: RT0.ModulePtr;
    my_proc: ProcList;
    my_unit: UnitList;
    my_expo: UnitList;
    my_min_pc, my_max_pc: ADDRESS;
  BEGIN
    IF (procInfo # NIL) THEN
      CleanUp();
    END;

    CountProcs ();
    my_min_pc := LOOPHOLE (LAST (INTEGER), ADDRESS);
    my_max_pc := NIL;

    (* allocate arrays of Info pointers *)
    my_proc := NEW (ProcList, 3 * nProcs);
    my_unit := NEW (UnitList, 3 * nProcs);
    my_expo := NEW (UnitList, 3 * nProcs);

    (* for each procedure, insert its info entry into the array *)
    FOR i := 0 TO RTModule.Count () - 1 DO
      m := RTModule.Get (i);
      p := m.proc_info;
      IF (p # NIL) THEN
        WHILE (p.proc # NIL) DO
          Insert (m, p, my_proc, my_unit, my_expo);
          IF (p.proc < my_min_pc) THEN my_min_pc := p.proc; END;
          IF (p.proc > my_max_pc) THEN my_max_pc := p.proc; END;
          INC (p, ADRSIZE (p^));
        END;
      END;
    END;

    (* update the globals to record the new info *)
    RTOS.LockHeap();
    min_pc := my_min_pc;
    max_pc := my_max_pc + Max_procedure_length;
    procInfo := my_proc;
    unitInfo := my_unit;
    expoInfo := my_expo;

    RTOS.UnlockHeap();
  END Init;


BEGIN
END RTProcedure.

