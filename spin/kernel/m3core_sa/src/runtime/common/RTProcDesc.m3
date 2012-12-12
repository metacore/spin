(*
 * Procedure information
 *)

UNSAFE MODULE RTProcDesc EXPORTS RTProcDesc, RTProcDescF;

IMPORT RTIO, RTOS, RTModule, RT0, Word, ProcDescUtbl;

REVEAL
  ProcType = UNTRACED BRANDED REF INTEGER;
  (* FIXME: describe the encoding *)

VAR
  ProcTable:  ProcDescUtbl.T;
  (* a hash table from procedure to procedure descriptors *)

CONST
  doChecks = TRUE;
  (* Should the initialization double-check procedure types *)

VAR
  nInitialized : INTEGER := 0;
  (* Number of modules with parsed procedure information *)

(*
 * Initialize information about all procedures in the system.
 *)
(* FIXME: put type information together with proc information *)
(* FIXME: do this on demand? *)
PROCEDURE InitializeProcTable() =
  VAR
    procDesc: T;
    procPtr: RT0.ProcPtr;
    proc_types: ProcType;
    p: PROCANY;
    module: RT0.ModulePtr;
    num_types: INTEGER;
  BEGIN
    (* scan module descriptors for exported procedures *)
    FOR i := nInitialized TO RTModule.Count() - 1 DO
      module := RTModule.Get(i);
      procPtr := module.proc_info;
      proc_types := LOOPHOLE(module.proc_types, ProcType);
      IF procPtr # NIL THEN
        WHILE (procPtr.proc # NIL) DO
          p := procPtr.proc;

          IF doChecks THEN
            IF NOT CheckProcType(p, proc_types) THEN
              RTOS.Crash();
            END;
          END;

          (* create a procedure descriptor *)
          procDesc := NEW (T);
          procDesc.proc := p;
          procDesc.jtblPtr := procPtr.export;
          procDesc.type := proc_types;

          IF ProcTable.put(p, procDesc) THEN
            RTIO.PutText("ERROR >> RTProcedure: duplicate procedure\n");
            RTOS.Crash();
          END;
          
          (* next type pointer *)
          num_types := 2 * GetNextVal(proc_types);
          IF Word.And(GetNextVal(proc_types), RT0.ProcHasResult) # 0 THEN
            INC(num_types);
          END;
          INC (proc_types, (num_types) * ADRSIZE (INTEGER));

          (* next procedure *)
          INC (procPtr, ADRSIZE (procPtr^));
        END;
      END;
    END;
    nInitialized := RTModule.Count();
  END InitializeProcTable;

(*
 * Get next value in the type descriptor.
 *)
PROCEDURE GetNextVal (VAR type: ProcType): INTEGER =
  VAR 
    val: INTEGER;
  BEGIN
    val := type^;
    INC (type, ADRSIZE (INTEGER));
    RETURN val;
  END GetNextVal;

(*
 * Verify that the procedure type is legal
 *)
PROCEDURE CheckProcType(p: PROCANY; type: ProcType): BOOLEAN =
  VAR
    nArgs, nRes: INTEGER;
    ptr := type;
  BEGIN
    nArgs := ptr^;
    INC (ptr, ADRSIZE (INTEGER));
    nRes := Word.And (ptr^, RT0.ProcHasResult);
    INC (ptr, ADRSIZE (INTEGER));

    FOR i := 0 TO nArgs - 1 DO
      INC (ptr, ADRSIZE (INTEGER));
      IF ptr^ # RT0.FormalModeVALUE AND
         ptr^ # RT0.FormalModeVAR   AND
         ptr^ # RT0.FormalModeCONST 
       THEN
        RTIO.PutText ("ERROR >> RTProcDesc: bad type representation for procedure: ");
        RTIO.PutAddr(p); RTIO.PutText("\n");
        PrintProcType(type);
        RETURN FALSE;
      END;
      INC (ptr, ADRSIZE (INTEGER));
    END;

    RETURN TRUE;
  END CheckProcType; 

(*
 * Print procedure type
 *)
(* FIXME: print type names *)
PROCEDURE PrintProcType(type: ProcType; p: RTIO.SimplePutter := NIL) =
  VAR
    nArgs, nRes: INTEGER;
  BEGIN
    IF p = NIL THEN
      p := simplePutter;
    END;

    p.putAddr(type); p.putText(" = ");

    nArgs := type^;
    INC (type, ADRSIZE (INTEGER));

    nRes := Word.And (type^, RT0.ProcHasResult);
    IF Word.And (type^, RT0.ProcIsFUNCTIONAL) # 0 THEN
      p.putText(" FUNCTIONAL ");
    END;
    IF Word.And (type^, RT0.ProcIsEPHEMERAL) # 0 THEN
      p.putText(" EPHEMERAL ");
    END;
    INC (type, ADRSIZE (INTEGER));

    p.putText("PROCEDURE (");
    FOR i := 0 TO nArgs - 1 DO
      IF i # 0 THEN p.putText("; "); END;
      p.putHex(type^); 
      INC (type, ADRSIZE (INTEGER));
      CASE type^ OF
      | RT0.FormalModeVALUE => p.putText(" VALUE ");
      | RT0.FormalModeVAR   => p.putText(" VAR ");
      | RT0.FormalModeCONST => p.putText(" CONST ");
      ELSE
        p.putText (" <UNKNOWN-MODE ");
        p.putHex(type^);
        p.putText(" >");
      END;
      INC (type, ADRSIZE (INTEGER));
    END;
    p.putText(")");
    IF nRes = 1 THEN
      p.putText(": "); p.putHex(type^); 
    END;
  END PrintProcType; 

(*
 * Return the procedure descriptor
 *)
PROCEDURE GetDesc (p: PROCANY): T =
  VAR
    procDesc: T;
  BEGIN
    RTOS.LockHeap();
    IF NOT initialized THEN
      Init();
    END;
    IF NOT ProcTable.get(p, procDesc) THEN
      RTIO.PutText("ERROR >> RTProcDesc: could not find a procedure: ");
      RTIO.PutAddr(p); RTIO.PutText("\n"); 
      RETURN NIL;
    END;
    RTOS.UnlockHeap();
    RETURN procDesc;
  END GetDesc;

PROCEDURE GetType (p: PROCANY): ProcType =
  BEGIN
    RETURN GetDesc(p).type;
  END GetType;

PROCEDURE NumberOfArgs (type: ProcType): INTEGER =
  BEGIN
    RETURN type^;
  END NumberOfArgs;

PROCEDURE HasResult (type: ProcType): BOOLEAN =
  BEGIN
    INC (type, ADRSIZE (INTEGER));
    RETURN Word.And (type^, RT0.ProcHasResult) # 0;
  END HasResult;

PROCEDURE ResultType (type: ProcType): INTEGER =
  BEGIN
    IF HasResult(type) THEN
      INC(type, (NumberOfArgs(type) + 1) * 2 * ADRSIZE(INTEGER));
      RETURN type^;
    ELSE
      RETURN 0;
    END;
  END ResultType;

PROCEDURE ArgType (type: ProcType; n: INTEGER): INTEGER =
  BEGIN
    IF n <= 0 OR n > NumberOfArgs(type) THEN
      RETURN 0;
    END;
    INC(type, (2 + (n-1) * 2) * ADRSIZE(INTEGER));
    RETURN type^;
  END ArgType;

PROCEDURE ArgMode (type: ProcType; n: INTEGER): INTEGER =
  BEGIN
    IF n <= 0 OR n > NumberOfArgs(type) THEN
      RETURN 0;
    END;
    INC(type, (2 + (n-1) * 2 + 1) * ADRSIZE(INTEGER));
    RETURN type^;
  END ArgMode;

PROCEDURE IsFunctional (type: ProcType): BOOLEAN =
  BEGIN
    INC(type, ADRSIZE(INTEGER));
    RETURN Word.And(type^, RT0.ProcIsFUNCTIONAL) # 0;
  END IsFunctional;

PROCEDURE IsEphemeral (type: ProcType): BOOLEAN =
  BEGIN
    INC(type, ADRSIZE(INTEGER));
    RETURN Word.And(type^, RT0.ProcIsEPHEMERAL) # 0;
  END IsEphemeral;

(* FIXME : a preliminary hack to make Marc happy, what needs to be done? *)
(* legalization is idempotent and can be done multiple times *)
PROCEDURE Legalize (p: ADDRESS) =
  VAR
    procDesc: T;
  BEGIN
    RTOS.LockHeap();
    IF NOT initialized THEN
      Init();
    END;
    procDesc := NEW(T);
    procDesc.jtblPtr := NIL;
    procDesc.type := NIL;
    EVAL ProcTable.put(p, procDesc);
    RTOS.UnlockHeap();
  END Legalize;

(*
 * Destroy a procedure descriptor
 *)
(* FIXME: need to clean the record for domain destruction *)
<* UNUSED *>
PROCEDURE Destroy (p: PROCANY): BOOLEAN =
  VAR
    procDesc: T;
  BEGIN
    IF NOT ProcTable.get(p, procDesc) THEN
      RTIO.PutText("ERROR >> RTProcDesc: could not find a procedure: ");
      RTIO.PutAddr(p); RTIO.PutText("\n"); 
      RETURN FALSE;
    END;
    procDesc.proc := NIL;
    procDesc.jtblPtr := NIL;
    procDesc.type := NIL;
    IF NOT ProcTable.delete(p, procDesc) THEN
      RTIO.PutText("ERROR >> RTProcDesc: could not remove a procedure\n");
      RETURN FALSE;
    END;
    RETURN FALSE;
  END Destroy;

VAR
  initialized: BOOLEAN := FALSE;
          
VAR
  simplePutter : RTIO.SimplePutter := NIL;
  (* default printing *)

PROCEDURE Reset () =
  BEGIN
    initialized := FALSE;
  END Reset;

PROCEDURE Init () =
  BEGIN
    IF simplePutter = NIL THEN
      simplePutter := NEW(RTIO.SimplePutter);
    END;
    IF ProcTable = NIL THEN
      ProcTable := NEW(ProcDescUtbl.Default).init(2000);
    END;
    InitializeProcTable();
    initialized := TRUE;
  END Init;

BEGIN
END RTProcDesc.

