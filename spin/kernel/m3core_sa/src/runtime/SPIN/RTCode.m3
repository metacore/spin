(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 22-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	ModuleImplementsProcedure still treated its PROCANY argument
 *	as a REF to the procedure.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 10-Jan-96  Brian Bershad (bershad) at the University of Washington
 *      Added iteration support. Whisted.
 *
 * 10-26-95  Charlie Garrett
 *      Created.
 *
 *
 *      RTCode is the interface through which users access the
 *      runtime representation of interfaces and modules.
 *)



UNSAFE MODULE RTCode;

IMPORT RTCodeBuiltin;
IMPORT RT0;
IMPORT M3toC, Ctypes;

REVEAL RTCodeBuiltin.Module = UNTRACED BRANDED REF RT0.ModuleInfo;
REVEAL RTCodeBuiltin.Interface = UNTRACED BRANDED REF RT0.ModuleInfo;

PROCEDURE ModuleExportsInterface(m: Module; i: Interface): BOOLEAN =
BEGIN
    IF m = NIL OR i = NIL THEN RETURN FALSE; END;
    VAR
        definition := m.proc_info;
        lower_bound := LOOPHOLE(i, ADDRESS);
        upper_bound := LOOPHOLE(i.file, ADDRESS);
    BEGIN
        (* Check whether any of the procedures exported by m are
           part of interface i. If not, then return FALSE. *)

        WHILE (definition.proc # NIL) DO
            IF (definition.export > lower_bound AND 
		definition.export < upper_bound) THEN
	        RETURN TRUE;
	    END;
	    INC(definition, ADRSIZE(definition^));
        END;
        RETURN FALSE;
    END;
END ModuleExportsInterface;

PROCEDURE ModuleImplementsProcedure(m: Module; p: PROCANY): BOOLEAN =
BEGIN
    IF m = NIL OR p = NIL THEN RETURN FALSE; END;
    VAR
        definition := m.proc_info;
    BEGIN
      (* Compare each procedure exported by the module to the PROCANY
         argument. This code is similar to the RTLinker.m3 code which
         exports the procedures. *)
      WHILE (definition.proc # NIL) DO
        IF (definition.proc = p) THEN
          RETURN TRUE;
        END;
        INC(definition, ADRSIZE(definition^));
      END;
    END;
    RETURN FALSE;
END ModuleImplementsProcedure;


PROCEDURE IterateOnProcedures (m: RT0.ModulePtr; iter: Iterator) =
  VAR procPtr: RT0.ProcPtr;
  BEGIN
    procPtr := m.proc_info;
    IF procPtr # NIL THEN
      WHILE procPtr.proc # NIL DO
        IF iter.apply(
             procPtr.proc,
             M3toC.CopyStoT(LOOPHOLE(procPtr.name, Ctypes.char_star)))
             = FALSE THEN
          RETURN;
        END;
        INC(procPtr, ADRSIZE(procPtr^));
      END;
    END
  END IterateOnProcedures;


PROCEDURE IterateOnProceduresInInterface(i: Interface; iter: Iterator) =
  BEGIN
    IterateOnProcedures(LOOPHOLE(i, RT0.ModulePtr), iter);
  END IterateOnProceduresInInterface;

    
    
PROCEDURE IterateOnProceduresInModule(m: Module; iter: Iterator) =
  BEGIN
    IterateOnProcedures(LOOPHOLE(m, RT0.ModulePtr), iter);
  END IterateOnProceduresInModule;





BEGIN
END RTCode.
