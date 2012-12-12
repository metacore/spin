(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created.
 *
 *)

UNSAFE MODULE MachineDebugger;
IMPORT Strand, StrandRep, CPU, Word, DebugOption, IO, Fmt;

PROCEDURE UserModePC (pc: Word.T): BOOLEAN =
  BEGIN
    RETURN Word.Extract(pc, 28, 4) # 16_f;
  END UserModePC;

PROCEDURE CopySavedUserRegs (    s    : Strand.T;
                             VAR state: CPU.MachineState) =
  BEGIN
    IF s.trapFrame = LOOPHOLE(-1, UNTRACED REF CPU.SavedState) OR 
       s.trapFrame = NIL THEN
      (* Clear out the register frame. *)
      FOR i := 0 TO BYTESIZE(CPU.MachineState)-1 DO
	VIEW(state, ARRAY OF CHAR)[i] := '\000';
      END;
      RETURN;
    END;
    
    WITH tf = s.trapFrame^ DO
      state.eax      := LOOPHOLE(ADR(tf.eax), Word.T);
      state.ecx      := LOOPHOLE(ADR(tf.ecx), Word.T);
      state.edx      := LOOPHOLE(ADR(tf.edx), Word.T);
      state.ebx      := LOOPHOLE(ADR(tf.ebx), Word.T);
      state.ebp      := LOOPHOLE(ADR(tf.ebp), Word.T);
      state.esi      := LOOPHOLE(ADR(tf.edi), Word.T);
      state.eip      := LOOPHOLE(ADR(tf.pc), Word.T);
      state.efl      := LOOPHOLE(ADR(tf.eflags), Word.T);
      state.cs       := LOOPHOLE(ADR(tf.cs), Word.T);
      state.ds       := LOOPHOLE(ADR(tf.ds), Word.T);
      state.es       := LOOPHOLE(ADR(tf.es), Word.T);
      state.fs       := LOOPHOLE(ADR(tf.cs), Word.T);
      state.gs       := LOOPHOLE(ADR(tf.ds), Word.T);

      IF UserModePC(tf.pc) THEN
        IF DebugOption.DebuggerDebug THEN
          IO.Put("Getting user mode sp\n");
        END;
        state.esp := LOOPHOLE(ADR(tf.usp), Word.T);
        state.ss := LOOPHOLE(ADR(tf.uss), Word.T);
      ELSE
        IF DebugOption.DebuggerDebug THEN
          IO.Put("Getting kernel mode sp\n");
        END;
        state.esp := LOOPHOLE(ADR(tf.ksp), Word.T);
        state.ss  := LOOPHOLE(ADR(tf.ds), Word.T);
      END;

      IF DebugOption.DebuggerDebug THEN
        IO.Put("frame pc address is " & Fmt.Unsigned(state.eip) & "\n");
        IO.Put("frame pc should be " & Fmt.Unsigned(tf.pc) & "\n");
        IO.Put("frame pc is "
        & Fmt.Unsigned(LOOPHOLE(state.eip, UNTRACED REF Word.T)^) & "\n");
        IO.Put("frame ksp is "&Fmt.Unsigned(tf.ksp) & "\n");
        IO.Put("frame usp is "&Fmt.Unsigned(tf.usp) & "\n");
        IO.Put("frame sp address is "&Fmt.Unsigned(state.esp) & "\n");
        IO.Put("frame sp is "
        & Fmt.Unsigned(LOOPHOLE(state.esp, UNTRACED REF Word.T)^) & "\n");
        (*IO.Put("frame fp is " & Fmt.Unsigned(tf.s6) & "\n");
        IO.Put("frame pv is " & Fmt.Unsigned(tf.pv) & "\n");*)
      END;
    END;
  END CopySavedUserRegs;

BEGIN
END MachineDebugger.
