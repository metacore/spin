UNSAFE MODULE MachineDebugger;
IMPORT Strand, StrandRep, CPU, Word, DebugOption, IO, Fmt;

PROCEDURE UserModePS (ps: Word.T): BOOLEAN =
  BEGIN
    RETURN VAL(Word.Extract(ps, 3, 1), BOOLEAN);
  END UserModePS;

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
      state.v0 := LOOPHOLE(ADR(tf.v0), Word.T);
      state.s0 := LOOPHOLE(ADR(tf.s0), Word.T);
      state.s1 := LOOPHOLE(ADR(tf.s1), Word.T);
      state.s2 := LOOPHOLE(ADR(tf.s2), Word.T);
      state.s3 := LOOPHOLE(ADR(tf.s3), Word.T);
      state.s4 := LOOPHOLE(ADR(tf.s4), Word.T);
      state.s5 := LOOPHOLE(ADR(tf.s5), Word.T);
      state.fp := LOOPHOLE(ADR(tf.s6), Word.T);
      state.t0 := LOOPHOLE(ADR(tf.t0), Word.T);
      state.t1 := LOOPHOLE(ADR(tf.t1), Word.T);
      state.t2 := LOOPHOLE(ADR(tf.t2), Word.T);
      state.t3 := LOOPHOLE(ADR(tf.t3), Word.T);
      state.t4 := LOOPHOLE(ADR(tf.t4), Word.T);
      state.t5 := LOOPHOLE(ADR(tf.t5), Word.T);
      state.t6 := LOOPHOLE(ADR(tf.t6), Word.T);
      state.t7 := LOOPHOLE(ADR(tf.t7), Word.T);
      state.t8 := LOOPHOLE(ADR(tf.t8), Word.T);
      state.t9 := LOOPHOLE(ADR(tf.t9), Word.T);
      state.t10 := LOOPHOLE(ADR(tf.t10), Word.T);
      state.t11 := LOOPHOLE(ADR(tf.t11), Word.T);
      state.t12 := LOOPHOLE(ADR(tf.pv), Word.T);
      state.a0 := LOOPHOLE(ADR(tf.a0), Word.T);
      state.a1 := LOOPHOLE(ADR(tf.a1), Word.T);
      state.a2 := LOOPHOLE(ADR(tf.a2), Word.T);
      state.a3 := LOOPHOLE(ADR(tf.a3), Word.T);
      state.a4 := LOOPHOLE(ADR(tf.a4), Word.T);
      state.a5 := LOOPHOLE(ADR(tf.a5), Word.T);
      state.ra := LOOPHOLE(ADR(tf.ra), Word.T);
      state.at := LOOPHOLE(ADR(tf.at), Word.T);
      state.gp := LOOPHOLE(ADR(tf.gp), Word.T);
      state.pc := LOOPHOLE(ADR(tf.pc), Word.T);

      IF UserModePS(tf.ps) THEN
        IF DebugOption.DebuggerDebug THEN
          IO.Put("Getting user mode sp\n");
        END;
        state.sp := LOOPHOLE(ADR(tf.usp), Word.T);
      ELSE
        IF DebugOption.DebuggerDebug THEN
          IO.Put("Getting kernel mode sp\n");
        END;
        state.sp := LOOPHOLE(ADR(tf.ksp), Word.T);
      END;

      IF DebugOption.DebuggerDebug THEN
        IO.Put("frame pc address is " & Fmt.Unsigned(state.pc) & "\n");
        IO.Put("frame pc should be " & Fmt.Unsigned(tf.pc) & "\n");
        IO.Put("frame pc is "
        & Fmt.Unsigned(LOOPHOLE(state.pc, UNTRACED REF Word.T)^) & "\n");
        IO.Put("frame ksp is "&Fmt.Unsigned(tf.ksp) & "\n");
        IO.Put("frame usp is "&Fmt.Unsigned(tf.usp) & "\n");
        IO.Put("frame sp address is "&Fmt.Unsigned(state.sp) & "\n");
        IO.Put("frame sp is "
        & Fmt.Unsigned(LOOPHOLE(state.sp, UNTRACED REF Word.T)^) & "\n");
        (*IO.Put("frame fp is " & Fmt.Unsigned(tf.s6) & "\n");
        IO.Put("frame pv is " & Fmt.Unsigned(tf.pv) & "\n");*)
      END;
    END;
  END CopySavedUserRegs;

BEGIN
END MachineDebugger.
