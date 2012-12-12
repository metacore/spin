(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)



UNSAFE MODULE Fast;

IMPORT Thread, Wr, Stdio;

IMPORT Ir, Procedure, BasicBlock;
IMPORT Analysis;
IMPORT Analyzer, Live, Residue;

IMPORT Register;
IMPORT Traverse;
IMPORT JumpTable;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  Debug = FALSE;


PROCEDURE Reallocate (p: Procedure.T) RAISES {Analysis.Problem} =
  VAR
    (* Need to compute the number of variables used in the procedure. *)
    live : Live.T;

  PROCEDURE Fast (bb: BasicBlock.T; <* UNUSED *> r: REFANY)
    RAISES {Analysis.Problem} =

    PROCEDURE DoFast (VAR inst: Ir.Instruction; <* UNUSED *> u: REFANY)
      RAISES {Analysis.Problem} =
      VAR
        r: Residue.T;
      BEGIN
        IF inst.op = Ir.Opcode.jt THEN
          r := Traverse.GetResidueAfter (live, p, inst.index);

          (* allocate registers for jump table *)
          WITH jti = LOOPHOLE(inst, Ir.JtInstruction),
               jtc = JumpTable.Canonicalize (jti.table) DO
            VAR
              r0, r1, r2 := -1;
            BEGIN
              IF jtc.usesTable THEN
                (* need 3 registers *)
                r0 := Live.FindCallerSaveRegister (0, r);
                r1 := Live.FindCallerSaveRegister (r0+1, r);
                r2 := Live.FindCallerSaveRegister (r1+1, r);
                
                jtc.tmp0 := ORD (Register.caller_save [r0]);
                jtc.tmp1 := ORD (Register.caller_save [r1]);
                jtc.tmp2 := ORD (Register.caller_save [r2]);
              ELSE
                (* only need 1 register *)
                r0 := Live.FindCallerSaveRegister (0, r);
                jtc.tmp0 := ORD (Register.caller_save [r0]);
              END;
            END;
          END;
        END;
      END DoFast;

    BEGIN
      TRY
        BasicBlock.InstructionSweep (bb, DoFast, NIL);
      EXCEPT ELSE
        RAISE Analysis.Problem;
      END;
    END Fast;

  BEGIN
    IF Debug THEN
      Wr.PutText (Stdio.stderr, "relabeling procedure\n");
    END;
    Analyzer.Label (p);

    IF Debug THEN
      Wr.PutText (Stdio.stderr, "doing live analysis\n");
    END;
    live := Live.NewT (p.instructions.size);
    Analyzer.Visit (p, live);

    IF FALSE THEN
      Analyzer.Print (p, live);
    END;

    IF Debug THEN
      Wr.PutText (Stdio.stderr, "doing register allocation for jump tables\n");
    END;
    Procedure.Sweep (p, Fast, NIL);

  END Reallocate;

BEGIN
END Fast.
