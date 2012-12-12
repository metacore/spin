(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 17-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	moved some code into RCGInit.m3
 *      added Analyze
 *
 *)

MODULE RCG EXPORTS RCG;

IMPORT Stdio, Wr, Thread, Fmt;
IMPORT Procedure, Program, Disassembler, Assembler;

<* FATAL Thread.Alerted, Wr.Failure *>

VAR cloneCnt : INTEGER := 0;
 

PROCEDURE Clone (proc: PROCANY; name: TEXT): PROCANY =
  VAR
    newProc   : PROCANY;
    procedure : Procedure.T;
    program   : Program.T;
  BEGIN
    INC(cloneCnt);

    program := Program.NewT();

    (*
    IF verbose THEN
      Wr.PutText(Stdio.stdout, "RCG.Clone: printing original instructions\n");
      Instruction.PrintUntilReturn(proc, name);
    END;
    *)

    procedure := Disassembler.DisassembleOne (proc, 0, name, program);
    IF procedure = NIL THEN
      IF verbose THEN
        Wr.PutText(Stdio.stdout, "ERROR >> RCG.Clone: disassembling failed\n");
      END;
      RETURN NIL;
    END;

    IF verbose THEN
      Wr.PutText(Stdio.stdout, "RCG.Clone: printing ir\n");
      Procedure.Print(procedure);
    END;

    newProc := Assembler.AssembleOne(procedure, program);
    IF newProc = NIL THEN
      IF verbose THEN
        Wr.PutText(Stdio.stdout, "ERROR >> RCG.Clone: assembling failed\n");
      END;
      RETURN NIL;
    END;

    (*
    IF verbose THEN
      Wr.PutText(Stdio.stdout, "RCG.Clone: printing cloned instructions\n");
      Instruction.PrintUntilReturn(newProc, "Clone_" & name);
    END;
    *)

    IF verbose THEN
      Wr.PutText (Stdio.stdout,
                  "RCG.Clone: result 0x" & Fmt.Addr (newProc) & "\n");
    END;

    RETURN newProc;
  END Clone;


BEGIN
END RCG.
