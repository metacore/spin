(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)

INTERFACE Assembler;
IMPORT Procedure, Instruction, Program;

(* something unimplemented or buggy *)
EXCEPTION Problem;

VAR VerboseAssembly: BOOLEAN := FALSE;

PROCEDURE Assemble (proc: Procedure.T; program: Program.T) : Instruction.TPtr
  RAISES {Problem};

PROCEDURE AssembleOne (proc: Procedure.T; program: Program.T): PROCANY
  RAISES {Problem};

END Assembler.
