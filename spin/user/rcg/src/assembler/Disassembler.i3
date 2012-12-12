(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

INTERFACE Disassembler;

IMPORT Procedure, Program;
IMPORT Instruction;

EXCEPTION Cannot;

(* disassemble a single procedure *)
PROCEDURE DisassembleProc (VAR p: Procedure.T;  (* out parameter *)
                           entry: Instruction.TPtr; index: INTEGER;
                           name: TEXT; prog: Program.T) RAISES {Cannot};

(* declare an address in the program is an entry point *)
PROCEDURE EntryPoint (i: Instruction.TPtr; prog: Program.T) : CARDINAL;

(* disassemble a whole program -- call chain *)
PROCEDURE Disassemble (p: PROCANY; program: Program.T) : Procedure.T
  RAISES {Cannot};
PROCEDURE DisassembleRest (p: Program.T) RAISES {Cannot};

PROCEDURE DisassembleOne(proc: PROCANY; index: INTEGER;
                         name: TEXT; prog: Program.T)
                        : Procedure.T RAISES {Cannot};

PROCEDURE GetGP(ldgp: Instruction.TPtr): ADDRESS;

END Disassembler.
