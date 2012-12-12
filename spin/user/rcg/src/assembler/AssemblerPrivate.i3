(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Run-time code generation for the dispatcher
 *
 * HISTORY
 * 15-Oct-96  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. 
 *
 *)

INTERFACE AssemblerPrivate;

IMPORT Word;
IMPORT Procedure, Program;
IMPORT JumpTable, Instruction, Ir;
IMPORT Assembler;  (* Problem exception *)

(* flush the i-cache *)
<*EXTERNAL*> PROCEDURE InstructionMemoryBarrier();

(* assemble an array of instructions *)
PROCEDURE AssembleArray (ia: Ir.InstructionArray; addr: Instruction.TPtr;
                         pc: Word.T; proc: Procedure.T; program: Program.T;
                         emit: BOOLEAN)
  : CARDINAL RAISES {Assembler.Problem};

(* exported by AssemblerJT.m3 *)
PROCEDURE AssembleJT (jt: JumpTable.T;
                      addr: Instruction.TPtr; pc: Word.T;
                      proc: Procedure.T; program: Program.T)
  : CARDINAL RAISES { Assembler.Problem };

(* number of instructions - implemented in AssemblerJT.m3 *)
PROCEDURE InstructionCountJT (t: JumpTable.T): CARDINAL RAISES {Assembler.Problem};

END AssemblerPrivate.
