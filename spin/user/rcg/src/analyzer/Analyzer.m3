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

UNSAFE MODULE Analyzer;

IMPORT Wr, Stdio, Fmt, Thread;
IMPORT Ir, Traverse, Analysis, BasicBlock, Procedure;
IMPORT JumpTable, SortedRCGTbl;
IMPORT IntSet;

<* FATAL Thread.Alerted, Wr.Failure *>


CONST
  DebugLabel = FALSE;

PROCEDURE PrintBlock(block: BasicBlock.T; arg: REFANY) RAISES {Ir.Problem} =
  VAR
    analysis := NARROW(arg, Analysis.T);
  BEGIN
    Wr.PutText (Stdio.stderr, "\nBasic Block #" & Fmt.Int(block.index) & "\n");
    Wr.PutText (Stdio.stderr, "In: ");
    IF NOT block.terminal THEN
      analysis.PrintResidueIn (block);
    END;
    Wr.PutText (Stdio.stderr, "\nOut: ");
    IF NOT block.terminal THEN
      analysis.PrintResidueOut (block);
    END;
    Wr.PutText (Stdio.stderr, "\n");

    FOR i := 0 TO block.instruction_count-1 DO
      Ir.PrintInstruction(block.instructions[i], Stdio.stderr);
      Wr.PutText(Stdio.stderr, "  \t");
(*
      analysis.PrintResidue(block, i);
*)
      Wr.PutText(Stdio.stderr, "\n");
    END;
    
    Wr.PutText(Stdio.stderr, "\nBlock successors: ");
    FOR i := FIRST(block.successors^) TO LAST(block.successors^) DO
      Wr.PutText(Stdio.stderr, Fmt.Int(block.successors[i].index) & " ");
    END;
      
    Wr.PutText(Stdio.stderr, "\nBlock predecessors: ");
    FOR i := FIRST(block.predecessors^) TO LAST(block.predecessors^) DO
      Wr.PutText(Stdio.stderr, Fmt.Int(block.predecessors[i].index) & " ");
    END;
    Wr.PutText(Stdio.stderr, "\n");
  END PrintBlock;

PROCEDURE Print(p: Procedure.T; a: Analysis.T) RAISES {Problem} =
  BEGIN
    TRY
      BasicBlock.Sweep(p.top, PrintBlock, a);
    EXCEPT ELSE RAISE Problem;
    END;
  END Print;

PROCEDURE Visit (p: Procedure.T; a: Analysis.T) RAISES {Problem} =
  BEGIN
    TRY
      a.SetFlags (p);
      
      IF a.Direction () = Analysis.Direction.Forward THEN
        Traverse.IDFA_All (p.top, p.bottom, a, p);
      ELSE
        Traverse.IDFA_All (p.bottom, p.top, a, p);
      END;
    EXCEPT ELSE
      RAISE Problem;
    END;
  END Visit;

PROCEDURE Optimize(p: Procedure.T; proc: BasicBlock.Proc) =
  BEGIN
    BasicBlock.Sweep(p.top, proc, p);
  END Optimize;


(* build up instruction labels *)
PROCEDURE Label (p: Procedure.T) =

  (* label instruction in basic block bb *)
  PROCEDURE DoLabel (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =

    PROCEDURE DoNestedLabel (array: Ir.InstructionArray; index: CARDINAL) =
      VAR
        iter: SortedRCGTbl.Iterator;
        value: INTEGER; td: REFANY;
      BEGIN
        WITH instruction = array[index] DO
          IF instruction.op = Ir.Opcode.indirect THEN
            WITH nested_array = LOOPHOLE (instruction, Ir.IndirectInstruction).indirect DO
              FOR i := 0 TO LAST (nested_array^) DO
                DoNestedLabel (nested_array, i);
              END;
            END;
          ELSIF instruction.op = Ir.Opcode.jt THEN
            WITH jt = LOOPHOLE (instruction, Ir.JtInstruction).table DO
              iter := jt.data.iterateOrdered ();
              WHILE iter.next (value, td) DO
                WITH ia = NARROW (NARROW (td, JumpTable.TableData).block, Ir.InstructionArray) DO
                  FOR i := 0 TO LAST (ia^) DO
                    DoNestedLabel (ia, i);
                  END;
                END;
              END;
            END;
            instruction.index :=
                Procedure.IndexInstruction (p.instructions, bb);
          ELSE
            instruction.index :=
                Procedure.IndexInstruction (p.instructions, bb);
          END;
        END;
      END DoNestedLabel;

    BEGIN
      FOR i := 0 TO bb.instruction_count - 1 DO
        DoNestedLabel (bb.instructions, i);
      END;
    END DoLabel;

  PROCEDURE BuildDefsUses (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =

    
    PROCEDURE DU (VAR inst: Ir.Instruction; <* UNUSED *> u: REFANY) =
      BEGIN
        CASE inst.op OF
        | Ir.Opcode.addq .. Ir.Opcode.zapnot =>

          (* arithmetic ops with three registers use r1 and r2, define rdest *)
          WITH aluOp = LOOPHOLE(inst, Ir.AluRRInstruction) DO
            IntSet.Add (inst.index, p.defs[aluOp.rdest]);
            IntSet.Add (inst.index, p.uses[aluOp.r1]);
            IntSet.Add (inst.index, p.uses[aluOp.r2]);
            p.registers := p.registers +
                               Ir.RegisterSet { aluOp.r1, aluOp.r2, aluOp.rdest };
          END;
          
        | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>
          (* ALU ops with immediate fields use r1, define rdest *)
          WITH aluOp = LOOPHOLE(inst, Ir.AluRIInstruction) DO
            IntSet.Add (inst.index, p.defs[aluOp.rdest]);
            IntSet.Add (inst.index, p.uses[aluOp.r1]);
            p.registers := p.registers +
                           Ir.RegisterSet { aluOp.r1, aluOp.rdest };
          END;

	  | Ir.Opcode.li =>
   
          (* immed instruction defines rdest *)
          WITH immedOp = LOOPHOLE(inst, Ir.ImmedInstruction) DO
            IntSet.Add(inst.index, p.defs[immedOp.rdest]);
            p.registers := p.registers +
                           Ir.RegisterSet { immedOp.rdest };
          END;
          
        | Ir.Opcode.lda .. Ir.Opcode.ldb =>
   
	    (* load instruction uses raddr, defines r1 *)
	    WITH memOp = LOOPHOLE(inst, Ir.MemoryInstruction) DO
	      IntSet.Add (inst.index, p.defs[memOp.r1]);
	      IntSet.Add (inst.index, p.uses[memOp.raddr]);
            p.registers := p.registers +
                           Ir.RegisterSet { memOp.r1, memOp.raddr };
          END;
   
	  | Ir.Opcode.jsr .. Ir.Opcode.ret =>
   
	   (* call instruction uses r2 by jumping to it, defines r1 *)
	   WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
	     IntSet.Add (inst.index, p.defs[callOp.r1]);
	     IntSet.Add (inst.index, p.uses[callOp.r2]);
            p.registers := p.registers +
                           Ir.RegisterSet { callOp.r1, callOp.r2 };
	   END;
   
	  | Ir.Opcode.copy =>
   
	   (* copy instruction defines rdest and uses rsrc. *)
	   WITH copyOp = LOOPHOLE(inst, Ir.CopyInstruction) DO
	     IntSet.Add (inst.index, p.defs[copyOp.rdest]);
	     IntSet.Add (inst.index, p.uses[copyOp.rsrc]);
           p.registers := p.registers +
                          Ir.RegisterSet { copyOp.rsrc, copyOp.rdest };
	   END;
	     
   
	  | Ir.Opcode.call =>
   
	   (* call instruction defines reg *)
	   WITH callOp = LOOPHOLE(inst, Ir.CallInstruction) DO
	     IntSet.Add (inst.index, p.defs[callOp.reg]);
           p.registers := p.registers +
                          Ir.RegisterSet { callOp.target };
         END;

        | Ir.Opcode.indirect => <* ASSERT FALSE *>
        ELSE
          (* skip
           *)
        END;            
      END DU;
    BEGIN
      BasicBlock.InstructionSweep (bb, DU, NIL);
    END BuildDefsUses;

  BEGIN
    p.instructions.size := 0;
    IF p.instructions.map # NIL THEN
      p.instructions.map :=
          NEW (Procedure.IndexArray, NUMBER (p.instructions.map^));
    ELSE
      p.instructions.map := NEW (Procedure.IndexArray, 5);
    END;

    p.flags := Procedure.FlagSet {Procedure.Flags.Label} + p.flags;
    Procedure.Sweep (p, DoLabel, NIL);

    (* initialize defs *)
    p.defs := NEW (REF ARRAY OF IntSet.T, 32);
    p.uses := NEW (REF ARRAY OF IntSet.T, 32);
    FOR reg := 0 TO 31 DO
      p.defs[reg] := IntSet.New (p.instructions.size);
      p.uses[reg] := IntSet.New (p.instructions.size);
    END;

    Procedure.Sweep (p, BuildDefsUses, NIL);
  END Label;


PROCEDURE PrintDefs (p: Procedure.T) =
  BEGIN
    IF DebugLabel THEN
      FOR i := 0 TO p.instructions.size-1 DO
        Wr.PutText (Stdio.stderr, "defs for " & Fmt.Int (i) & " is ");
        IntSet.Print (p.defs[i]);
        Wr.PutText (Stdio.stderr, "\n");
      END;

      Procedure.Print (p);
    END;
  END PrintDefs;


BEGIN
END Analyzer.
