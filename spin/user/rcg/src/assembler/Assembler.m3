(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 16-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add blbs
 *
 *)

UNSAFE MODULE Assembler EXPORTS Assembler, AssemblerPrivate;

IMPORT Word;
IMPORT Ir, BasicBlock, Procedure;
IMPORT Instruction AS Inst;
IMPORT Wr, Stdio, Fmt, Thread;
IMPORT Program, Register;
FROM Program IMPORT InstArray, WordArray;

(* ON UNIX SYSTEMS
IMPORT RTMisc, Disassembler, RTIO, WeakRef, StrongRef;
*)

CONST DebugLdah = FALSE;

<* FATAL Thread.Alerted, Wr.Failure *>

(******************************************************************************
 *
 * assembly of instructions
 *
 *****************************************************************************)

CONST
  DescTable = ARRAY [Ir.Opcode.addq .. Ir.Opcode.indirect] OF Inst.Desc {
    Inst.DecodeTable[Inst.Opcode.addq],
    Inst.DecodeTable[Inst.Opcode.subq],
    Inst.DecodeTable[Inst.Opcode.mulq],
    Inst.DecodeTable[Inst.Opcode.addq],    (* I am not sure about this *)
    Inst.DecodeTable[Inst.Opcode.subq],    (* What are the correct insts *)
    Inst.DecodeTable[Inst.Opcode.mulq],    (* for unsigned arithmetic? *)
    Inst.DecodeTable[Inst.Opcode.cmpeq],
    Inst.DecodeTable[Inst.Opcode.cmple],
    Inst.DecodeTable[Inst.Opcode.cmplt],
    Inst.DecodeTable[Inst.Opcode.cmpule],
    Inst.DecodeTable[Inst.Opcode.cmpult],
    Inst.DecodeTable[Inst.Opcode.and],
    Inst.DecodeTable[Inst.Opcode.bic],
    Inst.DecodeTable[Inst.Opcode.bis],
    Inst.DecodeTable[Inst.Opcode.ornot],
    Inst.DecodeTable[Inst.Opcode.xor],
    Inst.DecodeTable[Inst.Opcode.eqv],
    Inst.DecodeTable[Inst.Opcode.sll],
    Inst.DecodeTable[Inst.Opcode.sra],
    Inst.DecodeTable[Inst.Opcode.srl],

    Inst.DecodeTable[Inst.Opcode.cmpbge],
    Inst.DecodeTable[Inst.Opcode.extbl],
    Inst.DecodeTable[Inst.Opcode.extwl],
    Inst.DecodeTable[Inst.Opcode.extll],
    Inst.DecodeTable[Inst.Opcode.extql],
    Inst.DecodeTable[Inst.Opcode.extwh],
    Inst.DecodeTable[Inst.Opcode.extlh],
    Inst.DecodeTable[Inst.Opcode.extqh],
    Inst.DecodeTable[Inst.Opcode.insbl],
    Inst.DecodeTable[Inst.Opcode.inswl],
    Inst.DecodeTable[Inst.Opcode.insll],
    Inst.DecodeTable[Inst.Opcode.insql],
    Inst.DecodeTable[Inst.Opcode.inswh],
    Inst.DecodeTable[Inst.Opcode.inslh],
    Inst.DecodeTable[Inst.Opcode.insqh],
    Inst.DecodeTable[Inst.Opcode.mskbl],
    Inst.DecodeTable[Inst.Opcode.mskwl],
    Inst.DecodeTable[Inst.Opcode.mskll],
    Inst.DecodeTable[Inst.Opcode.mskql],
    Inst.DecodeTable[Inst.Opcode.mskwh],
    Inst.DecodeTable[Inst.Opcode.msklh],
    Inst.DecodeTable[Inst.Opcode.mskqh],
    Inst.DecodeTable[Inst.Opcode.cmoveq],
    Inst.DecodeTable[Inst.Opcode.cmovlbc],
    Inst.DecodeTable[Inst.Opcode.cmovlbs],
    Inst.DecodeTable[Inst.Opcode.cmovge],
    Inst.DecodeTable[Inst.Opcode.cmovlt],
    Inst.DecodeTable[Inst.Opcode.cmovgt],
    Inst.DecodeTable[Inst.Opcode.cmovne],
    Inst.DecodeTable[Inst.Opcode.cmovle],
    Inst.DecodeTable[Inst.Opcode.s4addl],
    Inst.DecodeTable[Inst.Opcode.s4subl],
    Inst.DecodeTable[Inst.Opcode.s4addq],
    Inst.DecodeTable[Inst.Opcode.s4subq],
    Inst.DecodeTable[Inst.Opcode.s8addl],
    Inst.DecodeTable[Inst.Opcode.s8subl],
    Inst.DecodeTable[Inst.Opcode.s8addq],
    Inst.DecodeTable[Inst.Opcode.s8subq],
    Inst.DecodeTable[Inst.Opcode.zap],
    Inst.DecodeTable[Inst.Opcode.zapnot],

    Inst.DecodeTable[Inst.Opcode.lda],
    Inst.DecodeTable[Inst.Opcode.ldq],
    Inst.DecodeTable[Inst.Opcode.ldq_u],
    Inst.DecodeTable[Inst.Opcode.ldl],
    Inst.DecodeTable[Inst.Opcode.ldl],
    Inst.DecodeTable[Inst.Opcode.ldl],
    Inst.DecodeTable[Inst.Opcode.stq],
    Inst.DecodeTable[Inst.Opcode.stq_u],
    Inst.DecodeTable[Inst.Opcode.stl],
    Inst.DecodeTable[Inst.Opcode.stl],
    Inst.DecodeTable[Inst.Opcode.stl],

    Inst.DecodeTable[Inst.Opcode.addq],
    Inst.DecodeTable[Inst.Opcode.subq],
    Inst.DecodeTable[Inst.Opcode.mulq],
    Inst.DecodeTable[Inst.Opcode.addq],
    Inst.DecodeTable[Inst.Opcode.subq],
    Inst.DecodeTable[Inst.Opcode.mulq],
    Inst.DecodeTable[Inst.Opcode.cmpeq],
    Inst.DecodeTable[Inst.Opcode.cmple],
    Inst.DecodeTable[Inst.Opcode.cmplt],
    Inst.DecodeTable[Inst.Opcode.cmpule],
    Inst.DecodeTable[Inst.Opcode.cmpult],
    Inst.DecodeTable[Inst.Opcode.and],
    Inst.DecodeTable[Inst.Opcode.bic],
    Inst.DecodeTable[Inst.Opcode.bis],
    Inst.DecodeTable[Inst.Opcode.ornot],
    Inst.DecodeTable[Inst.Opcode.xor],
    Inst.DecodeTable[Inst.Opcode.eqv],
    Inst.DecodeTable[Inst.Opcode.sll],
    Inst.DecodeTable[Inst.Opcode.sra],
    Inst.DecodeTable[Inst.Opcode.srl],

    Inst.DecodeTable[Inst.Opcode.cmpbge],
    Inst.DecodeTable[Inst.Opcode.extbl],
    Inst.DecodeTable[Inst.Opcode.extwl],
    Inst.DecodeTable[Inst.Opcode.extll],
    Inst.DecodeTable[Inst.Opcode.extql],
    Inst.DecodeTable[Inst.Opcode.extwh],
    Inst.DecodeTable[Inst.Opcode.extlh],
    Inst.DecodeTable[Inst.Opcode.extqh],
    Inst.DecodeTable[Inst.Opcode.insbl],
    Inst.DecodeTable[Inst.Opcode.inswl],
    Inst.DecodeTable[Inst.Opcode.insll],
    Inst.DecodeTable[Inst.Opcode.insql],
    Inst.DecodeTable[Inst.Opcode.inswh],
    Inst.DecodeTable[Inst.Opcode.inslh],
    Inst.DecodeTable[Inst.Opcode.insqh],
    Inst.DecodeTable[Inst.Opcode.mskbl],
    Inst.DecodeTable[Inst.Opcode.mskwl],
    Inst.DecodeTable[Inst.Opcode.mskll],
    Inst.DecodeTable[Inst.Opcode.mskql],
    Inst.DecodeTable[Inst.Opcode.mskwh],
    Inst.DecodeTable[Inst.Opcode.msklh],
    Inst.DecodeTable[Inst.Opcode.mskqh],
    Inst.DecodeTable[Inst.Opcode.cmoveq],
    Inst.DecodeTable[Inst.Opcode.cmovlbc],
    Inst.DecodeTable[Inst.Opcode.cmovlbs],
    Inst.DecodeTable[Inst.Opcode.cmovge],
    Inst.DecodeTable[Inst.Opcode.cmovlt],
    Inst.DecodeTable[Inst.Opcode.cmovgt],
    Inst.DecodeTable[Inst.Opcode.cmovne],
    Inst.DecodeTable[Inst.Opcode.cmovle],
    Inst.DecodeTable[Inst.Opcode.s4addl],
    Inst.DecodeTable[Inst.Opcode.s4subl],
    Inst.DecodeTable[Inst.Opcode.s4addq],
    Inst.DecodeTable[Inst.Opcode.s4subq],
    Inst.DecodeTable[Inst.Opcode.s8addl],
    Inst.DecodeTable[Inst.Opcode.s8subl],
    Inst.DecodeTable[Inst.Opcode.s8addq],
    Inst.DecodeTable[Inst.Opcode.s8subq],
    Inst.DecodeTable[Inst.Opcode.zap],
    Inst.DecodeTable[Inst.Opcode.zapnot],

    Inst.DecodeTable[Inst.Opcode.lda],

    Inst.DecodeTable[Inst.Opcode.beq],
    Inst.DecodeTable[Inst.Opcode.bge],
    Inst.DecodeTable[Inst.Opcode.bgt],
    Inst.DecodeTable[Inst.Opcode.ble],
    Inst.DecodeTable[Inst.Opcode.blt],
    Inst.DecodeTable[Inst.Opcode.bne],
    Inst.DecodeTable[Inst.Opcode.blbc],
    Inst.DecodeTable[Inst.Opcode.blbs],
    Inst.DecodeTable[Inst.Opcode.br],

    Inst.DecodeTable[Inst.Opcode.jsr],
    Inst.DecodeTable[Inst.Opcode.ret],
    Inst.DecodeTable[Inst.Opcode.bis],

    Inst.DecodeTable[Inst.Opcode.bsr],
    Inst.DecodeTable[Inst.Opcode.bsr],
  
    (* indirect - just a fake placeholder *)
    Inst.DecodeTable[Inst.Opcode.pal]
  };

(* A single IR instruction could turn into many machine instructions.
   Return the exact number from this procedure. The block_index is
   the index of the block containing the instruction. We assume that
   the blocks are already laid out in the order in which they will be
   emitted, so that a jump to the following block is a nop. 
   Call instructions can either be 1 machine instruction when it
   is within the range of a bsr, or 3 instructions when the address
   has to be loaded into a register. *)

PROCEDURE NumInsts(ir_inst: Ir.Instruction; 
                   block_index: Ir.BasicBlockIndex;
                   pc: Word.T; 
                   program: Program.T): CARDINAL
  RAISES {Problem}=
  VAR
    sum, increment := 0;
  BEGIN
    CASE ir_inst.op OF 
    | Ir.Opcode.beq .. Ir.Opcode.br =>

      WITH branchOp = LOOPHOLE(ir_inst, Ir.BranchInstruction) DO
        IF branchOp.target = block_index + 1 THEN
          RETURN 0;
        ELSE
          RETURN 1;
        END;
      END;

    | Ir.Opcode.call .. Ir.Opcode.fault =>

      WITH callOp = LOOPHOLE(ir_inst, Ir.CallInstruction) DO
        IF VerboseAssembly THEN
          Wr.PutText(Stdio.stderr,
                     "$$$ " & Fmt.Int(callOp.target) & " 0x" &
                     Fmt.Unsigned(LOOPHOLE(program,INTEGER)) & "\n");
        END;
        WITH target = program.procs[callOp.target] DO
          IF target = NIL THEN
            Wr.PutText(Stdio.stderr, "ERROR >> unknown target procedure\n");
            (* FIXME: NEED SOME ERROR HANDLING *)
            RAISE Problem;
          END;
          WITH displacement = LOOPHOLE(target.addr - (pc + 1), INTEGER) DO
            IF displacement >= -16_100000 AND displacement < 16_100000 THEN
              (* FIXME: WHY??? a short jump??? *)
              RETURN 1;
            ELSE
              (* Add a new value to the global table *)
              EVAL Program.GetNewGPValue(program, 
                                         LOOPHOLE(target.addr, Word.T));
              RETURN 2;
            END;
          END;
        END;
      END;

    | Ir.Opcode.li =>

      (* load immediate instruction *)
      WITH immedOp = LOOPHOLE(ir_inst, Ir.ImmedInstruction) DO
        IF Word.LE (immedOp.immed, 16_ffff) THEN
          RETURN 1;
        ELSE
          RETURN 2;
        END;
      END;

    | Ir.Opcode.indirect =>

      (* calculate numbers for additional instructions *)
      WITH array = LOOPHOLE (ir_inst, Ir.IndirectInstruction).indirect DO
        FOR i := 0 TO LAST (array^) DO
          increment := NumInsts (array[i], block_index, pc, program);
          INC (sum, increment);
          INC (pc, increment);
        END;

        RETURN sum;
      END;

    | Ir.Opcode.jt =>
      
      WITH jt = LOOPHOLE (ir_inst, Ir.JtInstruction).table DO
        RETURN InstructionCountJT (jt);
      END;

    | Ir.Opcode.mulqi .. Ir.Opcode.zapnoti =>
      (* change if immediate is too big *)
      RETURN 1;

    | Ir.Opcode.deleted =>
      RETURN 0;

    ELSE
      RETURN 1;
    END;
  END NumInsts;



(*
 * assemble a single machine instruction
 *
 * code_ptr points to the destination address for code
 * pc is the offset in instructions from the beginning of the procedure (?)
 *)

PROCEDURE AssembleInst(ir_inst: Ir.Instruction; 
                       code_ptr: Inst.TPtr;
                       pc: Word.T; 
                       proc : Procedure.T;
                       program: Program.T;
                       ): CARDINAL
  RAISES {Problem} =
  VAR
    desc   : Inst.Desc;
    sum, increment := 0;
  BEGIN
    IF ir_inst.op # Ir.Opcode.noop
      AND ir_inst.op # Ir.Opcode.indirect
      AND ir_inst.op # Ir.Opcode.deleted
      AND ir_inst.op # Ir.Opcode.jt THEN
      desc := DescTable[ir_inst.op];
    END;

    CASE ir_inst.op OF 
    | Ir.Opcode.addqi =>

      (* ALU ops with immediate fields *)
      WITH aluOp = LOOPHOLE(ir_inst, Ir.AluRIInstruction),
           machine_inst = code_ptr^ DO
           
        IF Word.GE(aluOp.immed, Word.LeftShift(1, 8)) THEN
          Inst.SetOpcode(machine_inst,
                         Inst.DecodeTable[Inst.Opcode.lda].op_val);
          Inst.SetRegA(machine_inst, aluOp.rdest);
          Inst.SetRegB(machine_inst, aluOp.r1);
          Inst.SetDisp(machine_inst, aluOp.immed);
        ELSE 
          Inst.SetOpcode(machine_inst, desc.op_val);
          Inst.SetFunction(machine_inst, desc.fc_val);
          Inst.SetRegA(machine_inst, aluOp.r1);
          Inst.SetRegC(machine_inst, aluOp.rdest);
          Inst.SetLitFlag(machine_inst, TRUE);
          Inst.SetLit(machine_inst, aluOp.immed);
        END;
      END;
      RETURN 1;

    | Ir.Opcode.subqi =>

      (* ALU ops with immediate fields *)
      WITH aluOp = LOOPHOLE(ir_inst, Ir.AluRIInstruction),
           machine_inst = code_ptr^ DO
           
        IF Word.GE (aluOp.immed, Word.LeftShift(1, 8)) THEN
          Inst.SetOpcode(machine_inst,
                         Inst.DecodeTable[Inst.Opcode.lda].op_val);
          Inst.SetRegA(machine_inst, aluOp.rdest);
          Inst.SetRegB(machine_inst, aluOp.r1);
          Inst.SetDisp(machine_inst, -aluOp.immed);
        ELSE 
          Inst.SetOpcode(machine_inst, desc.op_val);
          Inst.SetFunction(machine_inst, desc.fc_val);
          Inst.SetRegA(machine_inst, aluOp.r1);
          Inst.SetRegC(machine_inst, aluOp.rdest);
          Inst.SetLitFlag(machine_inst, TRUE);
          Inst.SetLit(machine_inst, aluOp.immed);
        END;
      END;
      RETURN 1;

    | Ir.Opcode.mulqi .. Ir.Opcode.zapnoti =>

      (* ALU ops with immediate fields *)
      WITH aluOp = LOOPHOLE(ir_inst, Ir.AluRIInstruction),
           machine_inst = code_ptr^ DO
        IF Word.GE (aluOp.immed, Word.LeftShift(1, 8)) THEN
          (* have to load constant first *)
          RAISE Problem;
        ELSE
          Inst.SetOpcode(machine_inst, desc.op_val);
          Inst.SetFunction(machine_inst, desc.fc_val);
          Inst.SetRegA(machine_inst, aluOp.r1);
          Inst.SetRegC(machine_inst, aluOp.rdest);
          Inst.SetLitFlag(machine_inst, TRUE);
          Inst.SetLit(machine_inst, aluOp.immed);
          RETURN 1;
        END;
      END;

    | Ir.Opcode.addq .. Ir.Opcode.s8addq =>

      (* ALU ops with three registers *)
      WITH aluOp = LOOPHOLE(ir_inst, Ir.AluRRInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetFunction(machine_inst, desc.fc_val);
        Inst.SetRegA(machine_inst, aluOp.r1);
        Inst.SetLitFlag(machine_inst, FALSE);
        Inst.SetRegB(machine_inst, aluOp.r2);
        Inst.SetRegC(machine_inst, aluOp.rdest);
      END;
      RETURN 1;

    | Ir.Opcode.li =>

      (* load immediate instruction *)
      WITH immedOp = LOOPHOLE(ir_inst, Ir.ImmedInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetRegA(machine_inst, immedOp.rdest);
        Inst.SetRegB(machine_inst, ORD(Register.T.zero));

        (* simple constant? *)
        IF Word.LE (immedOp.immed, 16_ffff) THEN
          Inst.SetOpcode (machine_inst, Inst.DecodeTable[Inst.Opcode.lda].op_val);
          Inst.SetDisp (machine_inst, immedOp.immed);
          RETURN 1;
        END;

        Inst.SetOpcode(machine_inst, 
                       Inst.DecodeTable[Inst.Opcode.ldah].op_val);
        
        WITH second_inst = LOOPHOLE(code_ptr + BYTESIZE(Inst.T), Inst.TPtr)^ DO
          Inst.SetOpcode(second_inst, desc.op_val);
          Inst.SetRegA(second_inst, immedOp.rdest);
          Inst.SetRegB(second_inst, immedOp.rdest);

          VAR 
            o1, o2, o3, o4, o5, o6: Word.T;
            o7, o8: INTEGER;
            offset: INTEGER;
          BEGIN
            offset := immedOp.immed;
            o1 := Word.RightShift(offset, 16);
            IF offset < 0 THEN 
              o1 := Word.Or(o1, 16_ffff000000000000); 
            END;
            o2 := Word.LeftShift(o1, 16);
            o3 := offset - o2;
            o4 := Word.LeftShift(o3, 16);
            o5 := Word.RightShift(o4, 16);
            o6 := o1;
            IF Word.And(o5, 16_8000) # 0 THEN
              INC(o6);
              o5 := Word.And(o5, 16_ffff);
              o5 := Word.Or(o5, 16_ffffffffffff0000);
            END;
            o7 := o6;
            o8 := o5;
            Inst.SetDisp(machine_inst, o7);
            Inst.SetDisp(second_inst, o8);
          END;
        END;

        RETURN 2;
      END;

    | Ir.Opcode.lda =>

      (* load/store instruction *)
      WITH memOp = LOOPHOLE(ir_inst, Ir.MemoryInstruction),
           machine_inst = code_ptr^ DO
        IF memOp.high THEN
          Inst.SetOpcode(machine_inst, 
                         Inst.DecodeTable[Inst.Opcode.ldah].op_val);
        ELSE
          Inst.SetOpcode(machine_inst, desc.op_val);
        END;
        (*
        Wr.PutText(
            Stdio.stderr, "> " &
            Fmt.Unsigned(LOOPHOLE(code_ptr, Word.T)) & "\n");
        *)
        Inst.SetRegA(machine_inst, memOp.r1);
        Inst.SetRegB(machine_inst, memOp.raddr);
        Inst.SetDisp(machine_inst, memOp.disp);
      END;
      RETURN 1;

    | Ir.Opcode.ldq .. Ir.Opcode.stb =>

      (* load/store instruction *)
      WITH memOp = LOOPHOLE(ir_inst, Ir.MemoryInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetRegA(machine_inst, memOp.r1);
        Inst.SetRegB(machine_inst, memOp.raddr); 

        (*
        (* If the load was relative to the old gp, then it has a new
           displacement now. *)
        IF memOp.raddr = ORD(Register.T.gp) THEN
          Inst.SetDisp(machine_inst, 
                       Program.GetNewGPValue(
                           program, 
                           LOOPHOLE(proc.gp + memOp.disp,
                                    UNTRACED REF Word.T)^));
        ELSE
        *)
          Inst.SetDisp(machine_inst, memOp.disp);
        (*
          END;
        *)
      END;
      RETURN 1;

    | Ir.Opcode.beq .. Ir.Opcode.br =>

      (* jump instruction uses r1 by jumping to it *)
      WITH branchOp = LOOPHOLE(ir_inst, Ir.BranchInstruction),
           machine_inst = code_ptr^,
           displacement = proc.table[branchOp.target].offset - (pc+1) DO
        (* If the displacement happens to be zero, then we have a 
           branch to the fallthrough block. This instruction can
           be omitted. *)
        IF displacement # 0 THEN
          Inst.SetOpcode(machine_inst, desc.op_val);
          Inst.SetRegA(machine_inst, branchOp.r1);
          Inst.SetBranchDisp(machine_inst, displacement);
          RETURN 1;
        ELSE
          RETURN 0;
        END;
      END;

    | Ir.Opcode.ret =>

      (* call instruction uses r2 by jumping to it *)
      WITH callOp = LOOPHOLE(ir_inst, Ir.RetInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetBranchPred(machine_inst, desc.fc_val);
        Inst.SetRegA(machine_inst, callOp.r1);
        Inst.SetRegB(machine_inst, callOp.r2);
        (* We do not have any branch prediction information. *)
        Inst.SetTarget(machine_inst, 0);
      END;
      RETURN 1;

    | Ir.Opcode.copy =>

      WITH copyOp = LOOPHOLE(ir_inst, Ir.CopyInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetFunction(machine_inst, desc.fc_val);
        Inst.SetRegA(machine_inst, copyOp.rsrc);
        Inst.SetLitFlag(machine_inst, FALSE);
        Inst.SetRegB(machine_inst, copyOp.rsrc);
        Inst.SetRegC(machine_inst, copyOp.rdest);
      END;
      RETURN 1;

    | Ir.Opcode.noop =>

      WITH machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetFunction(machine_inst, desc.fc_val);
        Inst.SetRegA(machine_inst, 31);
        Inst.SetLitFlag(machine_inst, FALSE);
        Inst.SetRegB(machine_inst, 31);
        Inst.SetRegC(machine_inst, 31);
      END;
      RETURN 1;

    | Ir.Opcode.call..Ir.Opcode.fault => 

      (* Determine how far we have to jump to the entry point
         of the procedure. Then generate one of the sequences:
                 bsr  ra, displacement

                 ldq  t12, offset(gp)
                 jsr  ra, 0(t12)

         The latter sequence will be replaced by real gp relative
         loads in the future.
      *)

      WITH callOp = LOOPHOLE(ir_inst, Ir.CallInstruction),
           displacement = LOOPHOLE(program.procs[callOp.target].addr - (pc + 1),
                                   INTEGER) DO
        IF displacement >= -16_100000 AND displacement < 16_100000 THEN
          WITH machine_inst = code_ptr^ DO
            Inst.SetOpcode(machine_inst, Inst.DecodeTable[Inst.Opcode.bsr].op_val);
            Inst.SetRegA(machine_inst, ORD(Register.T.ra));
            Inst.SetBranchDisp(machine_inst, displacement);
          END;
          RETURN 1;
        ELSE
          WITH proc_entry = LOOPHOLE(program.procs[callOp.target].addr, Word.T),
               first_inst = code_ptr^, 
               second_inst = LOOPHOLE(code_ptr + BYTESIZE(Inst.T), 
                                      Inst.TPtr)^ DO

            Inst.SetOpcode(first_inst, 
                           Inst.DecodeTable[Inst.Opcode.ldq].op_val);
            Inst.SetRegA(first_inst, ORD(Register.T.t12));
            Inst.SetRegB(first_inst, ORD(Register.T.gp));
            Inst.SetDisp(first_inst, 
                         Program.GetNewGPValue(program, proc_entry));
            
            Inst.SetOpcode(second_inst, 
                           Inst.DecodeTable[Inst.Opcode.jsr].op_val);
            Inst.SetBranchPred(second_inst,
                               Inst.DecodeTable[Inst.Opcode.jsr].fc_val);
            Inst.SetRegA(second_inst, ORD(Register.T.ra));
            Inst.SetRegB(second_inst, ORD(Register.T.t12));
            Inst.SetDisp(second_inst, 0);
            RETURN 2;
          END;
        END;
      END;

    | Ir.Opcode.jsr =>

      WITH callOp = LOOPHOLE(ir_inst, Ir.RetInstruction),
           machine_inst = code_ptr^ DO
        Inst.SetOpcode(machine_inst, desc.op_val);
        Inst.SetBranchPred(machine_inst, desc.fc_val);
        Inst.SetRegA(machine_inst, callOp.r1);
        Inst.SetRegB(machine_inst, callOp.r2);
      END;
      RETURN 1;

    | Ir.Opcode.indirect =>
      
      WITH array = LOOPHOLE (ir_inst, Ir.IndirectInstruction).indirect DO
        FOR i := 0 TO LAST (array^) DO
          increment := AssembleInst (array[i], code_ptr, pc, proc, program);

          INC (code_ptr, increment*Inst.ByteWidth);
          INC (pc, increment);
          INC (sum, increment);
        END;

        RETURN sum;
      END;

    | Ir.Opcode.jt =>
      (* jump table! *)
      WITH jt = LOOPHOLE (ir_inst, Ir.JtInstruction).table DO
        increment := AssembleJT (jt, code_ptr, pc, proc, program);
        RETURN increment;
      END;

    | Ir.Opcode.deleted =>
      RETURN 0;

    ELSE
      Wr.PutText(Stdio.stderr, "Unknown opcode - " & Ir.InstructionNames[ir_inst.op] & "\n");
      RETURN 1;

    END;
  END AssembleInst;



PROCEDURE AssembleArray(ia: Ir.InstructionArray; addr: Inst.TPtr;
                        pc: Word.T; proc: Procedure.T; program: Program.T;
                        emit: BOOLEAN)
  : CARDINAL RAISES {Problem} =
  VAR
    actual_instructions := 0;
  BEGIN
    FOR i := 0 TO LAST (ia^) DO
      IF emit THEN
        INC(actual_instructions, 
            AssembleInst(ia[i], 
                         addr + actual_instructions*Inst.ByteWidth,
                         pc + actual_instructions, 
                         proc,
                         program));
      ELSE
        INC (actual_instructions,
             NumInsts (ia[i],
                       0,
                       0,
                       program));
      END;
    END;
    RETURN actual_instructions;
  END AssembleArray;


(******************************************************************************
 *
 * procedures
 *
 *****************************************************************************)


PROCEDURE ComputeLocations (block: BasicBlock.T; VAR offset: CARDINAL;
                            program: Program.T) RAISES {Problem} =
  (* compute the length of the basic block *)
  VAR
    actual_instructions := 0;
  BEGIN
    IF block = NIL THEN RETURN; END;

    (* The basic block will be located at offset 
       instructions from the beginning of the procedure. *)
    block.offset := offset;
    FOR i := 0 TO block.instruction_count-1 DO
      INC (actual_instructions,
           NumInsts (block.instructions[i],
                     block.index,
                     offset + actual_instructions,
                     program));
    END;
    
    INC (offset, actual_instructions);
  END ComputeLocations;



PROCEDURE RelocateProc (proc: Procedure.T; program: Program.T) RAISES {Problem} =
  (* compute the offsets of each basic block *)
  BEGIN
    proc.assembled_size := 0;

    FOR i := FIRST (proc.table^) TO LAST (proc.table^) DO
      ComputeLocations (proc.table[i], proc.assembled_size, program);
    END;
  END RelocateProc;



PROCEDURE AssembleBB (block: BasicBlock.T; proc: Procedure.T;
                      program: Program.T;
                      code_ptr: InstArray) : CARDINAL
  RAISES {Problem} =
  VAR
    actual_instructions := 0;
  BEGIN
    FOR i := 0 TO block.instruction_count-1 DO
      INC(actual_instructions, 
          AssembleInst (block.instructions[i], 
                        ADR (code_ptr[block.offset + actual_instructions]),
                        block.offset + actual_instructions, 
                        proc,
                        program));
    END;

    RETURN actual_instructions;
  END AssembleBB;


PROCEDURE AssembleProc(proc: Procedure.T; program: Program.T;
                       code_ptr : InstArray): Inst.TPtr
  RAISES {Problem} =
  VAR
    total_instructions: CARDINAL := 0;
  BEGIN
    (* assemble each basic block *)
    FOR i := FIRST(proc.table^) TO LAST(proc.table^) DO
      WITH bb = proc.table[i] DO
        IF bb # NIL THEN
          INC (total_instructions,
               AssembleBB (bb, proc, program, code_ptr));
        END;
      END;
    END;

    IF VerboseAssembly THEN
      Wr.PutText (Stdio.stderr,
                  "Reassembled procedure begins at " &
                  Fmt.Unsigned (LOOPHOLE(code_ptr, INTEGER)) & "\n");
      FOR i := 0 TO total_instructions-1 DO
        Wr.PutText (Stdio.stderr, Fmt.Int(4 * i) & " ");
        Inst.Print (code_ptr[i], Stdio.stderr);
        Wr.PutText (Stdio.stderr, "\n");
      END;
    END;

    RETURN LOOPHOLE(ADR(code_ptr[0]), Inst.TPtr);
  END AssembleProc;


(******************************************************************************
 * 
 * 
 * 
 *****************************************************************************)


PROCEDURE FixupMemory (<* UNUSED *> code: ADDRESS) =
  (* Stuff to enable execution on the heap page. *)
  VAR
    (* ON UNIX SYSTEMS
      page_bytes := 8 * 1024;
      start: ADDRESS;
    *)
  BEGIN
    (* ON UNIX SYSTEMS
      page_bytes := Unix.getpagesize ();
      start := RTMisc.Align (LOOPHOLE(code - page_bytes, ADDRESS), page_bytes);
      EVAL
      Umman.mprotect(start,
                     2 * page_bytes,
                     Umman.PROT_READ + Umman.PROT_WRITE + Umman.PROT_EXEC);
    *)

    (* flush icache *)
    InstructionMemoryBarrier();
  END FixupMemory;


(* Look for occurrences of the sequence:
                 ldah gp, const(???)
                 lda  gp, const(gp)
   and replace the constants. *)
PROCEDURE FixupGP (proc: Procedure.T;
                   program: Program.T;
                   code_ptr : InstArray) =
  (* requires:
       any NIL basic blocks are at the end of proc.table
   *)
  VAR
    code: Inst.TPtr := LOOPHOLE (code_ptr, Inst.TPtr);
  BEGIN
    FOR i := FIRST (proc.table^) TO LAST (proc.table^) DO
      WITH block = proc.table[i] DO
        IF block = NIL THEN RETURN; END;
        VAR
          j := 0;
        BEGIN
          WHILE j < block.instruction_count-1 DO
            CASE block.instructions[j].op OF
            | Ir.Opcode.lda =>              
              WITH firstOp = LOOPHOLE(block.instructions[j],
                                      Ir.MemoryInstruction),
                   secondOp = LOOPHOLE(block.instructions[j+1], 
                                       Ir.MemoryInstruction) 
               DO
                (* setting $gp of off the current $pc *)
                IF firstOp.op = Ir.Opcode.lda AND firstOp.high AND
                  firstOp.r1 = ORD(Register.T.gp) AND
                  secondOp.op = Ir.Opcode.lda AND NOT secondOp.high AND
                  secondOp.r1 = ORD(Register.T.gp) AND 
                  secondOp.raddr = ORD(Register.T.gp) 
                 THEN
                  WITH offset = LOOPHOLE(program.globals, Word.T) - 
                    (LOOPHOLE(code, Word.T) + 4 * (block.offset + j))
                   DO
                    (*
                      Wr.PutText(
                      Stdio.stderr, 
                      Fmt.Unsigned(LOOPHOLE(program.globals, Word.T)) & " " & 
                      Fmt.Unsigned(LOOPHOLE(code, Word.T)) & " " & 
                      Fmt.Unsigned(LOOPHOLE(code, Word.T) +
                      4 * (block.offset + j)) & " " &
                      Fmt.Unsigned(offset) & "\n");
                    *)
                    SetLdahLdaPair(firstOp, secondOp, offset);
                  END;
                END;
                
                (* setting other register of off $gp *)
                IF firstOp.op = Ir.Opcode.lda AND firstOp.high AND
                  firstOp.raddr = ORD(Register.T.gp) AND
                  secondOp.op = Ir.Opcode.lda AND NOT secondOp.high AND
                  secondOp.r1 = firstOp.r1 AND
                  secondOp.raddr = firstOp.r1
                 THEN
                  VAR
                    oldTarget, oldValue, offset: Word.T;
                  BEGIN
                    oldTarget := LOOPHOLE(proc.gp, Word.T) +
                                     Word.LeftShift(firstOp.disp, 16) +
                                     secondOp.disp;
                    (*
                    Wr.PutText(
                        Stdio.stderr, 
                        Fmt.Unsigned(LOOPHOLE(code, Word.T) +
                        4 * (block.offset + j)) & " " &
                        Fmt.Unsigned(LOOPHOLE(proc.gp, Word.T)) & " " & 
                        Fmt.Int(firstOp.disp) & " " & 
                        Fmt.Int(secondOp.disp) & " " & 
                        Fmt.Unsigned(LOOPHOLE(oldTarget, Word.T)) & "\n");
                    *)
                    (*
                      oldValue := LOOPHOLE(oldTarget, UNTRACED REF Word.T)^;
                    *)
                    
                    oldValue := oldTarget;
                    offset := Program.GetNewGPValue(program, oldValue);
                    (*
                    Wr.PutText(
                        Stdio.stderr, 
                        Fmt.Unsigned(LOOPHOLE(oldValue, Word.T)) & " " & 
                        Fmt.Unsigned(offset) & "\n");
                    *)
                    firstOp.op   := Ir.Opcode.ldq;
                    firstOp.disp := offset;
                    WITH cancelledSecondOp = LOOPHOLE(block.instructions[j+1], 
                                                      Ir.AluRRInstruction) 
                     DO
                      cancelledSecondOp.op := Ir.Opcode.or;
                      cancelledSecondOp.r1    := 31;
                      cancelledSecondOp.r2    := 31;
                      cancelledSecondOp.rdest := 31;
                    END;
                  END;
                END;
              END;
	    | Ir.Opcode.ldq .. Ir.Opcode.stb =>

	      (* load/store instruction *)
	      WITH memOp = LOOPHOLE(block.instructions[j],
                                    Ir.MemoryInstruction) 
               DO
                (* If the load was relative to the old gp, then it has a new
		   displacement now. *)
		IF memOp.raddr = ORD(Register.T.gp) THEN
                  memOp.disp := Program.GetNewGPValue(
                                    program, 
                                    LOOPHOLE(proc.gp + memOp.disp,
                                             UNTRACED REF Word.T)^);
                END;
	      END;
            ELSE
            END;
            INC(j);
          END;
        END;
      END;
    END;
  END FixupGP;

PROCEDURE SetLdahLdaPair(VAR firstOp  : Ir.MemoryInstruction; 
                         VAR secondOp : Ir.MemoryInstruction; 
                             offset   : INTEGER) =
  VAR 
    o1, o2, o3, o4, o5, o6: Word.T;
    o7, o8: INTEGER;
  BEGIN
    o1 := Word.RightShift(offset, 16);
    IF offset < 0 THEN 
      o1 := Word.Or(o1, 16_ffff000000000000); 
    END;
    o2 := Word.LeftShift(o1, 16);
    o3 := offset - o2;
    o4 := Word.LeftShift(o3, 16);
    o5 := Word.RightShift(o4, 16);
    o6 := o1;
    IF Word.And(o5, 16_8000) # 0 THEN
      INC(o6);
      o5 := Word.And(o5, 16_ffff);
      o5 := Word.Or(o5, 16_ffffffffffff0000);
    END;
    o7 := o6;
    o8 := o5;

    IF DebugLdah THEN
      Wr.PutText(Stdio.stderr, 
      Fmt.Unsigned(o1) & " " & Fmt.Int(o1) & " " &
      Fmt.Unsigned(o2) & " " & Fmt.Int(o2) & " " &
      Fmt.Unsigned(o3) & " " & Fmt.Int(o3) & " " &
      Fmt.Unsigned(o4) & " " & Fmt.Int(o4) & " " &
      Fmt.Unsigned(o5) & " " & Fmt.Int(o5) & " " &
      Fmt.Unsigned(o6) & " " & Fmt.Int(o6) & " " &
      Fmt.Unsigned(o7) & " " & Fmt.Int(o7) & " " &
      Fmt.Unsigned(offset) & " " & Fmt.Int(offset) & "\n");
      
      Wr.PutText(Stdio.stderr, 
      Fmt.Unsigned(o7) & " " &
      Fmt.Unsigned(o8) & " " &
      Fmt.Unsigned(Word.And(o7, 16_ffff)) & " "  &
      Fmt.Unsigned(Word.And(o8, 16_ffff)) & "\n");
    END;

    firstOp.disp := o7;
    secondOp.disp := o8;
  END SetLdahLdaPair;



(* do the assembly *)
PROCEDURE Assemble (proc: Procedure.T; program: Program.T) : Inst.TPtr
  RAISES {Problem} =
  VAR
    code: Inst.TPtr;
    code_ptr : InstArray;
  BEGIN
    (* Figure out how large the reassembled procedure will be
       and if any new global values are needed, then add them to
       the table. *)

    RelocateProc(proc, program);

    (* global area is allocated in the traced heap and strongref-ed
       it is un-strongref-ed by a weakref procedure for the corresponding 
       procedures.  Strongref keeps a count on the global area so
       it will be released only after the last procedure that uses it
       is cleaned-up *)
    IF program.globals = NIL THEN
      program.globals := NEW(WordArray,
                             MAX(2*program.GPsize+100, 1020));
      (*
      RTIO.PutText("G: "); 
      RTIO.PutAddr(ADR(program.globals[0])); RTIO.PutText("\n");
      *)
    END;

    (*
    StrongRef.Add(program.globals);
    *)

    (* Code is allocated in the traced heap and strongref-ed and weakref-ed.
       We rely on the collector releasing the strongref when no other
       reference exists. The GC will take care of keeping
       the code valid until last reference disappears from the stacks.
       When all references are gone, weakref-s will kick in and
       the clean-up procedure will un-strongref the global area. *)
    code_ptr := NEW(InstArray, MAX(proc.assembled_size*10, 2*1020));
    (*
    RTIO.PutText("P: "); RTIO.PutAddr(ADR(code_ptr[0])); RTIO.PutText("\n");
    StrongRef.Add(code_ptr);
    EVAL WeakRef.FromRef(code_ptr, CleanUpCode);
    INC(WeakCnt);
    *)

    (* We need to make a pass through the instructions and change the 
       places where the gp is set. This should probably be combined with
       some other phase to save time. *)
    FixupGP(proc, program, code_ptr);

    WITH values = program.GPtable.iterate() DO
      VAR
        value: Word.T;
        index: INTEGER;
      BEGIN
        WHILE values.next(value, index) DO
          program.globals[index] := value;
        END;
      END;
    END;

    (*
    Wr.PutText(Stdio.stderr, "New global table at " & 
      Fmt.Unsigned(LOOPHOLE(program.globals, INTEGER)) & "\n");
    FOR i := FIRST(program.globals^) TO LAST(program.globals^) DO
      Wr.PutText(Stdio.stderr, Fmt.Int(8 * i) & "\t" & 
      Fmt.Unsigned(program.globals[i]) & "\n");
    END;
    *)

    code := AssembleProc (proc, program, code_ptr);

    FixupMemory (code);
    RETURN code;
  END Assemble;


(*
VAR
  WeakCnt := 0;

PROCEDURE CleanUpCode(READONLY w: WeakRef.T; r: REFANY) =
  VAR
    code_ptr: REF ARRAY OF Inst.T;
    procPtr: Inst.TPtr;
    globalPtr: REF ARRAY OF Word.T;
    gpval: ADDRESS;
  BEGIN
    DEC(WeakCnt);
    RTIO.PutText("CleanUpCode: "); RTIO.PutInt(WeakCnt); RTIO.PutText("\n");
    IF NOT ISTYPE(r, REF ARRAY OF Inst.T) THEN
      RTIO.PutText("ERROR >> wrong type of code pointer in CleanUpCode\n");
      RETURN;
    END;

    code_ptr := NARROW(r, InstArray);
    procPtr := LOOPHOLE(ADR(code_ptr[0]), Inst.TPtr);
    gpval := Disassembler.GetGP(procPtr);
    RTIO.PutAddr(gpval); RTIO.PutText(" ");
    RTIO.PutAddr(procPtr); RTIO.PutText("\n");
    IF gpval = procPtr THEN
      RTIO.PutText("gp == proc\n");
    ELSIF gpval # NIL THEN
      globalPtr := LOOPHOLE(gpval - ADRSIZE(ADDRESS) - ADRSIZE(Word.T), 
                            WordArray);
      IF ADR(globalPtr[0]) # gpval THEN
        RTIO.PutText("ERROR >> wrong gp pointer in CleanUpCode\n");
        RETURN;
      END;
      VAR
        r : REFANY := globalPtr;
      BEGIN
        IF NOT ISTYPE(r, WordArray) THEN
          RTIO.PutText("ERROR >> wrong type of gp pointer in CleanUpCode\n");
          IF ISTYPE(r, InstArray) THEN
            RTIO.PutText("InstArray\n");
          ELSE
            RTIO.PutInt(TYPECODE(r)); RTIO.PutText("\n");
          END;
          RETURN;
        END;
      END;
      StrongRef.Remove(globalPtr);
    END;
  END CleanUpCode;
*)


PROCEDURE AssembleOne (proc: Procedure.T; program: Program.T): PROCANY
  RAISES {Problem} =
  BEGIN
    RETURN LOOPHOLE(Assemble(proc, program), PROCANY);
  END AssembleOne;


BEGIN
END Assembler.
