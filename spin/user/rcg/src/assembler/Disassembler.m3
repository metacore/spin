(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 04-Nov-97  Wilson Hsieh (whsieh) at the University of Washington
 *	only disassemble one level deep
 *      when numbering call both Sweep and ReverseSweep in case of
 *        weird entry points/RAS
 *
 * 16-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	do not disassemble m3_fault routines -- they do not end with
 *      a ret instruction
 *
 *)


UNSAFE MODULE Disassembler;

IMPORT Word, Fmt, Wr, Stdio, Thread;
IMPORT IntSet;
IMPORT Ir, BasicBlock, Procedure, Program;
IMPORT Instruction, Utils, Register;

<* FATAL Wr.Failure, Thread.Alerted *>
<* FATAL Ir.AlreadyImmediate, Ir.UnknownInstruction *>

CONST
  DoDiagnose = FALSE;
  DoCallGraph = TRUE;

(*
 * limit on procedure size 
 *)
  MAX_PROC_SIZE = 2000;

TYPE
  ErrorCode = {
    UnknownInstruction,
    UnknownFunctionCode,
    UnknownOpcode,
    CallIntoProcedure,
    BranchBeforeBeginning,
    BranchAfterEnd,
    UnknownFormat,
    BasicBlockOverflow,
    ReturnNotFound,
    PCNotInMap
  };

EXCEPTION 
  Error(ErrorCode);

CONST
  Debug = FALSE;

TYPE
  BlockInfo = REF RECORD
    size: CARDINAL;
    data: LabelDataArray;
  END;
  LabelDataArray = REF ARRAY OF LabelData;
  LabelData = RECORD
    set: LabelSet;
    bb: BasicBlock.T;
    (* count of edges in/out *)
    in, out: [0..Word.Shift(1, 32)-1] := 0;
    bottom: BOOLEAN := FALSE;
  END;
  LabelSet = SET OF Label;
  Label = { Start, End };

(*
 * disassemble a single machine instruction
 *)

PROCEDURE DisassembleInst (pc: Instruction.TPtr; inst_offset: CARDINAL;
                           info: BlockInfo; prog: Program.T; gp: ADDRESS)
          RAISES {Error} =
  VAR
    (* target info *)
    op_val : CARDINAL;
    fc_val : CARDINAL;
    idx    : Instruction.Opcode;
    flag   : BOOLEAN;
    inst_val : Instruction.T := pc^;

    (* ir info *)
    ir_op  : Ir.Opcode;
    target : Ir.Instruction;
    bb     : BasicBlock.T;
    jmp    := FALSE;
  BEGIN
    (*
    Wr.PutText(Stdio.stderr, "0x" & Fmt.Addr(pc)) & " : ");
    Instruction.Print(inst_val, Stdio.stderr);
    Wr.PutText(Stdio.stderr, "\n");
    *)

    bb := info.data[inst_offset].bb;

    (* start basic block if necessary *)
    IF bb.instructions = NIL THEN
      bb.instructions := NEW(REF ARRAY OF Ir.Instruction, bb.count);
    END;

    op_val := Instruction.GetOpcode(inst_val);
    idx    := Instruction.FindByOpcode(op_val);

    IF idx # Instruction.Opcode.unknown THEN
      WITH entry = Instruction.DecodeTable[idx] DO
        CASE entry.format OF
        | Instruction.Format.Operate =>
          fc_val := Instruction.GetFunction(inst_val);
          idx    := Instruction.FindByFunction(idx, op_val, fc_val);
          IF idx # Instruction.Opcode.unknown THEN
            ir_op := Instruction.DecodeTable[idx].ir;
            flag := Instruction.GetLitFlag(inst_val);

            IF ir_op = Ir.Opcode.unknown THEN
              Utils.Error ("Unknown instruction " &
                           Instruction.DecodeTable[idx].name);
              RAISE Error(ErrorCode.UnknownInstruction);

            (* Check for special case of copy instruction. *)
            ELSIF ir_op = Ir.Opcode.or AND NOT flag AND
              Instruction.GetRegA(inst_val) = Instruction.GetRegB(inst_val) 
             THEN
              WITH i = LOOPHOLE (target, Ir.CopyInstruction) DO
                i.op := Ir.Opcode.copy;
                i.rsrc := Instruction.GetRegA(inst_val);
                i.rdest := Instruction.GetRegC(inst_val);
                i.unused := 0;
              END;
            ELSIF flag THEN
              WITH i = LOOPHOLE (target, Ir.AluRIInstruction) DO
                i.op := Ir.MakeImmediate(ir_op);
                i.r1 := Instruction.GetRegA(inst_val);
                i.rdest := Instruction.GetRegC(inst_val);
                (* TODO
                i.immed := Word.Extract (Instruction.GetLit(inst_val), 0,
                                         Ir.ImmediateBits);
                *)
                i.immed := Instruction.GetLit (inst_val);
                i.unused := 0;
              END;
            ELSE
              WITH i = LOOPHOLE (target, Ir.AluRRInstruction) DO
                i.op := ir_op;
                i.r1 := Instruction.GetRegA(inst_val);
                i.r2  := Instruction.GetRegB(inst_val);
                i.rdest := Instruction.GetRegC(inst_val);
                i.unused := 0;
              END;
            END;
          ELSE
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name);
            RAISE Error(ErrorCode.UnknownFunctionCode);
          END;

        | Instruction.Format.Memory =>
          ir_op := entry.ir;
          IF ir_op = Ir.Opcode.unknown THEN
            Utils.Error ("Unknown Opcode " & entry.name &
              " in DisassembleInst");
            RAISE Error(ErrorCode.UnknownOpcode);
          END;
(**)          IF ir_op = Ir.Opcode.lda THEN
            (* An lda instruction can become one of three things in 
               the IR. If the source register is zero, then it is a
               load immediate instruction which can load a large constant.
               If the source register is non-zero then it can be
               a lda instruction with the high bit set or cleared. *)
            IF Instruction.GetRegB(inst_val) = ORD(Register.T.zero) THEN
              WITH i = LOOPHOLE (target, Ir.ImmedInstruction),
                   immed = Instruction.GetDisp (inst_val) DO
                i.op    := Ir.Opcode.li;
                i.rdest := Instruction.GetRegA (inst_val);
                IF idx = Instruction.Opcode.ldah THEN
                  i.immed := Word.LeftShift(immed, 16);
                ELSE
                  i.immed := immed;
                END;
              END;
            ELSE 
              WITH i = LOOPHOLE (target, Ir.MemoryInstruction) DO
                i.op := ir_op;
                i.r1 := Instruction.GetRegA (inst_val);
                i.raddr := Instruction.GetRegB(inst_val);
                i.disp := Instruction.GetDisp (inst_val);
                i.high := (idx = Instruction.Opcode.ldah);
              END;
            END;

          ELSE
            WITH i = LOOPHOLE (target, Ir.MemoryInstruction) DO
              i.op    := ir_op;
              i.r1    := Instruction.GetRegA (inst_val);
              i.raddr := Instruction.GetRegB (inst_val);

              (* If we find a gp-relative load, then we want to
                 recalculate the offset to match the new gp of the
                 reassembled code. If this is the first occurrence
                 of the offset, then we want to add a new entry to
                 the growing global table for the disassembled program. *)
              IF i.raddr = ORD(Register.T.gp) THEN
                EVAL Program.GetNewGPValue(
                         prog, 
                         LOOPHOLE(gp + Instruction.GetDisp(inst_val),
                                  UNTRACED REF Word.T)^);
              END;
              i.disp := Instruction.GetDisp (inst_val);

              (* TODO
              i.disp  := Word.Extract (Instruction.GetDisp (inst_val), 0,
                                       Ir.MemoryBits);
              *)
            END;
          END;

        | Instruction.Format.MemoryJMP =>
          fc_val := Instruction.GetBranchPred(inst_val);
          idx    := Instruction.FindByFunction(idx, op_val, fc_val);
          IF idx # Instruction.Opcode.unknown THEN
            ir_op := Instruction.DecodeTable[idx].ir;

            CASE ir_op OF
            | Ir.Opcode.unknown =>
              Utils.Error ("Unknown Opcode " & entry.name &
                " in DisassembleInst");
              RAISE Error(ErrorCode.UnknownOpcode);

            ELSE (* ret or jsr *)
              WITH i = LOOPHOLE (target, Ir.RetInstruction) DO
                i.op := ir_op;
                i.r1       := Instruction.GetRegA(inst_val);
                i.r2       := Instruction.GetRegB(inst_val);
              END;
            END;

            jmp := TRUE;
          ELSE
            Utils.Error ("Unknown function code " & Fmt.Unsigned (fc_val) &
              " for Opcode " & entry.name);
            RAISE Error(ErrorCode.UnknownFunctionCode);
          END;

        | Instruction.Format.Branch =>
          ir_op := Instruction.DecodeTable[idx].ir;

          CASE ir_op OF
          | Ir.Opcode.unknown =>
            Utils.Error ("Unknown Opcode " & entry.name &
              " in DisassembleInst");
            RAISE Error(ErrorCode.UnknownOpcode);
          | Ir.Opcode.call =>
            WITH i = LOOPHOLE (target, Ir.CallInstruction),
                 disp = Instruction.GetBranchDisp (inst_val),
                 dst = inst_offset + disp + 1,
                 target = pc + Instruction.ByteWidth*(disp + 1)
             DO
              (* m3fault is just a procedure, mark it with a special opcode but
                 keep all the information to do a regular procedure call *)
              IF IsM3Fault (target) THEN
                i.op := Ir.Opcode.fault;
                info.data[inst_offset].bb.terminal := TRUE;
              ELSE
                i.op := ir_op;
              END;
              i.reg := Instruction.GetRegA(inst_val);
                
              IF dst < 0 OR dst >= info.size THEN
                IF i.op # Ir.Opcode.fault THEN
                  i.target :=
                      EntryPoint(pc + 4 * Instruction.GetBranchDisp(inst_val) + 4,
                                 prog);
                END;
              ELSE
                Utils.Error ("Call into local procedure at pc " &
                  Fmt.Addr (pc) & " " &
                  Fmt.Int (inst_offset) & " " &
                  Fmt.Int (Instruction.GetBranchDisp (inst_val)) &
                  " " & Fmt.Unsigned (inst_val));
                RAISE Error(ErrorCode.CallIntoProcedure);
              END;
              (*
              Wr.PutText(Stdio.stderr, "### " & Fmt.Int(i.target) & " 0x" & 
                Fmt.Addr(pc+4*Instruction.GetBranchDisp(inst_val)+4) & 
                " 0x" & Fmt.Addr(prog) &
                "\n");
                *)
            END;
          ELSE
            WITH i = LOOPHOLE (target, Ir.BranchInstruction),
                 dst = inst_offset + Instruction.GetBranchDisp(inst_val) + 1
             DO
              i.op := ir_op;
              i.r1   := Instruction.GetRegA(inst_val);

              IF dst < 0 THEN
                Utils.Error("branch before beginning of procedure at offset " &
                  Fmt.Int (inst_offset));
                RAISE Error(ErrorCode.BranchBeforeBeginning);
              ELSIF dst >= info.size THEN
                (*
                Utils.Error ("branch after end of procedure at offset " &
                  Fmt.Int (inst_offset));
                *)
                RAISE Error(ErrorCode.BranchAfterEnd);
              ELSE
                i.target := info.data[dst].bb.index;
              END;
            END;
          END;
          jmp := TRUE;
        ELSE
          Utils.Error ("Unknown format in DisassembleInst");
          RAISE Error(ErrorCode.UnknownFormat);
        END;
      END;
    ELSE
      Utils.Error ("Unknown Opcode " & Fmt.Unsigned(op_val));
      RAISE Error(ErrorCode.UnknownOpcode);
    END;
              
    bb := info.data[inst_offset].bb;
    IF bb.instruction_count < FIRST(bb.instructions^) OR
      bb.instruction_count > LAST(bb.instructions^) THEN
      Utils.Error ("overflow in basic block " & Fmt.Int(bb.index) & "\n");
      RAISE Error(ErrorCode.BasicBlockOverflow);
    END;
    bb.instructions[bb.instruction_count] := target;
    INC(bb.instruction_count);
    
    IF jmp = FALSE AND Label.End IN info.data[inst_offset].set THEN
      IF bb.instruction_count > LAST(bb.instructions^) THEN
        Utils.Error ("overflow in basic block " & Fmt.Int(bb.index) & "\n");
        RAISE Error(ErrorCode.BasicBlockOverflow);
      END;

      (* put in fall-through jump *)
      WITH i = LOOPHOLE (target, Ir.BranchInstruction),
           dst = inst_offset+1 DO
        i.op := Ir.Opcode.br;
        i.r1   := Ir.ZeroRegister;
        IF dst < 0 THEN
          Utils.Error ("branch before beginning of procedure at offset " &
            Fmt.Int (inst_offset));
          RAISE Error(ErrorCode.BranchBeforeBeginning);
        ELSIF dst >= info.size THEN
          (*
            Utils.Error ("branch after end of procedure at offset " &
            Fmt.Int (inst_offset));
          *)
          RAISE Error(ErrorCode.BranchAfterEnd);
        ELSE
          i.target := info.data[dst].bb.index;
        END;
      END;
      bb.instructions[bb.instruction_count] := target;
      INC(bb.instruction_count);
    END;
  END DisassembleInst;


(******************************************************************************
 *
 * disassemble a whole procedure
 *
 *****************************************************************************)

(*
 * find procedure size 
 * currently only small procedures without funny jumps
 * waiting for libexcpt to pickup real sizes
 *)

PROCEDURE GuessProcSize (proc: Instruction.TPtr; count: CARDINAL) : INTEGER =
  BEGIN
    INC (proc, count*Instruction.ByteWidth);

    (* scan for a return *)
    WHILE Instruction.GetOp (proc^) # Instruction.Opcode.ret DO
      INC (proc, Instruction.ByteWidth);
      INC (count);
    END;

    (* add 1 for the ret instruction *)
    RETURN count+1;
  END GuessProcSize; 


(* helper for DisasssembleProc *)
PROCEDURE AddToTable(bb: BasicBlock.T; arg: REFANY) =
  VAR
    table := NARROW(arg, REF ARRAY OF BasicBlock.T);
  BEGIN
    table[bb.index] := bb;
  END AddToTable;

(* helper for DisasssembleProc *)
PROCEDURE CleanupTerminal (bb: BasicBlock.T; <* UNUSED *> arg: REFANY) =
  VAR
    old_succ, succ_pred: BasicBlock.TArray;
    succ: BasicBlock.T;
    count: INTEGER;
  BEGIN
    IF bb.terminal THEN
      old_succ := bb.successors;

      (* fix up this block *)
      bb.successors := NEW (BasicBlock.TArray, 0);

      (* fix up the successors *)
      FOR i := FIRST (old_succ^) TO LAST (old_succ^) DO
        succ := old_succ[i];
        succ_pred := succ.predecessors;
        succ.predecessors := NEW (BasicBlock.TArray, NUMBER (succ_pred^)-1);
        count := 0;
        (* copy the predecessors of the successor, except for bb *)
        FOR j := FIRST (succ_pred^) TO LAST (succ_pred^) DO
          IF succ_pred[j] # bb THEN
            succ.predecessors[count] := succ_pred[j];
            INC (count);
          END;
        END;
      END;
    END;
  END CleanupTerminal;


(* do the disassembly of a single procedure *)
PROCEDURE DisassembleProc(VAR proc: Procedure.T;  (* out parameter *)
                          entry: Instruction.TPtr; index: INTEGER;
                          name: TEXT; prog: Program.T) RAISES {Cannot} =
  VAR
    info : BlockInfo;
    current: Instruction.TPtr;
    nBlocks: CARDINAL;
    size : CARDINAL;

    ldgp : Instruction.TPtr;
    gpval : ADDRESS;
    
    n: INTEGER;
    new_table: REF ARRAY OF BasicBlock.T;

    bottom_found := FALSE;
  BEGIN
    (* do not try to disassemble m3_fault *)
    IF IsM3Fault (entry) THEN
      RETURN;
    END;

    IF proc = NIL THEN
      proc := Procedure.NewT ();
    END;

    IF Debug THEN
      Wr.PutText (Stdio.stderr, "Disassembling " & name & "\n");
    END;

    (* find real entry point *)
    IF Instruction.GetOp (entry^) = Instruction.Opcode.ldah AND
      Instruction.GetRegA (entry^) = ORD (Register.T.gp) AND
      Instruction.GetRegB (entry^) = ORD (Register.T.t12)
     THEN
      ldgp := entry;
    ELSE
      ldgp := entry-2*Instruction.ByteWidth;
      IF Instruction.GetOp (ldgp^) # Instruction.Opcode.ldah OR
        Instruction.GetRegA (ldgp^) # ORD (Register.T.gp) OR
        Instruction.GetRegB (ldgp^) # ORD (Register.T.t12) THEN
        (* leaf procedure - no gp *)
        ldgp := NIL;
      ELSE
        entry := ldgp;
      END;
    END;

    (* set the counter *)
    proc.index := index;
    proc.addr := entry;

    (* The following loop tries to find all the basic blocks of the procedure
       within the given size.  If the size is too small, increase it.
       and the process repeated *)
    size := GuessProcSize (entry, 0);
    WHILE size < MAX_PROC_SIZE DO
      TRY
        info := ScanBasicBlocks (entry, size, nBlocks);
        EXIT;
      EXCEPT 
      | Error(ec) =>
        IF ec = ErrorCode.BranchAfterEnd OR ec = ErrorCode.ReturnNotFound THEN
          size := GuessProcSize (entry, size);
        ELSE
          IF current # NIL THEN
            Wr.PutText (Stdio.stdout, "At pc " & Fmt.Unsigned (LOOPHOLE(current, Word.T)) & "\n");
            Instruction.Print (current^);
            Wr.PutText (Stdio.stdout, "\n");
          END;
          RAISE Cannot;
        END;
      END;
    END;
      
    proc.top := info.data[0].bb;
    proc.bottoms := NEW (REF ARRAY OF BasicBlock.T, 1);
    FOR i := 0 TO size-1 DO
      IF info.data[i].bottom THEN
        IF NOT bottom_found THEN
          proc.bottoms[0] := info.data[i].bb;
          bottom_found := TRUE;
        ELSE
          VAR
            oldsize := NUMBER (proc.bottoms^);
            tmp:= NEW (REF ARRAY OF BasicBlock.T, oldsize+1);
          BEGIN
            SUBARRAY (tmp^, 0, oldsize) := proc.bottoms^;
            proc.bottoms := tmp;
            proc.bottoms[oldsize] := info.data[size-1].bb;
          END;
        END;
      END;
    END;

    (* compute real bottom BB *)
    IF NUMBER (proc.bottoms^) = 1 THEN
      proc.bottom := proc.bottoms[0];
    ELSE
      (* must insert fake BB *)
      proc.bottom := NEW (BasicBlock.T);
      proc.bottom.index := nBlocks;
      proc.bottom.instructions := NEW (Ir.InstructionArray, 0);
      proc.bottom.predecessors := NEW (BasicBlock.TArray, NUMBER (proc.bottoms^));
      proc.bottom.successors := NEW (BasicBlock.TArray, 0);
      INC (nBlocks);

      FOR i := 0 TO LAST (proc.bottoms^) DO
        WITH retBlock = proc.bottoms[i] DO
          proc.bottom.predecessors[i] := retBlock;
          IF NUMBER (retBlock.successors^) # 0 THEN
            <* ASSERT FALSE *>
          END;
          retBlock.successors := NEW (BasicBlock.TArray, 1);
          retBlock.successors[0] := proc.bottom;
        END;
      END;
    END;

    proc.table := NEW(REF ARRAY OF BasicBlock.T, nBlocks);

    (* Calculate the gp of the procedure before disassembling the
       individual instructions. During the disassembly pass, we
       use this value to find out the entries in the global table.
       When we find them, we construct a new table for the reassembled
       program. *)

    IF ldgp # NIL THEN
      (* ldgp loads based on displacement from procedure PC *)
      gpval := ldgp;
      <* ASSERT Instruction.GetOp (ldgp^) = Instruction.Opcode.ldah *>
      <* ASSERT Instruction.GetRegA (ldgp^) = ORD (Register.T.gp) *>
      <* ASSERT Instruction.GetRegB (ldgp^) = ORD (Register.T.t12) *>
      INC (gpval, Word.Shift (Instruction.GetDisp (ldgp^), 16));
      
      INC (ldgp, Instruction.ByteWidth);
      <* ASSERT Instruction.GetOp (ldgp^) = Instruction.Opcode.lda *>
      <* ASSERT Instruction.GetRegA (ldgp^) = ORD (Register.T.gp) *>
      <* ASSERT Instruction.GetRegB (ldgp^) = ORD (Register.T.gp) *>
      INC (gpval, Instruction.GetDisp (ldgp^));
      proc.gp := gpval;
      
      (* find stack frame size *)
      INC (ldgp, Instruction.ByteWidth);
      IF Instruction.GetOp (ldgp^) = Instruction.Opcode.lda AND
        Instruction.GetRegA (ldgp^) = ORD (Register.T.sp) AND
        Instruction.GetRegB (ldgp^) = ORD (Register.T.sp)
       THEN
        (* not a leaf *)
        proc.frame := -Instruction.GetDisp (ldgp^);
      ELSE
        proc.frame := 0;
      END;
    END;

    (* disassemble instructions *)
    FOR i := 0 TO size-1 DO
      current := entry+i*Instruction.ByteWidth;
      TRY
        DisassembleInst(current, i, info, prog, proc.gp);
      EXCEPT
      | Error =>
        Wr.PutText (Stdio.stdout, "At pc " & Fmt.Addr (current) & "\n");
        Instruction.Print (current^);
        Wr.PutText (Stdio.stdout, "\n");
        RAISE Cannot;
      END;
    END;

    TRY
      (* delete outgoing edges from/back edges to terminal nodes *)
      BasicBlock.Sweep (proc.top, CleanupTerminal, NIL);

      (* store the procedure into the map *)
      EVAL prog.map.put (entry, proc);
    
      BasicBlock.Sweep (proc.top, AddToTable, proc.table);
      BasicBlock.ReverseSweep (proc.top, AddToTable, proc.table);
    EXCEPT ELSE
      RAISE Cannot;
    END;

    n := 0;
    FOR i := FIRST(proc.table^) TO LAST(proc.table^) DO
      IF proc.table[i] = NIL THEN
        Wr.PutText (Stdio.stdout, "Missing basic block: " & 
          Fmt.Int(NUMBER(proc.table^)) & " " & Fmt.Int(i) & "\n");
        INC(n);
      END;
    END;
    IF n # 0 THEN
      DEC(nBlocks, n);
      new_table := NEW(REF ARRAY OF BasicBlock.T, nBlocks);
      SUBARRAY(new_table^, 0, nBlocks-1) := 
          SUBARRAY(proc.table^, 0, nBlocks-1);
      proc.table := new_table;
    END;

    IdentifyLoops(proc);
  END DisassembleProc;

(* compute the GP from a pointer to a ldgp instruction *)
PROCEDURE GetGP(ldgp: Instruction.TPtr): ADDRESS =
  VAR
    gpval: ADDRESS;
  BEGIN
    IF Instruction.GetOp (ldgp^) = Instruction.Opcode.ldah AND
      Instruction.GetRegA (ldgp^) = ORD (Register.T.gp) AND
      Instruction.GetRegB (ldgp^) = ORD (Register.T.t12)
     THEN
      (* ldgp loads based on displacement from procedure PC *)
      gpval := ldgp;
      INC (gpval, Word.Shift (Instruction.GetDisp (ldgp^), 16));
    ELSE
      RETURN NIL;
    END;
    
    INC (ldgp, Instruction.ByteWidth);
    IF Instruction.GetOp (ldgp^) = Instruction.Opcode.lda AND
      Instruction.GetRegA (ldgp^) = ORD (Register.T.gp) AND
      Instruction.GetRegB (ldgp^) = ORD (Register.T.gp)
     THEN
      INC (gpval, Instruction.GetDisp (ldgp^));
    ELSE
      RETURN NIL;
    END;

    RETURN gpval;
  END GetGP;
  

(* break up procedure into basic blocks *)
PROCEDURE ScanBasicBlocks (pc: Instruction.TPtr; VAR size: CARDINAL;
                           VAR nBlocks: CARDINAL) : BlockInfo RAISES {Error} =
  (* sets the VAR parameter size to when the first RET instruction is found *)
  VAR
    info: BlockInfo := NEW (BlockInfo);
    op: Instruction.Opcode;
    current: Instruction.TPtr;
    currentBB: BasicBlock.T := NIL;
    prevStart: CARDINAL := 0;
    countBB: CARDINAL := 0;

  (* debugging *)
  PROCEDURE Diagnose () =
    BEGIN
      Wr.PutText (Stdio.stdout,
                  "Basic blocks starting at " &
                  Fmt.Unsigned (LOOPHOLE (pc, Word.T)) & "\n");
      FOR i := 0 TO size-1 DO
        Wr.PutText(Stdio.stdout, Fmt.Int(i) & " ");
        IF Label.Start IN info.data[i].set THEN
          Wr.PutText(Stdio.stdout, "S ");
        END;
        IF Label.End IN info.data[i].set THEN
          Wr.PutText(Stdio.stdout, "E ");
          IF i = LAST(info.data^) THEN
            Wr.PutText(Stdio.stdout, "<NO-FALL-THROUGH> ");
          END;
        END;
        Wr.PutText(Stdio.stdout, "in = " & Fmt.Int(info.data[i].in) & " ");
        Wr.PutText(Stdio.stdout, "out = " & Fmt.Int(info.data[i].out) & "\t");
        current := LOOPHOLE(pc+i*Instruction.ByteWidth, Instruction.TPtr);
        Instruction.Print(current^);
        Wr.PutText (Stdio.stdout, "\n");
      END;
    END Diagnose;

  BEGIN
    (* initialization *)
    info.data := NEW (LabelDataArray, size);
    FOR i := 0 TO size-1 DO
      info.data[i] := LabelData{set:=LabelSet{}, bb:=NIL};
    END;

    (* The first scan marks the instructions which are targets of
       jumps and their predecessors. The number of times an instruction
       is a jump target is also remembered. *)

    FOR i := 0 TO size-1 DO
      current := LOOPHOLE(pc+i*Instruction.ByteWidth, Instruction.TPtr);
      op := Instruction.GetOp(current^);

      IF op = Instruction.Opcode.unknown THEN
        Wr.PutText (Stdio.stderr, "Unknown instruction at pc " &
          Fmt.Addr(current) & "\n");
        Instruction.Print (current^);
        Wr.PutText (Stdio.stdout, "\n");
        RAISE Error(ErrorCode.UnknownInstruction);
      END;

      CASE op OF
      | Instruction.Opcode.br =>
        (* no fall through *)
        (* treat br and bsr as identical instructions since
           according the to the Alpha book, they are. Probably the compiler
           will never generate a bsr for a simple branch, we will
           conservatively assume that it might. *)

        WITH offset = i + Instruction.GetBranchDisp(current^) + 1 DO
          (* mark jump targets *)
          IF offset < 0 THEN
            Utils.Error ("branch to before beginning of procedure at pc " &
              Fmt.Addr (current) & " offset " &
              Fmt.Int (i));
            RAISE Error(ErrorCode.BranchBeforeBeginning);
          ELSIF offset > size-1 THEN
            (*
              Utils.Error ("branch to after end of procedure at pc " &
              Fmt.Addr (current) & " offset " &
              Fmt.Int (i));
            *)
            RAISE Error(ErrorCode.BranchAfterEnd);
          ELSE
            (* the target starts a bb *)
            WITH jump_target = info.data[offset] DO
              jump_target.set := jump_target.set + LabelSet{Label.Start};
              INC(jump_target.in);
            END;

            (* the instruction before must end a bb *)
            IF offset > 0 THEN
              WITH target_pred = info.data[offset-1] DO
                target_pred.set := target_pred.set + LabelSet{Label.End};
              END;
            END;
          END;
        END;

      | Instruction.Opcode.beq,
        Instruction.Opcode.bge,
        Instruction.Opcode.bgt,
        Instruction.Opcode.blbc,
        Instruction.Opcode.blbs,
        Instruction.Opcode.ble,
        Instruction.Opcode.blt,
        Instruction.Opcode.bne =>
        (* there is a fall through *)

        WITH offset = i + Instruction.GetBranchDisp(current^) + 1 DO
          (* mark jump targets *)
          IF offset < 0 THEN
            Utils.Error ("branch to before beginning of procedure at pc " &
              Fmt.Addr (current) & " offset " &
              Fmt.Int (i));
            RAISE Error(ErrorCode.BranchBeforeBeginning);
          ELSIF offset > size-1 THEN
            (*
            Utils.Error ("branch to after end of procedure at pc " &
              Fmt.Addr (current) & " offset " &
              Fmt.Int (i));
            *)
            RAISE Error(ErrorCode.BranchAfterEnd);
          ELSE
            (* the target starts a bb *)
            WITH jump_target = info.data[offset] DO
              jump_target.set := jump_target.set + LabelSet{Label.Start};
              INC(jump_target.in);
            END;

            (* the instruction before must end a bb *)
            IF offset > 0 THEN
              WITH target_pred = info.data[offset-1] DO
                target_pred.set := target_pred.set + LabelSet{Label.End};
              END;
            END;
          END;
        END;

      ELSE
        (* It is not a branch to some instruction in the same procedure,
           so it has no target to mark. *)
      END;
    END;


    (* mark the ends *)
    info.data[0].set := info.data[0].set + LabelSet{Label.Start};
    info.data[size-1].set := info.data[size-1].set + LabelSet{Label.End};

    IF DoDiagnose THEN Diagnose(); END;

    (* The second scan marks the jump and branch instructions that
       form the ends of basic blocks and their successors which start
       basic blocks. *)
    
    (* scan again to build basic block size information *)
    FOR i := 0 TO size-1 DO

      current := pc+i*Instruction.ByteWidth;
      op := Instruction.GetOp(current^);

      CASE op OF
      | Instruction.Opcode.br,
        Instruction.Opcode.jmp =>
        (* An unconditional branch marks the end of a block, while the
           following instruction is the start of another block. There is
           only one outgoing edge from the branch statement. *)

        info.data[i].set := info.data[i].set + LabelSet{Label.End};
        info.data[i].out := 1;

        IF i+1 < size THEN
          info.data[i+1].set := info.data[i+1].set + LabelSet{Label.Start};
        END;

      | Instruction.Opcode.beq,
        Instruction.Opcode.bge,
        Instruction.Opcode.bgt,
        Instruction.Opcode.blbc,
        Instruction.Opcode.blbs,
        Instruction.Opcode.ble,
        Instruction.Opcode.blt,
        Instruction.Opcode.bne =>
        (* Conditional branches mark the end of one block, while their
           successors are the start of another block. There is automatically
           an edge between these two blocks, which is taken when the condition
           is false. The conditional branches also have a second edge whose
           target instruction was determined in the previous pass. *)

        info.data[i].set := info.data[i].set + LabelSet{Label.End};
        info.data[i].out := 2;

        IF i+1 < size THEN
          info.data[i+1].set := info.data[i+1].set + LabelSet{Label.Start};
          INC(info.data[i+1].in);
        END;

      | Instruction.Opcode.bsr,
        Instruction.Opcode.jsr,
        Instruction.Opcode.jsr_co =>
        (* A jump instruction marks the end of a block and its successor
           is the start of another block. Since all of the jump instructions
           place the address of the succeeding instruction into register A,
           it is possible for control to flow to that block. This is 
           represented as an edge between the two blocks. *)

        info.data[i].set := info.data[i].set + LabelSet{Label.End};
        info.data[i].out := 1;

        IF i+1 < size THEN
          info.data[i+1].set := info.data[i+1].set + LabelSet{Label.Start};
          INC(info.data[i+1].in);
        END;

      | Instruction.Opcode.ret =>
        (* Technically, a return instruction also places the address of
           the succeeding instruction into register A, but by convention that
           register is the zero register so control cannot flow back to the
           successor. *)

        info.data[i].set := info.data[i].set + LabelSet{Label.End};
        info.data[i].out := 0;
        info.data[i].bottom := TRUE;

        IF i+1 < size THEN
          info.data[i+1].set := info.data[i+1].set + LabelSet{Label.Start};
        END;

      ELSE
        IF Label.End IN info.data[i].set AND i < size - 1 THEN
          (* A plain old instruction which precedes a jump target
             is now the end of a basic block and we must add an edge
             between it and its successor. *)
          INC(info.data[i].out);
          INC(info.data[i+1].in);
        END;
      END;

      (* At this point, all of the basic block start and end points are
         known for instructions up to and including i. We also know
         the in-degree and out-degree of those blocks. *)

      (* beginning of block *)
      IF Label.Start IN info.data[i].set THEN
        (* new basic block *)
        prevStart := i;
        currentBB := NEW (BasicBlock.T);
        currentBB.index := countBB;
        currentBB.predecessors := NEW (BasicBlock.TArray, info.data[i].in);
        INC(countBB);
      END;
      
      (* end of block *)
      IF Label.End IN info.data[i].set THEN
        currentBB.count := i - prevStart + 1;
        (* allocate space for the possible extra jump required
           the jump to a fall-through block should get stripped
             during assembly
         *)
        currentBB.instructions := NEW (Ir.InstructionArray, currentBB.count+1);
        currentBB.instructions[currentBB.count].op := Ir.Opcode.deleted;
        currentBB.successors := NEW (BasicBlock.TArray, info.data[i].out);
      END;
      
      info.data[i].bb := currentBB;
    END;

    IF DoDiagnose THEN Diagnose(); END;

    (* scan again to build basic block links *)
    FOR i := 0 TO size-1 DO
      current := pc+i*Instruction.ByteWidth;
      op := Instruction.GetOp(current^);

      CASE op OF
      | Instruction.Opcode.br,
        Instruction.Opcode.jmp =>
        (* no fall through *)

        WITH offset = i + Instruction.GetBranchDisp(current^) + 1 DO
          (* We do not need to check that the offset lies in the procedure, 
             it was checked before. *)
          (* find successor *)
          WITH this = info.data[i].bb, successor = info.data[offset].bb DO
            this.successors[0] := successor;
            DEC(info.data[offset].in);
            successor.predecessors[info.data[offset].in] := this;
          END;
        END;
      | Instruction.Opcode.beq,
        Instruction.Opcode.bge,
        Instruction.Opcode.bgt,
        Instruction.Opcode.blbc,
        Instruction.Opcode.blbs,
        Instruction.Opcode.ble,
        Instruction.Opcode.blt,
        Instruction.Opcode.bne =>
        (* there is a fall through *)

        WITH this = info.data[i].bb, fallthrough = info.data[i+1].bb DO 
          this.successors[0] := fallthrough;
          DEC(info.data[i+1].in);
          fallthrough.predecessors[info.data[i+1].in] := this;
          
          WITH offset = i + Instruction.GetBranchDisp(current^) + 1 DO
            (* mark jump targets *)
            WITH successor = info.data[offset].bb DO
              this.successors[1] := successor;
              DEC(info.data[offset].in);
              successor.predecessors[info.data[offset].in] := this;
            END;
          END;
        END;
      | Instruction.Opcode.bsr,
        Instruction.Opcode.jsr,
        Instruction.Opcode.jsr_co =>
        (* subroutine calls *)

        WITH this = info.data[i].bb, fallthrough = info.data[i+1].bb DO
          this.successors[0] := fallthrough;
          DEC(info.data[i+1].in);
          fallthrough.predecessors[info.data[i+1].in] := this;
        END;
      | Instruction.Opcode.ret =>
        (* return has no successors *)

      ELSE
        (* Check for a split at a jump target *)
        (* FIXME: I IGNORE THE RETURN INSTRUCTION *)
        (*
        IF Label.End IN info.data[i].set THEN
          Wr.PutText("# " & Fmt.Int(i) & " " & Fmt.Int(NUMBER(info.data^)) & 
            " " & Fmt.Int(LAST(info.data^)) 
        END;
        *)
        IF Label.End IN info.data[i].set AND i < size-1 THEN
          WITH this = info.data[i].bb, fallthrough = info.data[i+1].bb DO
            this.successors[0] := fallthrough;
            DEC(info.data[i+1].in);
            fallthrough.predecessors[info.data[i+1].in] := this;
          END;
        END;          
      END;
      
    END;

    IF DoDiagnose THEN Diagnose(); END;

    nBlocks := countBB;
    info.size := size;
    RETURN info;
  END ScanBasicBlocks;



(* We use the dragon book algorithm number 10.16 to identify dominator 
   nodes in the flow graph and then note the back edges.  The
   the memory allocation cost of this procedure will be ridiculously high
   since it uses the DynamicSet interface.
*)
PROCEDURE IdentifyLoops(proc: Procedure.T) =
  VAR
    nNodes := NUMBER(proc.table^);
    dominators := NEW(REF ARRAY OF IntSet.T, nNodes);
    fixedPoint : BOOLEAN;
  BEGIN
    (* The initial conditions are that the start node dominates
       itself and all other nodes are dominated by all nodes. *)
    FOR i := FIRST(dominators^) TO LAST(dominators^) DO
      IF i = proc.top.index THEN
        dominators[i] := IntSet.New(nNodes);
        IntSet.Add(i, dominators[i]);
      ELSE
        dominators[i] := IntSet.NewComplete(nNodes);
      END;
    END;

    (* We iterate the dominator computation until reaching a fixed 
       point. The basic computation is that a node N is dominated by
       itself and by any node which dominates all of the predecessors
       of N. *)
    REPEAT
      fixedPoint := TRUE;
      FOR i := FIRST(dominators^) TO LAST(dominators^) DO
        VAR
          bb := proc.table[i];
          newDom : IntSet.T;
        BEGIN
          (* Any block without a predecessor cannot change its set
             of dominators, so do not consider it. *)
          IF NUMBER(bb.predecessors^) # 0 THEN
            newDom := IntSet.NewComplete(nNodes);

            FOR p := FIRST(bb.predecessors^) TO LAST(bb.predecessors^) DO
              newDom := IntSet.Intersection(newDom, 
                                            dominators[bb.predecessors[p].index]);
            END;
            IntSet.Add(i, newDom);
            
            fixedPoint := fixedPoint AND 
                          IntSet.IsEqual(dominators[i], newDom);
            dominators[i] := newDom;
          END;
        END;
      END;
    UNTIL fixedPoint;

    (* Now that we have the dominator information, we can determine where the
       loop heads and tails are. If successor of node N dominates N, then N
       is a loop tail and the successor is the head. *)
    FOR i := FIRST(dominators^) TO LAST(dominators^) DO
      VAR
        bb := proc.table[i];
      BEGIN
        IF NUMBER(bb.successors^) # 0 THEN
          FOR s := FIRST(bb.successors^) TO LAST(bb.successors^) DO
            IF IntSet.IsMember(bb.successors[s].index, dominators[i]) THEN
              proc.has_loops := TRUE;

              bb.isLoopTail := TRUE;
              bb.head := bb.successors[s];
              bb.tail := bb;

              bb.successors[s].isLoopHead := TRUE;
              bb.successors[s].head := bb.successors[s];
              bb.successors[s].tail := bb;
            END;
          END;
        END;
      END;
    END;
  END IdentifyLoops;


(*
  dealing with EntryPoint branches
*)

PROCEDURE EntryPoint (i: Instruction.TPtr; prog: Program.T) : CARDINAL =
  VAR
    proc : Procedure.T;
  BEGIN
    IF NOT prog.map.get (i, proc) THEN
      proc := Procedure.NewT ();

      proc.must_disassemble := prog.disassembling_root;
      proc.index := Program.AddProc (prog, proc);
      proc.addr := i;

      EVAL prog.map.put (i, proc);

      (* HEY!  need to know that jumps between modules jump to
         a different entry point for the same procedure *)
      prog.remaining.addhi (i);
    END;

    RETURN proc.index;
  END EntryPoint;


(* find m3_fault *)
PROCEDURE IsM3Fault (i: Instruction.TPtr) : BOOLEAN =
  VAR
    inst_val : Instruction.T;
  BEGIN
    inst_val := i^;
    IF inst_val # 16_23defff0 THEN RETURN FALSE; END; (* lda sp, -16 (sp) *)
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF inst_val # 16_b75e0000 THEN RETURN FALSE; END; (* stq ra, 0 (sp) *)
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF Instruction.GetOp(inst_val) # Instruction.Opcode.ldq AND 
      Instruction.GetRegA(inst_val) # 1
     THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF inst_val # 16_46100411 THEN RETURN FALSE; END; (* bis a0, a0, a1 *)
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF Instruction.GetOp(inst_val) # Instruction.Opcode.ldq AND 
      Instruction.GetRegA(inst_val) # 16
     THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF Instruction.GetOp(inst_val) # Instruction.Opcode.ldq AND 
      Instruction.GetRegA(inst_val) # 27
     THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF inst_val # 16_6b5b4000 THEN  (* jsr ra, (t12), * *)
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF Instruction.GetOp(inst_val) # Instruction.Opcode.ldah THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF Instruction.GetOp(inst_val) # Instruction.Opcode.lda THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF inst_val # 16_2ffe0000 THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

(*
    inst_val := i^;
    IF inst_val # 16_47ff041f THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);

    inst_val := i^;
    IF inst_val # 16_2ffe0000 THEN
      RETURN FALSE;
    END;
    INC (i, Instruction.ByteWidth);
*)

    RETURN TRUE;
  END IsM3Fault;


(* disassemble a call graph *)
PROCEDURE Disassemble (p: PROCANY; prog: Program.T) : Procedure.T
  RAISES {Cannot} =
  VAR
    proc0 : Procedure.T;
    i : Instruction.TPtr := LOOPHOLE (p, Instruction.TPtr);
  BEGIN
    IF NOT prog.map.get (i, proc0) THEN
      proc0 := Procedure.NewT ();
      proc0.index := Program.AddProc (prog, proc0);
      proc0.addr := i;
      EVAL prog.map.put (i, proc0);
    END;

    prog.disassembling_root := TRUE;
    DisassembleProc (proc0, i, proc0.index,
                     "start " & Fmt.Int (proc0.index),
                     prog);
    prog.disassembling_root := FALSE;

    IF proc0 = NIL THEN
      RAISE Cannot;
    END;

    IF DoCallGraph THEN
      DisassembleRest (prog);
    END;

    RETURN proc0;
  END Disassemble;


(* disassemble un-disassembled procedures *)
PROCEDURE DisassembleRest (prog: Program.T) RAISES {Cannot} =
  VAR
    proc : Procedure.T;
    current : Instruction.TPtr;
  BEGIN
    (* disassemble the call graph *)
    WHILE prog.remaining.size () # 0 DO
      current := prog.remaining.remlo ();
      IF prog.map.get (current, proc) THEN
        
        IF proc.must_disassemble AND proc.top = NIL THEN
          (* there is a fake procedure already present *)
          DisassembleProc (proc, current, proc.index,
                           "procedure " & Fmt.Int (proc.index),
                           prog);
          IF proc = NIL THEN
            RAISE Cannot;
          END;
        END;
      ELSE
        Utils.Error ("pc in Remaining that is not in Map");
        RAISE Cannot;
      END;
    END;
  END DisassembleRest;


(* disassemble one procedure *)
PROCEDURE DisassembleOne(proc: PROCANY; index: INTEGER;
                         name: TEXT; prog: Program.T)
                        : Procedure.T RAISES {Cannot} =
  VAR
    p: Procedure.T;
  BEGIN
    DisassembleProc(p, LOOPHOLE(proc, Instruction.TPtr),
                    index, name, prog);
    RETURN p;
  END DisassembleOne;


BEGIN
END Disassembler.
