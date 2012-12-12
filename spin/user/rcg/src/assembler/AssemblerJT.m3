
(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Run-time code generation for the dispatcher
 *
 * HISTORY
 * 16-Dec-96  Wilson Hsieh (whsieh) at the University of Washington
 *      Created. 
 *
 *)

UNSAFE MODULE AssemblerJT EXPORTS AssemblerPrivate, JumpTable;

IMPORT Wr, Stdio, Thread, Word, Fmt;

IMPORT Procedure, Program, BasicBlock;
IMPORT Instruction AS Inst;
IMPORT Register;
IMPORT Ctypes;
IMPORT Assembler;

<* FATAL Thread.Alerted, Wr.Failure *>

(*
 * jump table generation
 * name is not quite accurate, as the implementation chosen
 *   for selection is not always a jump table
 *
 * "jump table" actually has 3 implementations
 *    following assumes that a branch takes 3 cycles
 *    on the 21064 and 21164, a taken branch causes a single bubble
 *    on the 21164, a mispredicted branch costs 5 cycles
 *      do not know the cost on the 21064
 *    cost is average of 1, 2, 6, 7 (roughly), which is 4 cycles;
 *      assume that prediction helps some (70% of the time)
 *
 *    linear search
 *      this takes 2 instructions/4 cycles per test
 *      average cost is (# of cases) instuctions / 2 * (# of cases) cycles
 *
 *    binary search
 *       c = subtract, g = branch if >0, e = branch if =0
 *
 *      4 cases - 5/10
 *       a: c/g/e/c/e    5/11
 *       b: c/g/e        3/7
 *       c: c/g/c/g/e    5/11
 *       d: c/g/c/g/c/e  6/12
 *
 *      6 cases - 6/12
 *      (a0: c/g/e/c/g/e/c/e 8/18)
 *       a: c/g/e/c/g/e    6/14
 *       b: c/g/e/c/g/c/e  7/15 
 *       c: c/g/e          3/7
 *       d: c/g/c/g/e/c/e  7/15
 *       e: c/g/c/g/e      5/11
 *       f: c/g/c/g/c/e    6/12
 *
 *      9 cases - 7/15
 *       a: c/g/e/c/g/e/c/e     8/18
 *       b: c/g/e/c/g/e         6/14
 *       c: c/g/e/c/g/c/g/e     8/18
 *       d: c/g/e/c/g/c/g/c/e   9/19
 *       e: c/g/e          	3/7
 *       f: c/g/c/g/e/c/e  	7/15
 *       d: c/g/c/g/e      	5/11
 *       e: c/g/c/g/c/g/e  	7/15
 *       f: c/g/c/g/c/g/c/e	8/16
 *
 *      crossover with linear search is at 7 cases
 *      if case table is dense, than the crossover goes
 *        down because some comparison/equality tests can
 *        be eliminated
 *
 *    jump table - 6/11
 *      range test - 3/5
 *      load address and jump - 3/6 (assume data cache hit on jump table)
 *
 *      crossover with linear is at 6 cases
 *
 * equality takes 2 instructions (2 if test against 0) or 4 (3) cycles
 *    compare val
 *    branch if equal
 *
 * binary search takes 3 instructions (2 if against 0) or 7 (6) cycles
 *    compare val
 *    branch if greater
 *    branch if equal
 *
 * range test takes 3 instructions (2 if lower bound is 0) or 5 (4) cycles
 *    compute val-lower bound
 *    compare (val-lower bound) to (upper bound - lower bound)
 *    branch if greater
 *
 *
 * register usage:
 *    comparisons take 1 register to hold comparison result
 *    jump table takes 3:
 *      1 to hold null branch
 *      1 for index into table
 *      1 for size comparison
 *)

CONST
  MinForJumpTable = 4;
  MinForBinSearch = 7;

  verbose = FALSE;

(* assemble jump tables *)


PROCEDURE AssembleJT (jt: T;
                      addr: Inst.TPtr;
                      pc: Word.T;
                      proc: Procedure.T;
                      program: Program.T)
  : CARDINAL RAISES {Assembler.Problem} =
  BEGIN
    IF verbose THEN
      Wr.PutText (Stdio.stderr, "Assembling jump table\n");
    END;
    RETURN DoAssembleJT (jt, addr, pc, proc, program, TRUE);
  END AssembleJT;


PROCEDURE DoAssembleJT (jt: T;
                        addr: Inst.TPtr;
                        pc: Word.T;
                        proc: Procedure.T;
                        program: Program.T;
                        emit: BOOLEAN)
  : CARDINAL RAISES {Assembler.Problem} =
  VAR
    clustering : ClusterInfo;
    jtc : Canonical := Canonicalize (jt);
    instructions := 0;
    br_addr := addr+4;
    cluster_last: CARDINAL;
  BEGIN
    IF jtc.usesTable THEN
      IF (jtc.tmp0 = -1 OR jtc.tmp1 = -1 OR jtc.tmp2 = -1) THEN
        RAISE Assembler.Problem;
      END;
    ELSE
      IF jtc.tmp0 = -1 THEN
        RAISE Assembler.Problem;
      END;
    END;

    clustering := jtc.clustering;
    cluster_last := LAST (clustering^);

    IF jtc.usesTable THEN
      (* load address of instruction stream *)
      IF emit THEN
        WITH inst = LOOPHOLE (addr, Inst.TPtr)^ DO
          Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.br].op_val);
          Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.br].fc_val);
          Inst.SetBranchDisp (inst, 0);
          Inst.SetRegA (inst, jtc.tmp2);
        END;
      END;
      INC (instructions);
    END;

    FOR i := 0 TO cluster_last DO
      (* do linear search through clusters for now *)
      INC (instructions,
           AssembleCluster (jtc,
                            clustering[i].lo, clustering[i].hi,
                            addr+instructions*Inst.ByteWidth,
                            br_addr,
                            addr+jt.size,
                            pc+instructions,
                            proc, program,

                            (* last one? *)
                            i = cluster_last AND jt.else_block = NIL,

                            emit));
    END;

    (* store size away *)
    IF NOT emit THEN
      jt.size := instructions;
    END;

    (* assemble else clause *)
    INC (instructions,
         AssembleArray (jt.else_block,
                        addr+instructions*Inst.ByteWidth,
                        pc+instructions,
                        proc, program,
                        emit));

    RETURN instructions;
  END DoAssembleJT;


PROCEDURE ImplementationType (lo, hi: INTEGER) : Implementation =
  VAR
    count := hi - lo + 1;
  BEGIN
    IF count >= MinForJumpTable THEN
      RETURN Implementation.Table;
    ELSIF count >= MinForBinSearch THEN
      RETURN Implementation.Binary;
    ELSE
      RETURN Implementation.Linear;
    END;
  END ImplementationType;


PROCEDURE AssembleCluster (jtc: Canonical;
                           lo, hi: CARDINAL;
                           addr: Inst.TPtr;
                           br_addr: Inst.TPtr;
                           else_addr: Inst.TPtr;
                           pc: Word.T; proc: Procedure.T; program: Program.T;
                           last: BOOLEAN; emit: BOOLEAN)
  : CARDINAL RAISES {Assembler.Problem} =
  VAR
  BEGIN
    CASE ImplementationType (lo, hi) OF
    | Implementation.Table =>
      RETURN AssembleAsTable (jtc, lo, hi, addr, br_addr, else_addr, pc, proc, program, last, emit);
    | Implementation.Linear =>
      RETURN AssembleAsLinear (jtc, lo, hi, addr, pc, proc, program, last, emit);
    | Implementation.Binary =>
      RAISE Assembler.Problem;  (* should not happen on Alpha *)
    END;
  END AssembleCluster;


(************************* code generation procedures ***********************)

PROCEDURE FitsInSixteenBits (value: INTEGER) : BOOLEAN =
  BEGIN
    RETURN -Word.Shift (1, 15) <= value AND value < Word.Shift (1, 15);
  END FitsInSixteenBits;


(* linear search *)
PROCEDURE AssembleAsLinear (jtc: Canonical;
                            lo, hi: INTEGER;
                            addr: Inst.TPtr;
                            pc: Word.T;
                            proc: Procedure.T;
                            program: Program.T;
                            last: BOOLEAN;
                            emit: BOOLEAN)
  : CARDINAL RAISES { Assembler.Problem } =
  VAR
    successor_bb : BasicBlock.T := jtc.successor;
    block_size, case_size, compare_size : CARDINAL;
    block_start := addr;
    block_pc := pc;
    skip_addr : ADDRESS;
    finish_pc, skip_pc : Word.T;
    value : INTEGER;

    compare_immed := TRUE;
    compare_start : ADDRESS;
  BEGIN
    (* generate each block within the cluster *)
    FOR i := lo TO hi DO
      (* compute number of instructions for comparison *)
      value := jtc.data[i].value;

      (* compute location of next basic block *)
      IF emit THEN
        IF jtc.data[i].next = NIL THEN
          finish_pc := successor_bb.offset;
        ELSE
          finish_pc := NARROW (jtc.data[i].next, BasicBlock.T).offset;
        END;
      END;

      IF 0 <= value AND value <= 16_ff THEN
        compare_size := 2;
      ELSIF FitsInSixteenBits (value) THEN
        compare_size := 3;
        compare_immed := FALSE;
      ELSE
        RAISE Assembler.Problem;
      END;

      (* compile the block -- we do it first so as to get its size *)
      block_size :=
          AssembleArray (jtc.data[i].block,
                         block_start+compare_size*Inst.ByteWidth,
                         block_pc+compare_size,
                         proc, program, emit);
      case_size := block_size + compare_size;
      
      (* generate last instruction first, because we need to know if it
         is generated before we can generate the jump over it *)
      IF (NOT last) OR (i # hi) OR (jtc.data[i].next # NIL) THEN
        (* skip past the rest of the blocks *)
        INC (case_size);
        IF emit THEN
          skip_addr := block_start + (case_size-1)*Inst.ByteWidth;
          skip_pc := block_pc + (case_size-1);
          WITH inst = LOOPHOLE (skip_addr, Inst.TPtr)^ DO
            Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.br].op_val);
            Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.br].fc_val);
            Inst.SetBranchDisp (inst, finish_pc-(skip_pc+1));
            Inst.SetRegA (inst, ORD (Register.T.zero));
          END;
        END;
      END;

      IF emit THEN
        compare_start := block_start;
        IF compare_immed THEN
          (* check if the value matches *)
          WITH inst = LOOPHOLE (compare_start, Inst.TPtr)^ DO
            Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.cmpeq].op_val);
            Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.cmpeq].fc_val);
            Inst.SetLitFlag (inst, TRUE);
            Inst.SetRegA (inst, jtc.input);
            Inst.SetLit (inst, value);
            Inst.SetRegC (inst, jtc.tmp0);
          END;
          INC (compare_start, Inst.ByteWidth);
        ELSE
          (* load the value into a register *)
          WITH inst = LOOPHOLE (compare_start, Inst.TPtr)^ DO
            Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.lda].op_val);
            Inst.SetRegA (inst, jtc.tmp0);
            Inst.SetRegB (inst, ORD (Register.T.zero));
            Inst.SetDisp (inst, value);
          END;
          (* check if the value matches *)
          WITH inst = LOOPHOLE (compare_start+4, Inst.TPtr)^ DO
            Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.cmpeq].op_val);
            Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.cmpeq].fc_val);
            Inst.SetLitFlag (inst, FALSE);
            Inst.SetRegA (inst, jtc.input);
            Inst.SetRegB (inst, jtc.tmp0);
            Inst.SetRegC (inst, jtc.tmp0);
          END;
          INC (compare_start, 2 * Inst.ByteWidth);
        END;

        (* skip past block *)
        WITH inst = LOOPHOLE (compare_start, Inst.TPtr)^ DO
          Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.beq].op_val);
          Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.beq].fc_val);
          Inst.SetBranchDisp (inst, case_size - compare_size);
          Inst.SetRegA (inst, jtc.tmp0);
        END;
      END;
      
      INC (block_start, case_size*Inst.ByteWidth);
      INC (block_pc, case_size);
    END;

    RETURN block_pc - pc;
  END AssembleAsLinear;


(* jump table implementation *)

PROCEDURE AssembleAsTable (jtc: Canonical;
                           lo, hi: INTEGER;
                           addr: Inst.TPtr;
                           br_addr: Inst.TPtr;
                           else_addr: Inst.TPtr;
                           pc: Word.T;
                           proc: Procedure.T;
                           program: Program.T;
                           last: BOOLEAN;
                           emit: BOOLEAN)
  : CARDINAL RAISES { Assembler.Problem } =
  (* table_offset is the number of bytes from the instruction that loads
     the address of its successor, which we keep in register 9,
     which is callee-save
     
     code is generated:
         range check
         load from jump table and jump
         jump table
         code blocks

     jump table calculation needs 2 registers
   *)
  VAR
    low_value := jtc.data[lo].value;
    size := jtc.data[hi].value - low_value + 1;
    block_size : CARDINAL;
    successor_bb : BasicBlock.T := jtc.successor;

    (* default size of header code *)
    header_start := addr;
    header_pc := pc;
    header_size := 7;

    table_offset : CARDINAL;
    table_addr, block_addr, finish_addr, skip_addr : ADDRESS;
    block_pc, finish_pc, skip_pc, next_pc : Word.T;
  BEGIN
    block_pc := pc;
    
    IF Word.LE (size-1, 16_ff) THEN
    ELSIF FitsInSixteenBits (size-1) THEN
      INC (header_size);
    ELSE RAISE Assembler.Problem;
    END;
    
    (* location of the jump table: sequence of longwords *)
    table_addr := addr + header_size*Inst.ByteWidth;
    table_offset := table_addr - br_addr;

    IF emit THEN
      (* compute location of next basic block *)
      finish_pc := successor_bb.offset;
      finish_addr := addr + (finish_pc-pc)*Inst.ByteWidth;

      (* generate header *)
      (* subtract low value from input value *)
      IF Word.LE (low_value, 16_ff) THEN
        WITH inst = LOOPHOLE (header_start, Inst.TPtr)^ DO
          Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.subq].op_val);
          Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.subq].fc_val);
          Inst.SetLitFlag (inst, TRUE);
          Inst.SetRegA (inst, jtc.input);
          Inst.SetLit (inst, low_value);
          Inst.SetRegC (inst, jtc.tmp0);
        END;
        INC (header_start, Inst.ByteWidth);
        INC (header_pc);
      ELSIF FitsInSixteenBits (-low_value) THEN
        (* load the value into a register *)
        WITH inst = LOOPHOLE (header_start, Inst.TPtr)^ DO
          Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.lda].op_val);
          Inst.SetRegA (inst, jtc.tmp0);
          Inst.SetRegB (inst, jtc.input);
          Inst.SetDisp (inst, -low_value);
        END;
        INC (header_start, Inst.ByteWidth);
        INC (header_pc, 1);
      ELSE
        RAISE Assembler.Problem;
      END;

      (* check input-low against the size of the jump table *)
      IF Word.LE (size-1, 16_ff) THEN
        WITH inst = LOOPHOLE (header_start, Inst.TPtr)^ DO
          Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.cmpule].op_val);
          Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.cmpule].fc_val);
          Inst.SetLitFlag (inst, TRUE);
          Inst.SetRegA (inst, jtc.tmp0);
          Inst.SetLit (inst, size-1);
          Inst.SetRegC (inst, jtc.tmp1);
        END;
        INC (header_start, Inst.ByteWidth);
        INC (header_pc);
      ELSIF FitsInSixteenBits (size-1) THEN
        (* load the value into a register *)
        WITH inst = LOOPHOLE (header_start, Inst.TPtr)^ DO
          Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.lda].op_val);
          Inst.SetRegA (inst, jtc.tmp1);
          Inst.SetRegB (inst, ORD (Register.T.zero));
          Inst.SetDisp (inst, size-1);
        END;
        WITH inst = LOOPHOLE (header_start+4, Inst.TPtr)^ DO
          Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.cmpule].op_val);
          Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.cmpule].fc_val);
          Inst.SetLitFlag (inst, FALSE);
          Inst.SetRegA (inst, jtc.tmp0);
          Inst.SetRegB (inst, jtc.tmp1);
          Inst.SetRegC (inst, jtc.tmp1);
        END;
        INC (header_start, 2*Inst.ByteWidth);
        INC (header_pc, 2);
      ELSE RAISE Assembler.Problem;
      END;

      (* otherwise load from jump table *)
      WITH inst = LOOPHOLE (header_start+4, Inst.TPtr)^ DO
        Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.s4addq].op_val);
        Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.s4addq].fc_val);
        Inst.SetRegA (inst, jtc.tmp0);
        Inst.SetRegB (inst, jtc.tmp2);
        Inst.SetRegC (inst, jtc.tmp0);
      END;
      WITH inst = LOOPHOLE (header_start+8, Inst.TPtr)^ DO
        Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.ldl].op_val);
        Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.ldl].fc_val);
        Inst.SetRegA (inst, jtc.tmp0);
        Inst.SetRegB (inst, jtc.tmp0);
        Inst.SetDisp (inst, table_offset);
      END;

      (* add back br_addr *)
      WITH inst = LOOPHOLE (header_start+12, Inst.TPtr)^ DO
        Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.addq].op_val);
        Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.addq].fc_val);
        Inst.SetRegA (inst, jtc.tmp0);
        Inst.SetRegB (inst, jtc.tmp2);
        Inst.SetRegC (inst, jtc.tmp0);
      END;

      (* jump through address *)
      WITH inst = LOOPHOLE (header_start+16, Inst.TPtr)^ DO
        Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.jsr].op_val);
        Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.jsr].fc_val);
        Inst.SetBranchDisp (inst, 0);
        Inst.SetRegA (inst, ORD (Register.T.zero));
        Inst.SetRegB (inst, jtc.tmp0);
        (* SET HINT *)
      END;      
    END;

    (* the first block *)
    block_addr := table_addr + size*4;
    block_pc := pc + header_size + size;

    (* fill in the jump table default values *)
    IF emit THEN
      FOR i := jtc.data[lo].value TO jtc.data[hi].value DO
        IF NOT last OR jtc.else_block = NIL THEN
          LOOPHOLE (table_addr+(i-low_value)*4, REF Ctypes.int)^ :=
              finish_addr-br_addr;
        ELSE
          LOOPHOLE (table_addr+(i-low_value)*4, REF Ctypes.int)^ :=
              else_addr-br_addr;
        END;
      END;
    END;

    (* generate the blocks first, so that we know where the code finishes *)
    FOR i := lo TO hi DO
      (* backpatch the jump table *)
      IF emit THEN
        LOOPHOLE (table_addr+(jtc.data[i].value-low_value)*4, REF Ctypes.int)^
          := block_addr-br_addr;

        (* compute location of next basic block *)
        IF jtc.data[i].next = NIL THEN
          next_pc := successor_bb.offset;
        ELSE
          next_pc := NARROW (jtc.data[i].next, BasicBlock.T).offset;
        END;
      END;

      (* compile the block *)
      block_size :=
          AssembleArray (jtc.data[i].block, block_addr, block_pc, proc,
                         program, emit);

      IF (NOT last) OR (i # hi) OR (jtc.data[i].next # NIL) THEN
        IF emit THEN
          (* skip past rest of blocks *)
          skip_addr := block_addr + block_size*Inst.ByteWidth;
          skip_pc := block_pc + block_size;
          WITH inst = LOOPHOLE (skip_addr, Inst.TPtr)^ DO
            Inst.SetOpcode (inst, Inst.DecodeTable[Inst.Opcode.br].op_val);
            Inst.SetFunction (inst, Inst.DecodeTable[Inst.Opcode.br].fc_val);
            Inst.SetBranchDisp (inst, next_pc-(skip_pc+1));
            Inst.SetRegA (inst, ORD (Register.T.zero));
          END;
        END;
        INC (block_addr, (1+block_size)*Inst.ByteWidth);
        INC (block_pc, 1+block_size);
      ELSE
        (* no skip instruction needed *)
        INC (block_addr, block_size*Inst.ByteWidth);
        INC (block_pc, block_size);
      END;
    END;

    IF emit THEN
      (* put in skip past jump table *)
      WITH inst = LOOPHOLE (header_start, Inst.TPtr)^ DO
        Inst.SetOpcode(inst, Inst.DecodeTable[Inst.Opcode.beq].op_val);
        Inst.SetFunction(inst, Inst.DecodeTable[Inst.Opcode.beq].fc_val);
        IF last THEN
          Inst.SetBranchDisp (inst, successor_bb.offset-(header_pc+1));
        ELSE
          Inst.SetBranchDisp (inst, block_pc-(header_pc+1));
        END;
        Inst.SetRegA (inst, jtc.tmp1);
      END;
    END;
    RETURN block_pc - pc;
  END AssembleAsTable;


PROCEDURE InstructionCountJT (jt: T) : CARDINAL RAISES {Assembler.Problem} =
  BEGIN
    RETURN DoAssembleJT (jt, NIL, 0, NIL, NIL, FALSE);
  END InstructionCountJT;


BEGIN
END AssemblerJT.
