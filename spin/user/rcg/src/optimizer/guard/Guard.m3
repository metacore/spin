
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
 * 03-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *
 *)


UNSAFE MODULE Guard;

IMPORT Wr, Stdio, Utils, Thread, Fmt, Word;
IMPORT Ir, JumpTable, BasicBlock, Procedure, Program;
IMPORT Expression, ExpressionPrivate;
IMPORT Register, Assembler, Disassembler;
IMPORT Analyzer, Analysis, Available, Reaching, Fast, Flow;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  verbose = FALSE;

(* analyze a single guard for optimization *)
PROCEDURE AnalyzeGuard (p: PROCANY; program: Program.T) : Procedure.T
  RAISES {Cannot} =
  VAR
    proc : Procedure.T;
    available := Available.NewT ();
    reaching : Reaching.T;
  BEGIN
    IF p = NIL THEN RETURN NIL; END;

    TRY
      proc := Disassembler.Disassemble (p, program);
      IF proc.has_loops THEN
        RAISE Cannot;
      END;

      Analyzer.Label (proc);
      Analyzer.Visit (proc, available);
      Available.LocateCalls (proc, program);

      IF FALSE THEN
        Procedure.Print (proc);
      END;

      Flow.Dominators (proc);
      Flow.Postdominators (proc);
      Flow.Edges (proc);
      Flow.CD (proc);
      Flow.ReverseTopologicalSort (proc);

      IF FALSE THEN
        Flow.Print (proc);
      END;

      Available.Condition (proc);

      reaching := Reaching.NewT (proc.instructions.size);
      Analyzer.Visit (proc, reaching);

      IF FALSE THEN
        Analyzer.Print (proc, reaching);
      END;

      (* compute the expression representing the result *)
      Reaching.Result (proc, available);

      IF FALSE THEN
        Procedure.Print (proc);
      END;
    EXCEPT ELSE
      RAISE Cannot;
    END;

    RETURN proc;
  END AnalyzeGuard;


PROCEDURE OptimizeAll (guards: REF ARRAY OF PROCANY)
  : PROCANY RAISES {Cannot,Problem} =
  BEGIN
    RETURN OptimizeGuards (guards, NUMBER (guards^));
  END OptimizeAll;


(* guard optimizations *)
PROCEDURE OptimizeGuards (guards: REF ARRAY OF PROCANY; count: INTEGER)
  : PROCANY RAISES {Cannot,Problem} =
  VAR
    expr: Expression.T := NIL;
    proc : Procedure.T;
    program := Program.NewT ();
    p : PROCANY;
    procs := NEW (REF ARRAY OF Procedure.T, count);
    wildCount : CARDINAL := 0;
    firstProc : Procedure.T := NIL;
  BEGIN
    IF count > 64 THEN RAISE Cannot; END;
    IF count = 1 THEN RETURN guards[0]; END;

    (* analyze each guard *)
    FOR i := 0 TO count-1 DO
      IF verbose THEN
        Wr.PutText (Stdio.stderr, "Analyzing guard " & Fmt.Int (i) & "\n");
      END;
      procs[i] := AnalyzeGuard (guards[i], program);
    END;

    TRY
      (* combine the resulting expressions *)
      FOR i := 0 TO count-1 DO
        IF firstProc = NIL AND procs[i] # NIL THEN
          firstProc := procs[i];
        END;

        IF verbose THEN
          Wr.PutText (Stdio.stdout, "Expr for guard " & Fmt.Int (i) & "\n");
          Expression.Print (Procedure.Returns (procs[i]));
        END;
        expr := Expression.Join (expr, Procedure.Returns (procs[i]), i);
        IF verbose THEN
          Wr.PutText (Stdio.stdout, "Join after guard " & Fmt.Int (i) & "\n");
          Expression.Print (expr);
        END;
      END;
    EXCEPT
      Expression.NoMatch => RAISE Cannot;
    ELSE
      RAISE Problem;
    END;


    (* remove redundant wildcard expressions *)
    wildCount := Expression.Collapse (expr);
    IF verbose THEN
      Wr.PutText (Stdio.stderr,
                  "Merged expr is:\n" & Expression.ToText (expr) & "\n");
    END;


    proc := Procedure.Copy (firstProc, wildCount);

    (* this is the rewrite *)
    IF verbose THEN
      Wr.PutText(Stdio.stdout, "Generating merged guard\n");
    END;
    GenerateGuard (proc, expr);

    TRY
      IF verbose THEN
        Wr.PutText(Stdio.stdout, "Running Fast.Reallocate\n");
      END;
      Fast.Reallocate (proc);
    EXCEPT
      Analysis.Problem => RAISE Problem;
    END;
    
    IF verbose THEN
      Wr.PutText(Stdio.stdout, "Assembling merged guard\n");
    END;

    TRY
      p := Assembler.AssembleOne (proc, program);
    EXCEPT
      Assembler.Problem => RAISE Problem;
    END;

    IF p = NIL THEN
      Wr.PutText(Stdio.stdout, "AssembleOne failed\n");
      RETURN NIL; 
    END;

    IF TRUE THEN
      Wr.PutText (Stdio.stdout, "Final merged guard at addr 0x" & Fmt.Addr (p) & "\n");
    END;

    IF verbose THEN
      Procedure.Print (proc);
    END;

    RETURN p;
  END OptimizeGuards;


(* generate the new guard *)
PROCEDURE GenerateGuard (p: Procedure.T; e: Expression.T) RAISES {Cannot, Problem} =
  VAR
    free: [0..31];
  BEGIN
    Analyzer.Label (p);

    TRY
      IF p.frame > 0 THEN
        free := Register.FindCalleeSave (p.registers);
      ELSE
        free := Register.FindCallerSave (p.registers);
      END;
    EXCEPT ELSE
      RAISE Cannot;
    END;

    (* rewrite guard with free as the extra register *)
    (* this goes first so as to not get screwed up by the next two calls *)
    RewriteGuard (p, e, free);

    (* initialize register *)
    InitRegister (p, free);

    (* rewrite the return to move free to r0 *)
    RewriteReturn (p, free);
  END GenerateGuard;


(* save register in prologue *)
PROCEDURE InitRegister (p: Procedure.T; free: [0..31]) =
  VAR
    top: BasicBlock.T := p.top;
    init : ARRAY [0..0] OF Ir.Instruction;

  PROCEDURE FindFrame (VAR i: Ir.Instruction; <* UNUSED *> r: REFANY) =
    VAR
      ia: ARRAY [0..1] OF Ir.Instruction;
      slot: INTEGER;
    BEGIN
      IF i.op = Ir.Opcode.lda THEN
        WITH inst = LOOPHOLE (i, Ir.MemoryInstruction) DO
          IF inst.r1 = ORD (Register.T.sp) AND inst.raddr = ORD (Register.T.sp) THEN
            (*
            Wr.PutText (Stdio.stdout, "disp is " & Fmt.Int (-inst.disp) & " frame is " & Fmt.Int (p.frame) & "\n");
            *)
            <* ASSERT p.frame = -inst.disp *>
            
            INC (p.frame, 16);  (* frame sizes must be multiple of 16 *)
            DEC (inst.disp, 16); (* stack grows down *)
            slot := p.frame-8;

            WITH store = LOOPHOLE (ia[0], Ir.MemoryInstruction) DO
              store.op := Ir.Opcode.stq;
              store.r1 := free;
              store.raddr := ORD (Register.T.sp);
              store.disp := slot;
              store.index := -1;
            END;
            ia[1] := init[0];

            Ir.AddInstructionsAfter2 (i, ia);

            (* OK, done *)
            RETURN;
          END;
        END;
      END;
    END FindFrame;
    
  BEGIN
    init[0].op := Ir.Opcode.subqi;
    WITH first = LOOPHOLE (init[0], Ir.AluRIInstruction) DO
      first.rdest := free;
      first.r1 := ORD (Register.T.zero);
      first.immed := 1;
      first.index := -1;
    END;

    IF p.frame > 0 THEN
      (* rewrite register push instruction *)
      BasicBlock.InstructionSweep (top, FindFrame, NIL);
    ELSE
      Ir.AddInstructionsBefore2 (top.instructions[0], init);
    END;

  END InitRegister;


(* fix up the guard to match the expression *)
PROCEDURE RewriteGuard (p: Procedure.T; e: Expression.T; free: [0..31])
  RAISES {Cannot, Problem} =
  VAR
    index : INTEGER;
  BEGIN
    TYPECASE e OF
    | Expression.Wildcard =>
      (* hmm *)
      RETURN;
    | Expression.Arg, Expression.Immed, Expression.Reg, Expression.Bottom =>
      RETURN;
    | Expression.Phi =>
      (* hmm *)
      RETURN;
    | Expression.Call (e1c) =>
      FOR i := 0 TO e1c.last DO
        RewriteGuard (p, e1c.args[i], free);
      END;
    | Expression.Compound (e1c) =>
      IF TYPECODE (e1c.right) = TYPECODE (Expression.Wildcard) THEN
        index := e1c.def;
        DoRewrite (p, e1c, index, free);
      ELSE
        RewriteGuard (p, e1c.left, free);
        RewriteGuard (p, e1c.right, free);
      END;
    ELSE
      (* uh oh *)
      Utils.Error ("in GenerateInternal " & e.toText ());
      RAISE Problem;
    END;
  END RewriteGuard;


(* process a compound expression whose rhs is a wildcard *)
PROCEDURE DoRewrite (p: Procedure.T; expr: Expression.Compound;
                     index: INTEGER; free: [0..31]) RAISES {Cannot,Problem} =
  (* requires:
       rhs of expr is an Expression.Wildcard
       index is non-negative, and is the index that corresponds to expr
   *)

  VAR
    map : Procedure.IndexArray := p.instructions.map;
    source: BasicBlock.T;

    insts, newArray : Ir.InstructionArray;
    val : INTEGER;
    wild : Expression.Wildcard;

    jtarray: ARRAY [0..1] OF Ir.Instruction;
          
  PROCEDURE DoIt (VAR inst:Ir.Instruction; <* UNUSED *> u: REFANY)
    RAISES {BasicBlock.Done} =
    VAR
      input, output : CARDINAL;(* registers to use in jump table *)
      const_loaded := -1;      (* register that constant is loaded into *)
      newjt : JumpTable.T;
      (*
        truebb -- basic block to jump to, from within jump table hits
        fallbb -- basic block to fall through to
      *)
      truebb, fallbb : BasicBlock.T := NIL;
      newbb: BasicBlock.T;

      firstconst : INTEGER;
      constindex : INTEGER;
      
      replace_branch := FALSE;

      wild_value : INTEGER;
      guard_offset: CARDINAL;
      
      is_negated: BOOLEAN := expr.op = Expression.Opcode.cmpne;
      val_done: REF ARRAY OF BOOLEAN;

      free2: CARDINAL;
      tmpArray : Ir.InstructionArray;
      
    PROCEDURE FindConst (VAR i: Ir.Instruction; <* UNUSED *> r: REFANY) =
      BEGIN
        IF i.index # constindex THEN RETURN; END;
        <* ASSERT i.op = Ir.Opcode.li *>
        WITH immedinst = LOOPHOLE (i, Ir.ImmedInstruction) DO
          i.op := Ir.Opcode.deleted;
          const_loaded := immedinst.rdest;
        END;
      END FindConst;
              
    BEGIN
      IF inst.index # index THEN RETURN; END;
      
      (* find instruction to replace with jump table
         if instruction is rr instruction,
         find the instruction that puts the constant
         (assume it is an immediate size) into a register
         delete it  -- ONLY WORKS IF REGISTER IS NO LONGER LIVE
         replace rr instruction with jump table
         
         if instruction is ri instruction,
         
         
         input register is from instruction
         put output in free *)

      (* all constants will be based around offsets from first *)
      firstconst := wild.values[0].immed.value;

      CASE inst.op OF
      | Ir.Opcode.cmpeq =>
        (* instruction is a reg/reg operation
           delete load of immediate,
           assume that leftmost constant is the original
        *)
        
        WITH i = LOOPHOLE (inst, Ir.AluRRInstruction) DO
          (* if constant is zero then there is no constant-creating
             instruction *)

          IF firstconst = 0 THEN
            IF i.r1 = ORD (Register.T.zero) THEN
              input := i.r2;
            ELSE
              input := i.r1;
            END;
            output := i.rdest;

          ELSE
            constindex := wild.values[0].immed.def;
            <* ASSERT constindex # index *>
            BasicBlock.InstructionSweep (map[constindex], FindConst, NIL);
            
            IF i.r1 = const_loaded THEN
              input := i.r2;
            ELSE
              input := i.r1;
            END;
            output := i.rdest;

          END;
        END;

        IF is_negated THEN
          <* ASSERT FALSE *>
        END;

      | Ir.Opcode.cmpeqi =>

        (* instruction is a reg/immed operation
           save the register for further use
           and replace the instruction with a jump table
        *)

        WITH i = LOOPHOLE (inst, Ir.AluRIInstruction) DO
          input := i.r1;
          output := i.rdest;
        END;

        IF is_negated THEN
          <* ASSERT FALSE *>
        END;

      | Ir.Opcode.bne =>

        replace_branch := TRUE;

        WITH i = LOOPHOLE (inst, Ir.BranchInstruction) DO
          input := i.r1;
          truebb := source.successors[0];
          fallbb := p.table[i.target];
          <* ASSERT fallbb = source.successors[1] *>
        END;

      | Ir.Opcode.beq =>

        replace_branch := TRUE;

        WITH i = LOOPHOLE (inst, Ir.BranchInstruction) DO
          input := i.r1;
          truebb := p.table[i.target];
          fallbb := source.successors[0];
          <* ASSERT truebb = source.successors[1] *>
        END;

      ELSE
        (* uh oh *)
        Wr.PutText (Stdio.stderr, "Problem with instruction:\n");
        Ir.PrintInstruction (inst, Stdio.stderr);
        RAISE Cannot;
      END;

      newjt := JumpTable.NewT (input);
      LOOPHOLE (jtarray[0], Ir.JtInstruction).op := Ir.Opcode.jt;
      LOOPHOLE (jtarray[0], Ir.JtInstruction).table := newjt;
      LOOPHOLE (jtarray[1], Ir.BranchInstruction).op := Ir.Opcode.br;
      LOOPHOLE (jtarray[1], Ir.BranchInstruction).r1 := ORD (Register.T.zero);

      (* delete instruction *)
      inst.op := Ir.Opcode.deleted;

      IF replace_branch THEN
        LOOPHOLE (jtarray[1], Ir.BranchInstruction).target := fallbb.index;
        JumpTable.SetSuccessor (newjt, fallbb);
      ELSE
        (* split bb *)
        newbb := BasicBlock.Split (source, index, p);
        
        LOOPHOLE (jtarray[1], Ir.BranchInstruction).target := newbb.index;
        JumpTable.SetSuccessor (newjt, newbb);
      END;
      Ir.AddInstructionsAfter2 (inst, jtarray);

      (* build jump table now
      *)
      TRY
        free2 := Register.FindCallerSave (p.registers, ignore := 1);
                
        (* keep track of done values *)
        val_done := NEW (REF ARRAY OF BOOLEAN, wild.next);
        FOR i := 0 TO wild.next-1 DO
          val_done[i] := FALSE;
        END;

        FOR i := 0 TO wild.next-1 DO
          wild_value := wild.values[i].immed.value;
          guard_offset := wild.values[i].offset;

          IF NOT val_done[i] THEN
            (* check if the value repeats *)
            IF NOT Recurs (wild_value, wild.values, i, wild.next-1) THEN
              newArray := AndConstant (guard_offset, free);
              
              IF NOT replace_branch THEN
                VAR
                  tmpArray := NEW (Ir.InstructionArray, 1);
                BEGIN
                  WITH inst = LOOPHOLE (tmpArray[0], Ir.AluRIInstruction) DO
                    inst.op := Ir.Opcode.addqi;
                    inst.r1 := output;
                    inst.immed := 1;
                    inst.rdest := output;
                  END;
                  newArray := Ir.JoinArrays (newArray, tmpArray);
                END;
              END;
              
            ELSE
              newArray := BuildMask (wild_value, wild.values, wild.next-1,
                                     free2, val_done);
              
              tmpArray := NEW (Ir.InstructionArray, 1);
              WITH inst = LOOPHOLE (tmpArray[0], Ir.AluRRInstruction) DO
                inst.op := Ir.Opcode.and;
                inst.r1 := free;
                inst.r2 := free2;
                inst.rdest := free;
              END;
              newArray := Ir.JoinArrays (newArray, tmpArray);
              
              IF NOT replace_branch THEN
                tmpArray := NEW (Ir.InstructionArray, 1);
                WITH inst = LOOPHOLE (tmpArray[0], Ir.AluRIInstruction) DO
                  inst.op := Ir.Opcode.addqi;
                  inst.r1 := output;
                  inst.immed := 1;
                  inst.rdest := output;
                END;
                newArray := Ir.JoinArrays (newArray, tmpArray);
              END;
            END;

            IF replace_branch THEN
              val := wild_value - firstconst;
            ELSE
              val := wild_value;
            END;
            JumpTable.Insert (newjt, val, newArray, truebb);
          END;
        END;

        (* insert else branch *)
        newArray := ElseMask (wild.values, wild.next-1, free2, output);
        JumpTable.InsertElse (newjt, newArray);
      EXCEPT ELSE
        RAISE Cannot;
      END;
      RAISE BasicBlock.Done;
    END DoIt;

  BEGIN
    (* check error conditions *)
    IF index = -1 THEN RAISE Problem; END;
    TRY
      wild := NARROW (expr.right, Expression.Wildcard);
    EXCEPT ELSE
      RAISE Problem;
    END;
    
    IF expr.op # Expression.Opcode.cmpeq AND
      expr.op # Expression.Opcode.cmpne THEN
      RAISE Cannot;
    END;
    
    source := map[index];
    insts := source.instructions;

    BasicBlock.InstructionSweep (source, DoIt, NIL);
  END DoRewrite;


(* rewrite ret instructions so that free gets copied to the return register *)
PROCEDURE RewriteReturn (p: Procedure.T; free: [0..31]) =
  VAR
    last_index: CARDINAL;
    copyArray : ARRAY [0..1] OF Ir.Instruction;
    frame := FALSE;
  BEGIN
    (* insert a copy before return *)
    FOR i := 0 TO LAST (p.bottoms^) DO
      WITH lastBB = p.bottoms[i] DO
        last_index := lastBB.instruction_count-1;

        WHILE last_index >= 0 DO
          WITH last_inst = lastBB.instructions[last_index] DO
            IF last_inst.op # Ir.Opcode.ret THEN
              DEC (last_index);
            ELSE
              EXIT;
            END;
          END;
        END;

        IF last_index >= 1 THEN
          WITH second_last = lastBB.instructions[last_index-1] DO
            IF second_last.op = Ir.Opcode.addqi THEN
              WITH second_last_real = LOOPHOLE (second_last, Ir.AluRIInstruction) DO
                IF second_last_real.r1 = ORD (Register.T.sp) AND
                  second_last_real.rdest = ORD (Register.T.sp) AND
                  second_last_real.immed = p.frame-16 THEN
                  
                  (* fix frame size *)
                  second_last_real.immed := p.frame;
                  frame := TRUE;
                END;
              END;
            END;
          END;
        END;

	WITH copy = LOOPHOLE (copyArray[0], Ir.AluRRInstruction) DO
	  copy.op := Ir.Opcode.cmovne;
	  copy.r1 := ORD (Register.T.v0);
          copy.r2 := free;
	  copy.rdest := ORD (Register.T.v0);
	  copy.unused := 0;
	  copy.index := -1;
	  copy.ignore := NIL;
	END;

        IF frame THEN
          WITH restore = LOOPHOLE (copyArray[1], Ir.MemoryInstruction) DO
            restore.op := Ir.Opcode.ldq;
            restore.raddr := ORD (Register.T.sp);
            restore.r1 := free;
            restore.disp := p.frame-8;
            restore.index := -1;
            restore.ignore := NIL;
          END;
          Ir.AddInstructionsBefore (lastBB.instructions, last_index-1, copyArray);
        ELSE
          copyArray[1].op := Ir.Opcode.deleted;
          Ir.AddInstructionsBefore (lastBB.instructions, last_index, copyArray);
        END;
      END;
    END;
  END RewriteReturn;


PROCEDURE Recurs (value: INTEGER; immeds: ExpressionPrivate.ImmedArray;
                  start: CARDINAL; last: CARDINAL)
  : BOOLEAN =
  BEGIN
    FOR i := start+1 TO last DO
      IF value = immeds[i].immed.value OR immeds[i].mark THEN
        RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END Recurs;


PROCEDURE BuildMask (val: INTEGER; immeds: ExpressionPrivate.ImmedArray;
                     last: CARDINAL; reg: CARDINAL;
                     val_done: REF ARRAY OF BOOLEAN)
  : Ir.InstructionArray =
  VAR
    count := 0; total := 0;
    indexArray : REF ARRAY OF CARDINAL;
    ia, iatmp: Ir.InstructionArray;
  BEGIN
    (* brute force *)
    FOR i := 0 TO last DO
      IF (immeds[i].immed.value = val AND NOT immeds[i].mark) OR 
        (immeds[i].immed.value # val AND immeds[i].mark) THEN
        INC (count);
        val_done[i] := TRUE;
      END;
    END;
    total := count;
    count := 0;
    indexArray := NEW (REF ARRAY OF CARDINAL, total);

    Wr.PutText (Stdio.stdout, "mask for value " & Fmt.Int (val) & "\n");

    FOR i := 0 TO last DO
      IF (immeds[i].immed.value = val AND NOT immeds[i].mark) OR
       (immeds[i].immed.value # val AND immeds[i].mark) THEN
        indexArray[count] := i;
        Wr.PutText (Stdio.stdout, "element " & Fmt.Int (i) & " is on\n");
        INC (count);
      END;
    END;
    <* ASSERT total = count *>
    <* ASSERT total > 0 *>

    ia := LoadConstant (indexArray[0], reg);
    FOR i := 1 TO total-1 DO
      iatmp := LoadConstant (indexArray[i], ORD (Register.T.at));  (* cheat? *)
      ia := Ir.JoinArrays (ia, iatmp);

      iatmp := NEW (Ir.InstructionArray, 1);
      WITH inst = LOOPHOLE (iatmp[0], Ir.AluRRInstruction) DO
        inst.op := Ir.Opcode.or;
        inst.r1 := ORD (Register.T.at);
        inst.r2 := reg;
        inst.rdest := reg;
      END;
      ia := Ir.JoinArrays (ia, iatmp);
    END;
    RETURN ia;
  END BuildMask;


PROCEDURE ElseMask (immeds: ExpressionPrivate.ImmedArray;
                    last: CARDINAL; reg: CARDINAL; output: CARDINAL)
  : Ir.InstructionArray =
  VAR
    count := 0; total := 0;
    indexArray : REF ARRAY OF CARDINAL;
    ia, iatmp: Ir.InstructionArray;
  BEGIN
    (* brute force *)
    FOR i := 0 TO last DO
      IF immeds[i].mark THEN
        INC (count);
      END;
    END;
    total := count;
    count := 0;
    indexArray := NEW (REF ARRAY OF CARDINAL, total);
    FOR i := 0 TO last DO
      IF immeds[i].mark THEN
        indexArray[count] := i;
        INC (count);
      END;
    END;
    <* ASSERT total = count *>

    IF total # 0 THEN
      ia := LoadConstant (indexArray[0], reg);
      FOR i := 1 TO total-1 DO
        iatmp := LoadConstant (indexArray[i], ORD (Register.T.at));  (* cheat? *)
        ia := Ir.JoinArrays (ia, iatmp);
        
        iatmp := NEW (Ir.InstructionArray, 1);
        WITH inst = LOOPHOLE (iatmp[0], Ir.AluRRInstruction) DO
          inst.op := Ir.Opcode.or;
          inst.r1 := ORD (Register.T.at);
          inst.r2 := reg;
          inst.rdest := reg;
        END;
        ia := Ir.JoinArrays (ia, iatmp);
      END;
    END;

    iatmp := NEW (Ir.InstructionArray, 1);
    WITH inst = LOOPHOLE (iatmp[0], Ir.AluRIInstruction) DO
      inst.op := Ir.Opcode.addqi;
      inst.r1 := ORD (Register.T.zero);
      inst.immed := 0;  (* FIX THIS - not general enough *)
      inst.rdest := output;
    END;

    IF total # 0 THEN
      ia := Ir.JoinArrays (ia, iatmp);
    ELSE
      ia := iatmp;
    END;

    RETURN ia;
  END ElseMask;


PROCEDURE LoadConstant (shift: CARDINAL; reg: CARDINAL) : Ir.InstructionArray =
  VAR
    constant_inst: CARDINAL;
    newArray: Ir.InstructionArray;
  BEGIN
    IF shift < 15 THEN
      constant_inst := 1;
    ELSIF shift = 15 OR ((31 <= shift) AND (shift <= 63)) THEN
      constant_inst := 2;
    ELSE (* 16 <= shift <= 31 *)
      constant_inst := 1;
    END;

    newArray := NEW (Ir.InstructionArray, constant_inst);
    
    IF shift < 15 THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := reg;
        inst.disp := Word.Shift (1, shift);
        inst.raddr := ORD (Register.T.zero);
      END;
    ELSIF shift = 15 OR ((31 <= shift) AND (shift <= 63)) THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := reg;  (* cheat *)
        inst.disp := 1;
        inst.raddr := ORD (Register.T.zero);
      END;
      WITH inst = LOOPHOLE (newArray[1], Ir.AluRIInstruction) DO
        inst.op := Ir.Opcode.slli;
        inst.r1 := reg;
        inst.immed := shift;
        inst.rdest := reg;
      END;
    ELSE (* shift < 31 *)
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := reg;  (* cheat *)
        inst.disp := Word.Shift (1, shift-16);
        inst.raddr := ORD (Register.T.zero);
        inst.high := TRUE;
      END;
    END;
    RETURN newArray;
  END LoadConstant;


PROCEDURE AndConstant (shift: CARDINAL; reg: CARDINAL) : Ir.InstructionArray =
  VAR
    and_inst: CARDINAL;
    newArray: Ir.InstructionArray;
  BEGIN
    IF verbose THEN
      Wr.PutText (Stdio.stderr, "AndConstant, shift = " & Fmt.Int (shift)
      & ", reg = " & Fmt.Int (reg) & "\n");
    END;

    IF shift < 8 THEN
      and_inst := 1;
    ELSIF shift < 15 THEN
      and_inst := 2;
    ELSIF shift = 15 OR ((32 <= shift) AND (shift <= 63)) THEN
      and_inst := 3;
    ELSE (* 16 <= shift <= 31 *)
      and_inst := 2;
    END;
    newArray := NEW (Ir.InstructionArray, and_inst);

    IF shift < 8 THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.AluRIInstruction) DO
        inst.op := Ir.Opcode.andi;
        inst.r1 := reg;
        inst.immed := Word.Shift (1, shift);
        inst.rdest := reg;
      END;
    ELSIF shift < 15 THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := ORD (Register.T.at);  (* cheat *)
        inst.disp := Word.Shift (1, shift);
        inst.raddr := ORD (Register.T.zero);
      END;
      WITH inst = LOOPHOLE (newArray[1], Ir.AluRRInstruction) DO
        inst.op := Ir.Opcode.and;
        inst.r1 := reg;
        inst.r2 := ORD (Register.T.at);
        inst.rdest := reg;
      END;
    ELSIF shift = 15 OR ((32 <= shift) AND (shift <= 63)) THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := ORD (Register.T.at);  (* cheat *)
        inst.disp := 1;
        inst.raddr := ORD (Register.T.zero);
      END;
      WITH inst = LOOPHOLE (newArray[1], Ir.AluRIInstruction) DO
        inst.op := Ir.Opcode.slli;
        inst.r1 := ORD (Register.T.at);
        inst.immed := shift;
        inst.rdest := ORD (Register.T.at);
      END;
      WITH inst = LOOPHOLE (newArray[2], Ir.AluRRInstruction) DO
        inst.op := Ir.Opcode.and;
        inst.r1 := reg;
        inst.r2 := ORD (Register.T.at);
        inst.rdest := reg;
      END;
    ELSIF shift < 31 THEN
      WITH inst = LOOPHOLE (newArray[0], Ir.MemoryInstruction) DO
        inst.op := Ir.Opcode.lda;
        inst.r1 := ORD (Register.T.at);  (* cheat *)
        inst.disp := Word.Shift (1, shift-16);
        inst.raddr := ORD (Register.T.zero);
        inst.high := TRUE;
      END;
      WITH inst = LOOPHOLE (newArray[1], Ir.AluRRInstruction) DO
        inst.op := Ir.Opcode.and;
        inst.r1 := reg;
        inst.r2 := ORD (Register.T.at);
        inst.rdest := reg;
      END;

    END;

    RETURN newArray;
  END AndConstant;


BEGIN
END Guard.

(*
  documentation of a sort

  analysis for each guard:
    compute available expressions in each register
    compute reaching definitions for each register
    compute the expresssion for the result

  merge the expressions into a single expression

  convert the merged expression back into code
 *)


