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
 * 16-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add blbs
 *
 *)

UNSAFE MODULE Available;

IMPORT Fmt, Stdio, Wr, Thread, Word;
IMPORT Ir, BasicBlock, Procedure, Program;
IMPORT Residue, Analysis, AnalysisPrivate, Expression;
IMPORT Disassembler, Instruction, Register;
IMPORT Traverse;

<* FATAL Wr.Failure, Thread.Alerted *>

CONST
  DebugLocate = FALSE;
  DebugIn = FALSE;
  DebugCondition = FALSE;
  OptimizeCondition = TRUE;

REVEAL
  T = Analysis.T BRANDED OBJECT
    numVars : CARDINAL;
  OVERRIDES
    Converged := Converged;

    Merge := Union;
    BestGuess := BestGuess;
    StartGuess := StartGuess;
    Union := Union;

    GetResidueIn := GetResidueIn;
    GetResidueOut := GetResidueOut;
    SetResidueIn := SetResidueIn;
    SetResidueOut := SetResidueOut;
    PrintResidueIn := PrintResidueIn;
    PrintResidueOut := PrintResidueOut;

    GenKill := GenKill;
    Copy := Copy;

    (* null methods *)
    Initialize := Initialize;
    Cleanup:= Cleanup;

    SetFlags := SetFlags;
  END;

  R = BRANDED REF RECORD
    (* initially lets make it [0..31] for registers *)
    regs: ExpressionArray;
    stack: ExpressionArray;

    (* need to add args *)
  END;

VAR
  BlankT : T;

TYPE
  ExpressionArray = REF ARRAY OF Expression.T;

  (* exported procedures *)

PROCEDURE NewT () : T =
  VAR t : T := NEW (T);
  BEGIN
    t.direction := Analysis.Direction.Forward;
    t.name := "Available Expressions";
    RETURN t;
  END NewT;


  (* methods *)

PROCEDURE Converged(<* UNUSED *> t: T; r1, r2: Residue.T) : BOOLEAN =
  VAR real1 : R := r1;
      real2 : R := r2;
  BEGIN
    IF r1 = NIL OR r2 = NIL THEN RETURN FALSE; END;
    IF NUMBER (real1.regs^) # NUMBER (real2.regs^) THEN RETURN FALSE; END;
    FOR i := FIRST (real1.regs^) TO LAST (real1.regs^) DO
      IF Expression.Match (real1.regs[i], real2.regs[i]) = NIL THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END Converged;


PROCEDURE BestGuess(<* UNUSED *> t: T; p: Procedure.T) : Residue.T =
  VAR r := NEW (R);
  BEGIN
    r.regs := NEW (ExpressionArray, 32);
    r.stack := NEW (ExpressionArray, p.frame);
    FOR i := FIRST (r.regs^) TO LAST (r.regs^) DO
      r.regs[i] := NEW (Expression.Top);
    END;
    RETURN r;
  END BestGuess;


PROCEDURE StartGuess(<* UNUSED *> t: T; p: Procedure.T) : Residue.T =
  VAR r := NEW (R);
  BEGIN
    r.regs := NEW (ExpressionArray, 32);
    r.stack := NEW (ExpressionArray, p.frame);
    FOR i := FIRST (r.regs^) TO LAST (r.regs^) DO
      r.regs[i] := NIL;
    END;
    r.regs[16] := NEW (Expression.Arg, num := 0);
    r.regs[17] := NEW (Expression.Arg, num := 1);
    r.regs[18] := NEW (Expression.Arg, num := 2);
    r.regs[19] := NEW (Expression.Arg, num := 3);
    r.regs[20] := NEW (Expression.Arg, num := 4);
    r.regs[21] := NEW (Expression.Arg, num := 5);
    r.regs[26] := NEW (Expression.Return);
    r.regs[29] := NEW (Expression.Immed, value := LOOPHOLE(p.gp, Word.T));
    r.regs[30] := NEW (Expression.SP);
    r.regs[31] := Expression.Zero;
    RETURN r;
  END StartGuess;

PROCEDURE GetResidueIn(<* UNUSED *> t: T; bb: BasicBlock.T) : Residue.T =
  BEGIN
    RETURN bb.infoTop.avail;
  END GetResidueIn;

PROCEDURE GetResidueOut(<* UNUSED *> t: T; bb: BasicBlock.T) : Residue.T =
  BEGIN
    RETURN bb.infoBottom.avail;
  END GetResidueOut;

PROCEDURE SetResidueIn(<* UNUSED *> t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    IF DebugIn THEN
      Wr.PutText (Stdio.stdout, "Residue in for bb #" & Fmt.Int (bb.index) & "\n");
      Print (r);
      BasicBlock.Print (bb);
    END;
    bb.infoTop.avail := r;
  END SetResidueIn;

PROCEDURE SetResidueOut(<* UNUSED *> t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    IF DebugIn THEN
      Wr.PutText (Stdio.stdout, "Residue out for bb #" & Fmt.Int (bb.index) & "\n");
      Print (r);
    END;
    bb.infoBottom.avail := r;
  END SetResidueOut;

PROCEDURE Print (r: R) =
  BEGIN
    FOR i := FIRST (r.regs^) TO LAST (r.regs^) DO
      IF r.regs[i] # NIL THEN
        Wr.PutText (Stdio.stderr, "R" & Fmt.Int (i) & " is " &
          Expression.ToText (r.regs[i]) & "\n\t");
      END;
    END;
    FOR i := FIRST (r.stack^) TO LAST (r.stack^) DO
      IF r.stack[i] # NIL THEN
        Wr.PutText (Stdio.stderr, "Frame " & Fmt.Int (i) & " is " &
          Expression.ToText (r.stack[i]) & "\n\t");
      END;
    END;
  END Print;

PROCEDURE PrintResidueIn(<*UNUSED*> t: T; bb: BasicBlock.T) =
  BEGIN
    Print (bb.infoTop.avail);
  END PrintResidueIn;

PROCEDURE PrintResidueOut(<*UNUSED*> t: T; bb: BasicBlock.T) =
  BEGIN
    Print (bb.infoBottom.avail);
  END PrintResidueOut;


PROCEDURE Union(<*UNUSED*> t: T; a, b: Residue.T) : Residue.T =
  VAR
    ra : R := a; rb : R := b;
    newR : R;
  BEGIN
    IF a = NIL OR b = NIL THEN RETURN NIL; END;
    newR := NEW (R);
    newR.regs := NEW (ExpressionArray, NUMBER (ra.regs^));
    newR.stack := NEW (ExpressionArray, NUMBER (ra.stack^));

    <* ASSERT NUMBER (ra.regs^) = NUMBER (rb.regs^) *>
    <* ASSERT NUMBER (ra.stack^) = NUMBER (rb.stack^) *>

    FOR i := 0 TO LAST (ra.regs^) DO
      newR.regs[i] := Expression.Match (ra.regs[i], rb.regs[i]);
    END;

    FOR i := 0 TO LAST (ra.stack^) DO
      newR.stack[i] := Expression.Match (ra.stack[i], rb.stack[i]);
    END;

    RETURN newR;
  END Union;


PROCEDURE GenKill (<* UNUSED *> t: T; i: Ir.Instruction; r: Residue.T;
                   p: Procedure.T)
  : Residue.T RAISES {Analysis.Problem} =
  VAR rr : R := Copy (NIL, r, NIL);
  BEGIN
    CASE i.op OF
    | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>

      WITH aluOp = LOOPHOLE(i, Ir.AluRIInstruction) DO
        VAR
          newE : Expression.Compound;
          newI : Expression.Immed;
        BEGIN
          newE := NEW (Expression.Compound);
          newI := NEW (Expression.Immed);

          newE.left := rr.regs[aluOp.r1];
          newE.right := newI;
          newE.def := i.index;
          newE.op := Expression.Op (aluOp.op);

          newI.value := aluOp.immed;
          newI.def := i.index;

          rr.regs[aluOp.rdest] := newE.optimize ();
        END;
      END;

    | Ir.Opcode.addq .. Ir.Opcode.mskqh,
      Ir.Opcode.s4addl .. Ir.Opcode.zapnot =>

      WITH aluOp = LOOPHOLE(i, Ir.AluRRInstruction) DO
        VAR newE := NEW (Expression.Compound);
        BEGIN
          newE.left := rr.regs[aluOp.r1];
          newE.right := rr.regs[aluOp.r2];
          newE.op := Expression.Op (aluOp.op);
          newE.def := i.index;

          rr.regs[aluOp.rdest] := newE;
        END;
      END;
      
    | Ir.Opcode.li =>
      
      WITH immedOp = LOOPHOLE(i, Ir.ImmedInstruction) DO
        VAR newE := NEW (Expression.Immed);
        BEGIN
          newE.value := immedOp.immed;
          newE.def := i.index;

          rr.regs[immedOp.rdest] := newE;
        END;
      END;

    | Ir.Opcode.lda =>
      (* The result of an lda is much like an addqi, except
         that there is a greater range of constants. *)
      WITH memOp = LOOPHOLE(i, Ir.MemoryInstruction) DO
        VAR newE : Expression.Compound;
            newI : Expression.Immed;
        BEGIN
          IF memOp.r1 # ORD (Register.T.gp) THEN
            newE := NEW (Expression.Compound);
            newI := NEW (Expression.Immed);

            newE.left := rr.regs[memOp.raddr];
            newE.right := newI;
            newE.def := i.index;
            IF memOp.high THEN
              newI.value := Word.LeftShift(memOp.disp, 16);
            ELSE
              newI.value := memOp.disp;
            END;
            newI.def := i.index;

            newE.op := Expression.Op (memOp.op);
            rr.regs[memOp.r1] := newE.optimize ();
          ELSIF memOp.high THEN
            (* ldah into gp, can ignore lda into gp *)
            newI := NEW (Expression.Immed);
            newI.value := LOOPHOLE (p.gp, Word.T);
            newI.def := i.index;
            rr.regs[ORD (Register.T.gp)] := newI;
          END;
        END;
      END;

    | Ir.Opcode.ldq .. Ir.Opcode.ldb =>

      WITH loadOp = LOOPHOLE(i, Ir.MemoryInstruction) DO
        IF loadOp.raddr = 30 THEN
          (* load off of the stack *)
          <* ASSERT loadOp.disp < NUMBER (rr.stack^) *>

          rr.regs [loadOp.r1] := rr.stack [loadOp.disp];
        ELSE
          VAR
            newE := NEW (Expression.Compound);
            offset := NEW (Expression.Immed);
          BEGIN
            offset.value := loadOp.disp;
            offset.def := i.index;
            
            newE.left := rr.regs[loadOp.raddr];
            newE.right := offset;
            newE.op := Expression.Op (loadOp.op);
            newE.def := i.index;

            rr.regs[loadOp.r1] := newE;
          END;
        END;
      END;

    | Ir.Opcode.stq .. Ir.Opcode.stb =>

      (* TODO: not quite right, but leave it for now *)
      WITH storeOp = LOOPHOLE (i, Ir.MemoryInstruction) DO
        IF storeOp.raddr = ORD (Register.T.sp) THEN
          (* store on the stack *)
          <* ASSERT storeOp.disp < NUMBER (rr.stack^) *>
          rr.stack[storeOp.disp] := rr.regs[storeOp.r1];
        END;

        (* stores can be to handler stack too *)
      END;

    | Ir.Opcode.beq .. Ir.Opcode.br =>

      (* branches create no expressions *)

    | Ir.Opcode.ret =>

      WITH callOp = LOOPHOLE (i, Ir.RetInstruction) DO
        rr.regs[callOp.r1] := NIL;
      END;

    | Ir.Opcode.jsr =>

      WITH callOp = LOOPHOLE (i, Ir.RetInstruction) DO
        rr.regs[callOp.r1] := NIL;

        (* TODO: get more precise type information *)
        rr.regs[0] := Expression.NewCall (callOp.unused,
                                          rr.regs[16],
                                          rr.regs[17],
                                          rr.regs[18],
                                          rr.regs[19],
                                          rr.regs[20],
                                          rr.regs[21]);
        rr.regs[0].def := i.index;
        CallerSave (rr);
      END;

    | Ir.Opcode.call =>

      WITH callOp = LOOPHOLE (i, Ir.CallInstruction) DO
        rr.regs[callOp.reg] := NIL;

        (* TODO: get more precise type information *)
        rr.regs[0] := Expression.NewCall (callOp.target,
                                          rr.regs[16],
                                          rr.regs[17],
                                          rr.regs[18],
                                          rr.regs[19],
                                          rr.regs[20],
                                          rr.regs[21]);
        rr.regs[0].def := i.index;
        CallerSave (rr);
      END;

    | Ir.Opcode.copy =>

      WITH copyOp = LOOPHOLE (i, Ir.CopyInstruction) DO
        rr.regs[copyOp.rdest] := rr.regs[copyOp.rsrc];
      END;

    | Ir.Opcode.noop =>
      
      (* skip *)

    | Ir.Opcode.fault =>
      FOR i := FIRST (rr.regs^) TO LAST (rr.regs^) DO
        rr.regs[i] := NEW (Expression.Top);
      END;

    ELSE
      (* unused, unknown, or indirect *)
      Wr.PutText (Stdio.stderr,
                  "bad opcode in Available.GenKill " &
                  Ir.InstructionNames[i.op]);
      RAISE Analysis.Problem;
    END;

    RETURN rr;
  END GenKill;


PROCEDURE CallerSave (r: R) =
  BEGIN
    (* temps *)
    FOR i := 1 TO 8 DO
      r.regs[i] := Expression.BottomValue;
    END;

    (* argument registers and more temps *)
    FOR i := 16 TO 25 DO
      r.regs[i] := Expression.BottomValue;
    END;

    (* 27 *)
    r.regs[ORD (Register.T.t12)] := Expression.BottomValue;
    r.regs[ORD (Register.T.at)] := Expression.BottomValue;

    (* gp *)
    r.regs[ORD (Register.T.gp)] := Expression.BottomValue;
  END CallerSave;


PROCEDURE Copy (<* UNUSED *> t: T; rfrom, rto: Residue.T) : Residue.T =
  VAR
    result: R := rto;
    r: R := rfrom;
  BEGIN
    IF result = NIL THEN
      result := NEW (R);
      result.regs := NEW (ExpressionArray, NUMBER (r.regs^));
      result.stack := NEW (ExpressionArray, NUMBER (r.stack^));
    END;
    result.regs^ := r.regs^;
    result.stack^ := r.stack^;
    RETURN result;
  END Copy;


(* generic stuff *)
PROCEDURE Initialize (<* UNUSED *> t: T; <* UNUSED *> c: Ir.Code) =
  BEGIN
    (* nothing *)
  END Initialize;

PROCEDURE Cleanup (<* UNUSED *> t: T; <* UNUSED *> c: Ir.Code) =
  BEGIN
    (* nothing *)
  END Cleanup;


(* compute call destinations *)
PROCEDURE LocateCalls (p: Procedure.T; prog: Program.T) =

  (* helper procedure *)
  PROCEDURE EvalCall (VAR inst: Ir.Instruction; <* UNUSED *> ignore: REFANY) =
    VAR r: R;
        val: Expression.T;
        target : INTEGER;
        bb: BasicBlock.T;
    BEGIN
      CASE inst.op OF
      | Ir.Opcode.jsr =>
        r := Traverse.GetResidueBefore (BlankT, p, inst.index);
        bb := p.instructions.map[inst.index];

        (* find the expression *)
        IF DebugLocate THEN
          Wr.PutText (Stdio.stderr, "initial jump to " & Expression.ToText (r.regs[27]) & "\n");
        END;
        val := Expression.Eval (r.regs[27]);
        IF DebugLocate THEN
          Wr.PutText (Stdio.stderr, "jump to " & Expression.ToText (val) & "\n");
        END;
        IF TYPECODE (val) = TYPECODE (Expression.Immed) THEN
          target :=
              Disassembler.EntryPoint (LOOPHOLE (NARROW (val, Expression.Immed).value,
                                                 Instruction.TPtr),
                                       prog);
        
          (* update the instruction *)
          WITH jsrInst = LOOPHOLE (inst, Ir.RetInstruction) DO
            jsrInst.unused := target;
          END;
          
          (* update the residue *)
          WITH callResidue = NARROW (bb.infoBottom.avail, R).regs[0] DO
            <* ASSERT callResidue # NIL *>
            <* ASSERT TYPECODE (callResidue) = TYPECODE (Expression.Call) *>
            NARROW (callResidue, Expression.Call).target := target;
          END;
        END;
        
      | Ir.Opcode.ret =>
        r := Traverse.GetResidueBefore (BlankT, p, inst.index);
        p.returns := r.regs[0];

      ELSE
        (* skip *)
      END;
    END EvalCall;

  BEGIN
    Procedure.InstructionSweep (p, EvalCall, NIL);
  END LocateCalls;


(* compute conditions that govern exit edges of basic blocks *)
PROCEDURE Condition (p: Procedure.T) RAISES {Analysis.Problem} =

  (* helper procedure *)
  PROCEDURE DoIt (bb: BasicBlock.T; <* UNUSED *> arg: REFANY) RAISES {Analysis.Problem} =
    VAR index: Ir.BasicBlockIndex;
        reg: CARDINAL;
        edgeT, edgeF: CARDINAL;
        expr: Expression.T;
        exprT, exprF: Expression.Compound;
    BEGIN
      IF NUMBER (bb.successors^) = 1 OR NUMBER (bb.successors^) = 0 THEN
        RETURN;
      END;
      <* ASSERT NUMBER (bb.successors^) = 2 *>

      WITH inst = LOOPHOLE (bb.instructions[bb.instruction_count-1],
                            Ir.BranchInstruction)
       DO
        index := inst.target;
        reg := inst.r1;
        expr := NARROW (bb.infoBottom.avail, R).regs[reg];

        IF index = bb.successors[0].index THEN
          edgeT := bb.edges_out[0];
          edgeF := bb.edges_out[1];
        ELSE
          edgeT := bb.edges_out[1];
          edgeF := bb.edges_out[0];
        END;

        exprT := NEW (Expression.Compound);
        exprT.left := expr;
        exprT.right := Expression.Zero;
        exprT.def := inst.index;

        exprF := NEW (Expression.Compound);
        exprF.left := expr;
        exprF.right := Expression.Zero;
        exprF.def := inst.index;

        CASE inst.op OF
        | Ir.Opcode.beq =>
          exprT.op := Expression.Opcode.cmpeq;
          exprF.op := Expression.Opcode.cmpne;
        | Ir.Opcode.bge =>
          exprT.op := Expression.Opcode.cmpge;
          exprF.op := Expression.Opcode.cmplt;
        | Ir.Opcode.bgt =>
          exprT.op := Expression.Opcode.cmpgt;
          exprF.op := Expression.Opcode.cmple;
        | Ir.Opcode.ble =>
          exprT.op := Expression.Opcode.cmple;
          exprF.op := Expression.Opcode.cmpgt;
        | Ir.Opcode.blt =>
          exprT.op := Expression.Opcode.cmplt;
          exprF.op := Expression.Opcode.cmpge;
        | Ir.Opcode.bne =>
          exprT.op := Expression.Opcode.cmpne;
          exprF.op := Expression.Opcode.cmpeq;
        | Ir.Opcode.blbc =>
          exprT.op := Expression.Opcode.even;
          exprF.op := Expression.Opcode.odd;
        | Ir.Opcode.blbs =>
          exprT.op := Expression.Opcode.odd;
          exprF.op := Expression.Opcode.even;
        ELSE
          (* should not get here *)
          RAISE Analysis.Problem;
        END;

        IF OptimizeCondition THEN
          exprT := exprT.canonicalize ();
          exprF := exprF.canonicalize ();
        END;
        
        p.edges[edgeT].expr := exprT;
        p.edges[edgeF].expr := exprF;
      END;

    END DoIt;

  VAR
    expr: Expression.Public;
  BEGIN
    Procedure.Sweep (p, DoIt, NIL);

    FOR i := 0 TO p.edge_count-1 DO
      (* set source edge for expr *)
      expr := p.edges[i].expr;  (* implicit narrow *)
      IF expr # NIL THEN
        expr.edge := i;
      END;

      IF DebugCondition THEN
        Wr.PutText (Stdio.stderr,
                    "Edge " & Fmt.Int (i) & " has expr " &
                    Expression.ToText (expr) & "\n");
      END;
    END;
  END Condition;

PROCEDURE SetFlags (<* UNUSED *> t: T; p: Procedure.T)
  RAISES {Analysis.NotReady} =
  BEGIN
    IF NOT Procedure.Flags.Label IN p.flags THEN RAISE Analysis.NotReady; END;
    p.flags := Procedure.FlagSet {Procedure.Flags.Available} + p.flags;
  END SetFlags;

PROCEDURE GetReg (r: R; num: [0..31]) : Expression.T =
  BEGIN
    RETURN r.regs[num];
  END GetReg;

BEGIN
  BlankT := NewT ();
END Available.
