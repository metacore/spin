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


UNSAFE MODULE Constant;

IMPORT Stdio, Wr, Fmt, Thread;
IMPORT Analysis, AnalysisPrivate, Residue, Ir, BasicBlock, Procedure;
IMPORT Word;

<* FATAL Wr.Failure, Thread.Alerted *>

CONST
  Debug = FALSE;
  Sanity = TRUE;

(* types *)

REVEAL
  T = Analysis.T BRANDED OBJECT
  OVERRIDES
    Merge := Merge;
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

    Initialize := Initialize;
    Cleanup := Cleanup;
    Converged := Converged;

    SetFlags := SetFlags;
  END;

REVEAL
  (* can represent constants with magnitude 0 to 2^62-1 *)
  R = BRANDED REF ARRAY [0 .. 31] OF Constant;

TYPE
  Constant = RECORD
    is: BITS 2 FOR Is;
    unused: BITS 62 FOR [0 .. Word.Shift(1,62)-1];
    val: Word.T;
  END;
  Is = { Const, NotConst, Top };

(* analysis procedures *)

PROCEDURE Merge (t: T; r1, r2: Residue.T) : Residue.T  =
  BEGIN
    RETURN Intersection (t, NARROW (r1, R), NARROW (r2, R));
  END Merge;

PROCEDURE BestGuess (<* UNUSED *> t: T; <* UNUSED *> p: Procedure.T)
  : Residue.T =
  VAR r : R := NEW (R);
  BEGIN
    Top (r);
    RETURN r;
  END BestGuess;

PROCEDURE StartGuess (<* UNUSED *> t: T; <* UNUSED *> p: Procedure.T)
  : Residue.T =
  VAR r : R := NEW (R);
  BEGIN
    Clear (r);
    RETURN r;
  END StartGuess;

(* residue functions *)

PROCEDURE GetResidueIn (<* UNUSED *>t: T; bb: BasicBlock.T) : Residue.T =
  BEGIN
    RETURN bb.infoTop.constant;
  END GetResidueIn;

PROCEDURE GetResidueOut (<* UNUSED *>t: T; bb: BasicBlock.T) : Residue.T =
  BEGIN
    RETURN bb.infoBottom.constant;
  END GetResidueOut;

PROCEDURE SetResidueIn (<* UNUSED *> t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoTop.constant := r;
  END SetResidueIn;

PROCEDURE SetResidueOut (<* UNUSED *> t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoBottom.constant := r;
  END SetResidueOut;

(* print functions *)

PROCEDURE PrintResidueIn (t: T; bb: BasicBlock.T) =
  BEGIN
    Print (GetResidueIn (t, bb));
  END PrintResidueIn;

PROCEDURE PrintResidueOut (t: T; bb: BasicBlock.T) =
  BEGIN
    Print (GetResidueOut (t, bb));
  END PrintResidueOut;


(* compute residues *)

PROCEDURE GenKill (<* UNUSED *> t: T; i: Ir.Instruction; rin: Residue.T;
                   <* UNUSED *> p: Procedure.T)
  : Residue.T RAISES {Analysis.Problem} =
  VAR r := Copy (NIL, rin, NIL);
  BEGIN

    CASE i.op OF
    | Ir.Opcode.addqi .. Ir.Opcode.srli =>

      WITH aluOp = LOOPHOLE(i, Ir.AluRIInstruction) DO
        IF ResidueIsConstant(r, aluOp.r1) THEN
          WITH r1val = ResidueGetConstant(r, aluOp.r1) DO
            VAR final: INTEGER;
                immed: INTEGER := aluOp.immed;
            BEGIN
              (* have to check for overflow *)
              CASE i.op OF
                | Ir.Opcode.addqi =>
                  final := r1val + immed;
                | Ir.Opcode.subqi =>
                  final := r1val - immed;
                | Ir.Opcode.mulqi =>
                  final := r1val * immed;
                | Ir.Opcode.adduqi =>
                  final := r1val + immed;
                | Ir.Opcode.subuqi =>
                  final := r1val - immed;
                | Ir.Opcode.muluqi =>
                  final := r1val * immed;
                | Ir.Opcode.cmpeqi =>
                  final := ORD(r1val = immed);
                | Ir.Opcode.cmplei =>
                  final := ORD(r1val <= immed);
                | Ir.Opcode.cmplti =>
                  final := ORD(r1val < immed);
                | Ir.Opcode.andi =>
                  final := Word.And(r1val, immed);
                | Ir.Opcode.andnoti =>
                  final := Word.And(r1val, Word.Not(immed));
                | Ir.Opcode.ori =>
                  final := Word.Or(r1val, immed);
                | Ir.Opcode.ornoti =>
                  final := Word.Or(r1val, Word.Not(immed));
                | Ir.Opcode.xori =>
                  final := Word.Xor(r1val, immed);
                | Ir.Opcode.xornoti =>
                  final := Word.Xor(r1val, Word.Not(immed));
                | Ir.Opcode.slli =>
                  final := Word.Shift(r1val, immed);
                | Ir.Opcode.srai =>
                  IF Word.And (FIRST (INTEGER), immed) # 0 THEN
                    final := Word.Shift (r1val, immed);
                    final := Word.Insert (final, -1,
                                          BITSIZE (Word.T)-immed, immed);
                  ELSE
                    final := Word.Shift (r1val, immed);
                  END;
                | Ir.Opcode.srli =>
                  final := Word.Shift(r1val, immed);
              ELSE
                RAISE Analysis.Problem;
              END;

              (* do the computation *)
              ResidueSetConstant(r, aluOp.rdest, final);
            END;
          END;
        ELSE
          ResidueSetNotConstant (r, aluOp.rdest);
        END;
      END;

    | Ir.Opcode.addq .. Ir.Opcode.srl =>

      WITH aluOp = LOOPHOLE(i, Ir.AluRRInstruction) DO
        IF ResidueIsConstant(r, aluOp.r1) AND
           ResidueIsConstant(r, aluOp.r2) THEN
          WITH r1val = ResidueGetConstant(r, aluOp.r1),
               r2val = ResidueGetConstant(r, aluOp.r2) DO
            VAR final: INTEGER;
            BEGIN
              (* have to check for overflow *)
              CASE i.op OF
                | Ir.Opcode.addq =>
                  final := r1val + r2val;
                | Ir.Opcode.subq =>
                  final := r1val - r2val;
                | Ir.Opcode.mulq =>
                  final := r1val * r2val;
                | Ir.Opcode.adduq =>
                  final := r1val + r2val;
                | Ir.Opcode.subuq =>
                  final := r1val - r2val;
                | Ir.Opcode.muluq =>
                  final := r1val * r2val;
                | Ir.Opcode.cmpeq =>
                  final := ORD(r1val = r2val);
                | Ir.Opcode.cmple =>
                  final := ORD(r1val <= r2val);
                | Ir.Opcode.cmplt =>
                  final := ORD(r1val < r2val);
                | Ir.Opcode.and =>
                  final := Word.And(r1val, r2val);
                | Ir.Opcode.andnot =>
                  final := Word.And(r1val, Word.Not(r2val));
                | Ir.Opcode.or =>
                  final := Word.Or(r1val, r2val);
                | Ir.Opcode.ornot =>
                  final := Word.Or(r1val, Word.Not(r2val));
                | Ir.Opcode.xor =>
                  final := Word.Xor(r1val, r2val);
                | Ir.Opcode.xornot =>
                  final := Word.Xor(r1val, Word.Not(r2val));
                | Ir.Opcode.sll =>
                  final := Word.Shift(r1val, r2val);
                | Ir.Opcode.sra =>
                  (* not right *)
                  final := Word.Shift(r1val, r2val);
                | Ir.Opcode.srl =>
                  final := Word.Shift(r1val, r2val);
              ELSE
                RAISE Analysis.Problem;
              END;

              (* do the computation *)
              ResidueSetConstant(r, aluOp.rdest, final);
            END;
          END;
        ELSE
          ResidueSetNotConstant (r, aluOp.rdest);
        END;
      END;
      
    | Ir.Opcode.lda =>

      (* A lda instruction is much like an addqi, except that 
         the range of constants is bigger. *)
       
      WITH memOp = LOOPHOLE(i, Ir.MemoryInstruction) DO
        IF ResidueIsConstant(r, memOp.raddr) THEN
          WITH r1val = ResidueGetConstant(r, memOp.raddr) DO
            VAR final: INTEGER;
                immed: INTEGER;
            BEGIN
              IF memOp.high THEN
                immed := Word.LeftShift(memOp.disp, 16);
              ELSE
                immed := memOp.disp;
              END;

              (* have to check for overflow *)
              final := r1val + immed;

              (* do the computation *)
              ResidueSetConstant(r, memOp.r1, final);
            END;
          END;
        ELSE
          ResidueSetNotConstant (r, memOp.r1);
        END;
      END;

    | Ir.Opcode.li =>
      
      WITH immedOp = LOOPHOLE (i, Ir.ImmedInstruction) DO
        ResidueSetConstant (r, immedOp.rdest, immedOp.immed);
      END;

    | Ir.Opcode.ldq .. Ir.Opcode.stb,
      Ir.Opcode.beq .. Ir.Opcode.br =>

      (* memory ops and jumps create no constants *)

    | Ir.Opcode.jsr .. Ir.Opcode.ret =>

      WITH callOp = LOOPHOLE (i, Ir.RetInstruction) DO
        ResidueSetNotConstant (r, callOp.r1);
      END;

    | Ir.Opcode.call =>

      WITH callOp = LOOPHOLE (i, Ir.CallInstruction) DO
        ResidueSetNotConstant (r, callOp.reg);
      END;

    | Ir.Opcode.copy =>

      WITH copyOp = LOOPHOLE (i, Ir.CopyInstruction) DO
        ResidueCopy (r, copyOp.rsrc, copyOp.rdest);
      END;

    | Ir.Opcode.noop =>

      (* skip *)

    ELSE
      (* unused *)
      RAISE Analysis.Problem;
    END;
    RETURN r;
  END GenKill;

(* generic stuff *)

PROCEDURE Initialize (<* UNUSED *> t: T; <* UNUSED *> c: Ir.Code) =
  BEGIN
    (* nothing *)
  END Initialize;

PROCEDURE Cleanup (<* UNUSED *> t: T; <* UNUSED *> c: Ir.Code) =
  BEGIN
    (* nothing *)
  END Cleanup;

PROCEDURE Converged (<* UNUSED *> t: T; r1, r2: Residue.T) : BOOLEAN =
  BEGIN
    RETURN Equal (r1, r2);
  END Converged;

(* creator *)

PROCEDURE NewT () : T =
  VAR r : T := NEW (T);
  BEGIN
    r.direction := Analysis.Direction.Forward;
    r.name := "Constant Propagation";
    RETURN r;
  END NewT;

(* residue procedures *)

PROCEDURE ResidueSetConstant (r: R; reg, val: Word.T) =
  BEGIN
    r[reg].is := Is.Const;
    r[reg].val := val;
  END ResidueSetConstant;

PROCEDURE ResidueIsConstant (r: R; reg: Word.T) : BOOLEAN =
  BEGIN
    RETURN r[reg].is = Is.Const;
  END ResidueIsConstant;

PROCEDURE ResidueGetConstant (r: R; reg: Word.T) : INTEGER =
  BEGIN
    <* ASSERT r[reg].is = Is.Const *>
    RETURN r[reg].val;
  END ResidueGetConstant;

<* UNUSED *> PROCEDURE ResidueSetTop (r: R; reg: Word.T) =
  BEGIN
    r[reg].is := Is.Top;
  END ResidueSetTop;

<* UNUSED *> PROCEDURE ResidueIsTop (r: R; reg: Word.T) : BOOLEAN =
  BEGIN
    RETURN r[reg].is = Is.Top;
  END ResidueIsTop;

PROCEDURE ResidueSetNotConstant (r: R; reg: Word.T) =
  BEGIN
    r[reg].is := Is.NotConst;
  END ResidueSetNotConstant;

<* UNUSED *> PROCEDURE ResidueIsNotConstant (r: R; reg: Word.T) : BOOLEAN =
  BEGIN
    RETURN r[reg].is = Is.NotConst;
  END ResidueIsNotConstant;

PROCEDURE ResidueCopy (r: R; r1, r2: Word.T) =
  BEGIN
    r[r2].is := r[r1].is;
    r[r2].val := r[r1].val;
  END ResidueCopy;

(* residue ops hand-inlined here for speed *)

PROCEDURE Intersection (<* UNUSED *> t: T; r1, r2: R) : R =
  (* *)
  BEGIN
    IF r2 = NIL THEN r2 := NEW (R); END;

    FOR i := FIRST(r1^) TO LAST(r1^) DO
      WITH r1is = r1[i].is, r2is = r2[i].is DO
        IF r1is = Is.Const AND r2is = Is.Const THEN
          IF r1[i].val # r2[i].val THEN
            r2[i].is := Is.NotConst;
          END;
        ELSIF r1is = Is.Top THEN
        ELSIF r2is = Is.Top THEN
          r2[i] := r1[i];
        ELSE
          r2[i].is := Is.NotConst;
        END;
      END;
    END;
    
    RETURN r2;
  END Intersection;

PROCEDURE Union (<* UNUSED *> t: T; a, b: Residue.T) : Residue.T =
  VAR
    r1, r2 : R;
    r : R := NEW (R);
  BEGIN
    r1 := a;  r2 := b;
    FOR i := FIRST(r1^) TO LAST(r1^) DO
      WITH r1is = r1[i].is, r2is = r2[i].is DO
        IF r1is = Is.Const AND r2is = Is.Const THEN
          IF r1[i].val = r2[i].val THEN
            r[i] := r1[i];
          ELSE
            r[i].is := Is.NotConst;
          END;
        ELSIF r1is = Is.Top THEN
          r[i] := r2[i];
        ELSIF r2is = Is.Top THEN
          r[i] := r1[i];
        ELSIF r1is = Is.Const THEN
          r[i] := r1[i];
        ELSIF r2is = Is.Const THEN
          r[i] := r2[i];
        ELSE
          r[i].is := Is.NotConst;
        END;
      END;
    END;
    
    RETURN r;
  END Union;

(*
PROCEDURE Minus (<* UNUSED *> t: T; a, b: Residue.T) : Residue.T =
  (* If a x is Top or a constant in r1, and
     not a constant in r2, then return the value of r1.
     Otherwise, it is not a constant. *)
  VAR
    r1, r2: R;
    r : R := NEW (R);
  BEGIN
    r1 := a;  r2 := b;
    FOR i := FIRST(r1^) TO LAST(r1^) DO
      WITH r1is = r1[i].is, r2is = r2[i].is DO
        IF r1is # Is.NotConst AND r2is = Is.NotConst THEN
          r[i] := r1[i];
        ELSE
          r[i].is := Is.NotConst;
        END;
      END;
    END;

    RETURN r;
  END Minus;
*)

PROCEDURE Clear (r: R) =
  BEGIN
    FOR i := FIRST(r^) TO LAST(r^) DO
      r[i].is := Is.NotConst;
    END;
  END Clear; 

PROCEDURE Top (r: R) =
  BEGIN
    FOR i := FIRST(r^) TO LAST(r^) DO
      r[i].is := Is.Top;
    END;
  END Top;

PROCEDURE Print (r: R) =
  BEGIN
    IF r # NIL THEN
      FOR i := FIRST(r^) TO LAST(r^) DO
        IF r[i].is = Is.Const THEN
          Wr.PutText (Stdio.stdout,
                      "r" & Fmt.Int (i) & "=" &
                      Fmt.Int (r[i].val) &
                      ",");
        ELSIF r[i].is = Is.Top THEN
          Wr.PutText (Stdio.stdout, "v" & Fmt.Int (i) & "=Top,");
        END;
      END;
      Wr.PutText (Stdio.stdout, "\n");
    END;
  END Print;

PROCEDURE Copy (<* UNUSED *> t: T; rfrom, rto: Residue.T) : Residue.T =
  VAR
    result: R := rto;
    r: R := rfrom;
  BEGIN
    IF result = NIL THEN
      result := NEW (R);
    END;
    result^ := r^;
    RETURN result;
  END Copy;

PROCEDURE Equal (r1, r2: R) : BOOLEAN =
  BEGIN
    IF r1 = NIL OR r2 = NIL THEN RETURN FALSE; END;
    IF NUMBER (r1^) # NUMBER (r2^) THEN RETURN FALSE; END;
    FOR i := FIRST(r1^) TO LAST(r1^) DO
      WITH r1is = r1[i].is, r2is = r2[i].is DO
        IF r1is # r2is THEN RETURN FALSE; END;
        IF r1is = Is.Const AND r1[i].val # r2[i].val THEN
          RETURN FALSE;
        END;
      END;
    END;
    RETURN TRUE;
  END Equal;

(* optimizations *)

PROCEDURE Optimize (b: BasicBlock.T; a: REFANY) RAISES {Problem} =
  VAR change: BOOLEAN;
      r: R := Copy(NIL, b.infoTop.constant, b.infoBottom.constant);
      p: Procedure.T := a;
  BEGIN
    FOR i := 0 TO b.instruction_count-1 DO
      WITH inst = b.instructions[i] DO
        change := PropagateConstants (inst, r);
        IF Debug AND change THEN
          Wr.PutText (Stdio.stderr,
                      "Changed instruction " & Fmt.Int(i) &
                      " in basic block " & Fmt.Int(b.index) & "\n");
        END;
        EVAL GenKill(NIL, inst, r, p);
      END;
    END;
    IF Sanity AND NOT Equal(r, b.infoBottom.constant) THEN
      Wr.PutText (Stdio.stderr, "Problem!\n");
      RAISE Problem;
    END;
  END Optimize;

PROCEDURE PropagateConstants (VAR i: Ir.Instruction; r: R) : BOOLEAN =
  (* returns true if a change was made *)
  BEGIN
    CASE i.op OF
    | Ir.Opcode.addqi .. Ir.Opcode.srli =>

      WITH aluOp = LOOPHOLE(i, Ir.AluRIInstruction) DO
        IF ResidueIsConstant(r, aluOp.rdest) THEN
          i :=
              LOOPHOLE(Ir.ImmedInstruction{Ir.Opcode.li,
                                           aluOp.rdest,
                                           ResidueGetConstant(r, aluOp.rdest)},
                       Ir.Instruction);
          RETURN TRUE;
        END;
      END;

    | Ir.Opcode.addq .. Ir.Opcode.srl =>

      WITH aluOp = LOOPHOLE (i, Ir.AluRRInstruction) DO
        IF ResidueIsConstant (r, aluOp.rdest) THEN
          i :=
              LOOPHOLE (Ir.ImmedInstruction {Ir.Opcode.li,
                                             aluOp.rdest,
                                             ResidueGetConstant (r, aluOp.rdest)},
                        Ir.Instruction);
          RETURN TRUE;
        END;
      END;

    | Ir.Opcode.lda =>
      (* A lda instruction is much like an addqi, except that the range
         of constant values is larger. I think that we do not have to look
         at the memOp.high bit in this procedure, because we used it
         to set the constant residue in the GenKill procedure already. *)
      WITH memOp = LOOPHOLE(i, Ir.MemoryInstruction) DO
        IF ResidueIsConstant(r, memOp.r1) THEN
          i :=
              LOOPHOLE(Ir.ImmedInstruction{Ir.Opcode.li,
                                           memOp.r1,
                                           ResidueGetConstant(r, memOp.r1)},
                       Ir.Instruction);
          RETURN TRUE;
        END;
      END;


    | Ir.Opcode.li,
      Ir.Opcode.ldq .. Ir.Opcode.stb,
      Ir.Opcode.beq .. Ir.Opcode.br,
      Ir.Opcode.jsr .. Ir.Opcode.ret,
      Ir.Opcode.call =>

      (* memory ops and jumps create no constants *)

    | Ir.Opcode.copy =>

      WITH copyOp = LOOPHOLE (i, Ir.CopyInstruction) DO
        IF ResidueIsConstant (r, copyOp.rdest) THEN
          i :=
              LOOPHOLE (Ir.ImmedInstruction {Ir.Opcode.li,
                                             copyOp.rdest,
                                             ResidueGetConstant (r, copyOp.rdest)},
                        Ir.Instruction);
        END;
      END;

    | Ir.Opcode.noop =>

      (* skip *)

    ELSE
      (* unused *)
      RAISE Problem;
    END;
    RETURN FALSE;
  END PropagateConstants;


PROCEDURE SetFlags (<* UNUSED *> t: T; p: Procedure.T) =
  BEGIN
    p.flags := Procedure.FlagSet {Procedure.Flags.Constant} + p.flags;
  END SetFlags;


BEGIN
END Constant.
