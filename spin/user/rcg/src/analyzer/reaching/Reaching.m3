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
 *
 *)

UNSAFE MODULE Reaching;

IMPORT Wr, Fmt, Stdio, Word, Thread;
IMPORT Ir, BasicBlock, Procedure;
IMPORT Analysis, AnalysisPrivate, Residue;
IMPORT IntSet, ReachIB;
IMPORT ReachIBArray AS RIBArray;
IMPORT Register;
IMPORT Flow, Expression, Traverse, Available;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  DebugResult = FALSE;

REVEAL
  T = Analysis.T BRANDED OBJECT
    numDefs : CARDINAL;
  OVERRIDES
    Converged := Converged;

    Merge := Union;
    BestGuess := BestGuess;
    StartGuess := StartGuess;

    GetResidueIn := GetResidueIn;
    GetResidueOut := GetResidueOut;
    SetResidueIn := SetResidueIn;
    SetResidueOut := SetResidueOut;
    PrintResidueIn := PrintResidueIn;
    PrintResidueOut := PrintResidueOut;

    GenKill := GenKill;
    Copy := Copy;

    Initialize := Initialize;

    SetFlags := SetFlags;
  END;

(* Residues are sets, but their length is only known at runtime. *)
TYPE
  R = IntSet.T;

PROCEDURE NewT(n: CARDINAL): T =
  BEGIN
    RETURN NEW(T, 
               direction := Analysis.Direction.Forward, 
               name := "Reaching Definitions Analysis",
               numDefs := n);
  END NewT;

PROCEDURE IsEqual(a, b: R): BOOLEAN =
  BEGIN
    IF a = NIL OR b = NIL THEN RETURN FALSE; END;
    RETURN IntSet.IsEqual(a, b);
  END IsEqual;

PROCEDURE Converged(<* UNUSED *> t: T; r1, r2: Residue.T) : BOOLEAN =
  BEGIN
    RETURN IsEqual(r1, r2);
  END Converged;

PROCEDURE BestGuess(t: T; <* UNUSED *> p: Procedure.T): Residue.T =
  BEGIN
    RETURN IntSet.New(t.numDefs);
  END BestGuess;


PROCEDURE StartGuess(t: T; <* UNUSED *> p: Procedure.T): Residue.T =
  VAR
    result := IntSet.New(t.numDefs);
  BEGIN
    RETURN result;
  END StartGuess;

PROCEDURE GetResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T): Residue.T =
  BEGIN
    RETURN bb.infoTop.reaching;
  END GetResidueIn;

PROCEDURE GetResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T): Residue.T =
  BEGIN
    RETURN bb.infoBottom.reaching;
  END GetResidueOut;

PROCEDURE SetResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoTop.reaching := r;
  END SetResidueIn;

PROCEDURE SetResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoBottom.reaching := r;
  END SetResidueOut;

PROCEDURE PrintResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T) =
  BEGIN
    IntSet.Print(bb.infoTop.reaching);
  END PrintResidueIn;

PROCEDURE PrintResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T) =
  BEGIN
    IntSet.Print(bb.infoBottom.reaching);
  END PrintResidueOut;


PROCEDURE Union(<*UNUSED*>t: T; a, b: Residue.T): Residue.T =
  BEGIN
    RETURN IntSet.Union(a, b);
  END Union;

(* Reaching definitions.  Forward analysis, where a definition
   of a variable kills previous definitions.
*)

PROCEDURE GenKill (<* UNUSED *> t: T; inst: Ir.Instruction; r: Residue.T; p: Procedure.T)
  : Residue.T =
  VAR
    result := IntSet.Copy (r);
  BEGIN
    CASE inst.op OF
    | Ir.Opcode.addq .. Ir.Opcode.zapnot =>
      (* arithmetic ops with three registers use r1 and r2, define rdest *)
      WITH aluOp = LOOPHOLE(inst, Ir.AluRRInstruction) DO
        result := IntSet.Diff (result, p.defs[aluOp.rdest]);
        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>
      (* ALU ops with immediate fields use r1, define rdest *)
      WITH aluOp = LOOPHOLE(inst, Ir.AluRIInstruction) DO
        result := IntSet.Diff (result, p.defs[aluOp.rdest]);
        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.li =>
      (* immed instruction defines rdest *)
      WITH immedOp = LOOPHOLE(inst, Ir.ImmedInstruction) DO
        result := IntSet.Diff (result, p.defs[immedOp.rdest]);
        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.lda .. Ir.Opcode.ldb =>
      (* load instruction uses raddr, defines r1 *)
      WITH memOp = LOOPHOLE(inst, Ir.MemoryInstruction) DO
        result := IntSet.Diff (result, p.defs[memOp.r1]);
        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.jsr =>
      (* call instruction uses r2 by jumping to it, defines r1 *)
      WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
        result := IntSet.Diff (result, p.defs[callOp.r1]);

        (* define all caller save registers *)
        FOR i := 0 TO LAST (Register.caller_save) DO
          result :=
              IntSet.Diff (result, p.defs[ORD (Register.caller_save[i])]);
        END;

        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.ret =>
      (* call instruction uses r2 by jumping to it, defines r1 *)
      WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
        result := IntSet.Diff (result, p.defs[callOp.r1]);
        IntSet.Add (inst.index, result);
      END;

    | Ir.Opcode.copy =>
      (* copy instruction defines rdest and uses rsrc. *)
      WITH copyOp = LOOPHOLE(inst, Ir.CopyInstruction) DO
        result := IntSet.Diff (result, p.defs[copyOp.rdest]);
        IntSet.Add (inst.index, result);
      END;
        
    | Ir.Opcode.call =>
      (* call instruction defines reg *)
      WITH callOp = LOOPHOLE(inst, Ir.CallInstruction) DO
        result := IntSet.Diff (result, p.defs[callOp.reg]);
        IntSet.Add (inst.index, result);
      END;
        
    | Ir.Opcode.fault =>

      (* nothing comes through a fault *)
      IntSet.Clear (result);
    ELSE
      (* skip *)
    END;
    RETURN result;
  END GenKill;


PROCEDURE Copy (t: T; rfrom, rto: Residue.T) : Residue.T =
  VAR
    result : R := rto;
  BEGIN
    IF result = NIL THEN
      result := IntSet.New (t.numDefs);
    END;
    result^ := NARROW (rfrom, R)^;
    RETURN result;
  END Copy;


PROCEDURE Initialize (<* UNUSED *> t: T; <*UNUSED*>c: Ir.Code) =
  BEGIN
  END Initialize;


(* compute the available expression that represents the result of p *)
PROCEDURE Result (p: Procedure.T; a: Available.T) RAISES {Procedure.NotYet} =
  VAR
    r : R;
    final : Expression.T := NIL;
    iba : RIBArray.T;
    lca : BasicBlock.T;
  BEGIN
    IF NOT Procedure.FlagSet { Procedure.Flags.Available,
                               Procedure.Flags.Dominators,
                               Procedure.Flags.TopoSort }
           <= p.flags THEN
      RAISE Procedure.NotYet;
    END;

    (* find the reaching definitions for register v0 *)
    r := p.bottom.infoBottom.reaching;
    r := IntSet.Intersection (r, p.defs[ORD (Register.T.v0)]);

    (* reaching definitions *)
    IF DebugResult THEN
      Wr.PutText (Stdio.stderr, "the following defs of v0 reach the end\n");
      IntSet.Print (r);
      Wr.PutText (Stdio.stderr, "\n");
    END;

    (* build data structure of insts/basic blocks *)
    iba := RIBArray.New ();
    FOR i := 0 TO p.instructions.size-1 DO
      IF IntSet.IsMember (i, r) THEN
        RIBArray.Add (iba,
                      ReachIB.T {inst := i,
                                 bb := p.instructions.map[i]});
      END;
    END;

    IF DebugResult THEN
      RIBArray.Print (iba);
    END;

    (* do it -- compute the result expression
       factor out the conditions to get to the dominator LCA
     *)
    lca := DomLCA (iba);

    final := ComputeExpression (lca, iba, p, a);
    IF DebugResult THEN
      Wr.PutText (Stdio.stderr,
                  "expr for procedure " &
                  Fmt.Unsigned (LOOPHOLE (p.addr, Word.T))  &
                  " is\n" & Expression.ToText (final) & "\n");
    END;

    final := Flow.PathExpression (p, lca, p.top, final);
    IF DebugResult THEN
      Wr.PutText (Stdio.stderr,
                  "final for procedure " &
                  Fmt.Unsigned (LOOPHOLE (p.addr, Word.T))  &
                  " is\n" & Expression.ToText (final) & "\n");
    END;

    p.returns := final;
  END Result;


(* compute the expression for the set of definitions in iba *)
PROCEDURE ComputeExpression (start: BasicBlock.T;
                             iba: RIBArray.T;
                             p: Procedure.T;
                             a: Available.T) : Expression.T =

  PROCEDURE Compare (a, b: ReachIB.T) : [-1 .. 1] =
    VAR
      a_order, b_order : CARDINAL;
    BEGIN
      a_order := p.topo[a.bb.index];
      b_order := p.topo[b.bb.index];

      <* ASSERT a_order # b_order *>
      IF a_order < b_order THEN
        RETURN -1;
      ELSE
        RETURN 1;
      END;
    END Compare;

  VAR
    new, expr : Expression.T := NIL;
  BEGIN
    (* sort iba *)
    RIBArray.Sort (iba, Compare);

    (* do a traversal of sorted array *)
    FOR i := 0 TO RIBArray.Size (iba)-1 DO
      new := DoInst (start, RIBArray.Get (iba, i), p, a);
      IF i = 0 THEN
        expr := new;
      ELSE
        expr := Expression.LogicalOr (new, expr);
      END;
    END;

    RETURN expr;
  END ComputeExpression;


PROCEDURE DoInst (start: BasicBlock.T; rib: ReachIB.T;
                  p: Procedure.T; a: Available.T) : Expression.T =
  VAR
    availableResidue: Available.R := NIL;
    availableExpr, result : Expression.T;
  BEGIN
    (* HACK: works for functional-type things*)
    availableResidue := Traverse.GetResidueAfter (a, p, rib.inst);
        
    (* get expr for register 0 *)
    availableExpr := Available.GetReg (availableResidue, 0);

    result := Flow.PathExpression (p, rib.bb, start, availableExpr);
    RETURN result;
  END DoInst;


(* dominator lca *)
PROCEDURE DomLCA (iba: RIBArray.T) : BasicBlock.T =
  VAR lca : BasicBlock.T;
  BEGIN
    FOR i := 0 TO RIBArray.Size (iba)-1 DO
      lca := Flow.DominatorLCA (lca, RIBArray.Get (iba, i).bb);
    END;
    RETURN lca;
  END DomLCA;


PROCEDURE SetFlags (<* UNUSED *> t: T; p: Procedure.T) =
  BEGIN
    p.flags := Procedure.FlagSet {Procedure.Flags.Reaching} + p.flags;
  END SetFlags;


BEGIN
END Reaching.
