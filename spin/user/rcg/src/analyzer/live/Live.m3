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

UNSAFE MODULE Live;

IMPORT Ir, BasicBlock, Procedure;
IMPORT Analysis, AnalysisPrivate, IntSet, Residue;
IMPORT Register;

TYPE
  R = REF ARRAY OF IntSet.T;

REVEAL
  T = Analysis.T BRANDED OBJECT
    numUses : CARDINAL;
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

PROCEDURE NewT (n: CARDINAL): T =
  BEGIN
    RETURN NEW(T, 
               direction := Analysis.Direction.Backward, 
               name := "Live Variable Analysis",
               numUses := n);
  END NewT;

PROCEDURE IsEqual(a, b: R): BOOLEAN =
  BEGIN
    IF a = NIL OR b = NIL THEN RETURN FALSE; END;
    FOR i := 0 TO 31 DO
      IF NOT IntSet.IsEqual(a[i], b[i]) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END IsEqual;

PROCEDURE Converged(<* UNUSED *>t: T; r1, r2: Residue.T) : BOOLEAN =
  BEGIN
    RETURN IsEqual(r1, r2);
  END Converged;



PROCEDURE BestGuess(<* UNUSED *>t: T; p: Procedure.T): Residue.T =
  VAR
    EmptyR : R;  (* default empty residue *)
  BEGIN
    EmptyR := NEW (REF ARRAY OF IntSet.T, 32);
    FOR i := 0 TO 31 DO
      EmptyR[i] := IntSet.New (p.instructions.size);
    END;
    RETURN EmptyR;
  END BestGuess;


PROCEDURE StartGuess(<* UNUSED *>t: T; p: Procedure.T): Residue.T =
  VAR
    EmptyR : R;  (* default empty residue *)
  BEGIN
    EmptyR := NEW (REF ARRAY OF IntSet.T, 32);
    FOR i := 0 TO 31 DO
      EmptyR[i] := IntSet.New (p.instructions.size);
    END;
    RETURN EmptyR;
  END StartGuess;


PROCEDURE GetResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T): Residue.T =
  BEGIN
    RETURN bb.infoBottom.live;
  END GetResidueIn;

PROCEDURE GetResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T): Residue.T =
  BEGIN
    RETURN bb.infoTop.live;
  END GetResidueOut;

PROCEDURE SetResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoBottom.live := r;
  END SetResidueIn;

PROCEDURE SetResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T; r: Residue.T) =
  BEGIN
    bb.infoTop.live := r;
  END SetResidueOut;

PROCEDURE PrintResidueIn(<*UNUSED*>t: T; bb: BasicBlock.T) =
  BEGIN
    FOR i := 0 TO 31 DO
      IntSet.Print(NARROW (bb.infoBottom.live, R)[i]);
    END;
  END PrintResidueIn;

PROCEDURE PrintResidueOut(<*UNUSED*>t: T; bb: BasicBlock.T) =
  BEGIN
    FOR i := 0 TO 31 DO
      IntSet.Print(NARROW (bb.infoTop.live, R)[i]);
    END;
  END PrintResidueOut;

PROCEDURE Union(<*UNUSED*>t: T; a, b: Residue.T): Residue.T =
  VAR
    result := NEW (REF ARRAY OF IntSet.T, 32);
  BEGIN
    FOR i := 0 TO 31 DO
      result[i] := IntSet.Union(NARROW (a, R)[i], NARROW (b, R)[i]);
    END;
    RETURN result;
  END Union;

(* Live variable analysis is a backwards analysis. A use
   of a variable adds it to the live set, while a definition
   of a variable removes it. *)

PROCEDURE GenKill (t: T; inst: Ir.Instruction; r: Residue.T; p: Procedure.T)
  : Residue.T RAISES {Analysis.Problem} =
  VAR
    result : R := Copy (t, NARROW (r, R), NIL);
  BEGIN
    CASE inst.op OF
    | Ir.Opcode.noop =>
      (* Do nothing *)

    | Ir.Opcode.addq .. Ir.Opcode.zapnot =>

      (* arithmetic ops with three registers use r1 and r2, define rdest *)
      WITH aluOp = LOOPHOLE(inst, Ir.AluRRInstruction) DO
        IntSet.Remove (inst.index, result[aluOp.rdest]);
        IntSet.Add (inst.index, result[aluOp.r1]);
        IntSet.Add (inst.index, result[aluOp.r2]);
      END;

    | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>

      (* ALU ops with immediate fields use r1, define rdest *)
      WITH aluOp = LOOPHOLE(inst, Ir.AluRIInstruction) DO
        IntSet.Remove (inst.index, result[aluOp.rdest]);
        IntSet.Add (inst.index, result[aluOp.r1]);
      END;

    | Ir.Opcode.li =>

      (* immed instruction defines rdest *)
      WITH immedOp = LOOPHOLE(inst, Ir.ImmedInstruction) DO
        IntSet.Remove (inst.index, result[immedOp.rdest]);
      END;

    | Ir.Opcode.lda .. Ir.Opcode.ldb =>

      (* load instruction uses raddr, defines r1 *)
      WITH memOp = LOOPHOLE(inst, Ir.MemoryInstruction) DO
        IntSet.Remove (inst.index, result[memOp.r1]);
        IntSet.Add (inst.index, result[memOp.raddr]);
      END;

    | Ir.Opcode.stq .. Ir.Opcode.stb =>

      (* store instruction uses r1 and raddr *)
      WITH memOp = LOOPHOLE(inst, Ir.MemoryInstruction) DO
        IntSet.Add (inst.index, result[memOp.r1]);
        IntSet.Add (inst.index, result[memOp.raddr]);
      END;

    | Ir.Opcode.beq .. Ir.Opcode.br =>

      (* jump instruction uses r1 as condition register *)
      WITH branchOp = LOOPHOLE(inst, Ir.BranchInstruction) DO
        IntSet.Add (inst.index, result[branchOp.r1]);
      END;

    | Ir.Opcode.jsr =>

      (* call instruction uses r2 by jumping to it, defines r1 *)
      WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
        IntSet.Remove (inst.index, result[callOp.r1]);
        IntSet.Add (inst.index, result[callOp.r2]);
      END;

    | Ir.Opcode.ret =>

      (* ret instruction uses r2 by jumping to it, defines r1, v0 is potentially returned *)
      WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
        IntSet.Remove (inst.index, result[callOp.r1]);
        IntSet.Add (inst.index, result[callOp.r2]);
        IntSet.Add (inst.index, result[ORD (Register.T.v0)]);
      END;

    | Ir.Opcode.copy =>

      (* copy instruction defines rdest and uses rsrc. *)
      WITH copyOp = LOOPHOLE(inst, Ir.CopyInstruction) DO
        IntSet.Remove (inst.index, result[copyOp.rdest]);
        IntSet.Add (inst.index, result[copyOp.rsrc]);
      END;

    | Ir.Opcode.call =>

      (* call instruction defines reg *)
      WITH callOp = LOOPHOLE(inst, Ir.CallInstruction) DO
        IntSet.Remove (inst.index, result[callOp.reg]);
      END;

    | Ir.Opcode.fault =>

      (* nothing is live before a fault *)
      result := NEW (REF ARRAY OF IntSet.T, 32);
      FOR i := 0 TO 31 DO
        result[i] := IntSet.New (p.instructions.size);
      END;

    | Ir.Opcode.jt =>

      (* skip *)

    | Ir.Opcode.deleted =>

      (* skip *)

    ELSE
      RAISE Analysis.Problem;
    END;
    RETURN result;
  END GenKill;

PROCEDURE Copy (<* UNUSED*>t: T; rfrom, rto: Residue.T) : Residue.T =
  VAR
    result : R := rto;
  BEGIN
    IF result = NIL THEN
      result := NEW (REF ARRAY OF IntSet.T, 32);
    END;
    IF rfrom = NIL THEN
      <* ASSERT FALSE *>
    END;
    FOR i := 0 TO 31 DO
      result[i] := IntSet.Copy (NARROW (rfrom, R)[i]);
    END;
    RETURN result;
  END Copy;

PROCEDURE Initialize (<* UNUSED *> t: T; <*UNUSED*> c: Ir.Code) =
  BEGIN
  END Initialize;


PROCEDURE SetFlags (<* UNUSED *> t: T; p: Procedure.T) =
  BEGIN
    p.flags := Procedure.FlagSet {Procedure.Flags.Live} + p.flags;
  END SetFlags;

  
(* return index into Register.T.caller_save *)
PROCEDURE FindCallerSaveRegister (start: CARDINAL; res: Residue.T)
  : CARDINAL RAISES {Analysis.Problem} =
  (* returns an index into caller_save *)
  VAR
    r : R;
    current : CARDINAL;
  BEGIN
    r := NARROW (res, R);

    FOR i := start TO LAST (Register.caller_save) DO
      current := ORD (Register.caller_save [i]);
      IF IntSet.IsEmpty (r[current]) THEN
        RETURN i;
      END;
    END;
    RAISE Analysis.Problem;
  END FindCallerSaveRegister;
  
BEGIN
END Live.
