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


UNSAFE MODULE Procedure;

IMPORT Wr, Stdio, Fmt, Thread;
IMPORT Ir, BasicBlock, JumpTable;

CONST debug = TRUE;

<* FATAL Wr.Failure, Thread.Alerted *>

(* sanity checking *)
PROCEDURE Check (t: T) : BOOLEAN =
  VAR
    ok := FALSE;

  PROCEDURE Checker (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =
    BEGIN
      IF bb.successors # NIL THEN
        FOR i := FIRST (bb.successors^) TO LAST (bb.successors^) DO
          WITH s = bb.successors[i] DO
            IF s # NIL THEN
              (* check that forward/backward links are OK *)
              FOR j := FIRST (s.predecessors^) TO LAST (s.predecessors^) DO
                IF s.predecessors[j] = bb THEN ok := TRUE; EXIT; END;
              END;
              IF ok = FALSE THEN
                Wr.PutText (Stdio.stderr,
                            "Basic block " & Fmt.Int (bb.index) & " is bad\n");
              END;
            END;
          END;
        END;
      END;
    END Checker; 

  BEGIN
    IF t.top = NIL THEN RETURN TRUE; END;
    TRY
      BasicBlock.Sweep (t.top, Checker, NIL);
    EXCEPT ELSE
      RETURN FALSE;
    END;
    RETURN ok;
  END Check;

(* print a procedure *)
PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem} =

  PROCEDURE Printer (bb: BasicBlock.T; <* UNUSED *> r: REFANY)
    RAISES {Problem} =
    (* using a nested proc avoids having to LOOPHOLE this proc
       into a BasicBlock.Proc *)
    BEGIN
      (* s is the stream *)
      TRY
        BasicBlock.Print (bb, s);
      EXCEPT ELSE
        RAISE Problem;
      END;
    END Printer; 

  BEGIN
    IF t.top = NIL THEN RETURN; END;
    IF s = NIL THEN s := Stdio.stdout; END;
    
    (* clear count *)
    TRY
      BasicBlock.Sweep (t.top, Printer, NIL);
    EXCEPT ELSE
      RAISE Problem;
    END;
  END Print;


(* procedures used for copying *)
PROCEDURE Clear (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =
  (* clear the count field in bb *)
  BEGIN
    bb.copy := NIL;
  END Clear;

(* fixup pointers to successor, predecessor blocks in copy *)
PROCEDURE Fixup (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =
  VAR
    copy := bb.copy;
  BEGIN
    FOR i := FIRST (copy.successors^) TO LAST (copy.successors^) DO
      WITH s = copy.successors[i] DO
        s := s.copy;
      END;
    END;

    FOR i := FIRST (copy.predecessors^) TO LAST (copy.predecessors^) DO
      WITH s = copy.predecessors[i] DO
        s := s.copy;
      END;
    END;
  END Fixup;


PROCEDURE Copy (t: T; bbs: CARDINAL := 0) : T RAISES {Problem} =
  (* copy a procedure
     add BBS more basic blocks to the copy

     TODO: copy other information too
   *)

  VAR
    newT : T := NewT ();

  (* copy bb *)
  PROCEDURE Copier (bb: BasicBlock.T; <* UNUSED *> r: REFANY) =
    VAR newBB := BasicBlock.Copy (bb);
    BEGIN
      bb.copy := newBB;
      IF newBB.instructions # NIL THEN
        FOR i := FIRST (newBB.instructions^) TO LAST (newBB.instructions^) DO
          WITH inst = newBB.instructions[i] DO
            CASE inst.op OF
            | Ir.Opcode.indirect =>
              WITH ind = LOOPHOLE (inst, Ir.IndirectInstruction) DO
                ind.indirect := Ir.CopyArray (ind.indirect);
              END;
            | Ir.Opcode.jt =>
              WITH jti = LOOPHOLE (inst, Ir.JtInstruction) DO
                jti.table := JumpTable.Copy (jti.table);
              END;
            ELSE
            END;
          END;
        END;
      END;
    END Copier;

  BEGIN
    IF t.top = NIL THEN RETURN newT; END;

    (* copy all of the basic blocks *)
    TRY
      BasicBlock.Sweep (t.top, Clear, NIL);
      BasicBlock.Sweep (t.top, Copier, NIL);
      BasicBlock.Sweep (t.top, Fixup, NIL);
    EXCEPT ELSE
      RAISE Problem;
    END;

    newT.top := t.top.copy;
    newT.bottom := t.bottom.copy;
    newT.bottoms := NEW (REF ARRAY OF BasicBlock.T, NUMBER (t.bottoms^));
    FOR i := 0 TO LAST (t.bottoms^) DO
      newT.bottoms[i] := t.bottoms[i].copy;
    END;
    newT.gp := t.gp;
    newT.registers := t.registers;
    newT.frame := t.frame;

    (* copy other data structures *)
    newT.table := NEW (BasicBlock.TArray, NUMBER (t.table^) + bbs);
    FOR i := 0 TO LAST (t.table^) DO
      newT.table[i] := t.table[i].copy;
    END;

    (* clear other fields *)
    newT.flags := FlagSet {};

    RETURN newT;
  END Copy;



PROCEDURE Sweep (t: T; p: BasicBlock.Proc; r: REFANY) RAISES {Problem} =
  BEGIN
    TRY
      BasicBlock.Sweep (t.top, p, r);
    EXCEPT ELSE
      RAISE Problem;
    END;
  END Sweep;


PROCEDURE SweepPost (t: T; p: BasicBlock.Proc; r: REFANY) =
  BEGIN
    BasicBlock.SweepPost (t.top, p, r);
  END SweepPost;


PROCEDURE Linearize (t: T) =
  BEGIN
    (* compute final order *)

    (* clean out jumps *)
    FOR bbindex := FIRST (t.table^) TO LAST (t.table^) DO
      WITH bb = t.table[bbindex],
           inst = bb.instructions[bb.instruction_count-1] DO
        CASE inst.op OF
        | Ir.Opcode.beq .. Ir.Opcode.br =>
            (* jump instruction *)
            WITH branchOp = LOOPHOLE(inst, Ir.BranchInstruction) DO
              IF branchOp.target = bbindex+1 THEN
                
                IF debug THEN
                  Wr.PutText (Stdio.stderr,
                              "removed fall-through jump in basic block " &
                              Fmt.Int (bb.index) & "\n");
                END;

                inst := Ir.Instruction { op := Ir.Opcode.noop };
                DEC(bb.instruction_count);
              END;
            END;
        ELSE
        END;
      END;
    END;

  END Linearize;

(* helper for building up instruction index for procedure *)
PROCEDURE IndexInstruction (VAR ia: InstructionIndex;
                            bb: BasicBlock.T) : CARDINAL =
  VAR
    new : IndexArray;
    original := NUMBER (ia.map^);
    number := ia.size;
  BEGIN
    IF number >= original THEN
      new := NEW (IndexArray, 2*original);
      SUBARRAY (new^, 0, original) := ia.map^;
      ia.map := new;
    END;

    ia.map[number] := bb;

    INC (ia.size);
    RETURN number;
  END IndexInstruction;


(* insert new bb into procedure table *)
PROCEDURE AddBB (p: T; bb: BasicBlock.T; index: INTEGER) =
  VAR
    tmp: BasicBlock.T;
  BEGIN
    FOR i := LAST (p.table^)-1 TO index BY -1 DO
      tmp := p.table[i];
      IF tmp # NIL THEN
        p.table[i+1] := tmp;
        INC (tmp.index);
      END;
    END;
    p.table[index] := bb;
    bb.index := index;
  END AddBB;


PROCEDURE NewT () : T =
  VAR t: T;
  BEGIN
    t := NEW (T);
    t.instructions.size := 0;
    t.instructions.map := NEW (IndexArray, 5);
    RETURN t;
  END NewT;

PROCEDURE InstructionSweep (t: T; p: BasicBlock.InstructionProc; r: REFANY)
  RAISES {Problem} =

  PROCEDURE NestedSweep (bb: BasicBlock.T; <* UNUSED *> u: REFANY) =
    BEGIN
      BasicBlock.InstructionSweep (bb, p, NIL);
    END NestedSweep;

  BEGIN
    TRY
      BasicBlock.Sweep (t.top, NestedSweep, r);
    EXCEPT ELSE
      RAISE Problem;
    END;
  END InstructionSweep;

PROCEDURE Returns (t: T) : REFANY (* Expression.T *) =
  BEGIN
    IF t = NIL THEN
      RETURN NIL;
    ELSE
      RETURN t.returns;
    END;
  END Returns;

BEGIN
END Procedure.
