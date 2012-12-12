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
 * 04-Nov-97  Wilson Hsieh (whsieh) at the University of Washington
 *	fix Split
 *
 *
 *)

UNSAFE MODULE BasicBlock;

IMPORT Fmt, Wr, Stdio, Thread;
IMPORT Ir;
IMPORT JumpTable, SortedRCGTbl;
IMPORT Procedure;

<* FATAL Wr.Failure, Thread.Alerted *>

(* Traverse the block and all of its successors.  Preorder. *)
PROCEDURE Sweep (block: T; p: Proc; arg: REFANY) RAISES ANY =

  PROCEDURE InnerSweep (block: T; p: Proc; arg: REFANY) RAISES ANY =
    VAR
      save: T;
    BEGIN
      IF block.sweepMarker = sweepId THEN
        RETURN;
      END;
      
      block.sweepMarker := sweepId;
      p (block, arg);
      
      FOR i := FIRST (block.successors^) TO LAST (block.successors^) DO
        save := block.successors[i];
        InnerSweep (block.successors[i], p, arg);
        <* ASSERT save = block.successors[i] *>
      END;
    END InnerSweep;

  BEGIN
    INC (sweepId);
    InnerSweep (block, p, arg);
  END Sweep;


(* Traverse the block and all of its predecessors. *)
PROCEDURE ReverseSweep(block: T; p: Proc; arg: REFANY) RAISES ANY =
  PROCEDURE InnerReverse(block: T; p: Proc; arg: REFANY) RAISES ANY =
    BEGIN
      IF block.sweepMarker = sweepId THEN
        RETURN;
      END;

      block.sweepMarker := sweepId;
      p(block, arg);

      FOR i := FIRST(block.predecessors^) TO LAST(block.predecessors^) DO
        InnerReverse(block.predecessors[i], p, arg);
      END;
    END InnerReverse;

  BEGIN
    INC(sweepId);
    InnerReverse(block, p, arg);
  END ReverseSweep;


(* Traverse the block and all of its successors.  Postorder. *)
PROCEDURE SweepPost (block: T; p: Proc; arg: REFANY) RAISES ANY =
  PROCEDURE InnerSweep (block: T; p: Proc; arg: REFANY) RAISES ANY =
    VAR
      save: T;
    BEGIN
      IF block.sweepMarker = sweepId THEN
        RETURN;
      END;
      
      block.sweepMarker := sweepId;
      
      FOR i := FIRST (block.successors^) TO LAST (block.successors^) DO
        save := block.successors[i];
        InnerSweep (block.successors[i], p, arg);
        <* ASSERT save = block.successors[i] *>
      END;

      p (block, arg);
    END InnerSweep;

  BEGIN
    INC (sweepId);
    InnerSweep (block, p, arg);
  END SweepPost;


(* Traverse the block and all of its successors in some topological order.
   Note that back edges from loop tails to heads are not considered real
   edges for the purposes of this sweep. *)
PROCEDURE TopSweep(block: T; p: Proc; arg: REFANY) RAISES ANY =
  PROCEDURE RealPredsDone(test: T): BOOLEAN =
    BEGIN
      FOR i := FIRST (test.predecessors^) TO LAST (test.predecessors^) DO
        IF test.predecessors[i].sweepMarker # sweepId THEN
          (* We may have found a predecessor that has not been visited.
             Check whether it is a back edge predecessor. *)
          IF NOT (test.predecessors[i].isLoopTail AND 
                  test.predecessors[i].head = test) THEN
            (* It is not a back edge, so it prevents the test node from
               being visited yet. *)
            RETURN FALSE;
          END;
        END;
      END;
      RETURN TRUE;
    END RealPredsDone;

  PROCEDURE InnerTop(block: T; p: Proc; arg: REFANY) RAISES ANY =
    BEGIN
      IF block.sweepMarker = sweepId THEN
        RETURN;
      END;

      IF RealPredsDone(block) THEN
        block.sweepMarker := sweepId;
        p(block, arg);

        FOR i := FIRST(block.successors^) TO LAST(block.successors^) DO
          InnerTop(block.successors[i], p, arg);
        END;
      END;
    END InnerTop;

  BEGIN
    INC(sweepId);
    InnerTop(block, p, arg);
  END TopSweep;

(* Traverse the start block and all of its successors until you 
   encounter stop. *)
PROCEDURE SweepBetween(start, stop: T; p: Proc; arg: REFANY) =
  PROCEDURE InnerBetween(start, stop: T; p: Proc; arg: REFANY) =
    BEGIN
      IF start.sweepMarker = sweepId OR start = stop THEN
        RETURN;
      END;

      start.sweepMarker := sweepId;
      p(start, arg);
      FOR i := FIRST(start.successors^) TO LAST(start.successors^) DO
        InnerBetween(start.successors[i], stop, p, arg);
      END;
    END InnerBetween;

  BEGIN
    INC(sweepId);
    InnerBetween(start, stop, p, arg);
  END SweepBetween;

PROCEDURE GetBackEdgePred(block: T): T =
  BEGIN
    <* ASSERT block.isLoopHead *>
    RETURN block.tail;
  END GetBackEdgePred;

PROCEDURE GetBackEdgeSucc(block: T): T =
  BEGIN
    <* ASSERT block.isLoopTail *>
    RETURN block.head;
  END GetBackEdgeSucc;


PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem} =
  BEGIN
    IF s = NIL THEN s := Stdio.stdout; END;
    Wr.PutText (s, "---Start basic block #" & Fmt.Int(t.index) & "---\n");

    TRY
      Ir.PrintArray (t.instructions, s);
    EXCEPT
      Ir.Problem => RAISE Problem;
    END;

    IF t.predecessors # NIL THEN
      Wr.PutText (s, "\nPredecessors: ");
      FOR i := FIRST (t.predecessors^) TO LAST(t.predecessors^) DO
        IF t.predecessors[i] = NIL THEN Wr.PutText(s, "XX "); 
        ELSE
          Wr.PutText (s, Fmt.Int(t.predecessors[i].index) & " ");
        END;
      END;
    END;

    IF t.successors # NIL THEN
      Wr.PutText (s, "\nSuccessors: ");
      FOR i := FIRST (t.successors^) TO LAST(t.successors^) DO
        IF t.successors[i] = NIL THEN Wr.PutText(s, "XX ");
        ELSE
          Wr.PutText (s, Fmt.Int(t.successors[i].index) & " ");
        END;
      END;
    END;

    IF t.isLoopHead THEN
      Wr.PutText(s, "\nThis is a loop head. The tail is block #" & 
        Fmt.Int(t.tail.index));
    END;

    IF t.isLoopTail THEN
      Wr.PutText(s, "\nThis is a loop tail. The head is block #" & 
        Fmt.Int(t.head.index));
    END;

    Wr.PutText(s, "\nOffset = " & Fmt.Int(t.offset) & "\n");
      
    Wr.PutText (s, "\n---End basic block #" & Fmt.Int(t.index) & "---\n");
    Wr.Flush(s);
  END Print;


PROCEDURE Copy (t: T) : T =
  VAR newT := NEW (T);
  BEGIN
    (* copy everything shallowly *)
(* FIXME: breaks reference counting
    newT^ := t^;
*)
    (* copy the instruction array *)
    newT.instructions := NEW (Ir.InstructionArray, NUMBER (t.instructions^));
    newT.instructions^ := t.instructions^;

    (* copy the connectivity *)
    newT.successors := NEW (TArray, NUMBER (t.successors^));
    newT.successors^ := t.successors^;
    newT.predecessors := NEW (TArray, NUMBER (t.predecessors^));
    newT.predecessors^ := t.predecessors^;

    RETURN newT;
  END Copy;


(* return array that looks like:
   a1[0..offset] a2[0..last(a2)] a1[offset+1..last(a1)] *)   
PROCEDURE InsertArray (a1, a2: TArray; offset: CARDINAL) : TArray =
  VAR returnArray := NEW (TArray, NUMBER (a1^) + NUMBER (a2^));
  BEGIN
    (* copy the first part of the array *)
    FOR i := 0 TO offset DO
      returnArray[i] := a1[i];
    END;
    (* copy a2 *)
    FOR i := 0 TO LAST (a2^) DO
      returnArray[offset+i+1] := a2[i];
    END;
    (* copy the rest of a1 *)
    FOR i := offset+1 TO LAST (a1^) DO
      returnArray[NUMBER(a2^)+i] := a1[i];
    END;
    RETURN returnArray;
  END InsertArray;


(* traverse all of the instructions in the block *)
PROCEDURE InstructionSweep (bb: T; p: InstructionProc; r: REFANY) =
  BEGIN
    ArraySweep (bb.instructions, p, r, FALSE);
  END InstructionSweep;


(* traverse all of the instructions in the block *)
PROCEDURE InstructionSweepShallow (bb: T; p: InstructionProc; r: REFANY) =
  BEGIN
    ArraySweep (bb.instructions, p, r, TRUE);
  END InstructionSweepShallow;


PROCEDURE ArraySweep (ia: Ir.InstructionArray; p: InstructionProc; r: REFANY;
                      shallow: BOOLEAN) =
  VAR
    iter: SortedRCGTbl.Iterator;
    value: INTEGER; td: REFANY;
  BEGIN
    TRY
      FOR i := 0 TO LAST (ia^) DO
        WITH inst = ia[i] DO
          IF inst.op = Ir.Opcode.indirect THEN
            WITH insti = LOOPHOLE (inst, Ir.IndirectInstruction) DO
              ArraySweep (insti.indirect, p, r, shallow);
            END;
          ELSIF inst.op = Ir.Opcode.jt THEN
            IF NOT shallow THEN
              WITH jt = LOOPHOLE (inst, Ir.JtInstruction).table DO
                iter := jt.data.iterateOrdered ();
                WHILE iter.next (value, td) DO
                  WITH jt_ia = NARROW (NARROW (td, JumpTable.TableData).block,
                                       Ir.InstructionArray) DO
                    FOR i := 0 TO LAST (jt_ia^) DO
                      ArraySweep (jt_ia, p, r, shallow);
                    END;
                  END;
                END;
              END;
            END;
            p (inst, r);
          ELSE 
            p (inst, r);
          END;
        END;
      END;
    EXCEPT
      Done => (* ok *)
    END;
  END ArraySweep;

PROCEDURE Split (bb: T; index: INTEGER; proc: REFANY (*Procedure.T*))
  : T RAISES {Problem} =
  VAR
    newbb : T;
    count : INTEGER;
    found : BOOLEAN;
    p: Procedure.T := proc;

  PROCEDURE Count (VAR inst: Ir.Instruction; <* UNUSED *> u: REFANY) =
    BEGIN
      IF found AND inst.index # -1 THEN
        INC (count);
        RETURN;
      END;
      IF inst.index = index THEN
        found := TRUE;
      END;
    END Count;

  PROCEDURE Move (VAR inst: Ir.Instruction; <* UNUSED *> u: REFANY) =
    BEGIN
      IF found AND inst.index # -1 THEN
        newbb.instructions[count] := inst;
        inst.op := Ir.Opcode.deleted;

        (* update instruction map *)
        p.instructions.map[inst.index] := newbb;

        INC (count);
        RETURN;
      END;
      IF inst.index = index THEN
        found := TRUE;
      END;
    END Move;

  BEGIN
    IF index < 0 THEN RAISE Problem; END;

    (* split basic block *)
    newbb := NEW (T);

    (* set count *)
    count := 0; found := FALSE;
    InstructionSweepShallow (bb, Count, NIL);

    newbb.instruction_count := count;
    newbb.instructions := NEW (Ir.InstructionArray, count);

    (* move instructions over *)
    count := 0; found := FALSE;
    InstructionSweepShallow (bb, Move, NIL);

    (* fix up successor/predecessor chains *)
    newbb.successors := bb.successors;

    newbb.predecessors := NEW (TArray, 1);
    newbb.predecessors[0] := bb;
    newbb.successors := bb.successors;

    bb.successors := NEW (TArray, 1);
    bb.successors[0] := newbb;
    
    FOR i := 0 TO LAST (newbb.successors^) DO
      WITH succ = newbb.successors[i] DO
        FOR j := 0 TO LAST (succ.predecessors^) DO
          WITH succ_pred = succ.predecessors[i] DO
            IF succ_pred = bb THEN
              succ_pred := newbb;
            END;
          END;
        END;
      END;
    END;

    Procedure.AddBB (p, newbb, bb.index+1);

    (* fix up procedure bottom ptr *)
    FOR i := 0 TO LAST (p.bottoms^) DO
      IF p.bottoms[i] = bb THEN p.bottoms[i] := newbb; END;
    END;
    IF p.bottom = bb THEN p.bottom := newbb; END;

    RETURN newbb;
  END Split;


BEGIN
END BasicBlock.
