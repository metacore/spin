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

UNSAFE MODULE Traverse EXPORTS Traverse;

IMPORT Wr, Stdio, Fmt, Thread;
IMPORT Ir, BasicBlock, BasicBlockQueue, Procedure;
IMPORT Analysis, Residue;

IMPORT JumpTable, SortedRCGTbl;

<* FATAL Thread.Alerted, Wr.Failure *>

EXCEPTION Stopped (Residue.T);

CONST
  Debug = FALSE;

PROCEDURE IDFA_All(start, finish: BasicBlock.T; analysis: Analysis.T;
                   p: Procedure.T) RAISES {Analysis.Problem} =
  VAR
    is_forward := analysis.Direction() = Analysis.Direction.Forward;
    q := BasicBlockQueue.NewQ();
    bbThis : BasicBlock.T;
  BEGIN
    IF Debug THEN
      Wr.PutText(Stdio.stderr, "IDFA_All " & Fmt.Int(start.index) & "\n");
    END;

    BasicBlockQueue.Enqueue(q, start);
    WHILE NOT BasicBlockQueue.IsEmpty(q) DO
      bbThis := BasicBlockQueue.Dequeue(q);
      IF CanAnalyze(bbThis, analysis) THEN
        EVAL IDFA(bbThis, analysis, p);
        GatherBlockSuccessors(bbThis, q, is_forward);
        IF bbThis = finish THEN
          RETURN;
        END;
      ELSIF IsLoopStart(bbThis, is_forward) THEN
        BasicBlockQueue.EnqueueUnique(q, IDFALoop(bbThis, analysis, p));
      END;
    END;
  END IDFA_All;


PROCEDURE IDFALoop(start: BasicBlock.T; analysis: Analysis.T;
                   p: Procedure.T)
  : BasicBlock.T RAISES {Analysis.Problem} =
  VAR
    is_forward := analysis.Direction() = Analysis.Direction.Forward;
    q := BasicBlockQueue.NewQ();

    bbThis, bbReturn : BasicBlock.T;
    bbOtherEnd := GetLoopOtherEnd(start);
    bbExit := GetLoopExit(start);
    bbPre := GetLoopPre(start);

    lastBackEdgeResidue : Residue.T;
  BEGIN
    IF Debug THEN
      Wr.PutText(Stdio.stderr, "IDFALoop " & Fmt.Int(start.index) & "\n");
    END;

    IF is_forward THEN
      bbReturn := bbExit;
      BasicBlock.SweepBetween(start, bbOtherEnd, InvalidateResidueBlock, 
                              analysis);
    ELSE
      bbReturn := bbPre;
      BasicBlock.SweepBetween(bbOtherEnd, start, InvalidateResidueBlock,
                              analysis);
    END;
    analysis.SetResidueOut (bbOtherEnd, analysis.BestGuess(p));

    BasicBlockQueue.Enqueue(q, start);
    WHILE NOT BasicBlockQueue.IsEmpty(q) DO
      bbThis := BasicBlockQueue.Dequeue(q);
      IF NOT bbThis = bbExit AND NOT bbThis = bbPre THEN
        IF IsLoopStart(bbThis, is_forward) AND bbThis = start THEN
          IF analysis.Converged(BackEdgeResidue(analysis, bbThis),
                                lastBackEdgeResidue) THEN
            RETURN bbReturn;
          END;

          WHILE NOT BasicBlockQueue.IsEmpty(q) DO 
            EVAL BasicBlockQueue.Dequeue(q); 
          END;

          (* save the last residue *)
          lastBackEdgeResidue := BackEdgeResidue(analysis, bbThis);
          EVAL IDFA(bbThis, analysis, p);
          GatherBlockSuccessors(bbThis, q, is_forward);
        ELSIF IsLoopStart(bbThis, is_forward) THEN
          BasicBlockQueue.EnqueueUnique(q, IDFALoop(bbThis, analysis, p));
        ELSIF CanAnalyze(bbThis, analysis) THEN
          EVAL IDFA(bbThis, analysis, p);
          GatherBlockSuccessors(bbThis, q, is_forward);
        END;
      END;
    END;

    IF CanAnalyze(bbPre, analysis) AND CanAnalyze(bbExit, analysis) THEN
      RETURN bbReturn;
    ELSE
      RETURN start;
    END;
  END IDFALoop;
  
PROCEDURE GatherBlockSuccessors(block: BasicBlock.T; q: BasicBlockQueue.T;
                                is_forward: BOOLEAN) =
  BEGIN
    IF is_forward THEN
      FOR i := 0 TO LAST(block.successors^) DO
        BasicBlockQueue.EnqueueUnique(q, block.successors[i]);
      END;
    ELSE
      FOR i := 0 TO LAST(block.predecessors^) DO
        BasicBlockQueue.EnqueueUnique(q, block.predecessors[i]);
      END;
    END;
  END GatherBlockSuccessors;

PROCEDURE IsLoopStart(block: BasicBlock.T; is_forward: BOOLEAN): BOOLEAN =
  BEGIN
    IF is_forward THEN
      RETURN block.isLoopHead;
    ELSE
      RETURN block.isLoopTail;
    END;
  END IsLoopStart;

PROCEDURE GetLoopPre(block: BasicBlock.T): BasicBlock.T =
  BEGIN
    RETURN block.pre;
  END GetLoopPre;

PROCEDURE GetLoopExit(block: BasicBlock.T): BasicBlock.T =
  BEGIN
    RETURN block.next;
  END GetLoopExit;

PROCEDURE GetLoopOtherEnd(block: BasicBlock.T): BasicBlock.T RAISES {Analysis.Problem} =
  BEGIN
    IF block.isLoopHead THEN
      RETURN block.tail;
    ELSIF block.isLoopTail THEN
      RETURN block.head;
    END;
    RAISE Analysis.Problem;
  END GetLoopOtherEnd;

PROCEDURE BackEdgeResidue(analysis: Analysis.T; block: BasicBlock.T): Residue.T =
  BEGIN
    IF analysis.Direction() = Analysis.Direction.Forward THEN
      RETURN analysis.GetResidueOut(BasicBlock.GetBackEdgePred(block));
    ELSE
      RETURN analysis.GetResidueOut(BasicBlock.GetBackEdgeSucc(block));
    END;
  END BackEdgeResidue;


PROCEDURE CanAnalyze(block: BasicBlock.T; analysis: Analysis.T): BOOLEAN =
  (* returns TRUE if the successors/predecessors of the block have been
     analyzed
     should only be FALSE if the block is a loop head/tail
   *)
  VAR
    ok : BOOLEAN := TRUE;
    next : BasicBlock.T;
  BEGIN
    IF analysis.Direction() = Analysis.Direction.Forward THEN
      FOR i := 0 TO LAST(block.predecessors^) DO
        next := block.predecessors[i];
        IF NOT next.terminal THEN
          ok := ok AND analysis.GetResidueOut(next) # NIL;
        END;
      END;
    ELSE
      FOR i := 0 TO LAST(block.successors^) DO
        next := block.successors[i];
        IF NOT next.terminal THEN
          ok := ok AND analysis.GetResidueOut(next) # NIL;
        END;
      END;
    END; 
    RETURN ok;
  END CanAnalyze;


PROCEDURE IDFA (block: BasicBlock.T;
                analysis: Analysis.T;
                p: Procedure.T;
                stopAt: INTEGER := -1;
                stopBefore: BOOLEAN := FALSE)
  : Residue.T RAISES {Analysis.Problem} =
  VAR
    r : Residue.T;
    is_forward := analysis.Direction() = Analysis.Direction.Forward;
    inst : Ir.Instruction;
    start, end, increment : INTEGER;
    next: BasicBlock.T;
    set: BOOLEAN;
  BEGIN
    IF Debug THEN
      Wr.PutText(Stdio.stderr, "IDFA " & Fmt.Int(block.index) & "\n");
    END;

    set := FALSE;
    IF is_forward THEN
      r := analysis.StartGuess(p);
      FOR i := 0 TO LAST (block.predecessors^) DO
        next := block.predecessors[i];
        IF NOT next.terminal THEN
          IF NOT set THEN
            r := analysis.Copy (analysis.GetResidueOut (next), NIL);
            set := TRUE;
          ELSE
            r := analysis.Merge (analysis.GetResidueOut (next), r);
          END;
        END;
      END;

      start := 0;
      end := block.instruction_count-1;
      increment := 1;
    ELSE
      r := analysis.StartGuess(p);
      FOR i := 0 TO LAST (block.successors^) DO
        next := block.successors[i];
        IF NOT next.terminal THEN
          IF NOT set THEN
            r := analysis.Copy (analysis.GetResidueOut (next), NIL);
            set := TRUE;
          ELSE
            r := analysis.Merge (analysis.GetResidueOut (next), r);
          END;
        END;
      END;

      start := block.instruction_count-1;
      end := 0;
      increment := -1;
    END;

    analysis.SetResidueIn (block, r);

    FOR i := start TO end BY increment DO
      inst := block.instructions[i];
      TRY
        r := GenKill (inst, r, p, analysis, stopAt, stopBefore);
      EXCEPT
        Stopped (res) =>
        RETURN res;
      END;
    END;

    analysis.SetResidueOut (block, r);
    RETURN r;
  END IDFA;


PROCEDURE GenKillArray (ia: Ir.InstructionArray; r: Residue.T; p: Procedure.T;
                        a: Analysis.T; stopAt: INTEGER; stopBefore: BOOLEAN)
  : Residue.T RAISES {Analysis.Problem, Stopped} =
  (* modifies: nothing
     effect: does GenKill over an instruction array
  *)
  VAR
    forward := a.Direction() = Analysis.Direction.Forward;
    start, end, increment : INTEGER;
  BEGIN
    IF forward THEN
      start := 0;
      end := LAST (ia^);
      increment := 1;
    ELSE
      start := LAST (ia^);
      end := 0;
      increment := -1;
    END;

    FOR i := start TO end BY increment DO
      r := GenKill (ia[i], r, p, a, stopAt, stopBefore);
    END;

    RETURN r;
  END GenKillArray;


PROCEDURE GetResidueAfter (a: Analysis.T; p: Procedure.T; i: INTEGER)
  : Residue.T RAISES {Analysis.Problem} =
  VAR
    bb: BasicBlock.T;
  BEGIN
    IF i < 0 THEN RAISE Analysis.Problem; END;

    bb := p.instructions.map[i];
    RETURN IDFA (bb, a, p, i, stopBefore := FALSE);
  END GetResidueAfter;


PROCEDURE GetResidueBefore (a: Analysis.T; p: Procedure.T; i: INTEGER)
  : Residue.T RAISES {Analysis.Problem} =
  VAR
    bb: BasicBlock.T;
  BEGIN
    IF i < 0 THEN RAISE Analysis.Problem; END;

    bb := p.instructions.map[i];
    RETURN IDFA (bb, a, p, i, stopBefore := TRUE);
  END GetResidueBefore;


PROCEDURE GenKill (i: Ir.Instruction; r: Residue.T; p: Procedure.T;
                   a: Analysis.T; stopAt: INTEGER; stopBefore: BOOLEAN)
  : Residue.T RAISES {Analysis.Problem, Stopped} =
  VAR
    forward := a.Direction() = Analysis.Direction.Forward;
  BEGIN
    (* check for stop *)
    IF stopAt # -1 AND i.index = stopAt AND stopBefore THEN
      RAISE Stopped (r);
    END;

    IF i.op = Ir.Opcode.indirect THEN
      (* we have a nested instruction *)
      WITH in = LOOPHOLE (i, Ir.IndirectInstruction).indirect DO
        RETURN GenKillArray (in, r, p, a, stopAt, stopBefore);
      END;

    ELSIF i.op = Ir.Opcode.jt THEN
      (* jump table *)
      WITH jt = LOOPHOLE (i, Ir.JtInstruction) DO
        VAR
          iter: SortedRCGTbl.Iterator := jt.table.data.iterateOrdered ();
          value: INTEGER; td: REFANY;
          first := TRUE;
          copyr : Residue.T;
        BEGIN
          IF forward THEN
            r := a.GenKill (i, r, p);
          END;
          
          copyr := a.Copy (r, NIL);
          WHILE iter.next (value, td) DO
            IF first THEN
              r := GenKillArray (NARROW (td, JumpTable.TableData).block, copyr, p, a, stopAt, stopBefore);
            ELSE
              first := FALSE;
              r := a.Merge (r, GenKillArray (NARROW (td, JumpTable.TableData).block, copyr, p, a, stopAt, stopBefore));
            END;
          END;
          
          IF NOT forward (* backward *) THEN
            r := a.GenKill (i, r, p);
          END;
        END;
      END;

    ELSE
      r := a.GenKill (i, r, p);
    END;

    (* check for stop *)
    IF stopAt # -1 AND i.index = stopAt AND NOT stopBefore THEN
      RAISE Stopped (r);
    END;

    RETURN r;
  END GenKill;


PROCEDURE InvalidateResidueBlock(block: BasicBlock.T; arg: REFANY) =
  VAR
    analysis := NARROW(arg, Analysis.T);
  BEGIN
    analysis.InvalidateResidueBlock(block);
  END InvalidateResidueBlock;
    
BEGIN
END Traverse.
