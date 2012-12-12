(*
 *
 * Copyright 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

MODULE Flow;

IMPORT Fmt, Wr, Stdio, Thread;
IMPORT BasicBlock, Procedure;
IMPORT Expression;
IMPORT IntSet;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST verbose = FALSE;

TYPE Direction = { Forward, Backward };


(* compute dominators *)
PROCEDURE DoDom (p: Procedure.T; d: Direction) =
  VAR
    count : INTEGER := NUMBER (p.table^);
    top : IntSet.T := IntSet.New (count);
    bottom : IntSet.T := IntSet.New (count);
    change : BOOLEAN := TRUE;

  (* set initial conditions for each basic block *)
  PROCEDURE SetSet (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    BEGIN
      IF d = Direction.Forward THEN
        IF bb = p.top THEN
          bb.dominators := top;
        ELSE
          bb.dominators := IntSet.NewComplete (count);
        END;
        bb.dominates := IntSet.New (count);
        bb.idominates := IntSet.New (count);
      ELSE
        IF bb = p.bottom THEN
          bb.postdominators := bottom;
        ELSE
          bb.postdominators := IntSet.NewComplete (count);
        END;
        bb.postdominates := IntSet.New (count);
        bb.ipostdominates := IntSet.New (count);
      END;
    END SetSet;
    
  (* do the work for computing dominators/post-dominators *)
  PROCEDURE Work (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    VAR
      tmp, out : IntSet.T;
    BEGIN
      IF d = Direction.Forward THEN
        IF bb # p.top THEN
          tmp := IntSet.New (count);
          IntSet.Add (bb.index, tmp);

          out := bb.predecessors[0].dominators;
          FOR i := 1 TO NUMBER (bb.predecessors^)-1 DO
            out := IntSet.Intersection (out, bb.predecessors[i].dominators);
          END;
          out := IntSet.Union (tmp, out);
          
          IF NOT IntSet.IsEqual (out, bb.dominators) THEN
            change := TRUE;
            bb.dominators := out;
          END;
        END;
      ELSE
        IF bb # p.bottom THEN
          tmp := IntSet.New (count);
          IntSet.Add (bb.index, tmp);

          IF NUMBER (bb.successors^) > 0 THEN
            out := bb.successors[0].postdominators;
            FOR i := 1 TO NUMBER (bb.successors^)-1 DO
              out := IntSet.Intersection (out, bb.successors[i].postdominators);
            END;

            (* add a fake edge from p.top to p.bottom *)
            IF bb = p.top THEN
              out := IntSet.Intersection (out, bottom);
            END;

            out := IntSet.Union (tmp, out);
          ELSE
            (* block calls m3_fault, has no successors *)
            out := tmp;
          END;
          
          IF NOT IntSet.IsEqual (out, bb.postdominators) THEN
            change := TRUE;
            bb.postdominators := out;
          END;
        END;
      END;
    END Work;

  (* compute reverse function: dominated to dominates *)
  PROCEDURE Reverse (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    BEGIN
      IF d = Direction.Forward THEN
        FOR i := 0 TO NUMBER (p.table^)-1 DO
          IF IntSet.IsMember (i, bb.dominators) THEN
            WITH dominator = p.table[i].dominates DO
              IntSet.Add (bb.index, dominator);
            END;
          END;
        END;
      ELSE
        FOR i := 0 TO NUMBER (p.table^)-1 DO
          IF IntSet.IsMember (i, bb.postdominators) THEN
            WITH postdominator = p.table[i].postdominates DO
              IntSet.Add (bb.index, postdominator);
            END;
          END;
        END;
      END;
    END Reverse;

  (* copy dominate/pdominate sets to immediate sets; remove self nodes *)
  PROCEDURE Copy (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    BEGIN
      IF d = Direction.Forward THEN
        bb.idominates := IntSet.Copy (bb.dominates);
        IntSet.Remove (bb.index, bb.dominates);
        IntSet.Remove (bb.index, bb.idominates);
      ELSE
        bb.ipostdominates := IntSet.Copy (bb.postdominates);
        IntSet.Remove (bb.index, bb.postdominates);
        IntSet.Remove (bb.index, bb.ipostdominates);
      END;
    END Copy;

  (* reduce sets to immediate dominators/post-dominators *)
  PROCEDURE Immediate (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    BEGIN
      IF d = Direction.Forward THEN

        FOR i := 0 TO LAST (p.table^) DO
          IF i # bb.index AND IntSet.IsMember (i, bb.dominates) THEN
            WITH curr = p.table[i].idominator DO
              IF curr = NIL THEN
                curr := bb;
              ELSIF IntSet.IsMember (bb.index, curr.dominates) THEN
                IntSet.Remove (i, curr.idominates);
                curr := bb;
              END;
            END;
          END;
        END;

      ELSE

        FOR i := 0 TO LAST (p.table^) DO
          IF i # bb.index AND IntSet.IsMember (i, bb.postdominates) THEN
            WITH curr = p.table[i].ipostdominator DO
              IF curr = NIL THEN
                curr := bb;
              ELSIF IntSet.IsMember (bb.index, curr.postdominates) THEN
                IntSet.Remove (i, curr.ipostdominates);
                curr := bb;
              END;
            END;
          END;
        END;

      END;
    END Immediate;

  BEGIN
    (* initialize *)
    IntSet.Add (p.top.index, top);
    IntSet.Add (p.bottom.index, bottom);
    Procedure.Sweep (p, SetSet, NIL);
    
    (* do the work *)
    WHILE change = TRUE DO
      change := FALSE;
      Procedure.Sweep (p, Work, NIL);
    END;

    Procedure.Sweep (p, Reverse, NIL);
    Procedure.Sweep (p, Copy, NIL);
    Procedure.Sweep (p, Immediate, NIL);
  END DoDom;


PROCEDURE Dominators (p: Procedure.T) =
  BEGIN
    DoDom (p, Direction.Forward);
    p.flags := Procedure.FlagSet {Procedure.Flags.Dominators} + p.flags;

    IF verbose THEN
      Procedure.Sweep (p, PrintDom, NIL);
    END;
  END Dominators;


PROCEDURE Postdominators (p: Procedure.T) =
  BEGIN
    DoDom (p, Direction.Backward);
    p.flags := Procedure.FlagSet {Procedure.Flags.Postdominators} + p.flags;
  END Postdominators;


(*           set up edge data structure               *)

PROCEDURE Edges (p: Procedure.T) =
  PROCEDURE EdgeCount (bb: BasicBlock.T; <* UNUSED *> arg: REFANY) =
    VAR new_edge : CARDINAL;
    BEGIN
      bb.edges_out := NEW (BasicBlock.EdgeArray, NUMBER (bb.successors^));
      FOR i := 0 TO NUMBER (bb.successors^)-1 DO
        new_edge := p.edge_count;
        p.edges[new_edge].source := bb;
        p.edges[new_edge].dest := bb.successors[i];
        bb.edges_out[i] := new_edge;
        INC (p.edge_count);
      END;
    END EdgeCount;

  BEGIN
    (* edges <= 2*nodes for the graphs we currently generate *)
    p.edges := NEW (Procedure.EdgeArray, 2*NUMBER (p.table^));
    Procedure.Sweep (p, EdgeCount, NIL);
    p.flags := Procedure.FlagSet {Procedure.Flags.Edges} + p.flags;
  END Edges;


PROCEDURE EdgeComplement (p: Procedure.T; edge: CARDINAL) : CARDINAL =
  (* assumes a max of two outgoing edges! *)
  VAR source, dest : BasicBlock.T;
  BEGIN
    <* ASSERT Procedure.Flags.Edges IN p.flags *>
    <* ASSERT NUMBER (source.successors^) > 1 *>

    source := p.edges[edge].source;
    dest := p.edges[edge].dest;

    IF dest = source.successors[0] THEN
      RETURN source.edges_out[1];
    ELSE
      RETURN source.edges_out[0];
    END;
  END EdgeComplement;


(* compute control dependence: need postdominators and edges *)

PROCEDURE CD (p: Procedure.T) =
  (* algorithm from Ferrante, Ottenstein, and Warren, 1987.
     TOPLAS, July 1987
  *)
  VAR current : BasicBlock.T;

  PROCEDURE Setup (bb: BasicBlock.T; <* UNUSED *> arg: REFANY) =
    BEGIN
      bb.cd := IntSet.New (NUMBER (p.table^));
    END Setup;

  (* assume that cfg is structured; cd should be a tree *)
  PROCEDURE Immediate (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    VAR
      immed : BasicBlock.T;
    BEGIN
      IF verbose THEN
        Wr.PutText (Stdio.stderr, "Immediate of " & Fmt.Int (bb.index) & "\n");
      END;
      FOR i := 0 TO NUMBER (p.table^)-1 DO
        IF IntSet.IsMember (i, bb.cd) THEN
          immed := p.table[i];

          (* if i controls bb then it could be the immediate controller *)
          IF bb.icd.bb = NIL OR
            IntSet.IsMember (bb.icd.bb.index, immed.cd) THEN
            bb.icd.bb := immed;

            IF immed # p.top THEN  (* top node is special *)
              <* ASSERT NUMBER (immed.successors^) = 2 *>
              IF IntSet.IsMember (immed.successors[0].index, bb.dominators) THEN
                bb.icd.edge := immed.edges_out[0];
              ELSE
                <* ASSERT
                IntSet.IsMember (immed.successors[1].index, bb.dominators) *>
                bb.icd.edge := immed.edges_out[1];
              END;
            END;
          END;

          IF verbose THEN
            Wr.PutText (Stdio.stderr,
                        "Immediate cd of " & Fmt.Int (bb.index) &
                        " is " & Fmt.Int (bb.icd.bb.index) & "\n");
          END;
        END;
      END;
    END Immediate;

  PROCEDURE Transitive (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    VAR
      current := bb;
    BEGIN
      WHILE current.icd.bb # NIL DO
        current := current.icd.bb;
        IntSet.UnionIn (bb.cd, current.cd);
      END;
    END Transitive;

  BEGIN
    <* ASSERT
    Procedure.FlagSet { Procedure.Flags.Postdominators,
                        Procedure.Flags.Edges } <= p.flags *>

    (* initialize *)
    Procedure.Sweep (p, Setup, NIL);

    (* sweep through edges *)
    FOR i := 0 TO p.edge_count-1 DO
      WITH edge = p.edges[i],
           source = edge.source,
           dest = edge.dest DO

        (* if p->q, where q does not immediately postdominate p *)
        IF (NOT IntSet.IsMember (source.index, dest.ipostdominates)) THEN
          current := dest;

          (* move up the postdominator tree *)
          REPEAT
            IntSet.Add (source.index, current.cd);
            current := current.ipostdominator;
          UNTIL current = source.ipostdominator;
        END;
      END;
    END;
    IF p.top # p.bottom THEN
      IntSet.Add (p.top.index, p.bottom.cd);
    END;

    (* move immediate control dependence to a different data structure *)
    Procedure.Sweep (p, Immediate, NIL);

    (* compute transitive control dependence *)
    Procedure.Sweep (p, Transitive, NIL);

    p.flags := Procedure.FlagSet {Procedure.Flags.ControlDependence} + p.flags;
  END CD;


(* least common ancestor *)
PROCEDURE DominatorLCA (bb1, bb2: BasicBlock.T) : BasicBlock.T =
  BEGIN
    IF bb1 = NIL THEN RETURN bb2; END;
    IF bb2 = NIL THEN RETURN bb1; END;
    IF IntSet.IsMember (bb1.index, bb2.dominates) THEN
      RETURN bb2;
    ELSIF IntSet.IsMember (bb2.index, bb1.dominates) THEN
      RETURN bb1;
    ELSE
      RETURN DominatorLCA (bb1.idominator, bb2.idominator);
    END;
  END DominatorLCA;


(* least common ancestor *)
PROCEDURE ControlLCA (bb1, bb2: BasicBlock.T) : BasicBlock.T =
  BEGIN
    IF bb1 = NIL THEN RETURN bb2; END;
    IF bb2 = NIL THEN RETURN bb1; END;
    IF IntSet.IsMember (bb1.index, bb2.cd) THEN
      RETURN bb1;
    ELSIF IntSet.IsMember (bb2.index, bb1.cd) THEN
      RETURN bb2;
    ELSE
      RETURN ControlLCA (bb1.icd.bb, bb2.icd.bb);
    END;
  END ControlLCA;


(* compute expression that represents going from bbfrom to bb *)
PROCEDURE PathExpression (p: Procedure.T; bb, bbfrom: BasicBlock.T;
                          initial: Expression.T) : Expression.T =
  VAR 
    result : Expression.T;
    expr : Expression.T;
    parent : BasicBlock.T;
    edge : CARDINAL;
  BEGIN
    IF (NOT IntSet.IsMember (bb.index, bbfrom.dominates)) AND bb # bbfrom THEN
      RETURN NIL;
    END;

    result := initial;

    (* move up the control dependence tree,
       consing up the and-expression for the conditions
     *)
    WHILE bb # bbfrom DO
      parent := bb.icd.bb;
      edge := bb.icd.edge;

      expr := p.edges[edge].expr;
      result := Expression.LogicalAnd (expr, result);

      (* move up the tree *)
      bb := parent;
    END;
    RETURN result;
  END PathExpression;


  (*          printing routines          *)

PROCEDURE PrintDom (bb: BasicBlock.T; ss : REFANY) =
  VAR s : Wr.T := ss;
  BEGIN
    IF s = NIL THEN s := Stdio.stdout; END;

    Wr.PutText (s, Fmt.Int (bb.index) & " dominates ");
    IntSet.Print (bb.dominates, s);
    Wr.PutText (s, "\n");
    Wr.PutText (s, Fmt.Int (bb.index) & " immediately dominates ");
    IntSet.Print (bb.idominates, s);
    Wr.PutText (s, "\n");

    Wr.PutText (s, Fmt.Int (bb.index) & " postdominates ");
    IntSet.Print (bb.postdominates, s);
    Wr.PutText (s, "\n");
    Wr.PutText (s, Fmt.Int (bb.index) & " immediately postdominates ");
    IntSet.Print (bb.ipostdominates, s);
    Wr.PutText (s, "\n");

    IF bb.idominator # NIL THEN
      Wr.PutText (s, "Immediate dominator of " & Fmt.Int (bb.index) &
        " is " & Fmt.Int (bb.idominator.index) & "\n");
    END;
    IF bb.ipostdominator # NIL THEN
      Wr.PutText (s, "Immediate postdominator of " & Fmt.Int (bb.index) &
        " is " & Fmt.Int (bb.ipostdominator.index) & "\n");
    END;

    Wr.PutText (s, "Control dependence of " & Fmt.Int (bb.index) & " is ");
    IntSet.Print (bb.cd, s);
    Wr.PutText (s, "\n");

  END PrintDom;


(* compute reverse topological sort *)
PROCEDURE ReverseTopologicalSort (p: Procedure.T) =
  VAR
    new_table := NEW (REF ARRAY OF CARDINAL, NUMBER (p.table^));
    current := 0;

  PROCEDURE DoNumber (bb: BasicBlock.T; <* UNUSED *> ignore: REFANY) =
    BEGIN
      new_table[bb.index] := current;
      INC (current);
    END DoNumber;

  BEGIN
    IF Procedure.Flags.TopoSort IN p.flags THEN RETURN; END;
    Procedure.SweepPost (p, DoNumber, NIL);

    p.topo := new_table;
    p.flags := p.flags + Procedure.FlagSet {Procedure.Flags.TopoSort};
  END ReverseTopologicalSort;


PROCEDURE Print (p: Procedure.T; s: Wr.T := NIL) =
  BEGIN
    IF s = NIL THEN s := Stdio.stderr; END;
    
    FOR i := 0 TO p.edge_count-1 DO
      Wr.PutText (s, "Edge " & Fmt.Int (i) & " " &
        Fmt.Int (p.edges[i].source.index) & ":" &
        Fmt.Int (p.edges[i].dest.index) & "\n");
    END;

    Procedure.Sweep (p, PrintDom, s);
  END Print;


BEGIN
END Flow.
