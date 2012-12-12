(*
 *
 * Copyright 1996,1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 *
 *)


MODULE JumpTable;

IMPORT Wr, Stdio, Fmt, Thread;

IMPORT IntPair, IntPairList;
IMPORT SortedRCGTbl;

IMPORT Ir;
IMPORT BasicBlock;

<* FATAL Thread.Alerted, Wr.Failure *>

CONST
  DoClustering = TRUE;
  MinCaseDensity = 5;  (* out of 10 *)
  Debug = TRUE;

PROCEDURE NewT (r: CARDINAL) : T =
  VAR
    newT : T := NEW (T);
  BEGIN
    newT.input := r;
    newT.data := NEW (SortedRCGTbl.Default).init ();
    newT.done := NIL;
    RETURN newT;
  END NewT;


PROCEDURE Insert (t: T; value: INTEGER;
                  block: REFANY (* Ir.InstructionArray *);
                  next: REFANY (* BasicBlock.T *) := NIL)
  RAISES {Duplicate,Done} =
  VAR
    blockcheck : Ir.InstructionArray;
    nextbb : BasicBlock.T;
    td : TableData;
  BEGIN
    IF Debug THEN
      blockcheck := block;
      nextbb := next;
    END;
    IF t.done # NIL THEN RAISE Done; END;

    td := NEW (TableData);
    td.block := block;
    td.next := next;
    IF t.data.put (value, td) THEN
      Wr.PutText (Stdio.stdout, "Duplicate entry entered in jump table\n");
      RAISE Duplicate;
    END;
  END Insert;


PROCEDURE InsertElse (t: T; block: REFANY (* Ir.InstructionArray *))
  RAISES {Duplicate,Done} =
  VAR
    blockcheck : Ir.InstructionArray;
  BEGIN
    IF Debug THEN
      blockcheck := block;
    END;
    IF t.done # NIL THEN RAISE Done; END;
    IF t.else_block # NIL THEN RAISE Duplicate; END;
    t.else_block := block;
  END InsertElse;


PROCEDURE Canonicalize (t: T) : Canonical =
  VAR
    iter: SortedRCGTbl.Iterator;
    jtc : Canonical;
    value: INTEGER;
    tdany: REFANY; td: TableData;
    i := 0;
    clustering : ClusterInfo;
  BEGIN
    IF t.done # NIL THEN RETURN t.done; END;
    jtc := NEW (Canonical);
    jtc.input := t.input;
    jtc.else_block := t.else_block;

    (* default register values *)
    jtc.tmp0 := -1;
    jtc.tmp1 := -1;
    jtc.tmp2 := -1;

    (* convert the representation *)
    jtc.data := NEW (DataArray, t.data.size ());
    jtc.successor := t.successor;
    iter := t.data.iterateOrdered ();
    WHILE iter.next (value, tdany) DO
      td := tdany;
      jtc.data[i].value := value;
      jtc.data[i].block := td.block;
      jtc.data[i].next := td.next;
      INC (i);
    END;
    t.done := jtc;

    (* compute the clusters necessary *)
    clustering := ComputeClusters (jtc);
    jtc.clustering := clustering;

    (* see if uses a table *)
    (* see if there are any jump tables that need to generated *)
    FOR i := 0 TO LAST (clustering^) DO
      IF ImplementationType (clustering[i].lo, clustering[i].hi)
        = Implementation.Table THEN
        jtc.usesTable := TRUE;
        EXIT;
      END;
    END;
    
    RETURN jtc;
  END Canonicalize;


(* compute optimal clustering *)

PROCEDURE Density (jtc: Canonical; i, j: CARDINAL) : CARDINAL =
  (* return density of jump table between indices i and j;
     since we cannot use FP in extensions, the density is
     10*density, rounded *)
  BEGIN
    RETURN 10*(j-i+1) DIV (jtc.data[j].value-jtc.data[i].value+1);
  END Density;


PROCEDURE ComputeClusters (jtc: Canonical) : ClusterInfo =
  (* algorithm taken from Kannan, Proebsting 1994
     SPE 24 (2), 2/94
   *)
  VAR
    n := NUMBER (jtc.data^);
    minClusters := NEW (Clusters, n+1);
    newPair: IntPair.T;
    tmp: IntPairList.T;
    returnInfo: ClusterInfo;
    index, size: CARDINAL;
  BEGIN
    IF NOT DoClustering THEN
      (* just one cluster *)
      returnInfo := NEW (ClusterInfo, 2);
      returnInfo[0].lo := 0;
      returnInfo[1].hi := n-1;
      RETURN returnInfo;
    END;

    (* compute clusters *)
    minClusters[0].min := 0;
    minClusters[0].intervals := NIL;
    FOR i := 1 TO n DO
      minClusters[i].min := LAST (INTEGER);
      minClusters[i].intervals := NIL;
      FOR j := 0 TO i-1 DO

        (* i and j are the positions in the order
           arguments to Density are indices into the array,
             so subtract 1 from both
         *)
        IF Density (jtc, j, i-1) >= MinCaseDensity AND
          minClusters[j].min+1 < minClusters[i].min THEN

          (* new cluster consists of [j+1,i] *)
          minClusters[i].min := minClusters[j].min+1;

          newPair := NEW (IntPair.T);
          newPair.left := j;
          newPair.right := i-1;
          minClusters[i].intervals :=
              IntPairList.Cons (newPair, minClusters[j].intervals);
        END;
      END;
    END;
    
    (* convert into more usable data structure - also reverse the
       interval info so that it is an ascending order*)
    size := IntPairList.Length (minClusters[n].intervals);
    returnInfo := NEW (ClusterInfo, size);
    index := 1;
    tmp := minClusters[n].intervals;
    WHILE tmp # NIL DO
      returnInfo[size-index].lo := tmp.head.left;
      returnInfo[size-index].hi := tmp.head.right;
      INC (index);
      tmp := tmp.tail;
    END;

    RETURN returnInfo;
  END ComputeClusters;


PROCEDURE SetSuccessor (t: T; bb: REFANY) =
  VAR
    realbb : BasicBlock.T := bb;
  BEGIN
    (* set the successor info *)
    t.successor := realbb;
  END SetSuccessor;


PROCEDURE Print (t: T; s: Wr.T := NIL) RAISES {Problem} =
  VAR
    iter : SortedRCGTbl.Iterator;
    value: INTEGER; td: REFANY;
    tabled : TableData;
  BEGIN
    IF s = NIL THEN s := Stdio.stderr; END;
    
    iter := t.data.iterate ();
    Wr.PutText (s, "Begin Jump Table, input is " & Fmt.Int (t.input) & "\n");
    WHILE iter.next (value, td) DO
      tabled := NARROW (td, TableData);
      Wr.PutText (s, "Jump Table [" & Fmt.Int(value) & "] :\n");
      TRY
        Ir.PrintArray (NARROW (tabled.block, Ir.InstructionArray), s);
      EXCEPT
        Ir.Problem => RAISE Problem;
      END;
      IF tabled.next # NIL THEN
        Wr.PutText (s, "Successor is " &
                    Fmt.Int (NARROW (tabled.next, BasicBlock.T).index) & "\n");
      END;
    END;
    IF t.else_block # NIL THEN
      Wr.PutText (s, "Jump Table else block\n");
      Ir.PrintArray (NARROW (t.else_block, Ir.InstructionArray), s);
    END;
    Wr.PutText (s, "Jump Table Successor is " &
                Fmt.Int (NARROW (t.successor, BasicBlock.T).index) & "\n");
    Wr.PutText (s, "End Jump Table\n");
  END Print;

PROCEDURE Copy (jt: T) :T RAISES {Problem} =
  BEGIN
    (* unimplemented *)
    RAISE Problem;
  END Copy;

BEGIN
END JumpTable.
