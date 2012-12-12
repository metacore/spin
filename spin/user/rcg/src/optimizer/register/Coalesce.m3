


UNSAFE MODULE Coalesce;

IMPORT IO, Fmt;

IMPORT IntBoolTbl, MoveBoolTbl, EdgeBoolTbl, IntSeq, IntSet;
IMPORT Ir, Procedure;
IMPORT Analyzer, Live, Residue;
IMPORT Move;
IMPORT Edge;

(* We are using the Iterated Register Coalescing algorithm from 
   the paper of that name by Lal George and Andrew Appel. *)

TYPE
  Node = INTEGER;

VAR
  Debug := TRUE;
  NumVariables : CARDINAL;
  NumMoves : CARDINAL;

CONST
  (* 32 registers minus zero, sp, gp, v0, a0-a5, ra, pv, at
     = 19 registers:
      t0-t7 (1-8), s0-s5 (9-14), s6 (15), t8-t11 (22-25)
   *)
  NumColors = 31;

VAR
  (* Lists of graph nodes.

     Invariant: These lists and stack are a disjoint partition of the set of
     temporary variables, meaning that every variable is on one and only one
     of these lists or the stack at all times. Since membership in these sets
     is often tested, the representation of a variable contains an enumeration
     telling which set it is in. 

     Precondition: Initially (on entry to Main) and on exiting RewriteProgram,
     only the sets precolored and initial are nonempty.
  *)

  (* precolored contains nodes that are already assigned machine registers,
     such as arguments to a procedure or return values. *)
  precolored        : IntBoolTbl.T;

  (* initial contains temporary registers (or variables) which have no
     preassigned color and have not been processed yet by the algorithm. *)
  initial           : IntBoolTbl.T;
  
  (* simplifyWorklist contains the low-degree, non move-related nodes, which
     can be colored in the simplify phase.
     Invariant:
         If u is in simplifyWorklist then degree[u] < K and the intersection
         of moveList[u] with (activeMoves U worklistMoves) is empty.
  *)
  simplifyWorklist  : IntBoolTbl.T;
  
  (* freezeWorklist contains the low-degree, move-related nodes which are 
     candidates for freezing.
     Invariant:
         If u is in freezeWorklist then degree[u] < K and the intersection
         of moveList[u] with (activeMoves U worklistMoves) is non-empty.
  *)
  freezeWorklist    : IntBoolTbl.T;

  (* spillWorklist contains the high-degree nodes that may have to be 
     spilled. 
     Invariant:
         If u is in spillWorklist then degree[u] >= K.
  *)
  spillWorklist     : IntBoolTbl.T;

  (* spilledNodes contains the nodes marked for spilling during this round. *)
  spilledNodes      : IntBoolTbl.T;

  (* coalescedNodes contains the registers that have been coalesced. When the
     move u := v is coalesced, one of u or v is added to this set and the other
     is put back on some worklist.*)
  coalescedNodes    : IntBoolTbl.T;

  (* coloredNodes contains the successfully colored nodes *)
  coloredNodes      : IntBoolTbl.T;

  (* selectStack contains the temporaries removed from the graph. *)
  selectStack       : IntSeq.T;


  (* Move sets.
     Invariant: Every move is in exactly one of these sets (after Build and
     through the end of Main.
  *)

  (* coalescedMoves are the moves that have been coalesced into a single 
     node. *)
  coalescedMoves    : MoveBoolTbl.T;
  
  (* contrainedMoves are the moves whose source and target interfere, so that
     they cannot be coalesced. *)
  constrainedMoves   : MoveBoolTbl.T;

  (* frozenMoves are the moves that will no longer be considered for 
     coalescing. *)
  frozenMoves       : MoveBoolTbl.T;

  (* worklistMoves are the moves enabled for possible coalescing. *)
  worklistMoves     : MoveBoolTbl.T;

  (* activeMoves are the moves not yet ready for coalescing. *)
  activeMoves       : MoveBoolTbl.T;


  (* Adjacency information. This information is redundant for the nonprecolored
     temporaries. *)
  (* adjSet is the set of interference edges (u, v) in the graph. If (u, v) 
     is in adjSet, then (v, u) is in adjSet. adjSet is represented as a hash
     table. *)
  adjSet            : EdgeBoolTbl.T;

  (* adjList is the adjacency list representation of the graph. For each
     nonprecolored temporary u, adjList[u] is the set of nodes that interfere
     with u. *)
  adjList           : REF ARRAY OF IntBoolTbl.T;


  (* Degree information.
     degree is an array containing the current degree of each node. Precolored
     nodes are initialized with a degree of LAST(INTEGER). 
     Invariant: For any node u in one of the lists simplifyWorklist, 
     freezeWorklist or spillWorklist, 
             degree[u] = | adjList[u] Intersect (precolored U simplifyWorklist
                                             U freezeWorklist U spillWorklist) |
  *)
  degree            : REF ARRAY OF CARDINAL;
  

  (* Redundant information. *)
  (* moveList is a mapping from nodes to the list of moves it is associated
     with. *)
  moveList          : REF ARRAY OF MoveBoolTbl.T;

  (* When a move u := v is coalesced and v put in coalescedNodes, then
     alias(v) = u. *)
  alias             : REF ARRAY OF CARDINAL;

  
  (* Color information. *)
  (* color is the color chosen by the algorithm for a node. For precolored
     nodes this is initialized to the given color. *)
  color             : REF ARRAY OF RegisterColors;

TYPE
  RegisterColors = [0 .. NumColors-1];
  NodeSet = {Precolored, Initial, SimplifyWork, FreezeWork, SpillWork, Spilled,
             Coalesced, Colored, Selected};
CONST
  FullRegisterSet = SET OF RegisterColors {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                                           11, 12, 13, 14, 15, 16, 17, 18, 19,
                                           20, 21, 22, 23, 24, 25, 26, 27, 28,
                                           29, 30};

VAR
  curSet            : REF ARRAY OF NodeSet;

(* Helper functions designed to ensure the consistency of the lists and
   the NodeSet flags. *)

PROCEDURE AddPrecolored (u: Node) =
  BEGIN
    EVAL precolored.put(u, TRUE);
    curSet[u] := NodeSet.Precolored;
  END AddPrecolored; 

PROCEDURE AddInitial(u: Node) =
  BEGIN
    EVAL initial.put(u, TRUE);
    curSet[u] := NodeSet.Initial;
  END AddInitial; 

PROCEDURE AddSpillWork(u: Node) =
  BEGIN
    EVAL spillWorklist.put(u, TRUE);
    curSet[u] := NodeSet.SpillWork;
  END AddSpillWork;

PROCEDURE AddFreezeWork(u: Node) =
  BEGIN
    EVAL freezeWorklist.put(u, TRUE);
    curSet[u] := NodeSet.FreezeWork;
  END AddFreezeWork;

PROCEDURE AddSimplifyWork(u: Node) =
  BEGIN
    EVAL simplifyWorklist.put(u, TRUE);
    curSet[u] := NodeSet.SimplifyWork;
  END AddSimplifyWork;

PROCEDURE AddSpilledNodes(u: Node) =
  BEGIN
    EVAL spilledNodes.put(u, TRUE);
    curSet[u] := NodeSet.Spilled;
  END AddSpilledNodes;

PROCEDURE AddCoalesced(u: Node) =
  BEGIN
    EVAL coalescedNodes.put(u, TRUE);
    curSet[u] := NodeSet.Coalesced;
  END AddCoalesced;

PROCEDURE AddColoredNodes(u: Node) =
  BEGIN
    EVAL coloredNodes.put(u, TRUE);
    curSet[u] := NodeSet.Colored;
  END AddColoredNodes;

PROCEDURE RemoveNode(list: IntBoolTbl.T; u: Node) =
  VAR
    dummy: BOOLEAN;
  BEGIN
    EVAL list.delete(u, dummy);
  END RemoveNode;



PROCEDURE AddCoalescedMoves(m: Move.T) =
  BEGIN
    EVAL coalescedMoves.put(m, TRUE);
    m.state := Move.State.Coalesced;
  END AddCoalescedMoves;

PROCEDURE AddConstrainedMoves(m: Move.T) =
  BEGIN
    EVAL constrainedMoves.put(m, TRUE);
    m.state := Move.State.Constrained;
  END AddConstrainedMoves;

PROCEDURE AddFrozenMoves(m: Move.T) =
  BEGIN
    EVAL frozenMoves.put(m, TRUE);
    m.state := Move.State.Frozen;
  END AddFrozenMoves;

PROCEDURE AddWorklistMoves(m: Move.T) =
  BEGIN
    EVAL worklistMoves.put(m, TRUE);
    m.state := Move.State.Worklist;
  END AddWorklistMoves;

PROCEDURE AddActiveMoves(m: Move.T) =
  BEGIN
    EVAL activeMoves.put(m, TRUE);
    m.state := Move.State.Active;
  END AddActiveMoves;

PROCEDURE RemoveMoveNode(list: MoveBoolTbl.T; m: Move.T) =
  VAR
    dummy: BOOLEAN;
  BEGIN
    EVAL list.delete(m, dummy);
  END RemoveMoveNode;


(* The algorithm iterates until no more spills are created, at which point
   every node has been colored. When AssignColors does produce spills, 
   RewriteProgram allocates memory locations for the spilled temporaries
   and inserts store and load instructions  to access them. These stores and
   loads are to newly created temporaries, so the Main loop must be performed
   on the altered graph. *)
PROCEDURE LivenessAnalysis(p: Procedure.T) =  
  VAR
    (* Need to compute the number of variables used in the procedure. *)
    live := Live.NewT(32);
  BEGIN
    IO.Put ("Liveness\n");
    Analyzer.Visit (p, live);
    Analyzer.Print (p, live);
  END LivenessAnalysis;

PROCEDURE RewriteProgram(<*UNUSED*>s: IntBoolTbl.T) = 
  BEGIN 
    <* ASSERT FALSE *>
  END RewriteProgram;

PROCEDURE Main(p: Procedure.T) =
  BEGIN
    NumVariables := 50;
    NumMoves := 20;
    LOOP
      Reset(NumVariables);

      LivenessAnalysis(p);       (* Determine the live ranges. *)
      Build(p);                  (* Construct the flow graph. *)

      IF Debug THEN CheckInvariants(); END;

      MakeWorkList();            (* Moves nodes from initial onto worklists. *)
  
      IF Debug THEN CheckInvariants(); END;

      (* If there is any work to be done, then do it. *)
      LOOP
        IF Debug THEN CheckInvariants(); END;
        IF simplifyWorklist.size() # 0 THEN Simplify(); 
        ELSIF worklistMoves.size() # 0 THEN Coalesce();
        ELSIF freezeWorklist.size() # 0 THEN Freeze();
        ELSIF spillWorklist.size() # 0 THEN SelectSpill();
        ELSE EXIT END;
      END;
      
      IF Debug THEN CheckInvariants(); END;
      AssignColors();
      IF spilledNodes.size() # 0 THEN RewriteProgram(spilledNodes);
      ELSE EXIT END;
    END;
    IO.Put("Chosen colors:\n");
    FOR i := FIRST(color^) TO LAST(color^) DO
      IO.Put(Fmt.Int(i) & " -> " & Fmt.Int(color[i]) & "\n");
    END;
  END Main;


(* Procedure Build constructs the interference graph and bit matrix. We use
   the sparse set representation described by Briggs and Torczon to implement
   the variable live. Build only adds an interference edge between a node that
   is defined at some point and the nodes that are currently live at that
   point.  It is not necessary to add interferences between nodes in the live
   set. These edges will be added when processing other blocks in the program.
   Build also initializes the worklistMoves to contain all of the 
   moves in the program. *)

PROCEDURE AddEdge(u, v: Node) =
  BEGIN
    IO.Put("{" & Fmt.Int(u) & "," & Fmt.Int(v) & "}\n");
    IF u # v THEN
      IF NOT adjSet.put(Edge.T{u, v}, TRUE) THEN
        EVAL adjSet.put(Edge.T{v, u}, TRUE);
        IF curSet[u] # NodeSet.Precolored THEN
          EVAL adjList[u].put(v, TRUE);
          INC(degree[u]);
        END;
        IF curSet[v] # NodeSet.Precolored THEN
          EVAL adjList[v].put(u, TRUE);
          INC(degree[v]);
        END;
      END;
    END;
  END AddEdge;

CONST 
  NoReg = -1;

PROCEDURE DefinedRegister(inst: Ir.Instruction): INTEGER = 
  BEGIN 
    CASE inst.op OF
    | Ir.Opcode.addq .. Ir.Opcode.zapnot =>

      WITH aluOp = LOOPHOLE(inst, Ir.AluRRInstruction) DO
        RETURN aluOp.rdest;
      END;

    | Ir.Opcode.addqi .. Ir.Opcode.zapnoti =>

      WITH aluOp = LOOPHOLE(inst, Ir.AluRIInstruction) DO
        RETURN aluOp.rdest;
      END;

    | Ir.Opcode.li =>

      WITH immedOp = LOOPHOLE(inst, Ir.ImmedInstruction) DO
        RETURN immedOp.rdest;
      END;

    | Ir.Opcode.lda .. Ir.Opcode.ldb =>

      WITH memOp = LOOPHOLE(inst, Ir.MemoryInstruction) DO
        RETURN memOp.r1;
      END;

    | Ir.Opcode.jsr .. Ir.Opcode.ret =>

      WITH callOp = LOOPHOLE(inst, Ir.RetInstruction) DO
        RETURN callOp.r1;
      END;

    | Ir.Opcode.copy =>

      WITH copyOp = LOOPHOLE(inst, Ir.CopyInstruction) DO
        RETURN copyOp.rdest;
      END;
        

    | Ir.Opcode.call .. Ir.Opcode.fault =>

      WITH callOp = LOOPHOLE(inst, Ir.CallInstruction) DO
        RETURN callOp.reg;
      END;

    ELSE
      RETURN NoReg 
    END;
  END DefinedRegister;

PROCEDURE Build (p: Procedure.T) =
  VAR
    copylive : Residue.T;
    live := Live.NewT (32);
  BEGIN
    FOR i := FIRST(p.table^) TO LAST(p.table^) DO
      WITH b = p.table[i] DO
        copylive := live.Copy (b.infoBottom.live, NIL);
        FOR j := LAST(b.instructions^) TO FIRST(b.instructions^) BY -1 DO
          IF b.instructions[j].op = Ir.Opcode.copy THEN
            WITH copyInst = LOOPHOLE(b.instructions[j], 
                                     Ir.CopyInstruction) DO
              IntSet.Remove(copyInst.rsrc, copylive);
              
              EVAL moveList[copyInst.rsrc].put(Move.T{copyInst.rdest, 
                                                      copyInst.rsrc}, TRUE);
              IF copyInst.rsrc # copyInst.rdest THEN
                EVAL moveList[copyInst.rdest].put(Move.T{copyInst.rdest, 
                                                         copyInst.rsrc}, TRUE);
              END;
              
              AddWorklistMoves(Move.T{copyInst.rdest, copyInst.rsrc});
            END;
          END;
          
          (* An instruction may only define one register at a time. *)
          WITH d = DefinedRegister(b.instructions[j]) DO
            IF d # NoReg THEN
              IO.Put("\nAdding Edges:\n");
              FOR elem := 0 TO NumVariables-1 DO
                IF IntSet.IsMember(elem, copylive) THEN
                  AddEdge(elem, d); 
                END;
              END;
            END;
          END;

          copylive := live.GenKill (b.instructions[j], copylive, p);
        END;
      END;
    END;

    (* At the beginning of the procedure, we must treat the callee-saved
       registers as if they were defined there. Otherwise, variables which
       are used but not defined in the procedure are missing interference
       edges. *)
    WITH live = p.top.infoTop.live DO
      IO.Put("\nAdding Edges:\n");
      FOR i := FIRST(Live.CalleeSaved) TO LAST(Live.CalleeSaved) DO
        FOR elem := 0 TO NumVariables-1 DO
          IF IntSet.IsMember(elem, live) THEN
            AddEdge(elem, Live.CalleeSaved[i]); 
          END;
        END;
      END;
    END;

    IF Debug THEN
      VAR
        e : Edge.T;
        u : Node;
        dummy : BOOLEAN;
        edgeCount := 0;
      BEGIN
        IO.Put("The Set of edges:\n");
        WITH edges = adjSet.iterate() DO
          WHILE edges.next(e, dummy) DO
            IO.Put("{" & Fmt.Int(e.u) & "," & Fmt.Int(e.v) & "} ");
            INC(edgeCount);
            IF edgeCount MOD 10 = 0 THEN
              IO.Put("\n");
            END;
          END;
        END;

        IO.Put("\n\nThe adjacency lists:\n");
        FOR i := FIRST(adjList^) TO LAST(adjList^) DO
          IO.Put("Adjacent to " & Fmt.Int(i) & " [" & 
            Fmt.Int(degree[i]) & "]: ");
          WITH nodes = adjList[i].iterate() DO
            WHILE nodes.next(u, dummy) DO
              IO.Put(Fmt.Int(u) & ", ");
            END;
          END;
          IO.Put("\n");
        END;
      END;
    END;
  END Build;

(* Some predicate functions. *)
PROCEDURE Adjacent(u: Node): IntBoolTbl.T =
  VAR
    result := NEW(IntBoolTbl.Default).init(10);
    n : Node;
    dummy: BOOLEAN;
  BEGIN
    WITH nodes = adjList[u].iterate() DO
      WHILE nodes.next(n, dummy) DO
        IF curSet[n] # NodeSet.Selected AND curSet[n] # NodeSet.Coalesced THEN
          EVAL result.put(n, TRUE);
        END;
      END;
    END;

    RETURN result;
  END Adjacent;


PROCEDURE UnionAdjacent(u, v: Node): IntBoolTbl.T =
  VAR
    result := NEW(IntBoolTbl.Default).init(10);
    n : Node;
    dummy: BOOLEAN;
  BEGIN
    WITH nodes = adjList[u].iterate() DO
      WHILE nodes.next(n, dummy) DO
        IF curSet[n] # NodeSet.Selected AND curSet[n] # NodeSet.Coalesced THEN
          EVAL result.put(n, TRUE);
        END;
      END;
    END;

    WITH nodes = adjList[v].iterate() DO
      WHILE nodes.next(n, dummy) DO
        IF curSet[n] # NodeSet.Selected AND curSet[n] # NodeSet.Coalesced THEN
          EVAL result.put(n, TRUE);
        END;
      END;
    END;

    RETURN result;
  END UnionAdjacent; 

PROCEDURE MoveRelated(n: Node): BOOLEAN =
  VAR
    dummy: BOOLEAN;
    m : Move.T;
  BEGIN
    WITH moves = moveList[n].iterate() DO
      WHILE moves.next(m, dummy) DO
        IF m.state = Move.State.Active OR m.state = Move.State.Worklist THEN
          RETURN TRUE;
        END;
      END;
    END;
    RETURN FALSE;
  END MoveRelated;

PROCEDURE MakeWorkList() =
  VAR
    dummy: BOOLEAN;
    n : Node;
  BEGIN
    WITH nodes = initial.iterate() DO
      WHILE nodes.next(n, dummy) DO
        IF degree[n] >= NumColors THEN
          AddSpillWork(n);
        ELSIF MoveRelated(n) THEN
          AddFreezeWork(n);
        ELSE
          AddSimplifyWork(n);
        END;
      END;
    END;
    (* The initial table is no longer needed. *)
    initial := NEW(IntBoolTbl.Default).init(10);
  END MakeWorkList;


PROCEDURE Simplify() =
  VAR
    dummy: BOOLEAN;
    u, v : Node;
  BEGIN
    IF simplifyWorklist.iterate().next(u, dummy) THEN
      (* Remove u from the list *)
      EVAL simplifyWorklist.delete(u, dummy);
      selectStack.addhi(u);

      WITH nodes = Adjacent(u).iterate() DO
        WHILE nodes.next(v, dummy) DO
          DecrementDegree(v);
        END;
      END;
    END;
  END Simplify;

(* Removing a node from the graph involves decrementing the degree of its
   current neighbors. If the degree is already less that K - 1 then the node
   must be move related and is not added to the simplifyWorklist. When the 
   degree of a node transitions from K to K - 1, moves associated with its
   neighbors may be enabled. Precolored nodes do not actually keep track of
   their edges, so ignore a message to decrement their degree. *)

PROCEDURE DecrementDegree(u: Node) =
  VAR
    dummy: BOOLEAN;
    v : Node;
  BEGIN
    IF degree[u] = NumColors THEN
      EnableMoves(u);
      WITH nodes = Adjacent(u).iterate() DO
        WHILE nodes.next(v, dummy) DO
          EnableMoves(v);
        END;
      END;

      RemoveNode(spillWorklist, u);
      IF MoveRelated(u) THEN
        AddFreezeWork(u);
      ELSE
        AddSimplifyWork(u);
      END;
    END;

    IF curSet[u] # NodeSet.Precolored THEN DEC(degree[u]); END;
  END DecrementDegree;


PROCEDURE EnableMoves(u: Node) =
  VAR
    dummy: BOOLEAN;
    m : Move.T;
  BEGIN
    WITH moves = moveList[u].iterate() DO
      WHILE moves.next(m, dummy) DO
        IF m.state = Move.State.Active THEN
          RemoveMoveNode(activeMoves, m);
          AddWorklistMoves(m);
        END;
      END;
    END;
  END EnableMoves;

(* Only moves in the worklistMoves are considered in the coalesce phase.
   When a move is coalesced, it may no longer be move related and can be
   added to the simplify worklist by the procedure AddWorkList. OK implements
   the heuristic used for coalescing a precolored register. Conservative 
   implements the Briggs conservative coalescing heuristic. *)

PROCEDURE Coalesce() =
  VAR
    u, v : Node;
    m : Move.T;
    dummy: BOOLEAN;
  BEGIN
    IF worklistMoves.iterate().next(m, dummy) THEN
      EVAL worklistMoves.delete(m, dummy);
      WITH x = alias[m.u],
           y = alias[m.v] DO
        IF curSet[y] = NodeSet.Precolored THEN
          u := y;
          v := x;
        ELSE
          u := x;
          v := y;
        END;

        IF u = v THEN
          AddCoalescedMoves(m);
          AddWorkList(u);
        ELSIF curSet[v] = NodeSet.Precolored OR adjSet.get(Edge.T{u, v}, dummy) THEN
          (* If v is precolored, then u is also. *)
          AddConstrainedMoves(m);
          AddWorkList(u);
          AddWorkList(v);
        ELSIF curSet[u] = NodeSet.Precolored AND OK(Adjacent(v).iterate(), u) OR
          curSet[u] # NodeSet.Precolored AND Conservative(UnionAdjacent(u, v).iterate()) 
         THEN
          AddCoalescedMoves(m);
          Combine(u, v);
          AddWorkList(u);
        ELSE
          AddActiveMoves(m);
        END;
      END;
    END;
  END Coalesce;
    

PROCEDURE AddWorkList(u: Node) =
  BEGIN
    IF curSet[u] # NodeSet.Precolored AND NOT MoveRelated(u) AND degree[u] < NumColors THEN
      RemoveNode(freezeWorklist, u);
      AddSimplifyWork(u);
    END;
  END AddWorkList;

PROCEDURE OK(nodes: IntBoolTbl.Iterator; u: Node): BOOLEAN =
  VAR
    dummy: BOOLEAN;
    v : Node;
  BEGIN
    WHILE nodes.next(v, dummy) DO
      IF degree[v] >= NumColors AND curSet[v] # NodeSet.Precolored AND 
        NOT adjSet.get(Edge.T{v, u}, dummy) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END OK;

PROCEDURE Conservative(nodes: IntBoolTbl.Iterator): BOOLEAN =
  VAR
    dummy: BOOLEAN;
    u : Node;
    k := 0;
  BEGIN
    WHILE nodes.next(u, dummy) DO
      IF degree[u] >= NumColors THEN INC(k); END;
    END;

    RETURN k < NumColors;
  END Conservative;

PROCEDURE Combine(u, v: Node) =
  VAR
    dummy: BOOLEAN;
    n : Node;
    m : Move.T;
  BEGIN
    IF curSet[v] = NodeSet.FreezeWork THEN
      RemoveNode(freezeWorklist, v);
    ELSE
      RemoveNode(spillWorklist, v);
    END;
    AddCoalesced(v);
    alias[v] := u;
    (* Form the union of the moveLists. *)
    WITH moves = moveList[v].iterate() DO
      WHILE moves.next(m, dummy) DO
        EVAL moveList[u].put(m, TRUE);
      END;
    END;
    
    WITH nodes = Adjacent(v).iterate() DO
      WHILE nodes.next(n, dummy) DO
        AddEdge(n, u);
        DecrementDegree(n);
      END;
    END;

    IF degree[u] >= NumColors AND curSet[u] = NodeSet.FreezeWork THEN
      RemoveNode(freezeWorklist, u);
      AddSpillWork(u);
    END;
  END Combine;

(* Procedure Freeze pulls out a node from the freezeWorklist and freezes
   all moves associated with this node. In principle, a heuristic could
   be used to select the freeze node. *)
PROCEDURE Freeze() =
  VAR
    u : Node;
    dummy : BOOLEAN;
  BEGIN
    IF freezeWorklist.iterate().next(u, dummy) THEN
      EVAL freezeWorklist.delete(u, dummy);
      AddSimplifyWork(u);
      FreezeMoves(u);
    END;
  END Freeze;

PROCEDURE FreezeMoves(u: Node) =
  VAR
    dummy: BOOLEAN;
    m : Move.T;
  BEGIN
    WITH moves = moveList[u].iterate() DO
      WHILE moves.next(m, dummy) DO
        IF m.state = Move.State.Active THEN
          RemoveMoveNode(activeMoves, m);
        ELSE
          RemoveMoveNode(worklistMoves, m);
        END;

        AddFrozenMoves(m);
          
        VAR
          v := 0;
        BEGIN
          IF m.u = u THEN
            v := m.v;
          ELSE
            v := m.u;
          END;

          IF moveList[v].size() = 0 AND degree[v] < NumColors THEN
            RemoveNode(freezeWorklist, v);
            AddSimplifyWork(v);
          END;
        END;
      END;
    END;
  END FreezeMoves;

PROCEDURE SelectSpill() =
  VAR
    n : Node;
    dummy : BOOLEAN;
  BEGIN
    IF spillWorklist.iterate().next(n, dummy) THEN
      EVAL spillWorklist.delete(n, dummy);
      AddSimplifyWork(n);
      FreezeMoves(n);
    END;
  END SelectSpill;

PROCEDURE ChooseColor(avail: SET OF RegisterColors): RegisterColors =
  BEGIN
    FOR c := FIRST(RegisterColors) TO LAST(RegisterColors) DO
      IF c IN avail THEN RETURN c; END;
    END;
    <* ASSERT FALSE *>
  END ChooseColor;

PROCEDURE AssignColors() =
  VAR
    dummy: BOOLEAN;
    n : Node;
    okColors : SET OF RegisterColors;
  BEGIN
    WHILE selectStack.size() # 0 DO
      WITH u = selectStack.remhi() DO
        okColors := FullRegisterSet;
        WITH nodes = adjList[u].iterate() DO
          WHILE nodes.next(n, dummy) DO
            WITH v = alias[n] DO
              IF curSet[v] = NodeSet.Colored OR
                curSet[v] = NodeSet.Precolored THEN
                okColors := okColors - SET OF RegisterColors{color[v]};
              END;
            END;
          END;
        END;

        IF okColors = SET OF RegisterColors{} THEN
          AddSpilledNodes(u);
        ELSE
          AddColoredNodes(u);
          WITH c = ChooseColor(okColors) DO
            color[u] := c;
          END;
        END;
      END;
    END;

    WITH nodes = coalescedNodes.iterate() DO
      WHILE nodes.next(n, dummy) DO
        color[n] := color[alias[n]];
      END;
    END;
  END AssignColors;

PROCEDURE CheckInvariants() =
  BEGIN
    CheckNodeLists();
    CheckMoveSets();
  END CheckInvariants;

PROCEDURE CheckNodeLists() =
  VAR
    dummy: BOOLEAN;
    n : Node;
    each := NEW(REF ARRAY OF INTEGER, NumVariables);
  BEGIN
    FOR i := FIRST(each^) TO LAST(each^) DO
      each[i] := 0;
    END;

    WITH nodes = precolored.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = initial.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = simplifyWorklist.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = freezeWorklist.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = spillWorklist.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = spilledNodes.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = coalescedNodes.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    WITH nodes = coloredNodes.iterate() DO
      WHILE nodes.next(n, dummy) DO
        INC(each[n]);
      END;
    END;
    
    FOR i := 0 TO selectStack.size() - 1 DO
      WITH n = selectStack.get(i) DO
        INC(each[n]);
      END;
    END;
    
    FOR i := FIRST(each^) TO LAST(each^) DO
      IF each[i] = 0 THEN 
        IO.Put("Node " & Fmt.Int(i) & " is not on any list\n");
      ELSIF each[i] > 1 THEN
        IO.Put("Node " & Fmt.Int(i) & " is on " & Fmt.Int(each[i]) & " lists\n");
      END;
    END;
  END CheckNodeLists;

PROCEDURE CheckMoveSets() =
  VAR
    dummy: BOOLEAN;
    m : Move.T;
    set := NEW(MoveBoolTbl.Default).init(20);
    unique := TRUE;
    total := 0;
  BEGIN
    WITH moves = coalescedMoves.iterate() DO
      WHILE moves.next(m, dummy) DO
        INC(total);
        unique := unique AND NOT set.put(m, TRUE);
      END;
    END;

    WITH moves = constrainedMoves.iterate() DO
      WHILE moves.next(m, dummy) DO
        INC(total);
        unique := unique AND NOT set.put(m, TRUE);
      END;
    END;

    WITH moves = frozenMoves.iterate() DO
      WHILE moves.next(m, dummy) DO
        INC(total);
        unique := unique AND NOT set.put(m, TRUE);
      END;
    END;

    WITH moves = worklistMoves.iterate() DO
      WHILE moves.next(m, dummy) DO
        INC(total);
        unique := unique AND NOT set.put(m, TRUE);
      END;
    END;

    WITH moves = activeMoves.iterate() DO
      WHILE moves.next(m, dummy) DO
        INC(total);
        unique := unique AND NOT set.put(m, TRUE);
      END;
    END;

    IF total # NumMoves THEN 
      IO.Put("Some move is not on any list\n");
    END;
    IF NOT unique THEN
      IO.Put("Some move is on multiple lists\n");
    END;
  END CheckMoveSets;



PROCEDURE Reset(num: CARDINAL) =
  BEGIN
    precolored := NEW(IntBoolTbl.Default).init(10);
    initial := NEW(IntBoolTbl.Default).init(10);
    simplifyWorklist := NEW(IntBoolTbl.Default).init(10);
    freezeWorklist := NEW(IntBoolTbl.Default).init(10);
    spillWorklist := NEW(IntBoolTbl.Default).init(10);
    spilledNodes := NEW(IntBoolTbl.Default).init(10);
    coalescedNodes := NEW(IntBoolTbl.Default).init(10);
    coloredNodes := NEW(IntBoolTbl.Default).init(10);
    selectStack := NEW(IntSeq.T).init(10);
    
    coalescedMoves := NEW(MoveBoolTbl.Default).init(10);
    constrainedMoves := NEW(MoveBoolTbl.Default).init(10);
    frozenMoves := NEW(MoveBoolTbl.Default).init(10);
    worklistMoves := NEW(MoveBoolTbl.Default).init(10);
    activeMoves := NEW(MoveBoolTbl.Default).init(10);
    
    adjSet := NEW(EdgeBoolTbl.Default).init(10);

    degree := NEW(REF ARRAY OF CARDINAL, num);
    alias := NEW(REF ARRAY OF CARDINAL, num);
    color := NEW(REF ARRAY OF RegisterColors, num);
    curSet := NEW(REF ARRAY OF NodeSet, num);

    adjList := NEW(REF ARRAY OF IntBoolTbl.T, num);
    moveList := NEW(REF ARRAY OF MoveBoolTbl.T, num);

    FOR i := 0 TO num-1 DO
      adjList[i] := NEW(IntBoolTbl.Default).init(10);
      moveList[i] := NEW(MoveBoolTbl.Default).init(10);
    END;

    (* Add the precolored and initial nodes *)
    FOR i := 0 TO NumColors - 1 DO
      color[i] := i;
      alias[i] := i;
      AddPrecolored(i);
    END;

    alias[NumColors] := NumColors;
    AddPrecolored(NumColors);

    FOR i := NumColors + 1 TO num - 1 DO
      alias[i] := i;
      AddInitial(i);
    END;
  END Reset;

BEGIN
END Coalesce.
