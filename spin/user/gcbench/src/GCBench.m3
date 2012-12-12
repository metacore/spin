(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(*
 * HISTORY
 * 03-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

MODULE GCBench;

IMPORT IO, Fmt, ParseParams, Thread, GCBenchCmd, Rd, ThreadExtra, Text,
       TextRd, Lex;
CONST 
  MAX_SPIN_DELAY = 10;
  DEFAULT_ALLOCATION = 1 * 1024 * 1024;
  MAX_TREE_SIZE = 100;
  MAX_ARRAY_SIZE = 1024;

(*****************************)
(*  Random Number Generator  *)
(*****************************)

CONST AA = 38737;
CONST CC = 131071;
CONST MM = 43691;

VAR seed : CARDINAL;

PROCEDURE Random(minval: INTEGER; maxval: INTEGER) : INTEGER =
  VAR span: INTEGER;
 BEGIN
	IF minval > maxval THEN
          RETURN minval;
        ELSE
  	  span := (maxval - minval) + 1;
	  seed := (AA*seed + CC) MOD MM;
	  RETURN (minval + (seed MOD span));
        END;
 END Random;

(***********************************)
(*  Tree stuff                     *)
(***********************************)

TYPE 
  Node = MUTEX OBJECT
    cond  : Thread.Condition;
    word  : INTEGER := 0;
    array : Array;
    count : INTEGER := 1;
    left  : Node;
    right : Node;
  END;

TYPE
  Array = REF ARRAY OF CHAR;

VAR
  testing : BOOLEAN;    (* testing still in progress *)
  verbose : INTEGER := 1;    
  printmu := NEW(MUTEX);

PROCEDURE AddTree(t: Node; w: Node): Node =
  BEGIN
    (* This AddTree routine keeps duplicate nodes with the same word
       value. This is because we don't want to create garbage here by
       letting go of a pointer to w. The creation of garbage is determined
       by higher level decisions. *)
    IF t = NIL THEN
      RETURN w;
    ELSE
      INC(t.count);
      IF w.word < t.word THEN
        t.left := AddTree(t.left, w);
      ELSIF w.word >= t.word THEN
        t.right := AddTree(t.right, w);
      END;
    END;
    RETURN t;
  END AddTree;

(* 
PROCEDURE LockTree(tree: Node; read: BOOLEAN; write: BOOLEAN) =
  BEGIN
    <* ASSERT read # write *>
    LOCK tree DO
      WHILE tree.taken OR write AND tree.readers # 0 DO
        Thread.Wait(tree.mutex, tree.cond);
      END;
      IF write THEN
        tree.taken := TRUE;
      ELSIF read THEN
        INC(tree.readers);
      END;
    END;
  END LockTree;

PROCEDURE UnlockTree(tree: Node; read: BOOLEAN; write: BOOLEAN) =
  BEGIN
    <* ASSERT read # write *>
    <* ASSERT read AND NOT tree.taken OR write AND tree.readers = 0 *>
    LOCK tree DO
      IF write THEN
        tree.taken := FALSE;
      ELSIF read THEN
        DEC(tree.readers);
      END;
      IF NOT tree.taken AND tree.readers = 0 THEN
        Thread.Signal(tree.cond);
      END;
    END;
  END UnlockTree;

PROCEDURE TraverseTreeDF(tree: Node) : INTEGER =
  VAR
    cnt: INTEGER := 1;
  BEGIN
    IF tree = NIL THEN RETURN 0; END;
    LockTree(tree, TRUE, FALSE);
    INC(cnt, TraverseTreeDF(tree.left));
    INC(cnt, TraverseTreeDF(tree.right));
    IF cnt # tree.count THEN
      IO.Put("ERROR >> tree count mismatch\n");
    END;
    UnlockTree(tree, TRUE, FALSE);
    RETURN cnt;
  END TraverseTreeDF; 
*)

PROCEDURE TraverseUnlocked(tree: Node) : INTEGER =
  VAR
    cnt: INTEGER := 1;
  BEGIN
    IF tree = NIL THEN RETURN 0; END;
    INC(cnt, TraverseUnlocked(tree.left));
    INC(cnt, TraverseUnlocked(tree.right));
    IF cnt # tree.count THEN
      IO.Put("ERROR >> tree count mismatch\n");
    END;
    RETURN cnt;
  END TraverseUnlocked;

(****************)
(* Main Program *)
(****************)

PROCEDURE Zap() =
  BEGIN
    GCBenchCmd.Uninstall();
    IO.Put("GCBench is uninstalled\n");
  END Zap;

TYPE
  GCargs = RECORD
    id: INTEGER;
    number: INTEGER;
    size: INTEGER;
    iter: INTEGER;
    trav: INTEGER;
  END;

PROCEDURE NodeDestroyer(tree: Node; thresh: INTEGER): CARDINAL =
  VAR
    nremoved : CARDINAL := 0;
  BEGIN
    (* Walk down the tree until you find a node with less than
       or equal to thresh descendants (including itself). Let go
       of this node and return the actual number of nodes freed *)
    (* We decide to choose the left or right descendant randomly *)
    IF tree.left # NIL AND tree.right # NIL THEN
      IF Random(0, 1) = 0 THEN
	IF tree.left.count <= thresh THEN
	  nremoved := tree.left.count;
	  tree.left := NIL;
	ELSE
	  nremoved := NodeDestroyer(tree.left, thresh);
	END;
      ELSE
	IF tree.right.count <= thresh THEN
	  nremoved := tree.right.count;
	  tree.right := NIL;
	ELSE
	  nremoved := NodeDestroyer(tree.right, thresh);
	END;
      END;
    ELSIF tree.left # NIL THEN
      IF tree.left.count <= thresh THEN
	nremoved := tree.left.count;
	tree.left := NIL;
      ELSE
	nremoved := NodeDestroyer(tree.left, thresh);
      END;
    ELSIF tree.right # NIL THEN
      IF tree.right.count <= thresh THEN
	nremoved := tree.right.count;
	tree.right := NIL;
      ELSE
	nremoved := NodeDestroyer(tree.right, thresh);
      END;
    END;
    DEC(tree.count, nremoved);

    RETURN nremoved;
  END NodeDestroyer;

VAR
  a, b, c, d, e, f: INTEGER;

PROCEDURE NoGC(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    iter: CARDINAL := 100;
    id: CARDINAL := 0;
  BEGIN
    IF arg # NIL THEN
      WITH gcArgs = NARROW(arg, REF GCargs) DO
        iter := gcArgs.iter;
        id := gcArgs.id;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<N " & Fmt.Int(id) & " " & Fmt.Int(iter) & ">");
      END;
    END;
      
    FOR i := 1 TO iter DO
      IF NOT testing THEN EXIT; END;

      IF verbose > 0 THEN
        LOCK printmu DO 
          IO.Put("<N " & Fmt.Int(id) & " " & Fmt.Int(i) & ">");
        END;
      END;
      
      (* Calculate a lot *)
      f := 3;
      FOR i := 1 TO 1000000 DO
        a := b * 13;
        d := a * c;
        b := d DIV f;
        INC(f);
        IF b < 2 THEN b := 134; END;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<N " & Fmt.Int(id) & " done>"); 
      END;
    END;

    RETURN NIL;
  END NoGC;
 
PROCEDURE StaticGC(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    number: CARDINAL := 1024; 
    size: CARDINAL := 128;
    iter: CARDINAL := 100;
    trav: CARDINAL := 100;
    id: CARDINAL := 0;
    root : Node := NIL;
    ndeleted : CARDINAL := 0;
  BEGIN
    IF arg # NIL THEN
      WITH gcArgs = NARROW(arg, REF GCargs) DO
        number := gcArgs.number;
        size := gcArgs.size;
        iter := gcArgs.iter;
        trav := gcArgs.iter;
        id := gcArgs.id;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<S " & Fmt.Int(id) & " : " & Fmt.Int(iter) & " " &
          Fmt.Int(number) & "/" & Fmt.Int(size) & ".\n");
      END;
    END;
      
    (* Allocate a lot *)
    FOR i := 1 TO number DO
      root := AddTree(root, NEW(Node, 
                                word := Random(0, LAST(INTEGER)-1),
                                array := NEW(REF ARRAY OF CHAR, 
                                             Random(1, size))));
    END;

    FOR i := 1 TO iter DO
      IF NOT testing THEN EXIT; END;

      IF verbose > 0 THEN
        LOCK printmu DO 
          IO.Put("<S " & Fmt.Int(id) & " " & Fmt.Int(i) & ">");
        END;
      END;
      
      (* Traverse a lot *)
      FOR i := 1 TO trav DO
        EVAL TraverseUnlocked(root);
      END;

      (* Modify a little. Delete approximately 1% of the nodes and
         add back as many new ones. *)
      ndeleted := NodeDestroyer(root, number DIV 100);

      FOR i := 1 TO ndeleted DO
        root := AddTree(root, NEW(Node, 
                                  word := Random(0, LAST(INTEGER)-1),
                                  array := NEW(REF ARRAY OF CHAR, 
                                               Random(1, size))));
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<S " & Fmt.Int(id) & " done>"); 
      END;
    END;

    RETURN NIL;
  END StaticGC;

PROCEDURE DynamicGC(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    number: CARDINAL := 1024; 
    size: CARDINAL := 128;
    iter: CARDINAL := 100;
    id: CARDINAL := 0;
    root : Node;
  BEGIN
    IF arg # NIL THEN
      WITH gcArgs = NARROW(arg, REF GCargs) DO
        number := gcArgs.number;
        size := gcArgs.size;
        iter := gcArgs.iter;
        id := gcArgs.id;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<D " & Fmt.Int(id) & " : " & Fmt.Int(iter) & " " &
          Fmt.Int(number) & "/" & Fmt.Int(size) & ">");
      END;
    END;
      
    FOR i := 1 TO iter DO
      IF NOT testing THEN EXIT; END;

      LOCK printmu DO 
        IO.Put("<D " & Fmt.Int(id) & " : " & Fmt.Int(i) & ">");
      END;

      (* Allocate a lot *)
      FOR j := 1 TO number DO
        FOR k := 1 TO 1000 (*trav*) DO
          EVAL TraverseUnlocked(root);
        END;

        root := AddTree(root, NEW(Node, 
                                  word := Random(0, LAST(INTEGER)-1),
                                  array := NEW(REF ARRAY OF CHAR,
                                               Random(1, size))));
      END;

      (* Deallocate a lot *)
      root := NIL;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO
        IO.Put("<D " & Fmt.Int(id) & " done>"); 
      END;
    END;

    RETURN NIL;
  END DynamicGC;


PROCEDURE BurstyGC(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    number: CARDINAL := 1024; 
    size: CARDINAL := 128;
    iter: CARDINAL := 100;
    id: CARDINAL := 0;
    root : Node;
  BEGIN
    IF arg # NIL THEN
      WITH gcArgs = NARROW(arg, REF GCargs) DO
        number := gcArgs.number;
        size := gcArgs.size;
        iter := gcArgs.iter;
        id := gcArgs.id;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<B " & Fmt.Int(id) & " : " & Fmt.Int(iter) & " " &
          Fmt.Int(number) & "/" & Fmt.Int(size) & ".\n");
      END;
    END;
      
    FOR i := 1 TO iter DO
      IF NOT testing THEN EXIT; END;

      (* Allocate a lot *)
      FOR j := 1 TO number DO
        root := AddTree(root, NEW(Node, 
                                  word := Random(0, LAST(INTEGER)-1),
                                  array := NEW(REF ARRAY OF CHAR,
                                               Random(1, size))));
      END;
      
      (* Traverse a lot *)
      FOR j := 1 TO 1000 DO
        EVAL TraverseUnlocked(root);
      END;

      (* Deallocate a lot *)
      root := NIL;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO
        IO.Put("<B " & Fmt.Int(id) & " done>"); 
      END;
    END;

    RETURN NIL;
  END BurstyGC;

PROCEDURE NeglectGC(arg: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    number: CARDINAL := 1024; 
    size: CARDINAL := 128;
    iter: CARDINAL := 100;
    id: CARDINAL := 0;
    root : Node;
    child : Node;
    total : CARDINAL;
  BEGIN
    IF arg # NIL THEN
      WITH gcArgs = NARROW(arg, REF GCargs) DO
        number := gcArgs.number;
        size := gcArgs.size;
        iter := gcArgs.iter;
        id := gcArgs.id;
      END;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<G " & Fmt.Int(id) & " : " & Fmt.Int(iter) & " " &
          Fmt.Int(number) & "/" & Fmt.Int(size) & ".\n");
      END;
    END;
      
    FOR i := 1 TO iter DO
      IF NOT testing THEN EXIT; END;

      root := NIL;

      (* Allocate a lot, letting most of the memory become garbage 
         immediately *)
      total := 0;
      FOR j := 1 TO number DO
        REPEAT
          child := NEW(Node, word := Random(0, LAST(INTEGER)-1),
                       array := NEW(REF ARRAY OF CHAR, 
                                    Random(1, size)));
          INC(total);
        UNTIL Random(1, 10) = 5;
        root := AddTree(root, child);
      END;
      
      (* Traverse a lot *)
      FOR i := 1 TO 1000 DO
        EVAL TraverseUnlocked(root);
      END;

      (* Deallocate a lot *)
      root := NIL;
    END;

    IF verbose > 0 THEN
      LOCK printmu DO 
        IO.Put("<G " & Fmt.Int(id) & " done>"); 
      END;
    END;

    RETURN NIL;
  END NeglectGC;

PROCEDURE AwaitChar () =
  VAR rd: Rd.T := ThreadExtra.GetRdSelf();
  BEGIN
    LOOP
      TRY
        IF Rd.CharsReady(rd) > 0 THEN
          EVAL Rd.GetChar(rd);
          RETURN;
        ELSE
          ThreadExtra.Yield();
        END;
      EXCEPT
        Rd.EndOfFile, Rd.Failure, Thread.Alerted => (* do nothing *)
      END;
    END;
  END AwaitChar;

PROCEDURE GetArgs (pp: ParseParams.T): REF GCargs =

  PROCEDURE GetNextInt (pp: ParseParams.T; default: INTEGER): INTEGER =
    VAR
      arg    : TEXT;
      result : INTEGER := default;
    BEGIN
      TRY 
        arg := pp.peekNext();
        IF arg # NIL THEN
          result := Lex.Int(TextRd.New(arg));
          pp.skipNext();
        END;
      EXCEPT
      | ParseParams.Error =>
        (* End of list *)
      | Lex.Error =>
        (* Not a number *)
      END;
      RETURN result;
    END GetNextInt;

  BEGIN
    RETURN NEW(REF GCargs,
               id := GetNextID(),
               number := GetNextInt(pp, 1024), 
               size := GetNextInt(pp, 128), 
               iter := GetNextInt(pp, 100));
  END GetArgs;

VAR
  id : INTEGER := 0;

PROCEDURE GetNextID (): INTEGER =
  BEGIN
    INC(id);
    RETURN id;
  END GetNextID;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    job : TEXT;
  BEGIN
    pp.reset();
    pp.skipNext();             (* skip arg0 *)
    IF pp.testNext("zap") THEN
      Zap();
      RETURN TRUE;
    ELSIF pp.testNext("stop") THEN
      testing := FALSE;
      RETURN TRUE;
    ELSIF pp.testNext("verbose") THEN
      (* FIXME *)
      verbose := verbose;
      RETURN TRUE;
    END;
    
    testing := TRUE;

    TRY 
      (* Read off the arguments. They can be of the form
         job-type [number] [size]
         where job-type is one of the names static, dynamic, bursty or
         neglect. number and size are optional integer parameters.
         We can have any number of arguments on the command line.
         One thread will be forked for each of the job-types,
         and each thread will operate on its own tree. *)
      LOOP
        job := pp.peekNext();

        IF job = NIL THEN
          EXIT;
        ELSE
          pp.skipNext();
        END;

        IF Text.Equal(job, "nothing") THEN
          EVAL ThreadExtra.PFork(NoGC, GetArgs(pp));
        ELSIF Text.Equal(job, "static") THEN
          EVAL ThreadExtra.PFork(StaticGC, GetArgs(pp));
        ELSIF Text.Equal(job, "dynamic") THEN
          EVAL ThreadExtra.PFork(DynamicGC, GetArgs(pp));
        ELSIF Text.Equal(job, "bursty") THEN
          EVAL ThreadExtra.PFork(BurstyGC, GetArgs(pp));
        ELSIF Text.Equal(job, "neglect") THEN
          EVAL ThreadExtra.PFork(NeglectGC, GetArgs(pp));
        ELSE
          RAISE ParseParams.Error;
        END;
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
       RETURN FALSE;
    END;

    (* FIXME: why doesn't this work??? *)
    (*
    AwaitChar();

    testing := FALSE;
    *)

    RETURN TRUE;
  END Run;

BEGIN
END GCBench.

 
