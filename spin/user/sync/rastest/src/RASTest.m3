(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 25-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test for enqueue/dequeue ops using ras regions.
 *)
MODULE RASTest;
IMPORT FastList;
IMPORT ParseParams, IO;

EXCEPTION Bug;

CONST
  NumElems = 10000;
  OuterIterations = 1000;

(*
 * we'll use a global queue since it is cumbersome to init and name
 * resources from the shell. This makes this test single-threaded. 
 * That is, if you run it in parallel, it will falsely report errors.
 *)
VAR
  list1, list2: REF FastList.T;

PROCEDURE DoInit() =
  VAR
    t: FastList.T;
  BEGIN
    FOR i := 1 TO NumElems DO
      t := NEW(FastList.T);
      FastList.Enqueue(t, list1);
    END;
  END DoInit;

(*
 * we move a lot of stuff from one list to another. If we
 * ever drop or repeat an enqueue or dequeue operation, the
 * numbers won't match and we'll signal an error.
 *)
PROCEDURE DoTest() =
  VAR
    t: FastList.T;
  BEGIN
    TRY
      FOR i := 1 TO OuterIterations DO
        FOR j := 1 TO NumElems DO
          t := FastList.Dequeue(list1);
          IF t = NIL THEN RAISE Bug; END;
          FastList.Enqueue(t, list2);
        END;
        FOR j := 1 TO NumElems DO
          t := FastList.Dequeue(list2);
          IF t = NIL THEN RAISE Bug; END;
          FastList.Enqueue(t, list1);
        END;
      END;
      IO.Put("RASTest passed OK.\n");
    EXCEPT
      Bug =>
      IO.Put("Mismatch in enqueue/dequeue operations\n");
      IO.Put("Possible bug in ras implementation or possibly this\n");
      IO.Put("single-threaded test was run in parallel.\n");
    END;
  END DoTest;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      IF pp.keywordPresent("init") THEN
        DoInit();
      ELSIF pp.keywordPresent("test") THEN
        DoTest();
      ELSE
        RAISE ParseParams.Error;
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put("Usage: rastest\n");
    END;
    RETURN TRUE;
  END Run;

BEGIN 
  list1 := NEW(REF FastList.T);
  list2 := NEW(REF FastList.T);
  IO.Put("rastest initialized...\n");
END RASTest.

