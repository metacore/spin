(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test thread.t garbage collection.
 *
 *)
MODULE Fork;

IMPORT Thread, ThreadExtra, ParseParams;
IMPORT SafeConvert, Fmt, IO;

PROCEDURE Nop(<*UNUSED*>arg: REFANY) : REFANY =
  BEGIN
    RETURN NIL;
  END Nop;

TYPE Turd = REF RECORD next: Turd; END;

PROCEDURE SpaghettiMaker(n: INTEGER) =
  VAR new, cur: Turd;
  BEGIN
    FOR i := 0 TO n DO
      cur := NIL;
      FOR j := 0 TO 1000 DO
        new := NEW(Turd);
        new.next := cur;
      END;
    END;
  END SpaghettiMaker;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    num: INTEGER;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();               (* command *)
      num := SafeConvert.Atoi(pp.getNext());
      IF pp.keywordPresent("-spaghetti") THEN
        SpaghettiMaker(num);
      ELSE
        IO.Put("Forking " & Fmt.Int(num) & " threads.\n");
        FOR i := 1 TO num DO
          VAR
            th:  Thread.T;
          BEGIN
            th := ThreadExtra.PFork(Nop, NIL);
            EVAL Thread.Join(th);
            IO.Put(Fmt.Unsigned(SafeConvert.RefAnyToWord(th)));
            IO.Put("\n");
          END;
        END;
      END;
    EXCEPT ELSE END;
    RETURN TRUE;
  END Run;

BEGIN
END Fork.
