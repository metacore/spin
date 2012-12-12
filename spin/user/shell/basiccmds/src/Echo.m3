(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Dec-95  Whisted.
 *
 *)
MODULE Echo;

IMPORT IO, ParseParams;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
BEGIN
  pp.reset();
  TRY
    pp.skipNext();               (* command *)
    WITH nArgs = NUMBER(pp.arg^) - pp.next DO
      FOR i := 1 TO nArgs DO
        IO.Put(pp.getNext() & " ");
      END;
      IO.Put("\n");
    END;
  EXCEPT ELSE END;
  RETURN TRUE;
END Run;

BEGIN
END Echo.
