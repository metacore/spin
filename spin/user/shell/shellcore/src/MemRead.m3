(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-May-96  Stefan Savage (savage) at the University of Washington
 *	Allows you to read an address from the shell.
 *)
UNSAFE MODULE MemRead;

IMPORT IO, Fmt, ParseParams, Scan, Lex, Word;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  VAR t: TEXT;
      adr: Word.T;
      ra: REF Word.T;
  BEGIN
    pp.reset();
    TRY
      t := pp.getNext();
    EXCEPT ParseParams.Error =>
      IO.Put("Usage: " & CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
    TRY
      adr := Scan.Unsigned(t);
    EXCEPT Lex.Error =>
      IO.Put("Error: malformed address " & t &"\n");
    END;
    ra := LOOPHOLE(adr, REF Word.T);
    IO.Put("Address is " & Fmt.Unsigned(adr) & ", val is " & Fmt.Unsigned(ra^));
    RETURN TRUE;
  END Run;

BEGIN
END MemRead.

