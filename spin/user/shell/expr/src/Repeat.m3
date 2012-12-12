(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Call ExprCmds.Error()
 *
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 *)


MODULE Repeat;
IMPORT Shell, ParseParams, IO, Scan, Lex;
IMPORT ExprCmds, SpinException;
      

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    count: CARDINAL;
    ppCmd: ParseParams.T;
    res: BOOLEAN := TRUE;
  BEGIN
    pp.reset();
    TRY
      EVAL pp.getNext();         (* skip "repeat" *)
      count := Scan.Int(pp.getNext());
      ppCmd := pp.dup();
      LOOP
        IF count <= 0 THEN EXIT END;
        ppCmd.reset();
        res := res AND Shell.Run(ppCmd);
        DEC(count);
      END;
    EXCEPT
    | ParseParams.Error => IO.Put("repeat what?\n"); RETURN FALSE;
    | Lex.Error => IO.Put(CommandHelp); RETURN FALSE;
    | SpinException.Exception (ei) => ExprCmds.Error(ei); RETURN FALSE;
    END;
    RETURN res;
  END Run;

BEGIN
END Repeat.
