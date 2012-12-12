(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 *)


MODULE Priv;

IMPORT Shell, ParseParams;
IMPORT SpinException, IO, ExprCmds;
      

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
    res: BOOLEAN;
  BEGIN
    res := TRUE;
    pp.reset();
    TRY
      EVAL pp.getNext();       (* skip "priv" *)
      res := Shell.Run(pp.dup());
    EXCEPT
    | ParseParams.Error => IO.Put("priv what?\n"); RETURN FALSE;
    | SpinException.Exception (ei) => ExprCmds.Error(ei); RETURN FALSE;
    END;
    RETURN res;
  END Run;

BEGIN
END Priv.
