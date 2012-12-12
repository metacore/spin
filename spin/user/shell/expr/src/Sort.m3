(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Added ExprCmds.Error call.
 *
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 *)


MODULE Sort;
IMPORT Shell, ParseParams, Thread, ThreadExtra, IO;
IMPORT TextWr, TextExtras, TextConv, TextArraySort, ExprCmds;
IMPORT SpinException;



PROCEDURE Compare (t1: TEXT; t2: TEXT): [-1 .. 1] =
  VAR res: INTEGER;
  BEGIN
    res := TextExtras.CICompare(t1, t2);
    IF res < 0 THEN
      RETURN -1;
    ELSIF res = 0 THEN
      RETURN 0;
    ELSE
      RETURN 1;
    END;
  END Compare;
      


PROCEDURE RunThread (ra: ThreadExtra.ArgT): ThreadExtra.ResultT =
  VAR
    tw   : TextWr.T;
    t    : TEXT;
    array: REF ARRAY OF TEXT;
    len  : CARDINAL;
  CONST nl = TextConv.CharSet{'\n'};
  BEGIN
    TRY
      tw := TextWr.New();
      EVAL ThreadExtra.SetWrSelf(tw); (* discard old writer *)
      EVAL Shell.Run(NARROW(ra, ParseParams.T));
    EXCEPT
      SpinException.Exception (ei) => ExprCmds.Error(ei)
    END;

    (* Convert the writer into an array of TEXTs *)

    t := TextWr.ToText(tw);
    len := TextConv.ExplodedSize(t, nl);
    array := NEW(REF ARRAY OF TEXT, len);
    TextConv.Explode(t, array^, nl);
    TextArraySort.Sort(array^, Compare);
    RETURN TextConv.Implode(array^, '\n');
  END RunThread;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      EVAL pp.getNext();	(* skip "sort" *)
      IO.Put(
        NARROW(Thread.Join(ThreadExtra.PFork(RunThread, pp.dup())), TEXT) & "\n");
    EXCEPT
      ParseParams.Error => IO.Put("sort what?\n"); RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

BEGIN
END Sort.
