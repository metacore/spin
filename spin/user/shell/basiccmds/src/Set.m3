(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 21-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
MODULE Set EXPORTS CoreCommands;
IMPORT IO, Glob, ParseParams, Shell;
IMPORT InfoFile, Wr, ThreadExtra, Error;
IMPORT Commands;

PROCEDURE ShowAllVariables () =
  VAR vars: REF ARRAY OF TEXT;
  BEGIN
    vars := Glob.GetVariableList(Shell.Vars());
    IF vars # NIL THEN
      FOR i := FIRST(vars^) TO LAST(vars^) DO ShowVariable(vars^[i]); END;
    ELSE
      IO.Put("No variables defined\n");
    END;
  END ShowAllVariables;

PROCEDURE ShowVariable(var: TEXT) =
  VAR val: TEXT;
  BEGIN
    IF var = NIL THEN
      ShowAllVariables();
      RETURN;
    ELSE
      val := Glob.GetVariable(Shell.Vars(), var);
      IF val = NIL THEN
        IO.Put(var & "NIL\n");
      ELSE
        IO.Put("$" & var & " " & val & "\n");
      END;
    END;
  END ShowVariable;

PROCEDURE Run (<*UNUSED*>r: REFANY; pp: ParseParams.T): BOOLEAN =
  VAR var: TEXT;
      val: TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* skip set *)
      var := pp.getNext();
      TRY
        val := pp.getNext();
      EXCEPT
        ParseParams.Error => ShowVariable(var); RETURN TRUE;
      END;
      Glob.SetVariable(Shell.Vars(), var, val);
      ShowVariable(var);
    EXCEPT
      ParseParams.Error => ShowAllVariables(); (* all variables *)
    END;
    RETURN TRUE;
  END Run;


PROCEDURE Vars (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    ShowAllVariables ();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END Vars;

BEGIN
  TRY
    EVAL Commands.Install(Run, "set", "variable value");
    InfoFile.Create("/proc/spinshell_vars",Vars);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END Set.
