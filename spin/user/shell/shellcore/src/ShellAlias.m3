(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
MODULE ShellAlias;
IMPORT IO;
IMPORT Commands;
IMPORT ParseParams;
IMPORT TextRefTbl;

CONST
  AliasHelp = " command body...";
  UnaliasHelp = " command";
  
VAR
  aliases := NEW(TextRefTbl.Default).init();
PROCEDURE Find (cmd: TEXT): TEXT =
  VAR r: REFANY;
  BEGIN
    IF aliases.get(cmd, r) THEN
      RETURN r;
    ELSE
      RETURN NIL;
    END;
  END Find;

PROCEDURE ShowAliasList () =
  VAR
    itr := aliases.iterate();
    cmd: TEXT;
    body: REFANY;
  BEGIN
    WHILE itr.next(cmd, body) DO
      IO.Put(cmd & "\t" & body & "\n");
    END;
  END ShowAliasList;
  
PROCEDURE Alias (<*UNUSED*>c: REFANY; pp : ParseParams.T) : BOOLEAN =
  VAR
    cmd: TEXT;
    body: TEXT;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      IF pp.next > LAST(pp.arg^) THEN
	ShowAliasList();
	RETURN TRUE;
      END;
      
      cmd := pp.getNext();
      FOR i := pp.next TO LAST(pp.arg^) DO
	IF i > pp.next THEN body := body & " " & pp.arg[i];
	ELSE body := pp.arg[i];
	END;
      END;
      EVAL aliases.put(cmd, body);
    EXCEPT
    | ParseParams.Error =>
      IO.Put("alias:" & AliasHelp & ".\n");
    END;
    RETURN TRUE;
  END Alias;

PROCEDURE Unalias (<*UNUSED*>c: REFANY; pp : ParseParams.T) : BOOLEAN =
  VAR
    cmd: TEXT;
    body: REFANY;
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      cmd := pp.getNext();
      EVAL aliases.delete(cmd, body);
    EXCEPT
    | ParseParams.Error =>
      IO.Put("unalias:" & UnaliasHelp & ".\n");
    END;
    RETURN TRUE;
  END Unalias;
  
BEGIN
  EVAL Commands.Install(Alias, "alias", AliasHelp);
  EVAL Commands.Install(Unalias, "unalias", UnaliasHelp);
END ShellAlias.
