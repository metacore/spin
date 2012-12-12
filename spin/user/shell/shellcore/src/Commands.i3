(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

INTERFACE Commands;
TYPE T <: TPublic;
TYPE TPublic = OBJECT
  Run: PROCANY;
  name: TEXT;
  synopsis: TEXT;
  help: TEXT;
END;

PROCEDURE Install(Run: PROCANY;
		  command: TEXT;
		  synopsis: TEXT;
		  help1: TEXT := NIL;
		  help2: TEXT := NIL;
		  help3: TEXT := NIL;
		  help4: TEXT := NIL;
		  help5: TEXT := NIL): T;

PROCEDURE Uninstall(t: T);
  
PROCEDURE ParseError(closure: TPublic);

END Commands.


