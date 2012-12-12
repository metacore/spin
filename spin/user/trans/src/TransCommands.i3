(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to  one-line-at-a-time execution.
 *)

INTERFACE TransCommands;
IMPORT IO, Error, Lex;

TYPE Command = RECORD
  name: TEXT;
  help: TEXT;
  proc: PROCEDURE(READONLY argv: ARRAY OF TEXT)
      RAISES {IO.Error, Error.E, Lex.Error};
END;

PROCEDURE Execute(READONLY argv: ARRAY OF TEXT);
(* Do command interpretation.  *)

PROCEDURE AddCommands(READONLY commands: ARRAY OF Command);
  
END TransCommands.
