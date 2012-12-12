(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

INTERFACE Priv;

IMPORT ParseParams;

CONST CommandName = "priv";
CONST CommandHelp = " run command args as privileged identity\n";

(* Should ask for passwd, but we don't *)

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END Priv.
