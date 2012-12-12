(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *
 *)

INTERFACE CacheCmd;

IMPORT ParseParams;

CONST CommandName = "cache";
CONST CommandHelp = "hello | stat";

PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END CacheCmd.

