(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 07-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added destroy.
 *
 * 11-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 18-Jan-96  David Becker (becker) at the University of Washington
 *      Created.
 *)

(* "DomainCmd" enables domain manipulations from the shell. *)

INTERFACE DomainCmd;
IMPORT ParseParams;

CONST CommandName = "domain";
CONST CommandHelp = "create foo|addfile foo file|link foo domainname|run foo|check foo|initialize foo|unbind foo|bind domainname1 domainname2|nlist domainname|destroy domainname";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END DomainCmd.
