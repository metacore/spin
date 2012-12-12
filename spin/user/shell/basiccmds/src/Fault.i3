(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Shell interface.
 * Need to revise to export all services as extensions.
 *
 *
 * HISTORY
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted. Added nilref, narrow and raise arguments.
 *
 *)
INTERFACE Fault;

IMPORT ParseParams;

CONST CommandName = "fault";
CONST CommandHelp = " [nilref|narrow|raise]";

PROCEDURE Run (<*UNUSED*> pp: ParseParams.T) : BOOLEAN;

END Fault.
