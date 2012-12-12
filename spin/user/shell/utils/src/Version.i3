(*
 * HISTORY
 * 15-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *
 *)
INTERFACE Version;

IMPORT ParseParams;

CONST CommandName = "version";
CONST CommandHelp = " -- show the kernel version";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END Version.
