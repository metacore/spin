(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE DrawPerfShell;

(*SPIN SPECIFIC*)
IMPORT ParseParams;
CONST Brand = "DrawPerfShell";
CONST CommandName = "drawperf";
      CommandHelp = " -help";

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

END DrawPerfShell.


