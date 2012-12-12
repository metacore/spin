(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE MpegShell;

(*SPIN SPECIFIC*)
IMPORT ParseParams;
CONST Brand = "MpegShell";
CONST CommandName = "mpeg";
      CommandHelp = " -play <filename>";

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

END MpegShell.


