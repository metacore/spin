(* HISTORY
 * 26-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	This command listens for connections on the specified port 
 *      and then spawns a shell which reads from and writes to the
 *      connection. This allows you to telnet to the machine and talk
 *      to the spin shell.
 *
 *)

INTERFACE TelnetShell;

IMPORT ParseParams;

CONST CommandName = "telnet_shell";
CONST CommandHelp = " port#";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END TelnetShell.


