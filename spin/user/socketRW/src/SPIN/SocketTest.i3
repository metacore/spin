(* HISTORY
 * 26-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	A command to test the use of socket readers and writers.
 *
 *)

INTERFACE SocketTest;

IMPORT ParseParams;

CONST CommandName = "socket";
CONST CommandHelp = " {-server|-client hostname} port#";

PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

END SocketTest.

