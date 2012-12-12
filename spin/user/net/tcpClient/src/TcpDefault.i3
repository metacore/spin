(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new spin shell commands.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Handler that counts all TCP packets.
 *
 *)

INTERFACE TcpDefault;
IMPORT TcpPktFormat;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "tcpdefault";
      CommandHelp = "-- -debug level| -packets";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE T = TcpPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
END TcpDefault.
