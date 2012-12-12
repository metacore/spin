(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE IpDefault;
IMPORT IpPktFormat;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "ipdefault";
      CommandHelp = "-- -debug level| -packets";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE T = IpPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
END IpDefault.
