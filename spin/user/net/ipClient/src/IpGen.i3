(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Mar-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE IpGen;
IMPORT IpPktFormat;
IMPORT IpRoute;
IMPORT Mbuf;
IMPORT ParseParams;

(* shell command support *)
CONST CommandName = "ipgen";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

PROCEDURE PacketSend(
	READONLY ip	: IpPktFormat.Header;
	READONLY data	: Mbuf.T;
                 rt     : IpRoute.T := NIL);

PROCEDURE Init(verbose:BOOLEAN);
END IpGen.
