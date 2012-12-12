(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE TcpGen;
IMPORT IpPktFormat;
IMPORT TcpPktFormat;
IMPORT Mbuf;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "tcpgen";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

PROCEDURE PacketSend(	
        READONLY ip     : IpPktFormat.Header;
	READONLY tcp  	: TcpPktFormat.Header;
	READONLY data	: Mbuf.T);
END TcpGen.
