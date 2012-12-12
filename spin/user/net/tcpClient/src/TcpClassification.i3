(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

(* Untrusted *)
INTERFACE TcpClassification;
IMPORT TcpPktFormat;
IMPORT IpPktFormat;
IMPORT Ctypes;

IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "tcpclassification";
      CommandHelp = "-- -debug level| -portredirect hostname hostname portnum | -hostredirect hostname hostname";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;


TYPE T = TcpPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Redirect(ipsaddr,ipdaddr: IpPktFormat.Address; port:Ctypes.unsigned_short; hostonly: BOOLEAN): REFANY;
END TcpClassification.
