(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Redirects specific UDP packets to another host.
 *
 * 03-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

(* Untrusted *)
INTERFACE UdpRedirect;
IMPORT UdpPktFormat;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "udpredirect";
      CommandHelp = "-- -debug level| -server hostname| -client hostname| -sport portnum| -mountdport portnum";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

<* OBSOLETE *> TYPE T = UdpPktFormat.T;
TYPE NewT = UdpPktFormat.NewT;
PROCEDURE Init(verbose:BOOLEAN);
END UdpRedirect. 
