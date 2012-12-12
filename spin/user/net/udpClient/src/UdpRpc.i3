(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	This module test latency and bandwidth performance using UDP send
 *	and receive directly.  At this point it doesn't implement a
 *	"real" RPC package.  
 *	
 *
 *)

(* Untrusted *)
INTERFACE UdpRpc;
IMPORT UdpPktFormat;
IMPORT ParseParams;
(* shell command support *)
CONST CommandName = "udprpc";
      CommandHelp = " -debug level| -cport num| -sport num| -ackonly| -reflect|\n\t-installserver| -installclient| -send srchostname dsthostname numpkts size\n\t-installguard [empty|full] num| -uninstallguard\n\t-addone| -delone| -toggleall| -togglefire| -toggleone";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

<* OBSOLETE *> TYPE T = UdpPktFormat.T;
TYPE NewT = UdpPktFormat.NewT;
PROCEDURE Init(verbose:BOOLEAN);
END UdpRpc.
