(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to add and lookup hosts from the shell.
 *
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Support to lookup hostname IP addresses.  
 *
 *)

INTERFACE NetDb;
IMPORT IpPktFormat;
IMPORT ParseParams;

(* shell command support *)
CONST CommandName = "netdb";
      CommandHelp = " -lookup hostname| -add hostname ip0 ip1 ip2 ip3| -del hostname";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

EXCEPTION HostNotFound;
PROCEDURE GetHostByName(machinename: TEXT):IpPktFormat.Address
  RAISES {HostNotFound};

<* OBSOLETE *> PROCEDURE HostName(name:TEXT);
<* OBSOLETE *> PROCEDURE GetHostName():TEXT;

CONST Brand = "NetDb";
END NetDb. 
