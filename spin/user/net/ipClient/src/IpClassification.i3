(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new spin shell command style.
 *
 *)

(* Untrusted *)
INTERFACE IpClassification;

IMPORT IpPktFormat, ParseParams, Mbuf;

(* shell command support *)
CONST CommandName = "ipclassification";
      CommandHelp = "-- -debug level";
PROCEDURE Run(pp: ParseParams.T): BOOLEAN;

TYPE T = IpPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);

FUNCTIONAL
PROCEDURE Guard_ICMP(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN;

FUNCTIONAL
PROCEDURE Guard_UDP(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN;

FUNCTIONAL
PROCEDURE Guard_TCP(
    <* UNUSED *> packet: Mbuf.T; 
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN;

END IpClassification.
