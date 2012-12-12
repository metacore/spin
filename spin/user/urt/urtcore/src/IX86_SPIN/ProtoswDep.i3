(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.  FreeBSD dependent constants.
 *)

INTERFACE ProtoswDep;
CONST 
  PRU_SEND_EOF = 22;
  PRU_NREQ     = 22;

  PRC_NCMDS    = 21;

 (* char   *prcrequests[] = { 
	"IFDOWN", "ROUTEDEAD", "#2", "DEC-BIT-QUENCH2",
	"QUENCH", "MSGSIZE", "HOSTDEAD", "#7",
	"NET-UNREACH", "HOST-UNREACH", "PROTO-UNREACH", "PORT-UNREACH",
	"#12", "SRCFAIL-UNREACH", "NET-REDIRECT", "HOST-REDIRECT",
	"TOSNET-REDIRECT", "TOSHOST-REDIRECT", "TX-INTRANS", "TX-REASS",
	"PARAMPROB", *)

  PRCO_NCMDS   = 2;

 (* char   *prcorequests[] = {"GETOPT", "SETOPT", *)
END ProtoswDep.
