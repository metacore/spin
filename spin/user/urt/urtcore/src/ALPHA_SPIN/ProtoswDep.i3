(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

INTERFACE ProtoswDep;
CONST 
  PRU_SEND_EOF = 21;
  PRU_NREQ     = 21;

  PRC_NCMDS    = 24;

 (* char   *prcrequests[] = { 
	"IFDOWN", "ROUTEDEAD", "#2", "DEC-BIT-QUENCH2",
	"QUENCH", "MSGSIZE", "HOSTDEAD", "#7",
	"NET-UNREACH", "HOST-UNREACH", "PROTO-UNREACH", "PORT-UNREACH",
	"#12", "SRCFAIL-UNREACH", "NET-REDIRECT", "HOST-REDIRECT",
	"TOSNET-REDIRECT", "TOSHOST-REDIRECT", "TX-INTRANS", "TX-REASS",
	"PARAMPROB", "NEWADDRSET", "EVENT", "NMADD"
 *)


  PRCO_NCMDS   = 5;

 (* char   *prcorequests[] = {
	"GETOPT", "SETOPT", "PIF", "NWMGT", "TRACE"
 *)
END ProtoswDep.
