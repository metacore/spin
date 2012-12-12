(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from User/net/etherClient/src and simplified.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to work with FreeBSD SAL.
 *
 * 06-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new style spin shell commands.
 *
 *)

INTERFACE StcpEtherArp;
IMPORT StcpMbuf;

VAR
  debug : BOOLEAN := FALSE;

PROCEDURE Init();

PROCEDURE PacketArrived(m, c : StcpMbuf.T ; offset : CARDINAL) : StcpMbuf.T ;

PROCEDURE SendRequest();

END StcpEtherArp.
