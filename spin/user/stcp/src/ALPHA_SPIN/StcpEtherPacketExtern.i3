(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Reverted If structure defined as urt/urtcore/src.
 *
 * 03-Feb-97 Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/net/ether/src/ALPHA_SPIN.
 *	Changed sal_ether_input to stcp_input.
 *)

UNSAFE (* for externals *)
INTERFACE StcpEtherPacketExtern;

IMPORT StcpMbuf;
IMPORT StcpIf;
IMPORT StcpEtherPktFormat;

<* EXTERNAL "sal_ether_input" *>
VAR ether_input: PROCEDURE( ifp: UNTRACED REF StcpIf.ifnet;
    READONLY ehp: StcpEtherPktFormat.T; m: StcpMbuf.T): BOOLEAN;

(* kernel/sal/Common/standalone/net/net_poll.c *)
<* EXTERNAL *> PROCEDURE net_poll_getifp(): UNTRACED REF StcpIf.ifnet;

END StcpEtherPacketExtern.

