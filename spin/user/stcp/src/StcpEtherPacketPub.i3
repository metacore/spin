(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Created to pass packets to user/net/ether.
 *)

INTERFACE StcpEtherPacketPub;
IMPORT StcpMbuf;

(* override this to get packets *)
VAR Arrived : PROCEDURE(packet: StcpMbuf.T);

PROCEDURE Init();

END StcpEtherPacketPub.
