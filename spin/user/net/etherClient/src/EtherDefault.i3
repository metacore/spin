(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 06-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new style spin shell commands.
 *
 *)

INTERFACE EtherDefault;
IMPORT EtherPktFormat, Net;
TYPE T = EtherPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
VAR ether_packet_counter : INTEGER;
    debug_level : Net.Level := Net.oLevel.NODEBUG;
END EtherDefault.
