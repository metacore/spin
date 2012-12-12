(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

UNSAFE (* for externals *)
INTERFACE T3PacketExtern;
IMPORT If;
IMPORT Mbuf;

<* EXTERNAL "t3_input_upcall" *>
VAR t3_input: PROCEDURE( ifp: UNTRACED REF If.ifnet; m: Mbuf.T);

END T3PacketExtern.
