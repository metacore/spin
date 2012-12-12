(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

INTERFACE Stcp;
(*
IMPORT ParseParams;
 *)

CONST Brand = "Stcp";

PROCEDURE Init();
 (* function called to initialize M3 module.  Cannot rely on M3
 * strongly connected graph ordering to initialize modules in the
 * right order.
 *)

END Stcp.
