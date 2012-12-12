(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-Oct-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

UNSAFE INTERFACE CerrnoExtern;

FROM Ctypes IMPORT int;

<*EXTERNAL*> VAR errno: int;
(* The value of errno is preserved across thread switches. *)

END CerrnoExtern.
