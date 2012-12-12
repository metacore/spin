(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)

(*
 *  Output module for C headers. Not all M3 constructs are translated into
 *  C counterparts. For those types we can't support, we just ignore them.
 *)
INTERFACE UserCHeader;
IMPORT Module;

PROCEDURE Output(READONLY m : Module.T);

END UserCHeader.

