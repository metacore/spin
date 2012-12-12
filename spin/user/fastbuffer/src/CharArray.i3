(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)

(* This module provides fast NEW(REF ARRAY OF CHAR)
   by reusing buffers of frequently used sizes *)
 
INTERFACE CharArray;

PROCEDURE Allocate(size: CARDINAL): REF ARRAY OF CHAR;
(* Allocate a buffer long enough to hold "size" bytes.
   The buffer may not be exactly "size" bytes long, and
   its initial contents are undefined. *)

PROCEDURE Free(buf: REF ARRAY OF CHAR);
(* The "buf" must be the one returned by "Allocate". *)

END CharArray.
