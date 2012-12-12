(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE Round;

PROCEDURE Up(v, boundary : INTEGER) : INTEGER;
(* Round up the value V	to BOUNDARY byte boundary.

 XXX this has to be inlined!! *)

PROCEDURE Up8(v: INTEGER) : INTEGER;
(* equivalent to Up(v, 8). *)
  
PROCEDURE Down(v, boundary : INTEGER) : INTEGER;
(* Round down the value V to BOUNDARY byte boundary. *)
  
PROCEDURE UpToPage(v:INTEGER): INTEGER;
(* Equiv. to Up(v, PageSize) *)
  
PROCEDURE DownToPage(v:INTEGER): INTEGER;
(* Equiv. to Down(v, PageSize) *)
  
END Round.
