(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Fake vm error codes. *)
INTERFACE VMError;
EXCEPTION E(INTEGER);
PROCEDURE Message(e: INTEGER): TEXT;
END VMError.
