(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 02-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "Error.i3" provides common types for handling exceptions.

   Error.T is a base type for exception arguments that can describe
   the exception as a string or a result code.  Error.E is the
   exception for this base class.

   A module may define it's own error messages by using a subtype of
   "Error.T".  *)

INTERFACE Error;

TYPE T <: Public;
  Public = OBJECT METHODS
    init(code: INTEGER): T;
    resultCode(): INTEGER;
    errno(): INTEGER;
    message(): TEXT;
  END;
  
EXCEPTION E(T);

END Error.
