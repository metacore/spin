(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Mon Mar 28 15:21:18 PST 1994 by heydon     *)

(* The "FmtBuf" interface provides functionality similar to the "Fmt"
   interface, but the formatted result is written into a user-supplied
   character buffer, rather than being returned as a "TEXT". *)

INTERFACE FmtBuf;

IMPORT Fmt, Word;

TYPE
  T = ARRAY OF CHAR;
  Base = Fmt.Base;

(* Each of these routines has the same specification as the corresponding
   routine in the "Fmt" interface, except that they write the result into
   the character buffer "b" and return the number of characters written.
   A checked run-time error occurs if the buffer "b" is not large enough
   to hold the result. See the "FmtBufF" interface for an analysis of a
   conservative upper-bound on the required buffer size. *)

PROCEDURE Int(VAR (*OUT*) b: T; n: INTEGER; base: Base := 10): CARDINAL;
PROCEDURE Unsigned(VAR (*OUT*) b: T; n: Word.T; base: Base := 16): CARDINAL;

END FmtBuf.
