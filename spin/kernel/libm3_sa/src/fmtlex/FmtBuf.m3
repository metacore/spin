(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Mar 17 12:37:12 PST 1994 by heydon     *)

MODULE FmtBuf EXPORTS FmtBuf;

IMPORT Word, Convert;

PROCEDURE Int(VAR (*INOUT*) b: T; n: INTEGER; base: Base := 10): CARDINAL =
  <* FATAL Convert.Failed *>
  BEGIN
    RETURN Convert.FromInt(b, n, base, prefix := FALSE)
  END Int;

PROCEDURE Unsigned(VAR (*INOUT*) b: T; n: Word.T; base: Base := 16): CARDINAL =
  <* FATAL Convert.Failed *>
  BEGIN
    RETURN Convert.FromUnsigned(b, n, base, prefix := FALSE)
  END Unsigned;

BEGIN
END FmtBuf.
