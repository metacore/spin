(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(* Last Modified On Fri Feb 11 14:59:57 PST 1994 by kalsow                   *)
(*      Modified On Sat Feb  6 11:41:23 PST 1993 by mjordan                  *)
(*      Modified On Fri May 29 17:37:42 PDT 1992 by muller                   *)

(*
 * HISTORY
 * 16-Feb-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added UNUSED pragmas to avoid warnings.
 *)

UNSAFE MODULE RTPerfTool;


PROCEDURE Start (<* UNUSED *> param: TEXT;  <* UNUSED *>VAR w: Handle): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE Close (<* UNUSED *>w: Handle) =
  BEGIN
  END Close;

PROCEDURE Send (<* UNUSED *> w: Handle;  <* UNUSED *>at: ADDRESS;  <* UNUSED *> len: CARDINAL): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Send;

(*-------------------------------------------------------------- internal ---*)


BEGIN
END RTPerfTool.

