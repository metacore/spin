(*
 *
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *)

(*
 * HISTORY
 *
 *)

MODULE Analysis EXPORTS Analysis, AnalysisPrivate;

IMPORT BasicBlock;

(* in AnalysisPrivate *)

PROCEDURE GetName (a: T) : TEXT =
  BEGIN
    RETURN a.name;
  END GetName;

PROCEDURE GetDirection (a: T) : Direction =
  BEGIN
    RETURN a.direction;
  END GetDirection;

PROCEDURE InvalidateResidueBlock(a: T; block: BasicBlock.T) =
  BEGIN
    a.SetResidueIn (block, NIL);
  END InvalidateResidueBlock;

BEGIN
END Analysis.
