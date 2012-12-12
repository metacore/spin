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

INTERFACE Analyzer;

IMPORT Analysis, BasicBlock, Procedure;

EXCEPTION Problem;

PROCEDURE Print(p: Procedure.T; a: Analysis.T) RAISES {Problem};

PROCEDURE Visit (p: Procedure.T; a: Analysis.T) RAISES {Problem};
PROCEDURE Optimize (p: Procedure.T; proc: BasicBlock.Proc);

(* build up instruction labels *)
PROCEDURE Label (p: Procedure.T);
PROCEDURE PrintDefs (p: Procedure.T);


END Analyzer.
