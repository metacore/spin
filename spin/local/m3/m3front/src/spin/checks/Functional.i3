

(*
 * HISTORY
 * 26-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created: contains code to check FUNCTIONAL
 *
 *)

INTERFACE Functional;

IMPORT Expr;

PROCEDURE Check () : BOOLEAN;
PROCEDURE CheckExpr (e: Expr.T; t: TEXT);

PROCEDURE Warn (t: TEXT);

END Functional.

