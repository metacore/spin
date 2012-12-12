

(*
 * HISTORY
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	globalness is more precise
 *      removed dead code
 *
 * 26-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created: contains code to check FUNCTIONAL
 *
 *)

MODULE Functional;

IMPORT M3, Expr, Value, Error, UserProc, Scope;

PROCEDURE Check () : BOOLEAN =
  BEGIN
    WITH s = Scope.Top () DO
      RETURN s # NIL AND Scope.InFunctional (s);
    END;
  END Check;

PROCEDURE CheckExpr (e: Expr.T; t: TEXT) =
  VAR val: Value.T;
  BEGIN
    WITH s = Scope.Top () DO
      IF s # NIL AND Scope.InFunctional (s) AND
        NOT UserProc.IsProcedureLiteral (e, val) AND
        Expr.IsGlobal (e) # M3.Global.No THEN

        Error.Msg ("Global " & t & " in FUNCTIONAL procedure");
        
      END;
    END;
  END CheckExpr;

PROCEDURE Warn (t: TEXT) =
  BEGIN
    WITH s = Scope.Top () DO
      IF s # NIL AND Scope.InFunctional (s) THEN
        Error.Warn (2, t);
      END;
    END;
  END Warn;

BEGIN
END Functional.
