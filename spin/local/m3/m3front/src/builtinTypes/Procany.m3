
(*
 * HISTORY
 * 15-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)


MODULE Procany;

IMPORT RefType, Tipe, M3String, TextExpr;

PROCEDURE Initialize () =
  BEGIN
    T := RefType.New (NIL, FALSE, TextExpr.New (M3String.Add ("$procany$")),
                      name := "PROCANY");
    Tipe.Define ("PROCANY", T, TRUE);
  END Initialize;

BEGIN
END Procany.
