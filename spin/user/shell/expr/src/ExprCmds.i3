(*
 * HISTORY
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Added Error.
 *
 * 16-May-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

INTERFACE ExprCmds;

IMPORT SpinException;

CONST Brand = "ExprCmds";  (* Use this name to link with this extension *)

PROCEDURE Error(ec: SpinException.ExceptionInfo);
	(* Call here if you get in trouble. *)

END ExprCmds.

