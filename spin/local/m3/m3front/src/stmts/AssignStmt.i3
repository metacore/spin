(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: AssignStmt.i3                                         *)
(* Last Modified On Fri Jun 24 08:56:06 PDT 1994 By kalsow     *)
(*      Modified On Tue Mar 20 01:30:09 1990 By muller         *)

(*
 * HISTORY
 * 25-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	add refCount arg to Emit
 *
 * 21-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	export NeedsClosureCheck so that Formal.m3 can use it
 *        to check when an argument might be a nested procedure
 *
 *)

INTERFACE AssignStmt;

IMPORT M3, Expr, Stmt, Type;

PROCEDURE Parse (): Stmt.T;

PROCEDURE Check (tlhs: Type.T;  rhs: Expr.T;  VAR cs: Stmt.CheckState);
(* check that rhs is assignable to a variable of type tlhs. *)

PROCEDURE Emit (tlhs: Type.T; rhs: Expr.T;
                isGlobal: M3.Global := M3.Global.No;
                compiled := FALSE);
(* emit code to assign  (s0.A).tlhs := rhs.
   Note that Emit assumes that TypeOf(rhs) is assignable to tlhs
   and that Expr.Prep(rhs) has been called.

   isGlobal is used to determine whether reference counting needs
   to anything
   if compiled is TRUE, that means that the rhs has been compiled
     already -- only OK if both sides are open arrays.  ugly, but
     it allows the open array assignment code to be reused
 *)

PROCEDURE EmitCheck (tlhs: Type.T;  rhs: Expr.T);
(* emit code to evaluate "rhs" and generate whatever
   runtime checks would be needed if it were assigned to
   a value of type 'tlhs'.  The new value is left on the stack.
   Note that Emit assumes that TypeOf(rhs) is assignable to tlhs
   and that Expr.Prep(rhs) has been called.  'tlhs' may not be
   an open array type.  *)

PROCEDURE NeedsClosureCheck (proc: Expr.T;  errors: BOOLEAN): BOOLEAN;
(* returns TRUE if proc is a procedure and might be a closure *)

END AssignStmt.
