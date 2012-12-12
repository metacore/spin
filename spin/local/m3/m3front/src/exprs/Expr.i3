(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Expr.i3                                               *)
(* Last Modified On Fri Feb 24 16:23:07 PST 1995 By kalsow     *)

(*
 * HISTORY
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	globalness is more precise
 *
 * 28-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	add IsGlobal
 *
 *)

INTERFACE Expr;

IMPORT M3, M3Buf, CG, Target;

TYPE
  T    = M3.Expr;
  List = REF ARRAY OF T;
  CheckState = M3.CheckState;

(*** phase 1 ***)

PROCEDURE Parse (): T;
(* parses an expression *)

(*** phase 2 ***)

PROCEDURE TypeOf (t: T): M3.Type;
(* returns the type of the expression *)

PROCEDURE TypeCheck (t: T;  VAR cs: CheckState);
(* typechecks the expression. *)

(*** phase 3 ***)

PROCEDURE IsGlobal (t: T) : M3.Global;
(*
  returns Yes if expr does represent a global designator
     that is, either the expr is a global variable or goes through a REF
     or it calls a non-FUNCTIONAL procedure
  returns Maybe if expr could represent a global designator
     that is, is a by-reference parameter
  returns No if expr could not represent a global designator
  
  used for checking FUNCTIONAL (SPIN), and also for reference counting
 *)

PROCEDURE ConstValue (t: T): T;
(* Returns NIL if t is not a constant, otherwise returns a simplified
   expression that denotes t.  Value may be called before the expression
   is typechecked. *)

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int);
(* returns upper and lower bounds for the value of the expression. *)

PROCEDURE IsDesignator (t: T): BOOLEAN;
(* TRUE iff t is a designator *)

PROCEDURE IsWritable (t: T): BOOLEAN;
(* TRUE iff t is a writable designator *)

PROCEDURE IsZeroes (t: T): BOOLEAN;
(* TRUE if t's binary representation is all zeroes *)

PROCEDURE GetSign (t: T): CG.Sign;
(* returns the best guess of t's sign *)

PROCEDURE NeedsAddress (t: T);
(* marks t as needing an L-value (ie. a memory address). *)

(*** phase 4 ****)

(* Expressions are compiled in two steps:
     Prep: emit any code that includes branchs or stores
     Compile: emit the rest of the code
*)

PROCEDURE Prep (t: T);
PROCEDURE Compile (t: T);
(* emits code to evaluate the expression onto the top of stack *)

PROCEDURE PrepLValue (t: T);
PROCEDURE CompileLValue (t: T);
(* emits code to evaluate 't's L-value into s0.A. *)

PROCEDURE CompileAddress (t: T);
(* emits code to evaluate 't's byte address onto the top of stack.
   Use PrepLValue to prep these expressions. *)

PROCEDURE PrepBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency);
PROCEDURE CompileBranch (t: T;  true, false: CG.Label;  freq: CG.Frequency);
(* emits code to evaluate the expression and conditionally branch to 'true'
   or 'false' depending on its boolean value.  'freq' is the estimated
   frequency that the specified branch will be taken. *)

PROCEDURE NoteWrite (t: T);
(* generates any tracing implied by a write to 't' *)


PROCEDURE IsEqual (a, b: T;  x: M3.EqAssumption): BOOLEAN;
(* TRUE iff (value(a) = value(b)), assuming a constant global store
   and the type equalities represented by 'x'.  *)

PROCEDURE PrepLiteral (t: T;  type: M3.Type);
(* prepares constant values for GenLiteral *)

PROCEDURE GenLiteral (t: T;  offset: INTEGER;  type: M3.Type);
(* initializes the global storage at ADR(x)+offset to the
   constant value t.  For any expression t, PrepLiteral(t) must
   be called before GenLiteral (t).  *)

PROCEDURE GenFPLiteral (t: T;  mbuf: M3Buf.T);
(* add the string denoting the literal value of 't' to 'mbuf' *)

PROCEDURE BadOperands (op: TEXT;  a, b: M3.Type := NIL): M3.Type;
(* generate an "illegal operands" error message if neither 'a' nor 'b'
   is the contagious error type and return the error type *)

END Expr.
