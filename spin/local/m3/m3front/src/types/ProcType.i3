(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ProcType.i3                                           *)
(* Last Modified On Tue Dec 20 14:39:09 PST 1994 By kalsow     *)
(*      Modified On Wed Aug 29 02:55:06 1990 By muller         *)

(*
 * HISTORY
 * 27-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for inline
 *
 * 14-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for CVAR
 *        added isExternal argument to ParseSignature
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added RealNFormals for generation of runtime type information 
 *	for procedures.  NFormals cannot be used because it includes
 *	large results in the set of formals.
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	support for bounded
 *
 *)

INTERFACE ProcType;

IMPORT M3, M3ID, CG, Type, Value, CallExpr;

PROCEDURE Parse          (): Type.T;

PROCEDURE ParseSignature (name: M3ID.T;  cc: CG.CallingConvention;
                          isBounded: BOOLEAN := FALSE;
                          isFunctional: BOOLEAN := FALSE;
                          isExternal: BOOLEAN := FALSE;
                          isInline: BOOLEAN := FALSE): Type.T;

PROCEDURE MethodSigAsProcSig (sig, objType: Type.T): Type.T;

PROCEDURE Is        (t: Type.T): BOOLEAN;
PROCEDURE NFormals  (t: Type.T): INTEGER;
PROCEDURE RealNFormals  (t: Type.T): INTEGER;
PROCEDURE Formals   (t: Type.T): Value.T (*list*);
PROCEDURE Result    (t: Type.T): Type.T;
PROCEDURE CGResult  (t: Type.T): CG.Type;
PROCEDURE Raises    (t: Type.T): M3.ExSet;
PROCEDURE Methods   (t: Type.T): CallExpr.MethodList;
PROCEDURE CallConv  (t: Type.T): CG.CallingConvention;

PROCEDURE LargeResult (t: Type.T): BOOLEAN;
PROCEDURE IsCompatible (procSig, objectType, methodSig: Type.T): BOOLEAN;

PROCEDURE New (result: Type.T;  f0, f1, f2, f3: Value.T := NIL;
               isBounded: BOOLEAN := FALSE;
               isFunctional: BOOLEAN := FALSE): Type.T;

PROCEDURE SetMethods (t: Type.T;  m: CallExpr.MethodList);

(* SPIN *)
PROCEDURE DoesCallInline (t: Type.T);
PROCEDURE IsActuallyInline (t: Type.T);

PROCEDURE IsBounded (t: Type.T)   : BOOLEAN;
PROCEDURE IsFunctional (t: Type.T): BOOLEAN;
PROCEDURE IsInline (t: Type.T)    : BOOLEAN;
PROCEDURE CallsInline (t: Type.T) : BOOLEAN;

END ProcType.
