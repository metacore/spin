(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: TypeRep.i3                                            *)
(* Last Modified On Wed Nov 16 14:38:18 PST 1994 by kalsow     *)
(*      Modified On Fri Dec 21 00:53:14 1990 by muller         *)

(*
 * HISTORY
 * 01-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added ref counting code
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 16-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	cosmetic change
 *
 * 06-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added set_align method, which defaults to Noop
 *
 * 04-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added to_text method to Type, which generates a textual
 *	 representation
 *	added NullText method, which is the default
 *
 *)

INTERFACE TypeRep;

IMPORT M3, M3FP, Type;

CONST
  NO_UID   = -1;
  NO_SCC   = 0;

TYPE
  TT = M3.Type;

REVEAL
  M3.Type = M3.Node BRANDED "Type.T" OBJECT
    fp          : M3FP.T;
    info        : Type.Info;
    uid         : INTEGER;
    scc_id      : INTEGER;
    rep_id      : INTEGER;
    checkDepth  : BITS 12 FOR [-2048..2047];
    checked     : M3.Flag;
    errored     : M3.Flag;
    next        : TT;   (* linked list of all types in the same module *)
  METHODS
    check       ();
    check_align (offset: INTEGER): BOOLEAN;
    set_align   (new_align : INTEGER) := Noop;
    isEqual     (t: TT;  x: Type.Assumption): BOOLEAN;
    isSubtype   (t: TT): BOOLEAN := NoSubtypes;
    compile     ();
    initCost    (zeroed: BOOLEAN): INTEGER;
    initValue   (zeroed: BOOLEAN);
    mapper      (offset, size: INTEGER; refs: BOOLEAN);
    gen_desc    ();
    fprint      (VAR x: M3.FPInfo);
    (* SPIN *)
    to_text     (): TEXT := NullText;
    isEquiv     (t: TT;  x: Type.Assumption; inArray: BOOLEAN): BOOLEAN;
    genRC       (definitelyGlobal: BOOLEAN; noOverlap := FALSE) := RCDefault;
  END;

PROCEDURE Init (t: TT;  c: Type.Class);
(* initialize the shared fields of a Type.T *)

PROCEDURE NeverEqual (a, b: TT;  x: Type.Assumption): BOOLEAN;
(* == RETURN FALSE *)

PROCEDURE NeverEquiv (a, b: TT; x: Type.Assumption; canTruncate: BOOLEAN)
  : BOOLEAN;
(* == RETURN FALSE *)

PROCEDURE NoSubtypes (a, b: TT): BOOLEAN;
(* == RETURN FALSE *)

PROCEDURE InitToZeros (t: TT;  zeroed: BOOLEAN);
(* == initialize Size(t) bits to zero *)

PROCEDURE GenRefMap (t: TT;  offset, size: INTEGER;  refs_only: BOOLEAN);
(* == TypeMap.Add (offset, Op.{Untraced}Ref, 0) *)

PROCEDURE GenRefDesc (t: TT);
(* == TypeDesc.AddO (Op.{Untraced}Ref); TypeDesc.AddI (UID(t)) *)

PROCEDURE ScalarAlign (t: TT;  offset: INTEGER): BOOLEAN;
(* == RETURN (t.alignment MOD offset = 0) *)

PROCEDURE NullText (t: TT): TEXT;
(* == RETURN ("") *)

PROCEDURE Noop (t: TT; new_align: INTEGER);
(* noop *)

PROCEDURE RCDefault (t: TT; definitelyGlobal: BOOLEAN; noOverlap := FALSE);
(* copy the type *)

END TypeRep.
