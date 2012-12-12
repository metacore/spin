(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Type.i3                                               *)
(* Last Modified On Fri Feb 24 16:22:20 PST 1995 by kalsow     *)
(*      Modified On Fri Dec 21 00:57:36 1990 by muller         *)

(*
 * HISTORY
 * 09-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	ref-counting code
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 06-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added procedure SetAlign to allow AlignedType to call down and
 *	 set the alignment; hack
 *
 * 03-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added Class.Aligned
 *
 * 15-Nov-95  Wilson Hsieh (whsieh) at the University of Washington
 *	CanView becomes CanViewAs - used to check 2nd arg of VIEW
 *	added new CanView - used to check 1st arg of VIEW
 *
 * 13-Nov-95  Wilson Hsieh (whsieh) at the University of Washington
 *	added CanView for VIEW
 *
 *)

INTERFACE Type;

IMPORT M3, CG, Target;

TYPE
  T          = M3.Type;
  Assumption = M3.EqAssumption;
  ModuleInfo <: REFANY;

TYPE
  Class = { Error, Named, Integer, Real, Longreal, Extended,
            Array, Enum, Object, Opaque, OpenArray, Packed,
            Procedure, Record, Ref, Set, Subrange, Aligned };

VAR
  recursionDepth: INTEGER := 0;
  (* incremented(decremented) every time the type checker enters(leaves)
     one of the types that is allowed to introduce recursions.
     (ie. REF, OBJECT, PROC) *)

TYPE
  Info = RECORD
    size      : INTEGER;  (* size in bits, -1 if variable sized *)
    min_size  : INTEGER;  (* minimum size in bits. *)
    alignment : INTEGER;  (* minimum alignment in bits *)
    hash      : INTEGER;  (* internal hash code *)
    cg_type   : CG.Type;  (* code generator representation *)
    class     : Class;
    isTraced  : M3.Flag;  (* TRUE if type must not be UNTRACED to be safe
                             in other words, it contains a REF
                           *)
    isEmpty   : M3.Flag;
    isSolid   : M3.Flag;
  END;

(*** phase 0 ***)

PROCEDURE Initialize ();
PROCEDURE Reset ();
(* initializes the module and all other type modules. *)

(*** phase 1 ***)

PROCEDURE Parse (): T;
(* parse a type expression *)

PROCEDURE SetModule (x: ModuleInfo): ModuleInfo;
(* sets the current module's type info to 'x' and returns
   the previous module's type info.  This routine is only called
   when the "current" module changes. *)

PROCEDURE ToText (t: T): TEXT;
(* returns the textual name of a type *)

(*** phase 2 ***)

PROCEDURE Check (t: T): T;
(* type check type 't'.   Return the underlying constructed
   (ie. class # Class.Named) type node. *)

PROCEDURE CheckInfo (t: T;  VAR(*OUT*) x: Info): T;
(* type check type 't'.  Return the underlying constructed
   (ie. class # Class.Named) type node and in 'x' its info. *)

PROCEDURE IsAlignedOk (t: T;  offs: INTEGER): BOOLEAN;
(* Returns TRUE iff no scalars within a value of type 't' at a bit offset
   of 'offs' cross word boundaries.  *)

PROCEDURE Strip (t: T): T;
(* return the constructed type of 't' (ie. strip renaming) *)

PROCEDURE Base (t: T): T;
(* return the base type of 't' (strip renaming, packing & subranges) *)

PROCEDURE CGType (t: T): CG.Type;
(* returns the code generator's stack representation for 't'  *)

PROCEDURE IsStructured (t: T): BOOLEAN;
(* <=> rec, set, or array <=> is represented as an address on the CG stack *)

PROCEDURE LoadScalar (t: T);
(* If 't' is not a structured type, generate code to load the scalar
   pointed to by the address on the CG stack *)

PROCEDURE RepresentationComplete (t: T) : BOOLEAN;
(* Return TRUE if the type is representation-complete *)

PROCEDURE RepresentationCompleteFor (t: T; b: CARDINAL) : BOOLEAN;
(* Return TRUE if the type is representation-complete for b bits *)

(*
PROCEDURE CanView (t: T; writable: BOOLEAN): BOOLEAN;
(* Return TRUE if the type fulfills the following conditions:
      1.  Is not a floating type
      2.  If the type is a REF, PROCEDURE, subrange, enum, object,
          opaque, set, then writable must be FALSE
      3.  All contained types fulfill these conditions
 *)

PROCEDURE CanViewAs (t: T): BOOLEAN;
(* Return TRUE if the type fulfills the following conditions:
      1.  Is not a REF
      2.  Is not opaque
      3.  Is not an open array
      4.  Is not a floating type, PROCEDURE, SET, OBJECT, enum,
          or subrange
      5.  All bit patterns are legal (no subranges)
      6.  All contained types fulfill these conditions, with the
            exception of PACKED types, which can contain an enum
            or subrange
 *)
*)

PROCEDURE SetAlign (t: T; align: INTEGER);
(* Set the alignment of type T to align *)

(*** phase 3 ***)

PROCEDURE BeginSetGlobals ();
(* Prepares the types of the current module for calls to SetGlobals *)

PROCEDURE SetGlobals (origin: INTEGER);
(* assign offsets to any needed global data for any types in the
   current module that occured lexically at or before 'origin'. *)

PROCEDURE IsOrdinal (t: T): BOOLEAN;
(* return TRUE if the type is an ordinal (Integer, Enum, Subrange) *)

PROCEDURE Number (t: T): Target.Int;
(* return the number of values of the type;  -1 if t is not an ordinal type *)

PROCEDURE GetBounds (t: T;  VAR min, max: Target.Int): BOOLEAN;
(* return the bounds and true for ordinal types, 
   [0,-1] and FALSE for non-ordinal types *)

PROCEDURE IsEqual (a, b: T;  x: Assumption): BOOLEAN;
(* TRUE iff (a == b) given the assumptions x *)

PROCEDURE IsEquiv (a, b: T;  x: Assumption; canTruncate: BOOLEAN): BOOLEAN;
(* TRUE iff a and b are the same, given the assumptions x
     ignores record field labels when comparing records
     treats representation-complete fields as the same
   if canTruncate is true, allows a to be "larger" than b
 *)

PROCEDURE IsSubtype (a, b: T): BOOLEAN;
(* TRUE iff (a <: b) *)

PROCEDURE IsAssignable (a, b: T): BOOLEAN;
(* TRUE iff (a := b) typechecks *)

PROCEDURE Name (t: T): TEXT;
PROCEDURE GlobalUID (t: T): INTEGER;
(* return the unique id for the type in the generated code *)

(*** phase 4 ***)

PROCEDURE CompileAll ();
(* compile all the local types for the current module *)

PROCEDURE Compile (t: T);
(* generates the debugging declarations for 't' *)

PROCEDURE LoadInfo (t: T;  offset: INTEGER;  addr := FALSE);
(* loads the specified field of 't's typecell.  If 'offset' is less than
   zero, 'LoadInfo' loads the address of the typecell.  *)

PROCEDURE InitValue (t: T;  zeroed: BOOLEAN);
(* initialize the variable addressed by s0.A to an arbitrary value of type 't'.
   If 'zeroed' the variable is assumed to already have all bits set to zero. *)

PROCEDURE Zero (t: T);
(* initialize the variable of type 't' addressed by s0.A to zeros. *)

PROCEDURE InitCost (t: T;  ifZeroed: BOOLEAN): INTEGER;
(* the cost of initializing a 't'.  (0 IFF no init required) *)

PROCEDURE GenMap (t: T;  offset, size: INTEGER;  refs_only: BOOLEAN);
(* emits the type map for type 't' occupying 'size' bits at 'offset'. *)

PROCEDURE GenDesc (t: T);
(* generate the runtime description for type 't' *)

PROCEDURE GenTag (t: T;  tag: TEXT;  offset: INTEGER);
(* generate a comment with 'tag' and 't's name *)

PROCEDURE GenRC (t: T; definitelyGlobal: BOOLEAN; noOverlap := FALSE);
(* generate code to reference count an assignment of type T
   messy interface:
   for a non-open-array type, the stack has 2 elements,
     otherwise 4
   CG stack has rhs, then lhs underneath
     for ref types, the rhs is the actual value
     otherwise, the rhs is address of the designator
   for an open array type, the top of stack has
     rhs, lhs, rhs array header, lhs array header
   GenRC pops all of these things off the CG stack
 *)

(*** phase 5 ***)

PROCEDURE GenCells (): INTEGER;
(* generate the current module's linked list of typecells
   and return its offset in the module global data. *)

PROCEDURE GenCellPtrs (): INTEGER;
(* generate the current module's linked list of pointers to typecells
   and return its offset in the module global data. *)

END Type.

(*
  The following sets of procedures are may be called during the
  various phases of the compilation:

   initialization:
     { Initialize* }
   parsing:
     { Parse, NoteDeclaration }
   type checking:
     { Check, Number, GetBounds, Base, IsEqual,
       IsSubtype, IsAssignable }
   code generation:
     { Name, Number, GetBounds, Base, IsEqual,
       IsSubtype, IsAssignable }

   ( * => may only be called once )
*)
