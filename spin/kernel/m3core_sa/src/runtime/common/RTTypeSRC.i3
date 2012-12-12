(*| Copyright (C) 1993, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*| Last modified on Thu Jul 14 11:33:02 PDT 1994 by kalsow     *)
(*|      modified on Tue Apr 27 11:42:41 PDT 1993 by mcjones    *)
(*|      modified on Sun Feb 21 14:27:08 PST 1993 by jdd        *)
(*|      modified on Tue Sep 25 00:38:09 1990 by muller         *)

(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Exported IDMap.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 29-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Added Typeidxname to accomodate the new Typecode/Typeidx dichotomy.
 *)

(*
  "RTTypeSRC" provides SRC Modula-3 extensions to RTType and RTTypeFP.
*)

INTERFACE RTTypeSRC;

IMPORT RT0;

PROCEDURE TypeName (ref: REFANY): TEXT;
(* = TypecodeName (TYPECODE (ref)) *)

PROCEDURE TypecodeName (tc: RT0.Typecode): TEXT;
(* Returns a text describing the ref whose typecode = "tc".  Its
   heuristic is to consider, in order, "named typecells", brands, arrays,
   object-subtypes, objects, and procedures.  If all else fails, it returns
   "<?>". *)

PROCEDURE FindType (id: INTEGER): RT0.TypeDefn;
(* Returns the type descriptor with UID id.  If no such type exists,
   reutrns NIL. *)

PROCEDURE ShowTypes (full := TRUE);

(*------------------------------------------------------------------ setup --*)

PROCEDURE Init (): BOOLEAN;
(* called by the startup code to initialize the runtime type system. *)

PROCEDURE Update ();
(* called by the reinitialization code to update the type ids *)

TYPE
 IDMap = RECORD uid: INTEGER;  defn: RT0.TypeDefn END;

VAR
  (* map from type id to typecode, sorted by type id. *)
  n_type_ids : INTEGER;
  type_ids   : ADDRESS; (* REF ARRAY [0..n_type_ids-1] OF IDMap *)

END RTTypeSRC.
