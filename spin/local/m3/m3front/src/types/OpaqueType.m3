(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: OpaqueType.m3                                         *)
(* Last modified on Wed Sep  7 15:32:48 PDT 1994 by kalsow     *)
(*      modified on Sun Feb 24 05:41:53 1991 by muller         *)

(*
 * HISTORY
 * 01-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added ref counting code
 *
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added ToText, which overrides the to_text inherited 
 *	method
 *      added name argument to New, and name field to P
 *
 *)

MODULE OpaqueType;

IMPORT M3, M3ID, CG, Type, TypeRep, Target, Reff, Error, Mutex;
IMPORT Revelation, Scope, M3Buf, Textt, Value, Host;

IMPORT RefType;

TYPE
  P = Type.T OBJECT
        declared   : Value.T;
	super      : Type.T;
        id         : M3ID.T;
        isTraced   : BOOLEAN;
	isBuiltin  : BOOLEAN;
        name       : TEXT;
      OVERRIDES
        check      := Check;
        check_align:= TypeRep.ScalarAlign;
        isEqual    := EqualChk;
        isEquiv    := EqualChkAlias;
        isSubtype  := Subtyper;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := TypeRep.GenRefMap;
        gen_desc   := TypeRep.GenRefDesc;
        fprint     := FPrinter;
        to_text    := ToText;
        genRC      := RefType.GenRC;
      END;

PROCEDURE New (super: Type.T;  decl: Value.T;  builtin: BOOLEAN := FALSE;
               name: TEXT := NIL): Type.T =
  VAR p: P;
  BEGIN
     p := NEW (P);
    TypeRep.Init (p, Type.Class.Opaque);
    p.super    := super;
    p.declared := decl;
    p.isBuiltin := builtin;
    p.id       := M3ID.NoID;
    p.name     := name;
    RETURN p;
  END New;

PROCEDURE Reduce (t: Type.T): P =
  BEGIN
    IF (t = NIL) THEN RETURN NIL END;
    IF (t.info.class = Type.Class.Named) THEN t := Type.Strip (t) END;
    IF (t.info.class # Type.Class.Opaque) THEN RETURN NIL END;
    RETURN t;
  END Reduce;

PROCEDURE Is (t: Type.T): BOOLEAN =
  BEGIN
    RETURN (Reduce (t) # NIL);
  END Is;

PROCEDURE Super (t: Type.T): Type.T =
  VAR p := Reduce (t);
  BEGIN
    IF (p # NIL)
      THEN RETURN p.super;
      ELSE RETURN t;
    END;
  END Super;

PROCEDURE UID (t: Type.T): INTEGER =
  VAR p := Reduce (t);
  BEGIN
    IF (p = NIL) THEN RETURN M3ID.NoID END;
    IF (p.id = M3ID.NoID) THEN
      p.id := M3ID.Add (Value.GlobalName (p.declared, dots := TRUE,
                                          with_module := TRUE));
    END;
    RETURN p.id;
  END UID;

PROCEDURE Check (p: P) =
  VAR info: Type.Info;
  BEGIN
    p.super := Type.CheckInfo (p.super, info);
    p.isTraced := info.isTraced;

    IF    (info.class # Type.Class.Opaque)
      AND (info.class # Type.Class.Ref)
      AND (info.class # Type.Class.Object) THEN
      Error.Msg ("opaque super type must be a reference type");
      p.super := Reff.T;
    END;

    p.info.size      := Target.Address.size;
    p.info.min_size  := Target.Address.size;
    p.info.alignment := Target.Address.align;
    p.info.cg_type   := CG.Type.Addr;
    p.info.class     := Type.Class.Opaque;
    p.info.isTraced  := info.isTraced;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := -p.id; (* all opaque types are unique *)
  END Check;

PROCEDURE Compiler (p: P) =
  VAR self := Type.GlobalUID (p);  super := Type.GlobalUID (p.super);
  BEGIN
    IF NOT p.isBuiltin THEN
        CG.Declare_opaque (self, super);
        Host.env.note_opaque (self, super);
    END;
  END Compiler;

PROCEDURE EqualChkAlias (a: P; t: Type.T; x: Type.Assumption;
                         <*UNUSED*> canTruncate: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN EqualChk (a, t, x);
  END EqualChkAlias;

PROCEDURE EqualChk (<*UNUSED*> a: P;  <*UNUSED*> b: Type.T;
                    <*UNUSED*> x: Type.Assumption): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END EqualChk;

PROCEDURE IsSubtype (a, b: Type.T): BOOLEAN =
  (* called if the normal subtype methods didn't prove a <: b. *)
  VAR p := Reduce (b);  t: Type.T;
  BEGIN
    IF (p = NIL) THEN RETURN FALSE END;
    t := Revelation.LookUp (p);
    IF (t = NIL) THEN RETURN FALSE END;
    t := Type.Check (t);
    RETURN Type.IsSubtype (a, t);
  END IsSubtype;

PROCEDURE Subtyper (a: P;  b: Type.T): BOOLEAN =
  VAR t: Type.T;  x: Revelation.TypeSet;
  BEGIN
    (* try a's declared super type *)
    IF Type.IsSubtype (a.super, b) THEN RETURN TRUE END;

    (***********************************************
    (* try for a full revelation *)
    t := Revelation.LookUp (a);
    IF (t # NIL) THEN
      t := Type.Check (t);
      RETURN Type.IsSubtype (t, b);
    END;
    *************************************************)

    (* finally, try all the visible revelations *)
    Revelation.LookUpAll (a, x);
    FOR i := 0 TO x.cnt-1 DO
      t := Type.Check (x.types[i]);
      IF Type.IsSubtype (t, b) THEN RETURN TRUE END;
    END;
    WHILE (x.others # NIL) DO
      t := Type.Check (x.others.type);
      IF Type.IsSubtype (t, b) THEN RETURN TRUE END;
      x.others := x.others.next;
    END;

    RETURN FALSE;
  END Subtyper;

PROCEDURE InitCoster (p: P; zeroed: BOOLEAN): INTEGER =
  BEGIN
    IF (p.isTraced) AND (NOT zeroed) THEN RETURN 1 ELSE RETURN 0 END;
  END InitCoster;

PROCEDURE FPrinter (p: P;  VAR x: M3.FPInfo) =
  VAR s: Scope.IDStack;
  BEGIN
    IF Type.IsEqual (p, Textt.T, NIL) THEN
      x.tag := "$text";
      x.n_nodes := 0;
    ELSIF Type.IsEqual (p, Mutex.T, NIL) THEN
      x.tag := "$mutex";
      x.n_nodes := 0;
    ELSE
      M3Buf.PutText (x.buf, "OPAQUE ");
      s.top := 0;
      Scope.NameToPrefix (p.declared, s, FALSE, TRUE);
      Scope.PutStack (x.buf, s);
      x.n_nodes := 1;
      x.nodes[0] := p.super;
    END;
  END FPrinter;

PROCEDURE ToText (p: P): TEXT =
  BEGIN
    IF (p.name # NIL) THEN
      RETURN (p.name);
    ELSE
      RETURN ("OPAQUE(" & p.super.to_text() & ")");
    END;
  END ToText;

BEGIN
END OpaqueType.

