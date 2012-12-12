(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: ErrType.m3                                            *)
(* Last Modified On Wed Sep  7 15:34:54 PDT 1994 by kalsow     *)
(*      Modified On Thu Dec  5 17:24:02 PST 1991 by muller     *)

(*
 * HISTORY
 * 06-Oct-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for VIEW between representation-equivalent types
 *
 * 05-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added ToText, which overrides the to_text inherited 
 *	method
 *
 *)

MODULE ErrType;

IMPORT M3, Type, TypeRep, Tipe, CG;

TYPE
  P = Type.T BRANDED "ErrType.T" OBJECT
      OVERRIDES
        check      := Check;
        check_align:= TypeRep.ScalarAlign;
        isEqual    := TypeRep.NeverEqual;
        isEquiv    := TypeRep.NeverEquiv;
        isSubtype  := TypeRep.NoSubtypes;
        compile    := Compiler;
        initCost   := InitCoster;
        initValue  := TypeRep.InitToZeros;
        mapper     := GenMap;
        gen_desc   := GenDesc;
        fprint     := FPrinter;
        to_text    := ToText;
      END;

PROCEDURE Check (p: P) =
  BEGIN
    p.info.size      := 0;
    p.info.min_size  := 0;
    p.info.alignment := 1;
    p.info.cg_type   := CG.Type.Addr;
    p.info.class     := Type.Class.Error;
    p.info.isTraced  := FALSE;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := 4;
  END Check;

PROCEDURE Compiler (<*UNUSED*> p: P) =
  BEGIN
  END Compiler;

PROCEDURE InitCoster (<*UNUSED*> t: Type.T; 
                      <*UNUSED*> zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN 0;
  END InitCoster;

PROCEDURE FPrinter (<*UNUSED*>t: Type.T;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag := "$ErrType";
    x.n_nodes := 0;
  END FPrinter;

PROCEDURE GenMap (<*UNUSED*> p: P;
                  <*UNUSED*> offset, size: INTEGER;
                  <*UNUSED*> refs_only: BOOLEAN) =
  BEGIN
    (* generate nothing *)
  END GenMap;

PROCEDURE GenDesc (<*UNUSED*> p: P) =
  BEGIN
    (* generate nothing *)
  END GenDesc;

PROCEDURE Initialize () =
  BEGIN
    T := NEW (P);
    TypeRep.Init (T, Type.Class.Error);
    Tipe.Define ("_ERROR", T, FALSE);
  END Initialize;

PROCEDURE ToText (<* UNUSED *> p: P): TEXT =
  BEGIN
    RETURN ("ERROR");
  END ToText;

BEGIN
END ErrType.
