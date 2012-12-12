(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: EReel.m3                                              *)
(* Last Modified On Wed Sep  7 15:35:49 PDT 1994 by kalsow     *)
(*      Modified On Wed Sep 26 18:57:30 1990 by muller         *)

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

MODULE EReel;

IMPORT M3, Type, Target, Tipe, TypeRep, CG, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "EReel.T" OBJECT
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
    p.info.size      := Target.Extended.size;
    p.info.min_size  := Target.Extended.size;
    p.info.alignment := Target.Extended.align;
    p.info.cg_type   := CG.Type.XReel;
    p.info.class     := Type.Class.Extended;
    p.info.isTraced  := FALSE;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := 3;
  END Check;

PROCEDURE Compiler (<*UNUSED*> p: P) =
  BEGIN
  END Compiler;

PROCEDURE InitCoster (<*UNUSED*> t: Type.T; zeroed: BOOLEAN): INTEGER =
  BEGIN
    (* not every bit pattern is a legal EXTENDED *)
    IF zeroed OR Target.All_floats_legal THEN RETURN 0 ELSE RETURN 1 END;
  END InitCoster;

PROCEDURE FPrinter (<*UNUSED*>t: Type.T;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag := "$extended";
    x.n_nodes := 0;
  END FPrinter;

PROCEDURE GenMap (<*UNUSED*> p: P; offset, size: INTEGER; refs_only: BOOLEAN) =
  BEGIN
    <*ASSERT size = Target.Extended.size*>
    IF NOT refs_only THEN
      TipeMap.Add (offset, TipeMap.Op.Extended, 0);
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    EVAL TipeDesc.AddO (TipeDesc.Op.Extended, p);
  END GenDesc;

PROCEDURE ToText (<* UNUSED *> p: P): TEXT =
  BEGIN
    RETURN ("EXTENDED");
  END ToText;

PROCEDURE Initialize () =
  BEGIN
    T := NEW (P);
    TypeRep.Init (T, Type.Class.Extended);
    Tipe.Define ("EXTENDED", T, TRUE);
  END Initialize;

BEGIN
END EReel.
