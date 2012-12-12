(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Int.m3                                                *)
(* Last Modified On Wed Sep  7 15:35:03 PDT 1994 by kalsow     *)
(*      Modified On Fri Dec 21 01:48:33 1990 by muller         *)

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

MODULE Int;

IMPORT M3, Type, Target, Tipe, TypeRep, CG, TipeMap, TipeDesc;

TYPE
  P = Type.T BRANDED "Int.T" OBJECT
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
    p.info.size      := Target.Integer.size;
    p.info.min_size  := Target.Integer.size;
    p.info.alignment := Target.Integer.align;
    p.info.cg_type   := CG.Type.Int;
    p.info.class     := Type.Class.Integer;
    p.info.isTraced  := FALSE;
    p.info.isEmpty   := FALSE;
    p.info.isSolid   := TRUE;
    p.info.hash      := 5;
  END Check;

PROCEDURE Compiler (<*UNUSED*> p: P) =
  BEGIN
  END Compiler;

PROCEDURE InitCoster (<*UNUSED*> t: Type.T; 
                      <*UNUSED*> zeroed: BOOLEAN): INTEGER =
  BEGIN
    RETURN 0;
  END InitCoster;

PROCEDURE FPrinter (<*UNUSED*> t: Type.T;  VAR x: M3.FPInfo) =
  BEGIN
    x.tag := "$integer";
    x.n_nodes := 0;
  END FPrinter;

PROCEDURE GenMap (<*UNUSED*> p: P; offset, size: INTEGER; refs_only: BOOLEAN) =
  VAR IntBytes := Target.Integer.bytes;
  BEGIN
    <*ASSERT size = Target.Integer.size*>
    IF    (refs_only)    THEN (* skip *)
    ELSIF (IntBytes = 4) THEN TipeMap.Add (offset, TipeMap.Op.Int_4, 0);
    ELSIF (IntBytes = 8) THEN TipeMap.Add (offset, TipeMap.Op.Int_8, 0);
    ELSIF (IntBytes = 2) THEN TipeMap.Add (offset, TipeMap.Op.Int_2, 0);
    ELSIF (IntBytes = 1) THEN TipeMap.Add (offset, TipeMap.Op.Int_1, 0);
    ELSE  <*ASSERT FALSE *>
    END;
  END GenMap;

PROCEDURE GenDesc (p: P) =
  BEGIN
    EVAL TipeDesc.AddO (TipeDesc.Op.Integer, p);
  END GenDesc;

PROCEDURE ToText (<* UNUSED *> p: P): TEXT =
  BEGIN
    RETURN ("INTEGER");
  END ToText;

PROCEDURE Initialize () =
  BEGIN
    T := NEW (P);
    TypeRep.Init (T, Type.Class.Integer);
    Tipe.Define ("INTEGER", T, TRUE);
  END Initialize;

BEGIN
END Int.
