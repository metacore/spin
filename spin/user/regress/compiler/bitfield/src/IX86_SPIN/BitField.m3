(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 29-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Turnned into a regression test and added more tests.
 *
 * 23-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. 
 *)
MODULE BitField;
IMPORT Fmt, IO;

(*
 * Exercise the bitfield bug where -1 is incorrectly sign
 * extended to 65535.
 *)
PROCEDURE BitField(): BOOLEAN =
  VAR
    bit16: [-2..10000];
    bit32: [-2..1000000];
(*
    bit64: [-2..429496729500];
    bit60: [0 .. 16_FFFFFFFFFFFFFFF];
    bit60b: BITS 60 FOR [0 .. 16_FFFFFFFFFFFFFFF];
*)
  BEGIN
    bit16 := -1;
    bit32 := -1;
(*
    bit64 := -1;
    bit60 := 16_FFFFFFFFFFFFFFF;
    bit60b := 16_FFFFFFFFFFFFFFF;
*)
    IO.Put("16 bit promotion (should see -1 here)-> " & 
      Fmt.Int(bit16) & "\n");
    IO.Put("32 bit promotion (should see -1 here)-> " & 
      Fmt.Int(bit32) & "\n");
(*
    IO.Put("64 bit promotion (should see -1 here)-> " & 
      Fmt.Int(bit64) & "\n");
    IO.Put("60 bit promotion (should see 16_FFFFFFFFFFFFFFF here)-> " & 
      Fmt.Int(bit60) & " " & Fmt.Unsigned(bit60) & "\n");
    IO.Put("60 bit promotion (should see 16_FFFFFFFFFFFFFFF here)-> " & 
      Fmt.Int(bit60b) & " " & Fmt.Unsigned(bit60b) & "\n");
*)

    IF bit16 # -1 THEN RETURN FALSE; END;
    IF bit32 # -1 THEN RETURN FALSE; END;
(*
    IF bit64 # -1 THEN RETURN FALSE; END;
    IF bit60 # 16_FFFFFFFFFFFFFFF THEN RETURN FALSE; END;
    IF bit60b # 16_FFFFFFFFFFFFFFF THEN RETURN FALSE; END;
*)

    RETURN TRUE;
  END BitField;

TYPE
  Cmp = [-1 .. +1]; (* comparison: <, =, > *)

PROCEDURE TestSign1(): Cmp =
  BEGIN
    RETURN -1;
  END TestSign1;

PROCEDURE Sign(): BOOLEAN =
  VAR
    cmp: Cmp;
  BEGIN
    cmp := TestSign1();
    IO.Put("value equals: "& Fmt.Int(cmp) & "\n");
    IO.Put("value equals: "& Fmt.Int(-1) & "\n");
    RETURN cmp = -1;
  END Sign;

PROCEDURE Start(<* UNUSED *>i: INTEGER): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Start;

PROCEDURE End(): BOOLEAN =
  BEGIN
    RETURN TRUE;
  END End;

BEGIN
END BitField.

