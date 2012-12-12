(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE PFM;
IMPORT Word;
IMPORT PFMExtern;
IMPORT RTIO, Fmt;
IMPORT ProfileSupport;

CONST
  (* They are ioctl constants. *)
  PCNTRENABLE = 16_20005001;
  PCNTRDISABLE = 16_20005000;
  PCNTSETMUX = 16_80085002;
  PCNTSETITEMS = 16_8008501f;
  PCNTGETCNT = 16_40085006;
  (* PCNTGETIPLHIS = 16_4008500b; *)
  PCNTCLEARCNT = 16_20005005;
  PCNTLOGALL = 16_20005003;
  PCNTGETRSIZE = 16_4008500a;
  PCNTSETKADDR = 16_80105021;
  PCNTSETUADDR = 16_80105020;

PROCEDURE Open (mode: Word.T) =
  BEGIN
    EVAL PFMExtern.Open(0, mode);
    EVAL PFMExtern.Ioctl(0, PCNTLOGALL, 0, 0);
  END Open;
  
PROCEDURE Close () =
  BEGIN
    PFMExtern.Close();
  END Close;

PROCEDURE Ioctl (name: TEXT; a, b: Word.T) =
  VAR errno: INTEGER;
  BEGIN
    errno := PFMExtern.Ioctl(0, a, b, 0);
    IF errno # 0 THEN
      RTIO.PutText("PFM." & name & ": " & Fmt.Int(errno) & ".\n");
    END;
  END Ioctl;
  
PROCEDURE Enable () =
  BEGIN
    Ioctl("Enable", PCNTLOGALL, 0);
    active := TRUE;
    Ioctl("Enable", PCNTRENABLE, 0);
  END Enable;
  
PROCEDURE Disable () =
  BEGIN
    Ioctl("Disable", PCNTRDISABLE, 0);
    ProfileSupport.StoreSamples();
  END Disable;

PROCEDURE GetCount (VAR c: Counter) =
  VAR
    a := ADR(c);
  BEGIN
    Ioctl("GetCount", PCNTGETCNT, LOOPHOLE(ADR(a), Word.T));
  END GetCount;

PROCEDURE GetBufferSize (): CARDINAL =
  VAR x: INTEGER;
  BEGIN
    Ioctl("GetBufferSize", PCNTGETRSIZE, LOOPHOLE(ADR(x), Word.T));
    RETURN x;
  END GetBufferSize;

PROCEDURE Read (VAR p: ARRAY OF PCount): CARDINAL =
  BEGIN
    RETURN PFMExtern.Read(ADR(p[0]), NUMBER(p));
  END Read;
  
PROCEDURE ClearCount () =
  BEGIN
    Ioctl("ClearCount", PCNTCLEARCNT, 0);
  END ClearCount;

PROCEDURE SetMux (VAR c: Iccsr) =
  BEGIN
    Ioctl("SetMux", PCNTSETMUX, LOOPHOLE(ADR(c), Word.T));
  END SetMux;

PROCEDURE SetItems (mode: INTEGER) =
  BEGIN
    Ioctl("SetItems", PCNTSETITEMS, LOOPHOLE(ADR(mode), Word.T));
  END SetItems;
  
PROCEDURE SetKaddr (READONLY r: AddrRange) =
  BEGIN
    Ioctl("SetKaddr", PCNTSETKADDR, LOOPHOLE(ADR(r), Word.T));
  END SetKaddr;

PROCEDURE SetUaddr (READONLY r: AddrRange) =
  BEGIN
    Ioctl("SetUaddr", PCNTSETUADDR, LOOPHOLE(ADR(r), Word.T));
  END SetUaddr;
  
BEGIN
END PFM.

