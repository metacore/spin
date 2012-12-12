(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Print priority of strands.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	XXX Commented out Regs.  Needs to be moved to a machine-dependent file.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread and ThreadExtra.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 * This module is UNSAFE because it imports M3toC.
 *)

UNSAFE MODULE Textify;
IMPORT Thread, Debugger, Fmt, ThreadRep, ThreadPrivate, CPU,
       SpinException, RTType, M3toC, Ctypes, Text;

PROCEDURE Htmlize (t: TEXT; in: TEXT; html: BOOLEAN := TRUE) : TEXT =
  BEGIN
    IF html THEN
      RETURN "<" & in & ">" & t & "</" & in & ">";
    ELSE
      RETURN t;
    END;
  END Htmlize;

PROCEDURE ThreadName (READONLY t: Thread.T; html: BOOLEAN := FALSE): TEXT =
  VAR 
    text: ARRAY [0..12] OF TEXT;
    res: TEXT;
  BEGIN
    text[0] := " pc=0x";
    text[1] := Fmt.Unsigned(t.ms.pc);
    text[2] := " sp=0x";
    text[3] := Fmt.Unsigned(t.ms.ksp);
    text[4] := " [";
    text[5] := Fmt.Int(t.intid) & " " & Fmt.Int(t.pri);
    text[6] := ThreadPrivate.StateName[t.state];
    IF t.state = ThreadPrivate.State.ExceptionPosted THEN
      text[7] := " --";
      text[8] := SpinException.ExceptionNames[t.exception.code];
      IF t.exception.msg # NIL THEN
        text[9] := "<";
        text[10] := t.exception.msg;
        text[11] := ">";
      END;
    END;
    text[12] := "]\n";
    res := Text.CatMultiple(text);
    RETURN Htmlize(Debugger.GetThreadName(t), ITALIC, html)
             & Htmlize(res, TYPESCRIPT, html);
  END ThreadName;


PROCEDURE Regs(READONLY ss: CPU.GeneralRegs; html: BOOLEAN := FALSE) : TEXT =
  VAR t: TEXT;
  BEGIN
    t := CPU.TextifyRegs(ss);
    RETURN Htmlize(t, TYPESCRIPT, html);
  END Regs;

PROCEDURE Ref (READONLY ref: REFANY; html: BOOLEAN := FALSE): TEXT =
  BEGIN
    RETURN
      Htmlize("0x" & Fmt.Unsigned(VIEW(ref, INTEGER)), TYPESCRIPT, html);
  END Ref;

PROCEDURE Address (READONLY addr: ADDRESS; html: BOOLEAN := FALSE): TEXT =
  BEGIN
    RETURN
      Htmlize("0x" & Fmt.Unsigned(VIEW(addr, INTEGER)), TYPESCRIPT, html);
  END Address;

PROCEDURE Type (READONLY ref: REFANY; <*UNUSED*> html: BOOLEAN := FALSE):
  TEXT =
  BEGIN
    IF ref # NIL THEN
      RETURN
        M3toC.CopyStoT(
          LOOPHOLE(RTType.Get(TYPECODE(ref)).name, Ctypes.char_star));
    ELSE
      RETURN "NIL";
    END;
  END Type;


BEGIN
END Textify.

