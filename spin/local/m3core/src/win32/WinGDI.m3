(* Copyright (C) 1994, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* by Stephen Harrison                                       *)
(*                                                           *)
(* Last modified on Tue Nov  8 14:41:56 PST 1994 by kalsow   *)
(*      modified on Tue Mar 23 17:31:53 PST 1993 by harrison *)

MODULE WinGDI;

FROM Word IMPORT Shift, Or, Extract;

FROM WinDef IMPORT BYTE, COLORREF, WORD;

PROCEDURE RGB (r, g, b: BYTE): COLORREF =
  BEGIN
    RETURN Or(r, Or(Shift(g, 8), Shift(b, 16)));
  END RGB;

PROCEDURE PALETTERGB (r, g, b: BYTE): COLORREF =
  BEGIN
    RETURN Or(16_02000000, RGB(r, g, b));
  END PALETTERGB;

PROCEDURE PALETTEINDEX (i: WORD): COLORREF =
  BEGIN
    RETURN Or(16_01000000, i);
  END PALETTEINDEX;

PROCEDURE GetRValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 8, 0);
  END GetRValue;

PROCEDURE GetGValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 8, 7);
  END GetGValue;

PROCEDURE GetBValue (rgb: COLORREF): BYTE =
  BEGIN
    RETURN Extract(rgb, 8, 15);
  END GetBValue;

BEGIN
END WinGDI.

