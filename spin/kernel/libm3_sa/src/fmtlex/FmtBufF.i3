(* Copyright (C) 1994, Digital Equipment Corporation                   *)
(* All rights reserved.                                                *)
(* See the file COPYRIGHT for a full description.                      *)
(*                                                                     *)
(* Last modified on Wed Mar 16 12:32:45 PST 1994 by heydon             *)

INTERFACE FmtBufF;

(* A "friends" interface for the "FmtBuf" interface that reveals
   internal types and procedures. *)

IMPORT Fmt, FmtBuf;

CONST

TYPE
  NumAttr = RECORD
    class: Class;
    kind: IEEEKind;
    sign: [0..1];
    maxExpDigits: CARDINAL;
    len: CARDINAL;
    exp: INTEGER;
    errorSign: [-1..1];
  END;
  Digits = ARRAY OF [0..9];

TYPE
  FmtRec = RECORD
    prec: CARDINAL;
    literal: BOOLEAN;
  END;

(* A "FmtRec" bundles together the three formatting parameters that
   determine how a floating-point value should be formatted. *)


END FmtBufF.
