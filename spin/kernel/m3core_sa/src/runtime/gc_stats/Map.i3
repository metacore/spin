(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 4-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

INTERFACE Map;

TYPE
  T = UNTRACED REF RECORD
    first: S := NIL;
  END;
  S = UNTRACED REF RECORD
    addr: ADDRESS := NIL;
    next: S := NIL;
  END;

CONST Brand = "Map";

PROCEDURE Add (t: T; ptr: ADDRESS);
PROCEDURE Remove (t: T; ptr: ADDRESS) : BOOLEAN;
PROCEDURE Move (t: T; old, new: ADDRESS) : BOOLEAN;

END Map.
