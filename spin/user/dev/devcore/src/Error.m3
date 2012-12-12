(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	added errno(), whisted.
 *)

MODULE Error;
IMPORT Fmt;

REVEAL T = Public BRANDED OBJECT
  code: INTEGER;
OVERRIDES
  init := Init;
  resultCode := ResultCode;
  errno := ResultCode;
  message := Message;
END;

PROCEDURE Init (e: T; code: INTEGER) : T =
  BEGIN
    e.code := code;
    RETURN e;
  END Init;

PROCEDURE ResultCode (self: T): INTEGER =
  BEGIN
    RETURN self.code;
  END ResultCode;

PROCEDURE Message (e : T) : TEXT =
  BEGIN
    RETURN "error " & Fmt.Int(e.code);
  END Message;

BEGIN
END Error.
