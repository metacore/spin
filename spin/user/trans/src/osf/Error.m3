(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE Error;
IMPORT Fmt;

REVEAL T = Public BRANDED OBJECT
		code : INTEGER;
		OVERRIDES
		  resultCode := ResultCode;
		  message := Message;
		END;
PROCEDURE Init(e : Public; code : INTEGER) : T =
  BEGIN
    NARROW(e,T).code := code;
    RETURN e;
  END Init;

PROCEDURE ResultCode(self: T): INTEGER =
  BEGIN
    RETURN self.code;
  END ResultCode;

PROCEDURE Message(e : T) : TEXT =
  BEGIN
    RETURN "Error " & Fmt.Int(e.code);
  END Message;

BEGIN
END Error.
