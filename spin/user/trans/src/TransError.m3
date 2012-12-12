(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE TransError;
IMPORT Error;

CONST
  messages = ARRAY [INVALID_STORAGE .. TIMEOUT] OF TEXT {
    "Invalid storage ID",
    "No such storage",
    "Can't abort this transaction",
    "Transaction aborted",
    "Deadlock detected",
    "Lock time out"
 };
  
REVEAL T = Error.T BRANDED OBJECT
OVERRIDES
  message := Message;
END;

PROCEDURE Message (e: T): TEXT =
  BEGIN
    IF e.resultCode() = SUCCESS THEN RETURN "ok";
    ELSE RETURN messages[e.resultCode()];
    END;
  END Message;

PROCEDURE Raise (code: INTEGER) RAISES {Error.E} =
  BEGIN
    RAISE Error.E(NEW(T).init(code));
  END Raise;
  
BEGIN
END TransError.
