(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE VMError;
IMPORT Fmt;

CONST Messages = ARRAY [NoAccess .. PrivilegedOperation] OF TEXT
  {
   "No access",
   "No space",
   "Virtual address range is reserved for the system",
   "No response from pager",
   "Out of memory",
   "Cache page already exist",
   "Cache page not found",
   "Invalid virtual address",
   "Invalid phys addr tag (ask yasushi)",
   "Stale PhysAddr.T(ask yasushi)",
   "Privileged operation"
   };
   
PROCEDURE Message(code: Code): TEXT =
  BEGIN
    IF code = Success THEN RETURN "Success"; END;
    IF code >= FIRST(Messages) AND code <= LAST(Messages) THEN
      RETURN Messages[code];
    END;
    RETURN "vm error(" & Fmt.Int(code) & ".";
  END Message;
  
BEGIN
END VMError.

