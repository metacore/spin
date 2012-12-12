(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE VMError;
PROCEDURE Message(e: INTEGER): TEXT =
  BEGIN
    RETURN "unknown vm error";
  END Message;
BEGIN
END VMError.
