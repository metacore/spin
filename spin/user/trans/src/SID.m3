(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE SID;
IMPORT Word;

PROCEDURE Equal (s1, s2: T): BOOLEAN =
  BEGIN
    RETURN s1 = s2;
  END Equal;

PROCEDURE Hash (s1: T): Word.T =
  BEGIN
    RETURN s1.hid + s1.lid;
  END Hash;
  
BEGIN
END SID.
