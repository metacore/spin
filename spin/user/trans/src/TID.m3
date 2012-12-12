(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
MODULE TID;
IMPORT Word, HostID;

(* XXX we assume HostID is 4 byte long. Maybe we have to move them to
 arch dependent directory *)


PROCEDURE GetHostID (tid: T): HostID.T =
  BEGIN
    RETURN Word.RightShift(tid, 32);
  END GetHostID;

PROCEDURE GetLocal(tid: T): INTEGER =
BEGIN
  RETURN Word.And(tid, 16_FFFFFFFF);
END GetLocal;

PROCEDURE CreateT(hid: HostID.T; local: INTEGER): T =
BEGIN
  RETURN Word.Or(Word.LeftShift(hid, 32), local);
END CreateT;

BEGIN
END TID.
