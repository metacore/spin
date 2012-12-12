(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

INTERFACE SID;
IMPORT HostID;
IMPORT Ctypes;
IMPORT Word;

(* Globally unique ID for a storage. It only has to
   identify the storage within a log device. *)
TYPE
  LID = Ctypes.unsigned_int;
  T = RECORD
    hid: BITS 32 FOR HostID.T;
    lid: LID;
  END;
    
CONST
  Void = T{hid := HostID.Void, lid := LAST(LID)};
  Brand = "SID";
    
PROCEDURE Equal(s1, s2: T): BOOLEAN;
PROCEDURE Hash(s1: T): Word.T;
 
END SID.
