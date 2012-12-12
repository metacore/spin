(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)


INTERFACE NfsKey;
IMPORT NfsProt;
IMPORT Word;

CONST Brand = "Nfs";
TYPE T = RECORD
  fh: NfsProt.nfs_fh;
  name: TEXT;
END;

(* FOR GENERIC TABLE *)
PROCEDURE Equal(READONLY rt1: T; READONLY rt2: T): BOOLEAN;
PROCEDURE Hash(READONLY rt: T): Word.T;

END NfsKey.
