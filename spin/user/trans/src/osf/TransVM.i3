(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* XXX This should be phased out and replaced by PhysAddr *)

INTERFACE TransVM;
IMPORT PhysAddr;
IMPORT MemoryObject;
IMPORT Storage;
IMPORT Buffer;
IMPORT Word;

TYPE
  
  T = REF RECORD
    st: Storage.T;
    base: Word.T;
  END;

  Page = PhysAddr.T;
  
PROCEDURE Init(st: Storage.T; size: INTEGER);

PROCEDURE Allocate(buf: Buffer.T): Page;
PROCEDURE Deallocate(page: Page; st: Storage.T; pos: INTEGER);
PROCEDURE Access(page: Page; st: Storage.T; pos: INTEGER;
		 callback: PROCEDURE(VAR page: ARRAY OF CHAR));

PROCEDURE BaseAddress(st: Storage.T): INTEGER;
END TransVM.
