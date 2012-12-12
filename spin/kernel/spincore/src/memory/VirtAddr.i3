(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed "T" stuff. It should be implemented in Translation.
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

INTERFACE VirtAddr;

IMPORT Word;

TYPE 
  Address = Word.T;
  (* Byte-unit virtual address *) 
  Size = Word.T;
  Page = Word.T;
  (* Page-unit virtual address. "Address = CPU.PAGESIZE * Page". *)

END VirtAddr.


