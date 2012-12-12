(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)
INTERFACE BufferPurge;
IMPORT Storage;
PROCEDURE PurgePages(st: Storage.T; shadow: BOOLEAN;
		     READONLY pos: ARRAY OF INTEGER): BOOLEAN;
(* 
   This proc is called when the physical frame has to be freed.
   "pos" is the array of page offsets.
 *)
  
END BufferPurge.
