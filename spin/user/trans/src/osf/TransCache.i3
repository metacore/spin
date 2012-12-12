(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE TransCache;
IMPORT Storage;

TYPE T <: ROOT;

PROCEDURE Create(st: Storage.T): T;
  
END TransCache.
