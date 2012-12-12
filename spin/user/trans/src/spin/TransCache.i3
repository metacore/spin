(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE TransCache;
IMPORT CacheObject;
IMPORT Storage;

PROCEDURE Create(st: Storage.T): CacheObject.T;
  
END TransCache.
