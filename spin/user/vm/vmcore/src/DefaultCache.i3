(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)
INTERFACE DefaultCache;
IMPORT CacheObject;
IMPORT VMTypes;

PROCEDURE Create(size : VMTypes.PageCount) : CacheObject.T;
  
END DefaultCache.
