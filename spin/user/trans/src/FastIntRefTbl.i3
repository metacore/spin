(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE FastIntRefTbl;

TYPE Default <: T;
TYPE T = OBJECT
METHODS
  init(): T;
  get(key: INTEGER; VAR v: REFANY): BOOLEAN;
  put(key: INTEGER; v: REFANY): BOOLEAN;
  (* put assumes there is no entry for "key" beforehand.
   Therefore, put always returns FALSE. *)
  delete(key: INTEGER; VAR v: REFANY): BOOLEAN;
  size(): INTEGER;
  iterate(): Iterator;
END;

TYPE Iterator <: IteratorPublic;
IteratorPublic = OBJECT
METHODS
  next(VAR key: INTEGER; VAR v: REFANY): BOOLEAN;
END;
  
END FastIntRefTbl.
