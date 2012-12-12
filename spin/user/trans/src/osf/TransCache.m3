(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransCache;
IMPORT Storage;
REVEAL T = BRANDED "TransCache" OBJECT END;

PROCEDURE Create (<*UNUSED*>st: Storage.T): T =
  BEGIN
    RETURN NEW(T);
  END Create;

BEGIN
END TransCache.
