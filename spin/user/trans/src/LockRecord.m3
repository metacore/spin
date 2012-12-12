(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE LockRecord;
IMPORT Q;

VAR
  mu := NEW(MUTEX);
  free := Q.NewHeader();
  
PROCEDURE Allocate (): T =
  BEGIN
    LOCK mu DO 
      IF Q.Empty(free) THEN
	RETURN NEW(T);
      END;

      RETURN Q.RemoveHead(free);
    END;
  END Allocate;

PROCEDURE Free (t: T) =
  BEGIN
    LOCK mu DO 
      Q.InsertHead(free, t);
    END;
  END Free;
  
BEGIN
END LockRecord.
