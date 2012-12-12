(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	created
 *	
 *)
UNSAFE MODULE Mutex;
IMPORT Thread;

TYPE FakeMUTEX = OBJECT
  holder       : FakeMUTEX;
  waitingForMe : FakeMUTEX;
END; 

PROCEDURE TryLock (mu: MUTEX): BOOLEAN =
  VAR holder := LOOPHOLE(mu, FakeMUTEX).holder;
  BEGIN
    IF holder # NIL THEN
      RETURN FALSE;
    ELSE
      Thread.Acquire(mu);
      RETURN TRUE;
    END;
  END TryLock;
  
BEGIN
END Mutex.
