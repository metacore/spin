(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE LightMutex;
IMPORT Mutex;
IMPORT Thread;

PROCEDURE TryLock (t: T): BOOLEAN =
  BEGIN
    RETURN Mutex.TryLock(t);
  END TryLock;

PROCEDURE Create (): T =
  BEGIN
    RETURN NEW(MUTEX);
  END Create;
  
PROCEDURE Lock (t: T) =
  BEGIN
    Thread.Acquire(t);
  END Lock;
 
PROCEDURE Unlock (t: T) =
  BEGIN
    Thread.Release(t);
  END Unlock;

BEGIN
END LightMutex.