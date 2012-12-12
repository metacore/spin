(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE LightMutex EXPORTS LightMutex, LightMutexPrivate;
(* unsafe because it calls C procefure. *)
IMPORT Strand;
IMPORT AtomicOpsExtern;
IMPORT Dispatcher, DispatcherPrivate;
IMPORT IO;

PROCEDURE TryLock (VAR t: T): BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.TryLock(t);
  END TryLock;

PROCEDURE Create (): T =
  BEGIN
    RETURN 0;
  END Create;
  
PROCEDURE Lock (VAR t: T) =
  BEGIN
    WHILE AtomicOpsExtern.TryLock(t) = FALSE DO
      Strand.Yield();
    END;
  END Lock;
 
PROCEDURE Unlock (VAR t: T) =
  BEGIN
    AtomicOpsExtern.Unlock(t);
  END Unlock;

PROCEDURE Init (<*UNUSED*>verbose: BOOLEAN) =
  BEGIN
    TRY
      DispatcherPrivate.Bypass(Unlock, AtomicOpsExtern.Unlock);
      DispatcherPrivate.Bypass(TryLock, AtomicOpsExtern.TryLock);
    EXCEPT
    | Dispatcher.Error =>
      IO.Put("LightMutex.Init: dispacher error.\n");
    END;
  END Init;
  
BEGIN

END LightMutex.
