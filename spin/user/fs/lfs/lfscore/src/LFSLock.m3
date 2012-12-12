(*
  Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *
 *)
MODULE LFSLock;
IMPORT Sema, IO, Fmt;

CONST
  DEBUG = FALSE;

REVEAL T = Public BRANDED OBJECT
  mu : MUTEX;		(* for critical section *)
  se : Sema.T;		(* blocks entire LFS *)
  count : CARDINAL;	(* number of thread reading/writing to LFS *)
OVERRIDES
  init := Init;		(* initialize self *)
  lock := Lock;		(* blocks sync, cleaner while allowing read/write *)
  unlock := UnLock;
  lockwhole := LockWhole;
  unlockwhole := UnLockWhole;
END;

PROCEDURE Init(self: T) : T =
  BEGIN
    self.mu := NEW(MUTEX);
    self.se := Sema.Alloc(1);
    self.count := 0;
    RETURN self;
  END Init;

PROCEDURE Lock(self: T) =
  BEGIN

    LOCK self.mu DO
      INC(self.count);
      IF self.count = 1 THEN		(* if this is the first client *)
	Sema.P(self.se);		(* lock LFS *)
	IF DEBUG THEN IO.Put("locked LFS \n"); END;
      END;
      IF DEBUG THEN IO.Put("Lock: count " & Fmt.Int(self.count) & "\n"); END;
    END;
  END Lock;

PROCEDURE UnLock(self: T) =
  BEGIN

    LOCK self.mu DO
      DEC(self.count);
      IF self.count = 0 THEN		(* if this is the last client *)
	Sema.V(self.se);		(* unlock LFS *)
	IF DEBUG THEN IO.Put("unlocked LFS \n"); END;
      END;
      IF DEBUG THEN IO.Put("UnLock: count " & Fmt.Int(self.count) & "\n"); END;
    END;
  END UnLock;


PROCEDURE LockWhole(self: T) =
  BEGIN
    IF DEBUG THEN IO.Put("LockWhole: about P-ing"); END;

    Sema.P(self.se);

    IF DEBUG THEN IO.Put("LockWhole: P-ed\n"); END;
  END LockWhole;

PROCEDURE UnLockWhole(self: T) =
  BEGIN
    IF DEBUG THEN IO.Put("UnLockWhole: about V-ing"); END;

    Sema.V(self.se);

    IF DEBUG THEN IO.Put("UnLockWhole: V-ed\n"); END;
  END UnLockWhole;
 
BEGIN
END LFSLock.
