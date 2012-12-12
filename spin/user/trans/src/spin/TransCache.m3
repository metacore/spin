(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransCache;
IMPORT Storage, StorageProtected, StorageRep;
IMPORT Buffer, BufferRep;
IMPORT CacheObject;
IMPORT PhysAddr;
IMPORT VMTypes;
IMPORT TransProc;
IMPORT ActiveTrans, ActiveTransRep;
IMPORT LockMode;
IMPORT Protection;
IMPORT Trap;
IMPORT VMError;
IMPORT Strand;
IMPORT Transaction;
IMPORT Spy;
IMPORT TransUtils;

<*NOWARN*>FROM TransUtils IMPORT Msg;
TYPE T = CacheObject.T OBJECT
  st: Storage.T;
OVERRIDES
  update := Update;
  lookup := Lookup;
  invalidate := Invalidate;
  iterate := Iterate;
END;

VAR
  lookupSpy: Spy.T;

PROCEDURE Create (st: Storage.T): CacheObject.T =
  BEGIN
    RETURN NEW(T, st := st);
  END Create;

PROCEDURE Update (<*UNUSED*>c: T; <*UNUSED*>off: VMTypes.PageNumber;
		  <*UNUSED*>frame: PhysAddr.T) =
  BEGIN
  END Update;

PROCEDURE Lookup (t: T; off: VMTypes.PageNumber;
		  type: INTEGER; VAR frame: PhysAddr.T;
		  VAR prot: Protection.T) : BOOLEAN =
  VAR
    proc: TransProc.T;
    tr: Transaction.T;
    allocate := FALSE;
    at: ActiveTrans.T;
    rc := FALSE;
    buf: Buffer.T;
    lock: LockMode.T;
  BEGIN
    IF TransUtils.SpyTime THEN Spy.Enter(lookupSpy); END;
    
    proc := TransProc.Self();    
    tr := proc.curTrans;
    
    IF type = Trap.Write THEN
      prot := Protection.All;
      lock := LockMode.T.Write;
    ELSE
      prot := Protection.ReadOnly;
      lock := LockMode.T.Read;
    END;
    
    LOCK t.st.mu DO
      at := StorageProtected.InternTrans(t.st, tr);
      TRY
	buf := t.st.pinFrame(at, lock, off, allocate, NIL);
	INC(at.nPagesMapped);
      EXCEPT
      | VMError.E(e) =>
	Msg("transcache.pagein: " & VMError.Message(e));
	Strand.Yield();
	RETURN FALSE;
      END;
    END;
    IF buf # NIL THEN
      rc := TRUE;
      frame := buf.frame;
      Buffer.Unlock(buf);
    END;
    IF TransUtils.SpyTime THEN Spy.Exit(lookupSpy); END;
    RETURN rc;
  END Lookup;

PROCEDURE Invalidate (<*UNUSED*>c: T; <*UNUSED*>off: VMTypes.PageNumber) =
  BEGIN
    (* do nothing *)
  END Invalidate;

TYPE
  Iterator = CacheObject.Iterator OBJECT
    itr: Buffer.Iterator;
  OVERRIDES
    next := Next;
  END;

PROCEDURE Iterate (c: T): CacheObject.Iterator =
  BEGIN
    RETURN NEW(Iterator, itr := Buffer.Iterate(c.st));
  END Iterate;
    
PROCEDURE Next (i: Iterator; VAR off: VMTypes.PageNumber; VAR frame: PhysAddr.T)
  : BOOLEAN =
  VAR
    rc: BOOLEAN;
    buf: Buffer.T;
  BEGIN
    rc := i.itr.next(buf);
    IF rc THEN
      off := buf.pos;
      frame := buf.frame;
    ELSE
      Buffer.EndIterate();
    END;
    RETURN rc;
  END Next;
  
BEGIN
  IF TransUtils.SpyTime THEN
    lookupSpy := Spy.Create("trans:cache-lookup", FALSE, TransUtils.NItr);
  END;
END TransCache.
