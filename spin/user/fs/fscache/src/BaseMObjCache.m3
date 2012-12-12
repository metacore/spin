(*
 * Copyright 1994 - 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)
MODULE BaseMObjCache;

IMPORT VMTypes, VMError, PhysAddr, PhysAddrPrivate;
IMPORT Buffer;
IMPORT DoubleList, Victim;
IMPORT IO, Fmt;

REVEAL
  T = Public BRANDED OBJECT
    pagelist: DoubleList.T;
    name: TEXT;
    size: VMTypes.PageCount;
    numfree: VMTypes.PageCount;
    lock: MUTEX;
  OVERRIDES
    (* file cache methods *)
    init := Init;
    getBuffer := GetBuffer;
    freeBuffer := FreeBuffer;
    print := Print;
    stat := Stat;
  END;

CONST KeepStats = TRUE;

PROCEDURE Init(self: T; numpages: VMTypes.PageCount; name: TEXT) : T
  RAISES { VMError.E } =
  VAR
    buf: Buffer.T;
  BEGIN
    (* set name *)
    self.name := name;

    self.size := numpages;
    self.numfree := numpages;
    self.lock := NEW(MUTEX);

    (* allocate victim list - this is needed for the default victim
       policy, which is LRU globally over this cache *)
    self.victimlist := NEW(DoubleList.T).init();

    (* make sure page demand is not too high *)
    IF numpages > MaxPagesInCache THEN
      IO.Put("BaseMObjCache.Init: requested " & Fmt.Int(numpages) & 
        " pages for cache; maximum " & Fmt.Int(MaxPagesInCache) & "\n");
      RAISE VMError.E(VMError.OutOfMemory);
    END;

    (* create a list to hold the allocated pages *)
    self.pagelist := NEW(DoubleList.T).init();
    
    (* allocate pages.  if allocation fails, go back and deallocate
       any that have been allocated *)
    FOR i := 1 TO numpages DO
      buf := NEW(Buffer.T).init();
      TRY
        (* XXX - to page this cache, start here!!! *)
        buf.data := PhysAddrPrivate.AllocatePinnedPage();
        buf.owner := NIL;
        buf.index := 0;
      EXCEPT
      | VMError.E(ec) =>
        IO.Put("BaseMObjCache.Init: exception allocating page " &
          Fmt.Int(i) & " out of " & Fmt.Int(numpages) & "\n");
        TRY
          LOOP
            buf := self.pagelist.removeHead();
            PhysAddr.Deallocate(buf.data);
          END;
        EXCEPT
        | DoubleList.Empty =>
          (* fall through *)
        END;
        RAISE VMError.E(ec);
      END;
      TRY
        self.pagelist.addHead(buf); (* could add from head or tail *)
      EXCEPT
      | DoubleList.InList =>
        IO.Put("BaseMObjCache: PANIC: corrupted list.\n");
        RETURN NIL;
      END;
    END;
    RETURN self;
  END Init;

PROCEDURE GetBuffer(self: T; newowner: REFANY := NIL; index: INTEGER := 0) 
  : Buffer.T =
  VAR
    buf: Buffer.T;
  BEGIN
    TRY
      buf := self.pagelist.removeTail(); (* could take from head or tail *)
    EXCEPT
    | DoubleList.Empty =>
      RETURN NIL;
    END;

    buf.owner := newowner;
    buf.index := index;

    (* stats *)
    IF KeepStats THEN
      LOCK self.lock DO
        DEC(self.numfree);
      END;
    END;

    (* report to Victim controller *)
    Victim.AllocBuffer(self, newowner, buf);

    RETURN buf;
  END GetBuffer;

PROCEDURE FreeBuffer(self: T; buf: Buffer.T; owner: REFANY := NIL;
  <*UNUSED*>index: INTEGER := 0) =
  BEGIN
    TRY
      self.pagelist.addHead(buf);
    EXCEPT
    | DoubleList.InList =>
      IO.Put("BaseMObjCache.FreeBuffer: buffer is already free.\n");
    END;
    buf.owner := NIL;
    buf.index := 0;

    (* stats *)
    IF KeepStats THEN
      LOCK self.lock DO
        INC(self.numfree);
      END;
    END;

    (* report to Victim controller *)
    Victim.FreeBuffer(self, owner, buf);
  END FreeBuffer;

PROCEDURE Print(self: T) : TEXT =
  BEGIN
    IF self.name = NIL THEN
      RETURN "BaseMObjCache(" & Fmt.Int(self.size) & ")";
    ELSE
      RETURN self.name & "(" & Fmt.Int(self.size) & ")";
    END;
  END Print;

PROCEDURE Stat(self: T; VAR capacity: VMTypes.PageCount; 
  VAR numfree: VMTypes.PageCount) =
  BEGIN
    IF KeepStats THEN
      LOCK self.lock DO
        capacity := self.size;
        numfree := self.numfree;
      END;
    ELSE
      IO.Put("BaseMObjCache.Stat: stats disabled in " & self.print() & "\n");
      numfree := 0;
    END;
  END Stat;

BEGIN
END BaseMObjCache.
      
      
