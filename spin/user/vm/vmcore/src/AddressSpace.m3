(*
 * Copyright 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed Unmap to invalidate TLB and MMU entries.
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Added RemoveMapping() in Munmap per Yasushi's suggestion.
 *
 * 18-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Changed range check in Allocate to allow zeroth page to be
 *	 allocated.
 *
 * 16-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Begin supporting demand paging.
 *	
 * 14-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Changed to object repreesentation
 *
 * 20-Dec-95  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE AddressSpace;

IMPORT VMTypes;
IMPORT AddressSpaceRep, AddressMapEntry, Translation, MemoryObject,
       PhysAddr, IO, Fmt;
IMPORT MemoryObjectRep;
IMPORT Dispatcher;
IMPORT Trap;
IMPORT VMError;
IMPORT PageFault;
IMPORT Protection;
IMPORT ExternalRef;
IMPORT AddressMapQ;
IMPORT Debugger;
VAR
  numAddressSpaces: CARDINAL := 0;

REVEAL
  T = AddressSpaceRep.T BRANDED OBJECT
  OVERRIDES
    init := Initialize;
    destroy := Destroy;
    allocate := Allocate;
    deallocate := Deallocate;
    map := Map;
    unmap := Unmap;
    clear := Clear;
    print := Print;
    stat := StatMethod;
    setXrefTbl := SetXrefTbl;
    getXrefTbl := GetXrefTbl;
  END;

PROCEDURE Initialize (self: T; name: TEXT): T =
  BEGIN
    self.lock := NEW(MUTEX);
    self.externs := NEW(ExternalRef.T).init();
    Translation.Initialize(self);
    self.first := 0;
    self.last := 536870912;
    self.maps := AddressMapQ.NewHeader();
    self.mapCache := NIL;

    (* Name this object "AddressSpace-x.y-n (name)" *)
    self.id := numAddressSpaces;
    IF name = NIL THEN
      self.name := Brand&"-"&Fmt.Int(self.id);
    ELSE
      self.name := name;
    END;
    INC(numAddressSpaces);

    TRY 
      self.bindings[0] :=
        Dispatcher.InstallHandler(Trap.InvalidTranslation,
				  NIL, PageFault.Handler,
				  key := self,
	       options := Dispatcher.Options{Dispatcher.Opt.First});
      self.bindings[1] :=
        Dispatcher.InstallHandler(Trap.AccessViolation,
				  NIL, PageFault.Handler,
				  key := self,
               options := Dispatcher.Options{Dispatcher.Opt.First});
      self.bindings[2] :=
        Dispatcher.InstallHandler(Trap.ProtectionFault,
				  NIL, PageFault.Handler,
				  key := self,
               options := Dispatcher.Options{Dispatcher.Opt.First});
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("AddressSpace.Initialize : dispatcher error("
	     & Fmt.Int(ORD(ec)) & ".\n");
    END;
    RETURN self;
  END Initialize;

PROCEDURE Destroy (self: T) =
  BEGIN
    TRY 
      MapOperate(self, 0, LAST(CARDINAL), DeallocateClosure, self);
      Translation.Destroy(self);
      (* Uninstall fault handlers *)
      FOR i := 0 TO LAST(self.bindings) DO
	Dispatcher.Uninstall(self.bindings[i]);
      END;
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("AddressSpace.Destroy: distpatcher (" & Fmt.Int(ORD(ec))
	     & ".\n");
    | VMError.E(ec) =>
      IO.Put("AddressSpace.Destroy: vm error(" & Fmt.Int(ec) & ".\n");
    END;
    self.externs.destroy();
    self.lock := NIL;
  END Destroy;

PROCEDURE GetXrefTbl(self: T): ExternalRef.T =
  BEGIN
    RETURN self.externs;
  END GetXrefTbl;
  
PROCEDURE SetXrefTbl(self: T; e: ExternalRef.T): ExternalRef.T =
  VAR
    old := self.externs;
  BEGIN
    self.externs := e;
    RETURN old;
  END SetXrefTbl;
  
PROCEDURE StatMethod (self: T; VAR buf: Stat) =
  VAR v, r: CARDINAL := 0;
  PROCEDURE Closure(<*UNUSED*>x: T;
		    entry: AddressMapEntry.T;
		    <*UNUSED*>arg: REFANY): VMError.Code =
    VAR
      mObj := entry.mObj;
      stat: MemoryObject.Stat;
    BEGIN
      IF mObj # NIL THEN 
	stat := mObj.stat();
	INC(v, stat.virtualSize);
	INC(r, stat.residentSize);
      END;
      RETURN VMError.Success;
    END Closure;
  BEGIN
    MapOperate(self, 0, LAST(CARDINAL), Closure, NIL);
    buf.virtualSize := v;
    buf.residentSize := r;
  END StatMethod;
  
PROCEDURE Clear(self: T) RAISES {VMError.E} =
  BEGIN
    LOCK self.lock DO
      MapOperate(self, 0, LAST(CARDINAL), DeallocateClosure, self);
      IF NOT AddressMapQ.Empty(self.maps) THEN
	IO.PutError("AddressSpace.Clear: ???");
	Debugger.Enter();
      END;
    END;
  END Clear;
    
(* XXX
 Note: allocate/map doesn't quite work; you can't map on a
 region that is a subregion of the one allocated. Therefore,
 the only meaningful usage is to allocate, then map on an exactly
 same region. This means allocate & map can be combined without problem.

 yas *)
PROCEDURE Allocate (self: T; VAR pageNum: VMTypes.PageNumber;
		    numPages: VMTypes.PageCount;
		    anyWhere: BOOLEAN) RAISES {VMError.E} =
  VAR
    newEntry: AddressMapEntry.T;
  BEGIN
    IF numPages = 0 THEN
      RETURN;
    END;
    
    IF pageNum < self.first OR pageNum+numPages > self.last THEN
      RAISE VMError.E(VMError.NoAccess);
    END;

    LOCK self.lock DO
      newEntry := AddressMapQ.Allocate();
      newEntry.from := pageNum;
      newEntry.end := pageNum+numPages;
      newEntry.mObj := NIL;

      IF MapInsert(self, newEntry) THEN
	RETURN;
      ELSE
	VAR 
	  itr := AddressMapQ.Iterate(self.maps);
	  entry: AddressMapEntry.T;
	  found := FALSE;
	BEGIN
	  IF NOT anyWhere THEN 
	    RAISE VMError.E(VMError.NoSpace);
	  END;
	  
	  WHILE AddressMapQ.NextItr(itr, entry) DO
	    IF entry.next # self.maps THEN
	      WITH nextEntry = entry.next DO
		IF nextEntry.from > pageNum THEN
		  IF nextEntry.from - entry.end > numPages THEN
		    newEntry.from := entry.end;
		    newEntry.end := newEntry.from + numPages;
		    AddressMapQ.InsertHead(entry, newEntry);
		    found := TRUE;
		    EXIT;
		  END;
		END;
	      END;
	    ELSE
	      (* tail of the virt addr space is already free.
		 XXX we really need to check it. *)
	      newEntry.from := entry.end;
	      newEntry.end := newEntry.from + numPages;
	      AddressMapQ.InsertHead(entry, newEntry);
	      found := TRUE;
	      EXIT;
	    END;
	  END;
		  
	  IF NOT found THEN
	    RAISE VMError.E(VMError.NoSpace);
	  END;
	END;
      END;
    END;
  END Allocate;

PROCEDURE Deallocate(self: T; from: VMTypes.PageNumber; len: VMTypes.PageCount)
                     RAISES {VMError.E} =
  BEGIN
    IF len = 0 THEN
      RETURN;
    END;

    LOCK self.lock DO
      IF from <= self.first OR from+len > self.last THEN
        RAISE VMError.E(VMError.NoAccess);
      END;
      MapOperate(self, from, from+len, DeallocateClosure, self);
    END;
  END Deallocate;



PROCEDURE Unmap (self: T; from: VMTypes.PageNumber;
		 len: VMTypes.PageCount) RAISES {VMError.E} = 
  BEGIN
    IF len = 0 THEN
      RETURN;
    END;

    LOCK self.lock DO
      IF from <= self.first OR from+len > self.last THEN
        RAISE VMError.E(VMError.NoAccess);
      END;
      MapOperate(self, from, from+len, UnmapClosure, self);
    END;
    Translation.RemoveMapping(self, from, from+len);
  END Unmap;

PROCEDURE Map (self: T;
	       pageNum: VMTypes.PageNumber; numPages: VMTypes.PageCount;
	       mObj: MemoryObject.T; mOff: VMTypes.PageNumber; lazy : BOOLEAN)
  RAISES {VMError.E} =
  
  PROCEDURE MapClosure(self: T;
		       entry: AddressMapEntry.T;
		       <*UNUSED*>arg: REFANY) : VMError.Code =
    BEGIN
      entry.mObj := mObj;
      entry.mOff := mOff + entry.from - pageNum;

      (* Link "entry" to the memory object. *)
      LOCK mObj.lock DO
	(* XXX deadlock ok? *)
	entry.link := mObj.mapEntries;
	mObj.mapEntries := entry;
	mObj.pager.mapNotify(entry.mOff, entry.end-entry.from,
			     self, entry.from);
      END;
      RETURN VMError.Success;
    END MapClosure; 
    
  VAR
    frame: PhysAddr.T;
    prot: Protection.T;
    reason := Trap.Write;
  BEGIN
    LOCK self.lock DO
      MapOperate(self, pageNum, pageNum+numPages, MapClosure, mObj);

      IF NOT lazy THEN 
	FOR i:= 0 TO numPages-1  DO
	  IF NOT mObj.request(mOff + i, reason, 
			      frame, prot) THEN
	    IO.Put("addrspace.map: page in failed.\n");
	  ELSE
	    mObj.pager.pageMapNotify(mOff + i, self, pageNum+i);
	    Translation.AddMapping(self, frame, pageNum+i, pageNum+i+1, prot);
	  END;
	END;
      END;
    END;
  END Map;

PROCEDURE Print(self: T): TEXT =
  BEGIN
    RETURN self.name;
  END Print;

PROCEDURE DeallocateClosure (self: T; entry: AddressMapEntry.T; arg: REFANY)
  : VMError.Code =
  VAR
    result: VMError.Code;
  BEGIN
    result := UnmapClosure(self, entry, arg);
    IF result = VMError.Success THEN
      self.mapCache := NIL;
      IF entry.prev # self.maps THEN
	(* memobject is likely to be mapped on the same position
	   or later. so cache it. *)
	self.mapCache := entry.prev;
      END;
      AddressMapQ.Remove(entry);
      entry.mObj := NIL;
      entry.from := 16_ffffffff;
      entry.end := 16_fffffffe;
      AddressMapQ.Free(entry);
    END;
    RETURN result;
  END DeallocateClosure;

PROCEDURE UnmapClosure(self: T;
		       entry: AddressMapEntry.T;
		       <*UNUSED*>arg: REFANY): VMError.Code =
  VAR
    mObj := entry.mObj;
    lastl, l: AddressMapEntry.T;
  BEGIN
    IF mObj = NIL THEN
      RETURN VMError.Success;
    END;
    Translation.RemoveMapping(self, entry.from, entry.end);
    
    LOCK mObj.lock DO 
      (* Remove the "entry" from memobject link. *)
      l := mObj.mapEntries;
      IF l = entry THEN
	mObj.mapEntries := l.link;
      ELSE
	lastl := l;
	l := l.link;
	WHILE l # NIL DO
	  IF l = entry THEN
	    lastl.next := l.link;
	    EXIT;
	  END;
	  l := l.link;
	END;
	<*ASSERT l = entry*>
      END;
      mObj.pager.unmapNotify(entry.mOff, entry.end-entry.from,
			     self, entry.from);
    END;  
    
    entry.mOff := 0;
    entry.mObj := NIL;
    
    RETURN VMError.Success;
  END UnmapClosure; 

(* Address space map manipulation *)

PROCEDURE MapOverlap (me1: AddressMapEntry.T; from, end: CARDINAL): BOOLEAN =
  BEGIN
    IF me1.from >= end OR
       from >= me1.end THEN
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END MapOverlap;

(* Insert "entry" into "self". Returns TRUE if successful,
   returns FALSE if an overlapping entry is found. *)
PROCEDURE MapInsert (self: T; newEntry: AddressMapEntry.T): BOOLEAN =
  VAR
    itr: AddressMapQ.Iterator;
    entry: AddressMapEntry.T;
  BEGIN
    IF self.mapCache # NIL
       AND self.mapCache.from < newEntry.from THEN
      IF self.mapCache.next = self.maps (* lastAcc is the last entry *)
	 OR self.mapCache.next.from >= newEntry.end THEN
	(* gap between lastAcc and lastAcc.next is wide enough to store
	   newEntry!! *)
	AddressMapQ.InsertHead(self.mapCache, newEntry);
	self.mapCache := newEntry;
	RETURN TRUE;
      END;
    END;

    (* Insert the newentry in the sorted position. *)
    itr := AddressMapQ.Iterate(self.maps);
    WHILE AddressMapQ.NextItr(itr, entry) DO
      IF MapOverlap(entry, newEntry.from, newEntry.end) THEN
	RETURN FALSE;
      ELSIF entry.from > newEntry.from THEN
	(* insert "newEntry" just before "entry". *)
	AddressMapQ.InsertTail(entry, newEntry);
	self.mapCache := newEntry;
	RETURN TRUE;
      END;
    END;
    (* no entry bigger than "newEntry" was found.
       So, insert "newEntry" at the overall tail. *)
    AddressMapQ.InsertTail(self.maps, newEntry);
    self.mapCache := newEntry;
    RETURN TRUE;
  END MapInsert;
  
		      
PROCEDURE MapOperate (self: T; from, end: VMTypes.PageNumber;
		      p: PROCEDURE (self:T; entry: AddressMapEntry.T;
				    arg: REFANY): VMError.Code;
		      arg: REFANY) RAISES {VMError.E} =
  VAR
    itr: AddressMapQ.Iterator;
    entry: AddressMapEntry.T;
    result, curResult : VMError.Code := VMError.Success;
  BEGIN
    IF self.mapCache # NIL AND
       self.mapCache.from = from AND self.mapCache.end = end THEN
      (* most common case. we are operating on exactly the cached region. *)
      result := p(self, self.mapCache, arg);
      IF result # VMError.Success THEN
	RAISE VMError.E(result);
      END;
      RETURN;
    END;
    
    itr := AddressMapQ.Iterate(self.maps);
    WHILE AddressMapQ.NextItr(itr, entry) DO
      (* Is there any overlap? *)
      IF MapOverlap(entry, from, end) THEN
	(* yes, is entry completly contained in the requested region? *)
	IF entry.from >= from AND entry.end <= end THEN
	  curResult := p(self, entry, arg);
	ELSE
	  curResult := Split(self, entry, from, end, p, arg);
	END;
	IF curResult # VMError.Success THEN
	  result := curResult;
	END;
      END;
    END;
    IF result # VMError.Success THEN
      RAISE VMError.E(result);
    END;
  END MapOperate;

PROCEDURE Split(self: T; entry: AddressMapEntry.T;
		from, end: VMTypes.PageNumber;
		p: PROCEDURE (self:T; entry: AddressMapEntry.T;
			      arg: REFANY): VMError.Code;
		arg: REFANY)
  : VMError.Code =
  VAR
    newEntry, splitEntry: AddressMapEntry.T;
  BEGIN

    newEntry := NEW(AddressMapEntry.T);

    (*
     * Three cases:
     * 1) we slip an existing range in two, forming two new ranges
     * 1) we clip the left of an existing range
     * 2) we clip the right of an existing range
     *)

    (* We do this so that the iterator doesn't get confused  by changing 
       key values out from under it.  Not sure this is necessary. *)
    AddressMapQ.Remove(entry);
    
    IF from > entry.from AND end < entry.end THEN
      splitEntry := AddressMapQ.Allocate();
      splitEntry.from := end;
      splitEntry.end := entry.end;
      entry.end := from;
      newEntry.from := from;
      newEntry.end := end;
    ELSIF from >= entry.from AND from < entry.end THEN
      newEntry.from := from;
      newEntry.end := entry.end;
      entry.end := from;
    ELSIF end > entry.from AND end <= entry.end THEN
      newEntry.from := entry.from;
      newEntry.end := end;
      DEC(entry.mOff, entry.from - end);
      entry.from := end;
    END;

    (* Now write back 2-3 entries *)
    IF NOT MapInsert(self, entry) THEN
      IO.Put("Error 1\n");
    END;
    
    newEntry.mObj := entry.mObj;
    newEntry.mOff := entry.mOff + newEntry.from - entry.from;
    newEntry.space := entry.space;
    newEntry.prot := entry.prot;
    IF NOT MapInsert(self, newEntry) THEN
      IO.Put("Error 2\n");
    END;
    IF splitEntry # NIL THEN
      splitEntry.mObj := entry.mObj;
      splitEntry.mOff := entry.mOff + splitEntry.from - entry.from;
      splitEntry.space := entry.space;
      splitEntry.prot := entry.prot;
      IF NOT MapInsert(self, splitEntry) THEN
	IO.Put("Error 3\n");
      END;
    END;
    RETURN p(self, newEntry, arg);
  END Split;

PROCEDURE Equal(as1, as2: T): BOOLEAN =
  BEGIN
    IF (as1.id = as2.id) THEN
      RETURN FALSE;
    ELSE
      RETURN TRUE;
    END;
  END Equal;

PROCEDURE Hash(as: T): CARDINAL =
  BEGIN
    RETURN as.id;
  END Hash;

PROCEDURE Compare(as1, as2: T): [-1..1] =
  BEGIN
    IF (as1.id > as2.id) THEN 
      RETURN 1;
    ELSE
      IF (as1.id < as2.id) THEN
        RETURN -1;
      ELSE
        RETURN 0;
      END;
    END;
  END Compare;

  
BEGIN
END AddressSpace.
