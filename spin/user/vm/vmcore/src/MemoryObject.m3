(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed request logic so that cache is looked up before doing 
 *	frame allocation. This speeds up the transaction, but slows down
 *      a tiny bit the real fault case where we have to do disk I/O.
 * 07-Jan-97  becker at the University of Washington
 *	Add DoChecksum flag and turned off check summing
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made guards FUNCTIONAL
 *
 * 15-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Asynchronous event support.
 *
 * 20-Feb-96  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

MODULE MemoryObject;
IMPORT Thread, ThreadExtra, Strand;
IMPORT Dispatcher;
IMPORT DebugOption;
IMPORT Auth;
IMPORT Word;
IMPORT Mutex;
IMPORT MemoryObjectRep, CacheObject;
IMPORT PhysAddr;
IMPORT PagerObject;
IMPORT VMError;
IMPORT VMTypes;
IMPORT VMStat;
IMPORT Debugger;
IMPORT IO, Fmt;
IMPORT DefaultPager;
IMPORT DefaultCache;
IMPORT MemoryObject;
IMPORT Protection;
IMPORT Trap;
IMPORT CPU;
IMPORT Bitmap;
IMPORT VMDebug;

REVEAL
  T = MemoryObjectRep.T BRANDED OBJECT
  OVERRIDES
    init := Initialize;
    destroy := Destroy;
    request := Request;
    copyOnWrite := CopyOnWrite;
    isMapped := IsMapped;
    fork := Fork;
    print := Print;
    stat := StatMethod;
  END;

PROCEDURE PageOut (self: T; off: PageNumber; frame: PhysAddr.T)
     : PagerObject.ResultCode =
  VAR rc : PagerObject.ResultCode;
  BEGIN
    INC(VMStat.pout);
    rc := self.pager.pageOut(off, frame, TRUE(*XXX bogus*));
    IF DebugOption.PhysAddr THEN
      VAR state := PhysAddr.GetState(frame);
      BEGIN
	IF NOT (state = PhysAddr.State.Zombie
		OR state = PhysAddr.State.Dead) THEN
	  (* Page is either zombie, or it is already freed. *)
	  Debugger.Enter();
	END;
      END;
    END;
    RETURN rc;
  END PageOut;

VAR serialID := 0;
    
PROCEDURE Initialize (self: T; numPages: PageCount;
		      pager: PagerObject.T;
		      cache: CacheObject.T;
		      name: TEXT): T =
  BEGIN
    IF VMDebug.DebugStat THEN
      LOCK VMDebug.mu DO 
	EVAL VMDebug.memObjTbl.put(self, self);
      END;
    END;
    self.lock := NEW(MUTEX);
    self.size := numPages;
    self.mapEntries := NIL;
    self.shadow := NIL;

    IF name # NIL THEN
      self.name := name;
    ELSE
      self.name := "mObj" & Fmt.Int(serialID);
      INC(serialID);
    END;
      
    self.pager := pager;
    IF self.pager = NIL THEN
      self.pager := DefaultPager.Create(numPages);
    END;
    self.pager.mObj := self;
    
    self.cache := cache;
    IF self.cache = NIL THEN
      self.cache := DefaultCache.Create(numPages);
    END;
    self.cache.mObj := self;
    
    RETURN self;
  END Initialize;


(* Pre: "self" is locked. *)
PROCEDURE DestroyThread (r: REFANY): REFANY =
  VAR self: MemoryObject.T := r;
  BEGIN
    IF self.shadow = NIL AND self.copy # NIL THEN
      (* I am a root. I can't delete the cache nor the pager because the
	 cow children may depend on them. Just wait till all the
	 children dies out. *)
      IF VMDebug.DebugMessage THEN 
	IO.Put("mobj destroy: " & self.print() & " is cow root. deferring.\n");
      END;
    ELSE
      (* Someone may depend on me, but I'm not a cow root. *)
      IF VMDebug.DebugMessage THEN 
	IO.Put("mobj destroy: " & self.print() & " destroying...\n");
      END;

      Thread.Acquire(self.lock);
      
      IF self.invalid THEN
	IF VMDebug.DebugMessage THEN 
	  IO.Put("destroy: already destroyed.\n");
	END;
      ELSE
	(* Unmap myself from all the address spaces
	   that map me. Here, we have to
	   be careful to avoid deadlocks. *)
	LOOP
	  WITH entry = self.mapEntries DO 
	    IF entry = NIL THEN
	      (* No more address space to traverse. *)
	      EXIT;
	    END;
	    Thread.Release(self.lock);
	    TRY
	      entry.space.unmap(entry.from, entry.end-entry.from);
	    EXCEPT
	    | VMError.E =>
	    IO.Put("whoa!! memory object delete failed.\n");
	    END;
	    Thread.Acquire(self.lock);
	  END;
	END;

	(* Destroy myself *)
	self.pager.destroy();
	self.invalid := TRUE;
	self.cache.destroy();
	MemoryObject.Destroyed(self);
      END;

      (* COW stuff. You are not supposed to understand this. *)
      IF self.shadow # NIL THEN 
	(* Lock my parent, and divorce myself from it*)
	WHILE NOT Mutex.TryLock(self.shadow.lock) DO
	  Strand.Yield();
	  Thread.Pause(1000);
	  IO.Put("destroy: avoiding deadlock.\n");
	END;
	<*ASSERT self.shadow.copy = self*>
	
	(* If I'm at the bottom of the chain, release myself from the
	   chain. Otherwise, the chain remain intact.
	   
	   Since "copy" field is used to see if we can delete the memobject
	   or not, setting it NIL here is imperative.
	*)
	IF self.copy = NIL AND self.shadow # NIL THEN
	  self.shadow.copy := NIL;
	END;
	
	(* Try cascaded destruction. *)
	IF NOT self.shadow.isMapped() THEN
	  (* this call will recursively nullify "copy" field. *)
	  self.shadow.destroy();
	END;
	
	Thread.Release(self.shadow.lock);
	
      END;
	
      IF VMDebug.DebugStat THEN
	VAR r: REFANY;
	BEGIN
	  LOCK VMDebug.mu DO 
	    EVAL VMDebug.memObjTbl.delete(self, r);
	  END;
	END;
      END;
      IF VMDebug.DebugMessage THEN
	IO.Put(self.print() & " done.\n");
      END;
      Thread.Release(self.lock);
    END;
    RETURN NIL;
  END DestroyThread;

PROCEDURE Destroy (self: T) =
  BEGIN
    (* This proc has to work when the address space it's mapped to
       is locked. So we fork off a thread to avoid a deadlock. *)
    EVAL ThreadExtra.PFork(DestroyThread, self);
  END Destroy;
  
CONST COWProtection = Protection.T{TRUE, FALSE, TRUE, VMTypes.CopyOnWrite};

PROCEDURE AssertChecksum (mObj: T; off: PageNumber; frame: PhysAddr.T) =
  VAR
    entry := mObj.mapEntries;
    sum: INTEGER;
  BEGIN
    IF NOT VMDebug.DoChecksum THEN RETURN END;

    sum := VMDebug.CalculateChecksum(frame);
    WHILE entry # NIL DO
      IF entry.mObj # mObj THEN Debugger.Enter(); END;

      IF off >= entry.mOff AND off < entry.mOff + entry.end-entry.from THEN
	IF sum # entry.checksum[off - entry.mOff]
	  AND entry.checksum[off - entry.mOff] # 0 THEN
	  IO.Put("checksum mismatch.\n");
	  Debugger.Enter();
	END;
      END;
      entry := entry.link;
    END;
  END AssertChecksum;
  
PROCEDURE Request (self: T; mOff: PageNumber; type: INTEGER;
		   VAR frame: PhysAddr.T; VAR newProt: Protection.T): BOOLEAN
  RAISES {VMError.E} =
  VAR
    mObj := self;
    nextmObj: T;
    prot: Protection.T;
    mu: MUTEX;
    newFrame: PhysAddr.T;
  BEGIN
    
    IF DebugOption.PhysAddr THEN
      IF  mOff < 0 THEN
	IO.Put("request: offset < 0.\n");
	Debugger.Enter();
      END;
      IF  mObj.invalid THEN
	IO.Put("faulted on deleted mobj.\n");
	Debugger.Enter();
      END;
    END;

    (* First, see if we have it in the cache. *)
    LOCK self.lock DO
      IF self.cache.lookup(mOff, type, frame, newProt) THEN
	RETURN TRUE;
      END;
    END;
    
    (* We allocate the frame here, because
       a) we can't call Allocate while the mobj is locked(this
          leads to deadlock).
       
       b) We already checked if we have the frame in the cache, so
       the fact that we are here means that we are likely to do
       some I/O. Since I/O is slow, doing one extra frame allocation
       won't hurt us so much even if it is eventually wasted.

       c) Such waste occur very infrequently.
       
       "newFrame" is set to NIL after it is used. *)
    newFrame := PhysAddr.Allocate(PhysAddr.Tag{NIL, 0});
    frame := NIL;
    
    Thread.Acquire(self.lock);
    
    IF DebugOption.PhysAddr AND mObj.invalid THEN
      IO.Put("faulted on deleted mobj.\n");
      Debugger.Enter();
    END;
      
    IF self.hasOwnContent # NIL
      AND Bitmap.Get(self.hasOwnContent, mOff) THEN
      
      (* Check the cache again, because we released "st.lock" since the
	 last lookup, the state of "st" might have changed. *)
      IF NOT self.cache.lookup(mOff, type, frame, newProt) THEN
	IF VMDebug.DebugMessage THEN
	  IO.Put("pf: miss@local:" & mObj.print() & " off="
		 & Fmt.Int(mOff) & ".\n");
	END;
	PhysAddr.ChangeTag(newFrame, PhysAddr.Tag{mObj, mOff});
	frame := newFrame;
	newFrame := NIL;
	
	INC(VMStat.pin);
	CASE mObj.pager.pageIn(mOff, type, frame, newProt) OF
	| PagerObject.ResultCode.Success =>
	  IF DebugOption.PhysAddr THEN 
	    AssertChecksum(mObj, mOff, frame);
	  END;
	  mObj.cache.update(mOff, frame);
	| PagerObject.ResultCode.AlreadyPagedIn =>
	  IO.Put("pf: already paged in.\n");
	  PhysAddr.Deallocate(frame);
	  (* retry *)
	ELSE
	  Thread.Release(mObj.lock);
	  RAISE VMError.E(VMError.NoResponseFromPager);
	END;
      ELSE
	PhysAddr.Deallocate(newFrame);
      END;
      Thread.Release(self.lock);
      RETURN TRUE;
    END; (* if we should have own content. *)

    (* We still lock "self" here. *)

    (* Follow up the cow chain until a frame is found. *)
    (* XXX memobject offset is now ignored. *)

    LOOP
      mu := mObj.lock;

      (* Lock both "self" and the current mobj.
	 (we can't just say "LOCK mu DO" because then we might lock
	 "self" twice, and deadlock.
	 
	 We (hopefully) avoid mutual deadlock by always locking
	 in child->parent order. *)
      IF mu # self.lock THEN Thread.Acquire(mu); END;
      
      TRY
	IF NOT mObj.invalid
	   AND mObj.cache.lookup(mOff, type, frame, newProt) THEN
	  (*
	     We found a frame.
	  *)
	  prot := PhysAddr.GetProtection(frame);
	  IF mObj = self THEN
	    IF Word.And(prot.others, VMTypes.CopyOnWrite) # 0
	      AND mObj.copy # NIL THEN
	      IF type = Trap.Write THEN
		IO.Put("cow on parent not supported.");
		Debugger.Enter();
	      END;
	      newProt := COWProtection;
	    ELSE
	      (* Page is cow, but there is no children.
		 We can just make it non-cow and continue. *)
	      PhysAddr.ChangeProtection(frame, newProt);
	    END;
	    
	    IF mu # self.lock THEN Thread.Release(mu); END;
	    EXIT; (* loop *)
	  ELSE
	    (* hit@parent. *)
	    IF Word.And(prot.others, VMTypes.CopyOnWrite) = 0 THEN
	      (* This happens when "mObj"(which is different from "self")
		 cow-faulted before on this page.
		 In this case, we can't use this frame as mine because
		 the page was already modified by "mObj".
		 We continue up the chain. *)
	      IF VMDebug.DebugMessage THEN
		IO.Put("pf: parent hit, but dirty, continuing..\n");
	      END;
	    ELSIF type = Trap.Write THEN
	      (* Write@cowpage. Copy the page*)
	      IF VMDebug.DebugMessage THEN
		IO.Put("pf: write hit@" & mObj.print() & ".\n");
	      END;
	      
	      IF VMStat.Enabled THEN
		INC(VMStat.cow);
	      END;
	      PhysAddr.Copy(newFrame, frame);
	      PhysAddr.ChangeTag(newFrame, PhysAddr.Tag{self, mOff});
	      
	      newProt := Protection.All;
	      frame := newFrame;
	      newFrame := NIL;
	      
	      self.cache.update(mOff, frame);
	      IF DebugOption.PhysAddr THEN 
		AssertChecksum(self, mOff, frame);
		IF Bitmap.Get(self.hasOwnContent, mOff) THEN
		  IO.Put("fault: bogus owncontents.\n");
		  Debugger.Enter();
		END;
	      END;
	      Bitmap.Set(self.hasOwnContent, mOff);
	      
	      IF mu # self.lock THEN Thread.Release(mu); END;
	      EXIT; (* loop *)
	    ELSE
	      (* Read@cowpage: just use the frame at parent. *)
	      IF VMDebug.DebugMessage THEN
		IO.Put("pf: read hit@" & mObj.print() & ".\n");
	      END;
	      newProt := COWProtection;

	      IF mu # self.lock THEN Thread.Release(mu); END;
	      EXIT; (* loop *)
	    END;
	  END;
	END;
	
	nextmObj := mObj.shadow;
	
	(* We are still locking mObj here.. *)
	
	IF nextmObj = NIL THEN
	  (* We are at the dead end without being able to find
	     any memobject that caches the page.
	     
	     Now, we have to call the pager and bring the page into the
	     object at the root of the chain.
	     Note that "mobj" holds the root of the chain. *)
	  
	  IF VMDebug.DebugMessage THEN
	    IO.Put("pf: miss@root:" & mObj.print() & " off="
		   & Fmt.Int(mOff) & ".\n");
	  END;
	  PhysAddr.ChangeTag(newFrame, PhysAddr.Tag{mObj, mOff});
	  PhysAddr.ChangeProtection(newFrame, COWProtection);
	  frame := newFrame;
	  newFrame := NIL;
	  
	  INC(VMStat.pin);
	  CASE mObj.pager.pageIn(mOff, type, frame, newProt) OF
	  | PagerObject.ResultCode.Success =>
	    IF DebugOption.PhysAddr THEN
	      AssertChecksum(mObj, mOff, frame);
	    END;
	    mObj.cache.update(mOff, frame);
	  | PagerObject.ResultCode.AlreadyPagedIn =>
	    IO.Put("pf2: already paged in.\n");
	    PhysAddr.Deallocate(frame);
	    IF mu # self.lock THEN Thread.Release(mu); END;
	    Thread.Release(self.lock);
	    RETURN TRUE;
	    (* retry *)
	  ELSE
	    IF mu # self.lock THEN Thread.Release(mu); END;
	    Thread.Release(self.lock);
	    RAISE VMError.E(VMError.NoResponseFromPager);
	  END;
	  
	  IF mObj # self THEN
	    IF type = Trap.Write THEN
	      (* If access type is Write, then we could do cow copying here.
		 We can also start over from the beginning.
		 We take the latter approach because it is easier to avoid
		 deadlock. *)
	      mObj := self;
	    ELSE
	      (* Read or Exec access. We can just use the frame.
		 Note that we don't put the page in the cache because
		 the page is not mine. *)
	      IF mu # self.lock THEN Thread.Release(mu); END;
	      newProt := COWProtection;
	      EXIT; (* from the loop *)
	    END;
	  ELSE
	    (* mobj = self. This is the most common case. *)
	    IF mu # self.lock THEN Thread.Release(mu); END;
	    EXIT; (* loop *)
	  END;
	ELSE
	  (* No page cached in "mObj". Go up the chain. *)
	  mObj := nextmObj;
	END;
      EXCEPT
      | VMError.E(ec) =>
	IO.Put("mobj.pagein: " & Fmt.Int(ec) & ".\n");
	Thread.Release(mu);
	IF newFrame # NIL THEN
	  PhysAddr.Deallocate(newFrame);
	END;
	RETURN FALSE;
      END; (* try *)

      (* Release the current mobj. But still holding the lock to "self". *)
      IF mu # self.lock THEN Thread.Release(mu); END;
      
      (* replenish the newframe. newframe becomes nil only when
	 readonly-cow page was supplied at the root in this iteration,
	 and the access type is Write. *)
      IF newFrame = NIL THEN
	newFrame := PhysAddr.Allocate(PhysAddr.Tag{self, mOff});
      END;
    END; (* loop *)

    (* self is still locked here. *)
    <*ASSERT NOT Mutex.TryLock(self.lock)*>
    Thread.Release(self.lock);
    
    (*UnlockPage(self, mOff); *)

    (* When exception happens, control doesn't reach here, but we
       don't care because the frame has {NIL,0} tag, meaning that
       it is repossessed without any processing. *)
    IF newFrame # NIL THEN
      PhysAddr.Deallocate(newFrame);
    END;
    
    IF frame # NIL THEN 
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Request;

PROCEDURE CopyOnWrite (self: T; from: PageNumber; len: PageCount): T
  	RAISES {VMError.E} =
  VAR
    newChild: T;
    oldChild: T;
    chainEstablished := FALSE;
    itr: CacheObject.Iterator;
  BEGIN
    newChild := NEW(T).init(MIN(len, self.size-from),
			    DefaultPager.Create(self.size),
			    DefaultCache.Create(self.size),
			    self.name & "(cow " &
			    Fmt.Int(serialID) & ")");
    newChild.hasOwnContent := Bitmap.Create(self.size);
    
    INC(serialID);
    
    (* Create new cow chain *)
    REPEAT
      LOCK self.lock DO
	oldChild := self.copy;
	
	IF oldChild # NIL THEN
	  (* we are making more than 1 copy of "self". Interpose "newChild" in
	     middle. *)

	  (* Be cautious to avoid deadlock! *)
	  IF NOT Mutex.TryLock(oldChild.lock) THEN
	    IO.Put("memobj.cow: avoiding deadlock..\n");
	    Thread.Pause(1);
	    (* XXX spinlocking is not desireable. *)
	  ELSE
	    oldChild.shadow := newChild;
	    newChild.copy := oldChild;
	    newChild.shadow := self;
	    self.copy := newChild;
	    chainEstablished := TRUE;
	    Thread.Release(oldChild.lock);
	  END;	  
	ELSE
	  (* "newChild" is the 1st child of "self". *)
	  newChild.copy := NIL;
	  newChild.shadow := self;
	  self.copy := newChild;
	  chainEstablished := TRUE;
	END;
      END; (* lock *)
    UNTIL chainEstablished;

    (* Make all the pages in self <readonly,cow> *)
    VAR
      off: PageNumber;
      frame: PhysAddr.T;
    BEGIN
      LOCK self.lock DO 
	itr := self.cache.iterate();
	WHILE itr.next(off, frame) DO
	  PhysAddr.ChangeProtection(frame, COWProtection);
	END;
      END;
    END;
    
    RETURN newChild;
  END CopyOnWrite;

(* XXX not tested sorry - yas *)
PROCEDURE Fork (self : T) : T RAISES {VMError.E} =
  VAR
    new := NEW(T);
    itr: CacheObject.Iterator;
    off: PageNumber;
    orgP, newP: PhysAddr.T;
    tag: PhysAddr.Tag;
  BEGIN
    
    EVAL new.init(self.size, self.pager, NIL, self.name & "(copy)");
    new.cache := DefaultCache.Create(self.size); (* XXX *)

    LOCK self.lock DO 
      (* XXX we need to lock self.cache, but may be ok
         because we are locking self itself. *)
      itr := self.cache.iterate();
      WHILE itr.next(off, orgP) DO
	(* XXX eager copy here. *)
	tag := PhysAddr.GetTag(orgP);
	<*ASSERT off = tag.off*>
	newP := PhysAddr.Allocate(tag);
	PhysAddr.Copy(newP, orgP);
	new.cache.update(off, newP);
      END;
    END;
    RETURN new;
  END Fork;

PROCEDURE IsMapped (self: T): BOOLEAN =
  BEGIN
    RETURN self.mapEntries # NIL;
  END IsMapped;
  
PROCEDURE Print (self: T): TEXT =
  BEGIN
    RETURN self.name;
  END Print;
  
PROCEDURE StatMethod (self: T): Stat =
  VAR buf: Stat;
  BEGIN
    buf.virtualSize := self.size;
    buf.residentSize := self.cache.frameCount();
    RETURN buf;
  END StatMethod;

PROCEDURE Read (self: T; from: CARDINAL; VAR buf: ARRAY OF CHAR)
  RAISES {VMError.E} =
  VAR
    to := from + NUMBER(buf);
    bufOff := 0;
    frame: PhysAddr.T;
    prot: Protection.T;
    retry := 0;
  BEGIN
    LOOP
      TRY
	WHILE from < to AND retry < 20 DO
	  IF Request(self, from DIV CPU.PAGESIZE, Trap.Read,
		     frame, prot) THEN
	    WITH off = from MOD CPU.PAGESIZE,
	         len = MIN(to-from, CPU.PAGESIZE-off) DO 
	      PhysAddr.Read(frame, off, SUBARRAY(buf, bufOff, len));
	      INC(from, len);
	      INC(bufOff, len);
	    END;
	  ELSE
	    INC(retry);
	  END;
	END;
	EXIT;
      EXCEPT
      | VMError.E(ec) =>
	IF ec = VMError.StaleFrame THEN
	  INC(retry);
	ELSE
	  RAISE VMError.E(ec);
	END;
      END;
    END;
  END Read;
  
PROCEDURE Write (self: T; from: CARDINAL; READONLY buf: ARRAY OF CHAR)
  RAISES {VMError.E} =
  VAR
    to := from + NUMBER(buf);
    bufOff := 0;
    frame: PhysAddr.T;
    prot: Protection.T;
    retry := 0;
  BEGIN
    LOOP
      TRY
	WHILE from < to AND retry < 20 DO
	  IF Request(self, from DIV CPU.PAGESIZE, Trap.Write,
		     frame, prot) THEN
	    WITH off = from MOD CPU.PAGESIZE,
	         len = MIN(to-from, CPU.PAGESIZE-off) DO 
	      PhysAddr.Write(frame, off, SUBARRAY(buf, bufOff, len));
	      INC(from, len);
	      INC(bufOff, len);
	    END;
	  ELSE
	    INC(retry);
	  END;
	END;
	EXIT;
      EXCEPT
      | VMError.E(ec) =>
	IF ec = VMError.StaleFrame THEN
	  INC(retry);
	ELSE
	  RAISE VMError.E(ec);
	END;
      END;
    END;
  END Write;
  
PROCEDURE Access (self: T; from, len: CARDINAL;
		  proc: PROCEDURE (pos: CARDINAL;
				   VAR buf: ARRAY OF CHAR))
  RAISES {VMError.E} =
  VAR
    to := from + len;
    frame: PhysAddr.T;
    prot: Protection.T;
    retry := 0;
    bufOff, bufLen: CARDINAL;
  PROCEDURE Callback (VAR buf: PhysAddr.Content) =
    BEGIN
      proc(from, SUBARRAY(buf, bufOff, bufLen));
    END Callback;
  BEGIN
    LOOP
      TRY
	WHILE from < to AND retry < 20 DO
	  IF Request(self, from DIV CPU.PAGESIZE, Trap.Write,
		     frame, prot) THEN
	    bufOff := from MOD CPU.PAGESIZE;
	    bufLen := MIN(to-from, CPU.PAGESIZE-bufOff);
	    PhysAddr.Access(frame, Callback);
	    INC(from, bufLen);
	    INC(bufOff, bufLen);
	  ELSE
	    INC(retry);
	  END;
	END;
	EXIT;
      EXCEPT
      | VMError.E(ec) =>
	IF ec = VMError.StaleFrame THEN
	  INC(retry);
	ELSE
	  RAISE VMError.E(ec);
	END;
      END;
    END;
  END Access;
  
PROCEDURE Repossess (p: PhysAddr.T) =
  VAR
    orgP := p;
    tag : PhysAddr.Tag;
    dummyFrame: PhysAddr.T;
    dummyProt: Protection.T;
    rc: PagerObject.ResultCode;
    sum: INTEGER;
  BEGIN
    TRY
      tag := PhysAddr.GetTag(p);
    
      NARROW(tag.obj, T).cache.chooseVictim(p);
      IF p # orgP THEN
	tag := PhysAddr.GetTag(p);
      END;

      WITH mObj = NARROW(tag.obj, T), off = tag.off DO
	IF VMDebug.DebugMessage THEN
	  IO.Put("purge " & mObj.print() & "("
		 & Fmt.Int(tag.off*CPU.PAGESIZE) & ")\n");
	END;
	LOCK mObj.lock DO 
	  (* Update the checksum entry. *)
	  IF VMDebug.DoChecksum THEN
	    VAR
	      entry := mObj.mapEntries;
	    BEGIN
	      sum := VMDebug.CalculateChecksum(p);
	      WHILE entry # NIL DO
		IF DebugOption.PhysAddr AND entry.mObj # mObj THEN
		  Debugger.Enter();
		END;
		entry.checksum[off - entry.mOff] := sum;
		entry := entry.link;
	      END;
	    END;
	  END;
	  
	  rc := PageOut(mObj, off, p);
	  CASE rc OF
	  | PagerObject.ResultCode.Success =>
	    mObj.cache.invalidate(off);
	  | PagerObject.ResultCode.AlreadyPagedOut =>
	    IO.Put("page out: already paged out.\n");
	    IF DebugOption.PhysAddr
	       AND mObj.cache.lookup(off, 0, dummyFrame, dummyProt)THEN
	      Debugger.Enter();
	    END;
	  ELSE
	    IO.Put("page out error: " & Fmt.Int(ORD(rc)) & ".\n");
	  END;
	  
	  IF VMDebug.DoChecksum THEN
	    WITH x = VMDebug.CalculateChecksum(p) DO
	      IF sum # x THEN
		Debugger.Enter();
	      END;
	    END;
	  END;
	END;
      END;
    EXCEPT
    | VMError.E =>
      Debugger.Enter();
    END;
  END Repossess;

FUNCTIONAL PROCEDURE RepossessGuard (p: PhysAddr.T): BOOLEAN =
  VAR tag := PhysAddr.GetTag(p);
  BEGIN
    RETURN tag.obj # NIL AND TYPECODE(tag.obj) = TYPECODE(T);
  END RepossessGuard;

(* Default implementation of destroyed does nothing. *)
PROCEDURE Destroyed (<*UNUSED*>mObj: T) =
  BEGIN
  END Destroyed;
  
FUNCTIONAL
PROCEDURE ImposedDestroyGuard (key, mObj: T): BOOLEAN =
  BEGIN
    RETURN key = mObj;
  END ImposedDestroyGuard;
  
PROCEDURE AuthorizeDestroy (<*UNUSED*>a: Auth.T;
			    k: Auth.Key; binding: REFANY): BOOLEAN =
  BEGIN
    IF k = NIL OR NOT ISTYPE(k, T) THEN
      IO.Put("MemoryObject auth : key is not MemoryObject.T\n");
      RETURN FALSE;
    END;
    TRY
      EVAL Dispatcher.ImposeGuard(binding, ImposedDestroyGuard, k);
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("MemoryObject auth : can't impose guard("
	     & Fmt.Int(ORD(ec)) & ").\n");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END AuthorizeDestroy;
  
BEGIN
  TRY
    EVAL Dispatcher.InstallHandler(PhysAddr.Repossess,
				   RepossessGuard, Repossess,
		   options := Dispatcher.Options{Dispatcher.Opt.First});
    Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
					     authorize := AuthorizeDestroy),
					 Destroyed,
					 THIS_MODULE());
  EXCEPT
  | Dispatcher.Error(ec) =>
    IO.Put("MemoryObject init : dispatcher error(" & Fmt.Int(ORD(ec)) & ".\n");
  END;

END MemoryObject.


