(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 14-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made PhysAddr.T an object, and allowed subtyping of it.
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed Deallocate() so that pmap_remove_all is called regardless
 *	of page.state.  (per Yasushi's input)
 *
 * 31-May-97  David Becker at the University of Washington
 *	Use new Sal interfaces
 *
 * 21-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	A bit of cleanup. Added GetVictims.
 * 02-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Eliminated the use of MachinePmap.
 * 26-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Dequeue leaves "mu" held, and Enqueue releases the lock.  This
 *	way, we can reduce the # of lockings to 1 per typical physaddr
 *	operation.
 * 23-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added lots of locks.
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Clean up.
 *
 * 15-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	PhysFreeMem is merged into PhysAddr. PhysAddr events(reclaim,
 *      repossess) are
 *	actually called from PhysFreeMem,
 *      and since they are now asynchronous, the
 *	result handler is needed, and it has to be defined in PhysAddr. To
 *	avoid these complications, I merged the two.
 *
 * 7-Apr-96 oystr at the University of Washington
 *	Added AllocateIOSpace routine.  Changed Deallocate
 *	to recognize IO space dummy headers.
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Marked unused definition to disable warnings. 
 *
 * 02-Nov-94  Stefan Savage (savage) at the University of Washington
 *	Created
 *)

UNSAFE MODULE PhysAddr EXPORTS PhysAddr, PhysAddrPrivate;
IMPORT PhysAddr, PhysAddrRep;
IMPORT AtomicOpsExtern;
IMPORT Strand;
IMPORT DebugOption;
IMPORT IO, Fmt;
IMPORT Thread;
IMPORT MachineMem;
IMPORT SalExtern;
IMPORT VMError;
IMPORT Protection;
IMPORT Sal;
IMPORT Dispatcher;
IMPORT Debugger;
IMPORT Mutex;
IMPORT ArrayUtils;
IMPORT Word;
CONST DebugMessage = FALSE;

CONST StateName = ARRAY State OF TEXT {
				       "transient",
				       "dead",
				       "zombie",
				       "free",
				       "reclaimed",
				       "active",
				       "pin"
				       };
  
TYPE
  IOAddr = T BRANDED OBJECT
    endAddress: Address;
  END;
  FreeQueue = RECORD
    prev, next: UNTRACED REF FreeQueue;
  END;
  
VAR
  mu: MUTEX;
  (* "mu" guards everything in this module; it is used to synchronize
     "Lock"/"Unlock", and it is also used to guard queues.
     We take advantage of the fact is that "Lock" and queue ops use
     same mutex, and in most cases, page locking is done without
     any mutex overhead. See also DequeuePage. *)

  nFreePages: CARDINAL;
  freeFrameMu: MUTEX;
  freeFrameCond: Thread.Condition;
  (* The above two are used to wait and signal when the free page queue
     becomes short. *)

  freeQueue: FreeQueue;
  (* All the free frames are chained from "freeQueue", using first two
     words in the frame as the queue header. *)
  
  requestThreshold: INTEGER;
  reclaimThreshold: INTEGER;
  repossesThreshold: INTEGER;

  pageList: ARRAY [State.Zombie .. State.StronglyPinned] OF RECORD
    head: T;
    size: Size;
  END;
  (* Each entry in "pageList" holds all the pages in that state.
     Note that free frames are not held in "pageList"; rather, it's
     queued into "freeQueue" using a first few bytes of each frame as the
     queue header. There are two purposes of not queueing free frames into
     "pageList". One is to save memory. Free frames need not be allocated
     traced memory. The other reason is to allow subtyping of PhysAddr.Ts. *)

VAR  
  repossessAsynchAlias: PROCEDURE (p: T);
    
PROCEDURE FillGarbage (VAR x: Content) =
  BEGIN
    ArrayUtils.Set(x, VAL(16_9c, CHAR));
  END FillGarbage;
(*
   Allocates a page of physical addresses.
 *)
PROCEDURE InitSub (newPage: T; VAR tag: Tag; state: State) =
  VAR
    oldPage: T;
  BEGIN
    (*
     * Simple FIFO policy for physical page reclamation
     *)
    WITH nReclaimedPages = pageList[State.Reclaimed].size DO 
      WHILE nReclaimedPages < reclaimThreshold
	AND nFreePages < repossesThreshold * 2 DO
	
	oldPage := DequeuePage(State.Active);
	IF oldPage = NIL THEN
	  IO.PutError("free page well went dry.\n");
	  Debugger.Enter();
	END;
	
	SalExtern.pmap_remove_all(oldPage.addr, TRUE);
	EnqueuePage(oldPage, State.Reclaimed);
	PhysAddr.Reclaim(oldPage);
	Unlock(oldPage);
	(* remember to make this async or put it in a thread *)
      END;

      IF nFreePages < repossesThreshold THEN
	oldPage := DequeuePage(State.Reclaimed);
	IF oldPage = NIL THEN
	  IO.PutError("reclaimed queue empty.\n");
	  Debugger.Enter();
	END;
	oldPage.valid := FALSE;
	EnqueuePage(oldPage, State.Zombie);
	Unlock(oldPage);
	
	IF DebugMessage THEN
	  IO.Put("reclaim -> reposses"& Print(oldPage) &"\n");
	END;
	repossessAsynchAlias(oldPage);
      END;
      
      LOCK freeFrameMu DO 
	IF nFreePages < repossesThreshold - 10 THEN
	  (* We are really running out of memory. *)
	  (*IO.Put("waiting for memory to be freed..."); *)
	  WHILE nFreePages < repossesThreshold -10 DO
	    Thread.Wait(freeFrameMu, freeFrameCond);
	  END;
	  (* IO.Put(".. ok.\n"); *)
	END;
	
	newPage.addr := DequeueFromFreeQueue();
	IF newPage.addr = 0 THEN
	  Debugger.Enter();
	END;
      END;
    END; (* WITH *)

    IF DebugOption.PhysAddr THEN
      CheckFreeQueueSanity();
    END;
    
    newPage.tag := tag;
    newPage.prot := Protection.All;
    newPage.isIO := FALSE;
    newPage.locked := 1;
    newPage.valid := TRUE;
    newPage.state := State.Transient;

    Thread.Acquire(mu); (* dummy acquire to make EnqueuePage happy. *)
    EnqueuePage(newPage, state);
    Unlock(newPage);
    
    IF DebugOption.PhysAddr THEN
      TRY
	Access(newPage, FillGarbage);
      EXCEPT
      | VMError.E =>
	IO.Put("Physaddr.Allocate: ???.\n");
      END;
    END;
  END InitSub;

(* RepossesCont is a result handler for Reposses event. Actually, this
   proc acts as a contitunation of the event hence the name.
   It's task is to return the frame into the free queue. *)
PROCEDURE RepossessCont (last: BOOLEAN; <*UNUSED*>VAR arg: REFANY; page: T) =
  BEGIN
    <*ASSERT NOT page.valid*>
    IF page.state # State.Zombie THEN
      (* A reposses handler called Deallocate. Nothing to do here. *)
      <*ASSERT page.state = State.Dead*>
      RETURN;
    END;
    IF NOT last THEN
      (* We still have handlers to call. *)
      RETURN;
    END;
    
    IF DebugMessage THEN
      IO.Put("repossess cont " & Print(page) & ".\n");
    END;
    Deallocate(page);
  END RepossessCont;
  
PROCEDURE AllocateIOSpace (start, end: Address): T =
  VAR newP := NEW(IOAddr);
  BEGIN
    newP.isIO := TRUE;
    newP.tag.obj := NIL;
    newP.tag.off := 0;
    newP.valid := TRUE;
    newP.addr := start;
    newP.endAddress := end;
    RETURN newP;
  END AllocateIOSpace;
  
(*
  Return page to the pool of free memory.  
 *)
PROCEDURE Deallocate (page: T) =
  BEGIN
    (* Make the original page invalid so that it can't be used any more *)
    IF page.isIO THEN
      (* IO page is not linked to anywhere, so we don't have anything more *)
      <*ASSERT ISTYPE(page, IOAddr)*>
      RETURN;
    END;

    (* Here, we can't just lock the page because of possible deadlock.
       "Deallocate" is usually called with mobject locked. If this
       very page was chosen as pageout victim, we might be locking
       this page first, and then try to lock mobject. Therefore we have
       potential resource wait cycle.

       XXX I suspect this is not neded any more -- yasushi. *)
    IF NOT TryLock(page) THEN
      (* Here, we should really fork off a thread to free the thing. *)
      IO.Put("deallocate: lock contension. just returning.\n");
      Lock(page);
      IO.Put("lock contension end.\n");
      RETURN;
    END;
    
    IF page.valid OR page.state = State.Zombie THEN
      LOCK freeFrameMu DO 
	page.valid := FALSE;
	SalExtern.pmap_remove_all(page.addr, TRUE);
	UnlinkFromQueue(page);
	
	page.state := State.Dead;
	page.next := NIL;
	page.prev := NIL;
	(* page is no longer a live object. It should be gc'ed unless
	   someone is holding onto the page. *)
	
	(* Enqueue the frame back to the free queue. *)
	EnqueueInFreeQueue(page.addr);
	Thread.Release(mu); (* release the mutex held in UnlinkFromQueue. *)
	Thread.Signal(freeFrameCond);
	
      END;
    END;
    Unlock(page);
  END Deallocate;

PROCEDURE ChangeStatePrivate (page: T; state: State) =
  BEGIN
    IF DebugOption.PhysAddr AND page.locked = 0 THEN
      Debugger.Enter();
    END;

    IF page.isIO THEN
      (* IO page is not linked to anywhere, so we don't have anything more *)
      <*ASSERT ISTYPE(page, IOAddr)*>
      RETURN;
    END;
    IF DebugMessage THEN
      IF state # page.state THEN
	IO.Put(StateName[page.state] & " -> " & StateName[state]
	       & Print(page) & "\n");
      END;
    END;

    IF page.state = state THEN
      Requeue(page);
    ELSE
      UnlinkFromQueue(page);
      (* Here we assume that nothing wrong happens if we unmap a memory
	 that isn't mapped anywhere *)
      IF state < State.Active THEN
	SalExtern.pmap_remove_all(page.addr, TRUE);
      END;
      EnqueuePage(page, state);
    END;
  END ChangeStatePrivate;

FUNCTIONAL
PROCEDURE GetState (page: T): State  =
  BEGIN
    RETURN page.state;
  END GetState;
  
PROCEDURE ChangeState (page: T; state: State) RAISES {VMError.E} =
  BEGIN
    CASE state OF
    | State.Active, State.Reclaimed, State.Zombie =>
      Lock(page);
      
      IF page.valid THEN
	IF page.state = state THEN
	  Requeue(page);
	ELSE
	  UnlinkFromQueue(page);
	  IF state = State.Reclaimed THEN
	    SalExtern.pmap_remove_all(page.addr, TRUE);
	  ELSIF state = State.Zombie THEN
	    page.valid := FALSE;
	    SalExtern.pmap_remove_all(page.addr, TRUE);
	  END;
	  EnqueuePage(page, state);
	END;
	Unlock(page);
      ELSE
	Unlock(page);
	RAISE VMError.E(VMError.StaleFrame);
      END;
    ELSE
      RAISE VMError.E(VMError.PrivilegedOperation);
    END;
  END ChangeState;

(* Bring "page" to the front of the queue the page is now in.
   This is same as unlinkfromqueue, then enqueuepage. *)
PROCEDURE Requeue (page: T) =
  BEGIN
    LOCK mu DO
      WITH list = pageList[page.state] DO
	page.next.prev := page.prev;
	page.prev.next := page.next;
	page.prev := list.head;
	page.next := list.head.next;
	list.head.next.prev := page;
	list.head.next := page;
      END;
    END;
  END Requeue;

PROCEDURE CheckFreeQueueSanity () =
  VAR
    virt: UNTRACED REF FreeQueue;
    n: CARDINAL := 0;
  BEGIN
    LOCK freeFrameMu DO
      virt := freeQueue.next;
      WHILE virt # ADR(freeQueue) DO
	IF LOOPHOLE(virt, INTEGER) MOD MachineMem.PAGESIZE # 0 THEN
	  IO.PutError("PhysAddr.CheckFreeQueueSanity: virt not aligned.\n");
	  Debugger.Enter();
	END;
	INC(n);
	virt := virt.next;
      END;
      IF n # nFreePages THEN
	IO.PutError("PhysAddr.CheckFreeQueueSanity: " & 
		    Fmt.Int(n) & "<-> " & Fmt.Int(nFreePages) & ".\n");
	Debugger.Enter();
      END;
    END;
  END CheckFreeQueueSanity;
  
PROCEDURE EnqueueInFreeQueue (phys: Word.T) =
  VAR
    virt := LOOPHOLE(SalExtern.phys_to_kseg(phys), UNTRACED REF FreeQueue);
  BEGIN
    <*ASSERT Mutex.IsLocked(freeFrameMu)*>
    virt.prev := ADR(freeQueue);
    virt.next := freeQueue.next;
    freeQueue.next.prev := virt;
    freeQueue.next := virt;
    INC(nFreePages);
  END EnqueueInFreeQueue;

PROCEDURE DequeueFromFreeQueue (): Word.T =
  VAR virt: UNTRACED REF FreeQueue;
  BEGIN
    <*ASSERT Mutex.IsLocked(freeFrameMu)*>
    virt := freeQueue.next;
    IF virt = ADR(freeQueue) THEN
      IO.PutError("free queue empty.");
      Debugger.Enter();
    END;
    freeQueue.next := virt.next;
    virt.next.prev := ADR(freeQueue);
    DEC(nFreePages);
    RETURN SalExtern.kseg_to_phys(LOOPHOLE(virt, Word.T));
  END DequeueFromFreeQueue;
  
(* Unlink the page from whatever queue it is in.
   Pre: page is locked, "mu" is not locked.
   Post: page is locked, "mu" is locked.
   *)
PROCEDURE UnlinkFromQueue (page: T) =
  BEGIN
    <*ASSERT NOT TryLock(page)*>
    Thread.Acquire(mu);
    WITH list = pageList[page.state] DO
      page.next.prev := page.prev;
      page.prev.next := page.next;
      DEC(list.size);
      page.state := State.Transient;
      IF DebugOption.PhysAddr THEN
	page.next := NIL;
	page.prev := NIL;
      END;
    END;
  END UnlinkFromQueue;

(* Put the "page" into the "state" queue.
 Pre: "page" is in transient state, locked.
 Post: "page" is still locked. *)
<*INLINE*>
PROCEDURE EnqueuePage (page: T; state: State) =
  BEGIN
    IF DebugOption.PhysAddr THEN
      IF Mutex.TryLock(mu) THEN
	Debugger.Enter();
      END;
      IF page = NIL OR TryLock(page) THEN
	Debugger.Enter();
      END;
      IF page.state # State.Transient OR (page.locked = 0) THEN 
	Debugger.Enter();
      END;
      IF page.next # NIL OR page.prev # NIL THEN
	Debugger.Enter();
      END;
    END;

    WITH list = pageList[state] DO
      (* insert at the head *)
      page.prev := list.head;
      page.next := list.head.next;
      list.head.next.prev := page;
      list.head.next := page;
      INC(list.size);
      page.state := state;
    END;
    Thread.Release(mu);
  END EnqueuePage;

(* Take out a page that is in "state".
   Pre: "mu" is not locked.
   Post: page is locked, "mu" is locked if page is not NIL.  *)
PROCEDURE DequeuePage (state: State): T  =
  VAR
    page: T;
  BEGIN
    WITH list = pageList[state] DO
      LOOP 
	Thread.Acquire(mu);
	IF list.size = 0 THEN
	  Thread.Release(mu);
	  RETURN NIL;
	END;

	(* Take off an element from the tail *)
	page := list.head.prev;

	IF TryLock(page) THEN 
	  page.next.prev := page.prev;
	  page.prev.next := page.next;
	  
	  DEC(list.size);
	  
	  IF DebugOption.PhysAddr THEN
	    page.prev := NIL;
	    page.next := NIL;
	    IF page.state # state THEN
	      IO.Put("page.state # state.\n");
	      Debugger.Enter();
	    END;
	  END;
	  page.state := State.Transient;
	  RETURN page;
	ELSE
	  (* avoid deadlock.. *)
	  IO.Put("Physaddr.DequeuePage: deadlock alert!!\n");
	  Thread.Release(mu);
	  Thread.Pause(10000);
	  (* try again. *)
	END;
      END;
    END;
  END DequeuePage;
  
FUNCTIONAL EPHEMERAL
PROCEDURE FreeMem (): Size =
  BEGIN
    RETURN nFreePages;
  END FreeMem;
  
(* XXX this has to be darn fast. Help, Gun! *)
PROCEDURE Lock (t: T) =
  BEGIN
    WHILE AtomicOpsExtern.TryLock(t.locked) = FALSE DO 
      Strand.Yield();
    END;
  END Lock;
  
PROCEDURE Unlock (t: T) =
  BEGIN
    IF DebugOption.PhysAddr AND t.locked = 0 THEN
      Debugger.Enter();
    END;
    AtomicOpsExtern.Unlock(t.locked);
  END Unlock;
  
(* Returns TRUE if the lock is established, FALSE if the lock was
   taken by someone else. *)
PROCEDURE TryLock (t: T): BOOLEAN =
  BEGIN
    RETURN AtomicOpsExtern.TryLock(t.locked);
  END TryLock;
  
PROCEDURE GetParams (VAR p: Params) =
  BEGIN
    p.requestThreshold := requestThreshold;
    p.reclaimThreshold := reclaimThreshold;
    p.repossesThreshold := repossesThreshold;
  END GetParams;
  
PROCEDURE SetParams (READONLY p: Params) =
  BEGIN
    requestThreshold := p.requestThreshold;
    reclaimThreshold := p.reclaimThreshold;
    repossesThreshold := p.repossesThreshold;
  END SetParams;

PROCEDURE GetStat (VAR p: Stat) =
  BEGIN
    FOR i := FIRST(State) TO LAST(State) DO
      IF i = State.Free THEN
	IF DebugOption.PhysAddr THEN
	  CheckFreeQueueSanity();
	END;
	p.sizes[i] := nFreePages;
      ELSIF i < FIRST(pageList) OR i > LAST(pageList) THEN
	p.sizes[i] := 0;
      ELSE
	p.sizes[i] := pageList[i].size;
      END;
    END;
  END GetStat;

PROCEDURE Initialize (t: T; tag: Tag): T =
  BEGIN
    InitSub(t, tag, State.Active);
    RETURN t;
  END Initialize;
  
PROCEDURE Allocate (tag: Tag): T  =
  VAR t := NEW(T);
  BEGIN
    InitSub(t, tag, State.Active);
    RETURN t;
  END Allocate;

PROCEDURE AllocatePinnedPage (): T =
  VAR
    t := NEW(T);
    tag := Tag{NIL, 0};
  BEGIN
    InitSub(t, tag, State.StronglyPinned);
    RETURN t;
  END AllocatePinnedPage;

FUNCTIONAL  
PROCEDURE GetTag (t: T): Tag =
  BEGIN
    IF NOT t.valid AND t.state # State.Zombie THEN
      RETURN Tag{NIL, 0};
    END;
    RETURN t.tag;
  END GetTag;
  
EPHEMERAL
PROCEDURE ChangeTag (t: T; tag: Tag) =
  BEGIN
    t.tag := tag;
  END ChangeTag;

FUNCTIONAL
PROCEDURE GetProtection (t: T): Protection.T =
  BEGIN
    IF NOT t.valid AND t.state # State.Zombie THEN
      RETURN Protection.T{FALSE, FALSE, FALSE, 0};
    END;
    RETURN t.prot;
  END GetProtection;

PROCEDURE ChangeProtection (t: T; prot: Protection.T) RAISES {VMError.E} =
  BEGIN
    Lock(t);
    IF NOT t.valid THEN
      Unlock(t);
      RAISE VMError.E(VMError.StaleFrame);
    END;
    SalExtern.pmap_page_protect(t.addr, prot);
    
    t.prot := prot;
    Unlock(t);
  END ChangeProtection;
  
PROCEDURE Print (t: T):TEXT = 
  VAR
    result : TEXT;
  BEGIN
    TYPECASE t OF
    | IOAddr(page) =>
      result := "("&Fmt.Unsigned(page.addr)&","&
                    Fmt.Unsigned(page.endAddress)&")";
    ELSE
      result := "("&Fmt.Unsigned(t.addr) & ")";
    END;
    RETURN result;
  END Print;


PROCEDURE Copy (dest, src: T) RAISES {VMError.E} =
  BEGIN
    (* XXX no locking now *)
    IF NOT dest.valid OR dest.isIO OR NOT src.valid OR src.isIO THEN
      RAISE VMError.E(VMError.StaleFrame);
    END;

    SalExtern.pmap_copy_page(src.addr, dest.addr);
  END Copy;

PROCEDURE Read (t: T; off: CARDINAL; VAR buf: ARRAY OF CHAR)
  RAISES {VMError.E} =
  VAR len : INTEGER;
  BEGIN
    Lock(t);
    (* XXX no locking now; in fact, we can't lock here because of
       deadlock. *)
    IF NOT t.valid AND t.state # State.Zombie THEN
      IO.Put("access: invalid and non-zombie.\n");
      Unlock(t);
      RAISE VMError.E(VMError.StaleFrame);
    END;
    
    IF t.isIO THEN
      WITH io = NARROW(t, IOAddr) DO
	len := MIN(io.endAddress - io.addr - off, NUMBER(buf));
      END;
    ELSE
      len := MIN(MachineMem.PAGESIZE - off, NUMBER(buf));
    END;
    IF len <= 0 THEN RETURN; END;
    
    SalExtern.copy_from_phys(t.addr+off, 
				LOOPHOLE(ADR(buf[0]), INTEGER), len);
    Unlock(t);
  END Read;

PROCEDURE Write (t: T; off: CARDINAL; READONLY buf: ARRAY OF CHAR)
  RAISES {VMError.E} =
  VAR len : INTEGER;
  BEGIN
    Lock(t);
    IF NOT t.valid AND t.state # State.Zombie THEN
      IO.Put("access: invalid and non-zombie.\n");
      Unlock(t);
      RAISE VMError.E(VMError.StaleFrame);
    END;
    
    IF t.isIO THEN
      WITH io = NARROW(t, IOAddr) DO
	len := MIN(io.endAddress - io.addr - off, NUMBER(buf));
      END;
    ELSE
      len := MIN(MachineMem.PAGESIZE - off, NUMBER(buf));
    END;
    IF len <= 0 THEN RETURN; END;
    SalExtern.copy_to_phys(LOOPHOLE(ADR(buf[0]), INTEGER),
				 t.addr+off, len);
    Unlock(t);
  END Write;

PROCEDURE Access (t: T; callback: PROCEDURE (VAR buf: Content))
  RAISES {VMError.E} =
  VAR
    virt := SalExtern.phys_to_kseg(t.addr);
  BEGIN
    Lock(t);
    
    IF NOT t.valid AND t.state # State.Zombie THEN
      IO.Put("access: invalid and non-zombie.\n");
      Unlock(t);
      RAISE VMError.E(VMError.StaleFrame);
    END;

    (* XXX try-except-else needed. *)
    callback(LOOPHOLE(virt, UNTRACED REF Content)^);
    Unlock(t);
  END Access;

PROCEDURE GetVictims (tag: REFANY; VAR frames: ARRAY OF T): CARDINAL =
  VAR
    frame: T;
    nPages := pageList[State.Reclaimed].size;
    n := 0;
  BEGIN
    FOR i := 0 TO nPages-1 DO
      frame := DequeuePage(State.Reclaimed);
      IF frame = NIL THEN EXIT; END;
      IF frame.tag.obj = tag THEN
	frame.valid := FALSE;
	EnqueuePage(frame, State.Zombie);
	Unlock(frame);
	frames[n] := frame;
	INC(n);
	IF n >= NUMBER(frames) THEN EXIT; END;
      ELSE
	EnqueuePage(frame, State.Active); (* XXX temp hack *)
	Unlock(frame);
      END;
    END;
    RETURN n;
  END GetVictims;
  
(*
 * Default event handlers  for PhysAddr reclamation protocol
 *)

PROCEDURE Reclaim (<*UNUSED*>frame: T) =
  BEGIN
  END Reclaim;

PROCEDURE Repossess (<*UNUSED*>frame: T) =
  BEGIN
  END Repossess;

(*
 *	Initialize the freePageList to hold all the physical pages
 *	All static attributes should be initialized here
 *	Future: If we make language references pools/lists it should be
 *	        done here (we only have free list now)
 *)

PROCEDURE Init (verbose: BOOLEAN;
		<*UNUSED*>tracedHeapStart: Address;
		<*UNUSED*>tracedHeapSize: Size) =
  VAR
    numPages              : INTEGER;
    numHeapPages	  : INTEGER;
    physMemSize           : Size; (* in bytes *)
    currentAddr           : Size;
  BEGIN
    mu := NEW(MUTEX);
    freeFrameMu := NEW(MUTEX);
    freeFrameCond := NEW(Thread.Condition);

    FOR i := FIRST(pageList) TO LAST(pageList) DO
      pageList[i].head := NEW(T);
      pageList[i].head.next := pageList[i].head;
      pageList[i].head.prev := pageList[i].head;
      pageList[i].size := 0;
    END;

    (* BUGBUG Hack for testing *)
    physMemSize := Sal.GetPhysicalMemorySize();
    physMemSize := physMemSize;
    numPages := MachineMem.BytesToPages(physMemSize);
    currentAddr := Sal.GetPhysicalMemoryStart();
    
    (*
     * Totally arbitrary thresholds at which the request, reclaim and reposses
     * events fired. 
     *)
    requestThreshold := numPages DIV 4;
    reclaimThreshold := numPages DIV 12;
    repossesThreshold := reclaimThreshold - 10;

    (* Create an empty free frame queue *)
    freeQueue.next := ADR(freeQueue);
    freeQueue.prev := ADR(freeQueue);
    nFreePages := 0;

    (* Enqueue all the frames into freeQueue. *)
    LOCK freeFrameMu DO
      FOR i := 1 TO numPages DO
	EnqueueInFreeQueue(currentAddr);
	INC(currentAddr, MachineMem.PAGESIZE);
      END;
    END;

    TRY
      Dispatcher.InstallResultHandler(Repossess, RepossessCont,
				      module := THIS_MODULE());
      repossessAsynchAlias := Dispatcher.GetAsynchAlias(Repossess);
    EXCEPT
    | Dispatcher.Error(e) =>
      IO.Put("PhysAddr.Init: disp error " & Fmt.Int(ORD(e)) & ".\n")
    END;

    verbose := FALSE;
    IF verbose THEN
      IO.Put("PhysAddr initialized.  numFreePages = " &
	     Fmt.Int(nFreePages) &
	     ",numHeapPages = "&Fmt.Int(numHeapPages)&"\n");
    END;
  END Init;

BEGIN
END PhysAddr.

