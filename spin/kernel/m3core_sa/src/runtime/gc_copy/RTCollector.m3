(* Copyright (C) 1993, Digital Equipment Corporation         *)
(* All rights reserved.                                      *)
(* See the file COPYRIGHT for a full description.            *)
(*                                                           *)
(* portions Copyright 1996, Critical Mass, Inc.              *)
(*                                                           *)
(*| Last modified on Fri Apr 26 10:29:11 PDT 1996 by heydon  *)
(*|      modified on Sat Nov 19 09:37:57 PST 1994 by kalsow  *)
(*|      modified on Fri Aug  5 14:04:35 PDT 1994 by jdd     *)
(*|      modified on Wed Jun  2 15:00:17 PDT 1993 by muller  *)
(*|      modified on Wed Apr 21 13:14:37 PDT 1993 by mcjones *)
(*|      modified on Wed Mar 10 11:01:47 PST 1993 by mjordan *)

(*
 * HISTORY
 * 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 *	Added parameterized heapsize code from Przemek.
 *
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added histogram initialization, memory utilization reporting, and
 *	a stub for DistributeMemory.  Modified meaning of spies.
 *
 * 14-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added initialization of type security features.
 *
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *      Use untraced heap for internal data strucutres. Some cleanup.
 *
 * 28-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	commented out everything referencing doRefCount
 *
 * 18-Jan-97  Richard Robinson (robinson) at the University of Washington
 *	GetMoreStatistics traverses the heap counting objects, fragments,
 *      and unused space. Movement of mobile objects off pinned pages works,
 *	unless VM support is enabled. Keep length of free chunks in link field.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Brought the code closer to SRC 3.6 release.  Enabled incremental,
 *	generational, and background collection.  More measurements.
 *	Made some procedures functional. Some clean-up. More robust
 *	method of triggering collections.
 *	
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	More counters to keep track of the GC operations.  Added 
 *	ResetStat().  Weakref-ed objects are cleaned up even if they
 *      are strongref-ed, this will be generalized to counted weakref-s.
 *
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *      Brought the code closer by using the SRC method of calculating
 *	page number for and address and vice versa.  Fixed the problems
 *	caused by very high addresses (which were treated as negative
 *	numbers in the SRC code) by using Word.RightShift to do divisions.
 *	Made MinAddress and MaxAddress FUNCTIONAL.  In the process, removed
 *	PAD fields, they can be reintroduced if we put back the optimization
 *	for free list length.  Cleaned-up and added new debugging code.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Added padding to descriptor to please 32-bit machines.
 *	Changed to new interrupt scheme including both classes and levels.
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added SweepClosure and SweepUntracedHeapCl.  Got rid of typeidx.
 *
 * 15-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed RTStrongRef.IsThere to StrongRef.IsThere.
 *
 * 04-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled turning the sweeping of untraced heap on and off.
 *
 * 22-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Moved DumpPages to RTHeapDebug.
 *
 * 21-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaning references in old weakTable and disposing it.
 *
 *	Added test in the NoteStackLocations() function to ignore refs
 *	that are in the weakref table.  
 *
 * 20-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	use RTMachine.PreferredAlignment instead of 64
 *
 * 06-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	use ALIGNED 64 FOR for type WeakRefAB
 *
 * 21-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Turned assertions into calls to RTOS.Crash to stop the system
 *	when heap consistency is violated.
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Measurements by Spy used only if Debug.DoTimings is on.
 *	Added sweeping of untraced heap.  Added tracing and statistics 
 *	of collector operations and ambiguous roots.
 *
 * 31-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Removed Stat.
 *
 * 23-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed sanity checking and enabled sanity checks for each 
 *	garbage collection.  Removed a bunch of commented out calls to RTIO.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Added GetStatistics. Removed TEXT and Fmt reliance. (Should
 *	 never have had this kind of low level printing from within RT).
 *
 * 30-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *	Added MinAddress and MaxAddress so that RTHeapStats can know
 *	what are valid addresses. Added StatText function which
 *      returns stat information as a string with HTML commands.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Modified to accomodate the split of RT0.Typecode into
 *	RT0.Typecode and RT0.Typeidx
 *
 * 15-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added free block length field to page descriptor, removed
 *	scanning of the free page list at allocation time.
 *
 * 10-Feb-95  Przemek Pardyak (pardy) at the University of Washington
 *	Ported to SPIN
 *)

(* Notes on SPIN standalone support 
 *   - Introduced Spy support
 *   - eliminated use of FLOATs for minPrefixAvgCost, threshold computation.
 *	and MinNewFactor.
 *   - compute InitialBytes as VAR INTEGER rather than CONST.
 *)

UNSAFE MODULE RTCollector EXPORTS RTCollector, RTCollectorSRC,
                                  RTHeapRep, RTWeakRef;

IMPORT RT0, RT0u, RTHeapEvent, RTHeapDep, RTHeapMap, RTMachine;
IMPORT RTMisc, RTOS, RTParams, RTPerfTool, RTProcess, RTType;
IMPORT Word, Cstdlib, Thread, ThreadF, RTHeapStats, RTRefCount;
IMPORT RTHisto;

FROM RT0 IMPORT Typecode, TypeDefn;
FROM RTIO IMPORT PutText, PutInt, PutAddr, PutBoolean, PutHex, PutString;
IMPORT RTIO;

IMPORT RTAllocator, RTStrongRef, StrongRef, RTMem, RTHeapDebug, RTHeapPages;
IMPORT RTTypeSRC, RTOSMachine, RTHeapTrace, RTutils, Text, RTTypeSecurity;
IMPORT Spy;
IMPORT ForwardHashUtbl;

(* The allocator/garbage collector for the traced heap is an adaptation of
   the algorithm presented in the WRL Research Report 88/2, ``Compacting
   Garbage Collection with Ambiguous Roots'', by Joel F.  Bartlett; see
   this report for a detailed presentation.  John DeTreville modified it to
   be incremental, generational, and VM-synchronized.

   The allocator/collector for the untraced heap is simply malloc/free. *)

(* Much of the code below incorrectly assumes no difference between ADRSIZE
   and BYTESIZE. *)

(* In the following procedures, "RTType.Get(tc)" will fail if "tc" is not
   proper. *)

(*** RTCollector ***)

PROCEDURE Disable () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishVM();
      INC(disableCount);
      INC(manu_cnt);
      partialCollectionNext := FALSE;
    END;
    RTOS.UnlockHeap();
    IF doPerf AND perfOn THEN PerfAllow(); END;
  END Disable;

PROCEDURE Enable () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      IF disableCount > 0 THEN
        DEC(disableCount);
        CollectEnough(FALSE);
      END;
    END;
    RTOS.UnlockHeap();
    IF doPerf AND perfOn THEN PerfAllow(); END;
  END Enable;

PROCEDURE DisableMotion () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      INC(disableMotionCount);
    END;
    RTOS.UnlockHeap();
    IF doPerf AND perfOn THEN PerfAllow(); END;
  END DisableMotion;

PROCEDURE EnableMotion () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      IF disableMotionCount > 0 THEN
        DEC(disableMotionCount);
        CollectEnough(FALSE);
      END;
    END;
    RTOS.UnlockHeap();
    IF doPerf AND perfOn THEN PerfAllow(); END;
  END EnableMotion;

PROCEDURE Collect () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishGC();
      StartGC();
      FinishGC();
    END;
    RTOS.UnlockHeap();
  END Collect;

(*** RTCollectorSRC ***)

(* StartCollection starts a total collection, if none is in progress and if
   collection and motion are enabled. *)

PROCEDURE StartCollection () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      CollectorOn();
      INC(total_cnt);
      INC(req_cnt);
      IF DoTimingsXtra THEN Spy.Enter(req_timer); END;
      IF collectorState = CollectorState.Off
           AND disableCount + disableMotionCount = 0 THEN
        INC(manu_cnt);
        partialCollectionNext := FALSE;

        (* first stage must be completed atomically *)
        REPEAT CollectSome(); UNTIL collectorState # CollectorState.Zero;

        (* remaining stages can be executed incrementally *)
        IF NOT (incremental AND RTHeapDep.VM AND disableVMCount = 0) THEN
          REPEAT CollectSome(); UNTIL collectorState = CollectorState.Off;
        END;
      END;
      IF DoTimingsXtra THEN Spy.Exit(req_timer) END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END StartCollection;

(* FinishCollection finishes the current collection, if one is on
   progress. *)

PROCEDURE FinishCollection () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      CollectorOn();
      INC(total_cnt);
      INC(req_cnt);
      IF DoTimingsXtra THEN Spy.Enter(req_timer) END;
      WHILE collectorState # CollectorState.Off DO CollectSome(); END;
      IF DoTimingsXtra THEN Spy.Exit(req_timer) END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END FinishCollection;

(* DisableVM disables the use of VM protection.  While VM protection is
   disabled, no objects on the heap will be protected.*)

PROCEDURE DisableVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishVM();
      INC(disableVMCount);
    END;
    RTOS.UnlockHeap();
  END DisableVM;

(* EnableVM reenables the use of VM protection if EnableVM has been called
   as many times as DisableVM.  It is a checked runtime error to call
   EnableVM more times than DisableVM. *)

PROCEDURE EnableVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      DEC(disableVMCount);
      CollectEnough(FALSE);
    END;
    RTOS.UnlockHeap();
  END EnableVM;

(* FinishVM is equivalent to DisableVM{}; EnableVM().  FinishVM unprotects
   all heap pages, and is intended for use from the debugger. *)

PROCEDURE FinishVM () =
  BEGIN
    RTOS.LockHeap();
    BEGIN
      FinishGC();
      CollectorOn();
      (* no gray pages now; only protected pages are in older generation *)
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0].protected THEN Unprotect(p); END;
      END;
      CollectorOff();
    END;
    RTOS.UnlockHeap();
  END FinishVM;

(* StartBackgroundCollection starts the background thread, if not already
   started *)

PROCEDURE StartBackgroundCollection () =
  VAR start := FALSE;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      IF NOT startedBackground THEN
        start := TRUE;
        startedBackground := TRUE;
      END;
    END;
    RTOS.UnlockHeap();
    IF start THEN
      IF doVerbose AND verbose > 0 THEN
        PutText ("GC >> forking a background thread\n");
      END;
      backgroundThread := Thread.Fork(NEW(Thread.Closure, 
                                          apply := BackgroundThread));
    END;
  END StartBackgroundCollection;

PROCEDURE StopBackgroundCollection () =
  BEGIN
    RTOS.LockHeap();
    IF doVerbose AND verbose > 0 THEN
      PutText ("GC >> background thread scheduled to exit\n");
    END;
    backgroundShouldExit := TRUE;
    RTOS.UnlockHeap();
  END StopBackgroundCollection;

(* ------------------------------- low-level allocation and collection *)

(* We assume that references (values of the types ADDRESS and REFANY) are
   the addresses of addressable locations and that locations with
   successive addresses are contiguous (that is, if a points to a
   n-locations referent then these n locations are at addresses a, a+1,
   ..., a+n-1).

   The memory is viewed as a collection of pages.  Each page has a number
   that identifies it, based on the addresses that are part of this page:
   page p contains the addresses p * BytesPerPage to (p+1) * BytesPerPage -
   1.

   The page size must be a multiple of the header size (see below).  Given
   our conventions about page boundaries, this implies that the first
   location of a page is properly aligned for a Header. *)

(* The array desc and the global variables p0, and p1 describe the pages
   that are part of the traced heap.  Either p0 and p1 are equal to Nil and
   no pages are allocated; or both are valid pages and page p is allocated
   iff

|          p0 <= p < p1
|      AND desc[p - p0] != Unallocated

   NUMBER (desc) must be equal to p1 - p0 if there are allocated pages.
   Index i in desc correspond to page i + p0; that is p0 is the number of
   the first page available in desc, and it must be in [p0 ..  p1) if there
   are allocated pages. *)

(* We keep the number of allocated pages in a global variable; it should
   satify the invariant:

|     allocatedPages = sigma (i = p0, p1-1,
|                              space [i - p0] # Unallocated)
|                                  if there are allocated pages,
|                      = 0 otherwise.

   We also keep the number of active pages in a global; it satisfies:

|     activePages = sigma (i = p0, p1-1,
|                           space [i - p0] = nextSpace)
|                                if there are allocated pages,
|                 = 0 otherwise. *)

(* Each referent is immediately preceded by a header that describes the
   type of the referent.  In the user world, this header is not visible;
   that is, a REFANY is the address of the referent, not the address of the
   header.

   Each referent is immediately followed by padding space so the combined
   size referent size + padding is a multiple of the header size.
   Actually, the low level routines are given a data size which is the sum
   of the referent size and padding size and assume this data size is a
   multiple of the header size.

   With this padding, addresses of headers and referent will always be
   multiple of ADRSIZE (Header).

   The combination of header/referent/padding space is called a "heap
   object".  The size of a heap object is the size of the header, plus the
   size of the referent, plus the size of the padding.  The alignment of a
   heap object is the greatest of the alignment of header and the alignment
   of the referent.

   We make the following assumptions:

   - alignment of headers is such what the addressable location following
   any properly aligned header is properly aligned for the type ADDRESS;
   and, for every referent: referent adrSize + padding adrSize >= ADRSIZE
   (ADDRESS)

   [During the garbage collection, we move heap objects.  But we need to
   keep the forwarding information somewhere.  This condition ensures that
   we can store the new address of the referent in the first word of the
   old referent.]

   - the pages are aligned more strictly than the headers (this means that
   the page size is a multiple of the header alignment).

   [We can put a header at the beginning of a page] *)

PROCEDURE HeaderOf (r: RefReferent): RefHeader =
  BEGIN
    RETURN LOOPHOLE(r - ADRSIZE(Header), RefHeader);
  END HeaderOf;

(* If a page is allocated, it can be normal or continued.  In the first
   case, there is a heap object just at the beginning of the page and
   others following.  The second case occurs when a heap object was too
   large to fit on a page: it starts at the beginning of a normal page and
   overflows on contiguous continued pages.  Whatever space is left on the
   last continued page is never used for another object or filler.  In
   other words, all the headers are on normal pages.

   Heap objects do not need to be adjacent.  Indeed, alignment constraints
   would make it difficult to ensure that property.  Filler objects may
   appear before objects to align them, or after the last object on a
   normal page to fill the page. *)

(* We need to be able to determine the size of an referent during
   collection; here is a functions to do just that.  It must be called with
   a non-nil pointer to the Header of a heap object that is there (has not
   been moved). *)

PROCEDURE ReferentSize (h: RefHeader): CARDINAL =
  VAR
    res: INTEGER;
    tc: Typecode;
    def: TypeDefn;
    size: INTEGER;
  BEGIN
    size := h.size;
    IF size # RT0.MaxReferentSize THEN
      RETURN size;
    END;

    tc := h.typecode;
    IF tc = Fill_1_type THEN RETURN 0; END;
    IF tc = Fill_N_type THEN
      res := LOOPHOLE(h + ADRSIZE(Header), UNTRACED REF INTEGER)^;
      RETURN res - BYTESIZE(Header);
    END;
    def := RTType.Get (tc);
    IF def.nDimensions = 0 THEN
      (* the typecell datasize tells the truth *)
      RETURN def.dataSize;
    END;
    (* ELSE, the referent is an open array; it has the following layout:
|         pointer to the elements (ADDRESS)
|         size 1
|         ....
|         size n
|         optional padding
|         elements
|         ....
       where n is the number of open dimensions (given by the definition)
       and each size is the number of elements along the dimension *)
    VAR
      sizes: UNTRACED REF INTEGER := h + ADRSIZE(Header) + ADRSIZE(ADDRESS);
                                                           (* ^ elt pointer*)
    BEGIN
      res := 1;
      FOR i := 0 TO def.nDimensions - 1 DO
        res := res * sizes^;
        INC(sizes, ADRSIZE(sizes^));
      END;
      res := res * def.elementSize;
    END;
    res := RTMisc.Upper(res + def.dataSize, BYTESIZE(Header));
    RETURN res;
  END ReferentSize;

(* The convention about page numbering allows for a simple conversion from
   an address to the number of the page in which it is, as well as from a
   page number to the first address is contains: *)

FUNCTIONAL PROCEDURE ReferentToPage (r: RefReferent): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    IF p < p0 OR p >= p1 OR desc[p - p0].space = Space.Unallocated
      THEN RETURN Nil;
      ELSE RETURN p;
    END;
  END ReferentToPage;

FUNCTIONAL PROCEDURE HeaderToPage (r: RefHeader): Page =
  (* VAR p: INTEGER := LOOPHOLE(r, INTEGER) DIV BytesPerPage; *)
  VAR p: INTEGER := Word.RightShift (LOOPHOLE(r, INTEGER), LogBytesPerPage);
  BEGIN
    IF p < p0 OR p >= p1 OR desc[p - p0].space = Space.Unallocated
      THEN RETURN Nil;
      ELSE RETURN p;
    END;
  END HeaderToPage;

FUNCTIONAL PROCEDURE PageToHeader (p: Page): RefHeader =
  BEGIN
    RETURN LOOPHOLE(p * BytesPerPage, RefHeader);
  END PageToHeader;

FUNCTIONAL PROCEDURE PageToAddress (p: Page): ADDRESS =
  BEGIN
    RETURN LOOPHOLE(p * BytesPerPage, ADDRESS);
  END PageToAddress;

FUNCTIONAL PROCEDURE AddressToPage (a: ADDRESS): Page =
  BEGIN
    RETURN ReferentToPage(a);
  END AddressToPage;

FUNCTIONAL PROCEDURE MinAddress(): ADDRESS =
  BEGIN
    RETURN PageToAddress(p0);
  END MinAddress;

FUNCTIONAL PROCEDURE MaxAddress(): ADDRESS =
  BEGIN
    RETURN PageToAddress(p1) - 1;
  END MaxAddress;

(* We remember where we should look for free space with the following
   globals: *)

VAR
  newPtr, newBoundary: RefHeader;
  (* memory in [newPtr, newBoundary) is available to AllocForNew *)

  pureCopyPtr, pureCopyBoundary: RefHeader;
  (* memory in [pureCopyPtr, pureCopyBoundary) is available to AllocForCopy
     for pure objects (objects with no REFs) *)

  impureCopyPtr, impureCopyBoundary: RefHeader;
  (* memory in [impureCopyPtr, impureCopyBoundary) is available to
     AllocForCopy for impure objects (objects with REFs) *)

  impureScannedPtr: RefHeader;
  (* a pointer to already scanned portion of the impureCopyPage used to 
     avoid double scanning (clashes with eager reclamation) *)

VAR
  activeBytes, prevActiveBytes := 0;


(* for fragmentation reporting, remember the active bytes at start
   of collection, then increment during collections only for 
   new allocations *)

VAR
  currentActiveBytes : INTEGER;
  oldPrevActivePages : INTEGER;

(* To move a heap object to the new space, modifying the original
   reference to it *)

TYPE Mover = RTHeapMap.Visitor OBJECT OVERRIDES apply := Move END;

PROCEDURE Move (self: Mover;  cp: ADDRESS) =
  VAR
    refref := LOOPHOLE(cp, UNTRACED REF RefReferent);
    ref    := refref^;
  BEGIN
    IF ref = NIL THEN RETURN; END;
    VAR p := ReferentToPage(ref);
    BEGIN
      IF p = Nil THEN RETURN; END;
      VAR
        pi        := p - p0;
        oldHeader := HeaderOf(ref);
        fwdHeader : RefHeader := NIL;
      BEGIN
        IF doEagerReclamation THEN
          fwdHeader := ForwardHash(oldHeader);
          IF doChecks THEN
            VAR
              pp := ReferentToPage(cp);
            BEGIN
              IF fwdHeader # NIL AND pp >= p0 AND pp < p1 AND
                desc[pp-p0].space = Space.Current AND
                pp # impureCopyPage AND
                NOT desc[ReferentToPage(cp)-p0].gray
               THEN
                PutText("GC ERROR >> bad forwarding in eager reclamation:\n"); 
                DumpPointer(cp); DumpPointer(ref); DumpPointer(fwdHeader);
                RTHeapPages.PutPageStatus(NIL, ReferentToPage(cp));
                PutBoolean(desc[ReferentToPage(cp)-p0].gray);
                RTOSMachine.Debugger();
              END;
            END;
          END;
        END;

        (*
        IF (oldHeader.forwarded) # (fwdHeader # NIL) THEN
          PutText("FUCKED 1: ");
          PutAddr(oldHeader); PutText(" ");
          PutBoolean(oldHeader.forwarded); PutText(" ");
          PutAddr(fwdHeader); PutText("\n");
        END;
        *)

        IF doEagerReclamation THEN
          IF fwdHeader = NIL AND desc[pi].space = Space.Current AND 
            (desc[pi].note = Note.Allocated OR desc[pi].note = Note.Copied)
           THEN
            IF impureCopyPage = ReferentToPage(refref) THEN
            ELSE
              PutText("<#");
              PutText("!>");
              (*
                PutInt(ReferentToPage(refref)-p0);
                PutText(">");
              *)
            END;
            (*
              PutText("<<### ");
              PutInt(impureCopyPage-p0);
              PutText(" - ");
              PutInt(ReferentToPage(refref)-p0);
              PutText(" - ");
              PutInt(pi);
              PutText("###>>");
            *)
            (* RTOSMachine.Debugger();*)
          END;
        END;
        
        IF (NOT doEagerReclamation OR fwdHeader = NIL) AND
          desc[pi].space # Space.Previous
         THEN
          IF moveFromPinned THEN
            <* ASSERT FALSE *>
            <* NOWARN *> 
            IF (desc[pi].note = Note.AmbiguousRoot OR
              desc[pi].note = Note.StrongRef) AND 
              NOT oldHeader.immobile 
             THEN
              <* ASSERT desc[pi].space = Space.Current *>
              desc[pi].space := Space.Previous;
              Move(self, cp);
              desc[pi].space := Space.Current;
            END;
          END;
          RETURN;
        END;

        IF doChecks THEN
          <* ASSERT smallCopyPages + largeCopyPages + smallPromotionPages + 
          largePromotionPages + smallNewPages + largeNewPages = activePages *>
        END;

        IF DoTimingsXtra THEN Spy.Enter(move_timer) END;

        IF (NOT doEagerReclamation OR fwdHeader = NIL) AND
          p + 1 < p1 AND desc[pi + 1].continued
         THEN
          (* if this is a large object, just promote the pages *)
          VAR def := RTType.Get (oldHeader.typecode);
          BEGIN
            IF (def.gc_map = NIL) AND (def.parent = NIL) THEN
              PromotePage(
                p, Desc{space := Space.Current, generation := copyGeneration,
                        pure := TRUE, note := Note.Large, gray := FALSE,
                        protected := FALSE, continued := FALSE});
            ELSE
              PromotePage(
                p, Desc{space := Space.Current, generation := copyGeneration,
                        pure := FALSE, note := Note.Large, gray := TRUE,
                        protected := FALSE, continued := FALSE});
              desc[pi].link := impureCopyStack;
              impureCopyStack := p;
            END;
          END;
        ELSIF NOT doEagerReclamation AND oldHeader.forwarded THEN
          (* if already moved, just update the reference *)
          refref^ := LOOPHOLE(ref, UNTRACED REF RefReferent)^;
        ELSIF doEagerReclamation AND fwdHeader # NIL THEN
          (* if already moved, just update the reference *)
          (*
          IF LOOPHOLE(ref, UNTRACED REF RefReferent)^ #
            fwdHeader+ADRSIZE(Header)
           THEN
            PutText("FUCKED 2\n");
          END;
          *)
          refref^ := fwdHeader+ADRSIZE(Header);
        ELSE
          (* move the object *)
          IF doChecks THEN
            <* ASSERT NOT oldHeader.immobile *>
          END;
          VAR
            def      := RTType.Get(oldHeader.typecode);
            dataSize := ReferentSize(oldHeader);
            np       : RefReferent;
            newHeader: RefHeader;
          BEGIN
            IF (def.gc_map # NIL) OR (def.parent # NIL) THEN
              np := AllocForImpureCopy(dataSize, def.dataAlignment);
            ELSE
              np := AllocForPureCopy(dataSize, def.dataAlignment);
            END;
            newHeader := HeaderOf(np);
            RTMisc.Copy(
                oldHeader, newHeader, BYTESIZE(Header) + dataSize);
            IF def.nDimensions # 0 THEN
              (* open array: update the internal pointer *)
              LOOPHOLE(np, UNTRACED REF ADDRESS)^ := np + def.dataSize;
            END;
            IF doEagerReclamation THEN
              AddForwardHash(oldHeader, newHeader);
              WITH tmp = Word.And(LOOPHOLE(oldHeader, INTEGER),MaxAlignMask) DO
                InsertFiller(oldHeader,
                             ADRSIZE(Header) + dataSize +
                             align[tmp,def.dataAlignment]);
              END;
            ELSE
              oldHeader.forwarded := TRUE;
              LOOPHOLE(ref, UNTRACED REF RefReferent)^ := np;
            END;

            refref^ := np;
            IF traceOn THEN 
              ObjectMoved(LOOPHOLE(ref, Word.T), LOOPHOLE(np, REFANY), 
                          ORD(collectorState)); 
            END;


            IF doEagerReclamation THEN
              IF ActiveBytesOnPage(p) = 0 THEN
                InsertFreePage(p);
              END;
            END;
          END;
        END;
        IF DoTimingsXtra THEN Spy.Exit(move_timer) END;
      END;
    END;
  END Move;

(* Determines whether a REF has yet been moved into the new space.  Follows
   the logic in "Move".*)

PROCEDURE Moved (ref: RefReferent): BOOLEAN =
  BEGIN
    IF ref = NIL THEN RETURN TRUE; END;
    (* check the space *)
    VAR p := ReferentToPage(ref);
    BEGIN
      IF p = Nil OR desc[p - p0].space # Space.Previous THEN
        RETURN TRUE;
      END;
    END;
    (* check the forwarded bit *)
    IF NOT doEagerReclamation THEN
      IF HeaderOf(LOOPHOLE(ref, ADDRESS)).forwarded THEN
        RETURN TRUE;
      END;
    ELSE
      IF ForwardHash(HeaderOf(LOOPHOLE(ref, ADDRESS))) # NIL THEN
        RETURN TRUE;
      END;
    END;

    (* not moved *)
    RETURN FALSE;
  END Moved;

PROCEDURE GetRealPointer (ref: RefReferent): RefReferent =
  VAR
    header: RefHeader;
  BEGIN
    IF ref = NIL THEN RETURN NIL; END;
    header := LOOPHOLE(ref - ADRSIZE(Header), RefHeader);

    IF NOT doEagerReclamation THEN
      IF header.forwarded THEN
        RETURN LOOPHOLE(ref, UNTRACED REF RefReferent)^;
      ELSE
        RETURN ref;
      END;
    ELSE
      WITH fwdHeader = ForwardHash(HeaderOf(LOOPHOLE(ref, ADDRESS))) DO
        IF fwdHeader # NIL THEN
          RETURN LOOPHOLE(fwdHeader+ADRSIZE(Header),UNTRACED REF RefReferent)^;
        ELSE
          RETURN ref;
        END;
      END;
    END;
  END GetRealPointer;

(* When an allocated page is referenced by the stack, we have to move it to
   the next space and insert it in the list of promoted pages.  In the case
   where the page is actually part of a group of pages for a big referent,
   we have to promote all these pages to the new space, but only the first
   one needs to be inserted in the queue, as it is the only one containing
   referent headers.

   This routine is passed to the Threads implementation.  It is called for
   each stack, where start and stop are the addresses of the first and last
   word of the stack under consideration. *)

(* there are four sources of ambiguous roots, each of them has a separete
   stage of processing:
     1 - stacks
     2 - strongrefs
     3 - untraced heap
     4 - global data segment *)

VAR 
  ambiguousStage: INTEGER;

PROCEDURE NoteStackLocations (start, stop: ADDRESS) =
  VAR
    fp                                := start;
    firstAllocatedAddress             := PageToAddress(p0);
    firstNonAllocatedAddress          := PageToAddress(p1);
    p                       : ADDRESS;
    pp                      : Page;
    note                    : Note;
    realFp := fp;
  BEGIN
    IF doVerbose AND verbose > 3 THEN
      PutText("Ambiguous region: "); 
      PutHex(LOOPHOLE(start, INTEGER)); PutText(" ");
      PutHex(LOOPHOLE(stop, INTEGER)); PutText("\n"); 
    END;
    IF DoTimingsXtra THEN Spy.Enter(nsl1Timer); END;
    WHILE fp <= stop DO
      IF DoTimingsXtra THEN Spy.Enter(nsl2Timer); END;
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF firstAllocatedAddress <= p AND p < firstNonAllocatedAddress THEN
        pp := Word.RightShift (LOOPHOLE(p, INTEGER), LogBytesPerPage);

        IF desc[pp - p0].space = Space.Previous OR
          desc[pp - p0].space = Space.Current 
         THEN
          IF DoTimingsXtra THEN Spy.Enter(nsl3Timer); END;
          (*
          NoteAmbiguousReference(realFp, p);
          *)
          IF DoTimingsXtra THEN Spy.Exit(nsl3Timer); END;
        END;

        IF (NOT moveFromPinned OR ImmobilizeObjectContaining(p, pp)) AND
          desc[pp - p0].space = Space.Previous
         THEN
          VAR fp := FirstPage(pp);
          BEGIN
            IF ambiguousStage < 3 OR anchorUnsafeAmbiguous THEN
              IF doChecks THEN
                IF NOT(desc[fp - p0].space = Space.Previous) THEN
                  PutText("GC ERROR >> Garbage collector crash (664)\n");
                  RTOS.Crash();
                END;
                <* ASSERT desc[fp - p0].space = Space.Previous *>
              END;
              IF ambiguousStage = 2 THEN
                note := Note.StrongRef;
              ELSE
                note := Note.AmbiguousRoot;
              END;
                
              IF DoTimingsXtra THEN Spy.Enter(nsl4Timer); END;
              IF desc[fp - p0].pure THEN
                PromotePage(fp, Desc{space := Space.Current, pure := TRUE,
                                      note := note, gray :=
                                      FALSE, generation := copyGeneration,
                                      protected := FALSE, continued := FALSE},
                            p, realFp, NIL);
              ELSE
                PromotePage(fp, Desc{space := Space.Current, pure := FALSE,
                                      note := note, gray :=
                                      TRUE, generation := copyGeneration,
                                      protected := FALSE, continued := FALSE},
                            p, realFp, NIL);
                desc[fp - p0].link := impureCopyStack;
                impureCopyStack := fp;
              END;
              IF DoTimingsXtra THEN Spy.Exit(nsl4Timer); END;
            END;
          END;
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
      IF DoTimingsXtra THEN Spy.Exit(nsl2Timer); END;
    END;
    IF DoTimingsXtra THEN Spy.Exit(nsl1Timer); END;
  END NoteStackLocations;

PROCEDURE PromotePage (p: Page;  READONLY d: Desc;
                       ptr: ADDRESS := NIL; 
                       loc: ADDRESS := NIL; 
                       src: REFANY := NIL) =
  BEGIN
    IF DoTimingsXtra THEN Spy.Enter(prom_timer) END;

    IF doChecks THEN
      IF d.space # Space.Current THEN
        PutText("GC ERROR >> Promoting not to the current space\n");
        RTOS.Crash();
      END;
      <* ASSERT d.space = Space.Current *>
      IF NOT (desc[p - p0].space = Space.Previous)  THEN
        PutText("GC ERROR >> Garbage collector crash (1029)\n");
        RTOS.Crash();
      END;
      <* ASSERT desc[p - p0].space = Space.Previous *>
      IF NOT (NOT desc[p - p0].continued)  THEN
        PutText("GC ERROR >> Promoting a continued page: ");
        PutInt(p-p0); PutText("\n");
        RTOS.Crash();
      END;
      <* ASSERT NOT desc[p - p0].continued*>
    END;
    VAR n := PageCount(p);
    BEGIN
      desc[p - p0] := d;
      IF traceOn THEN ObjectsPromotedOnPage(p, ptr, loc, src); END;
      IF n > 1 THEN
        VAR dd := d;
        BEGIN
          dd.continued := TRUE;
          FOR pp := p + 1 TO p + n - 1 DO desc[pp - p0] := dd; END;
        END;
      END;
      IF doPerf AND perfOn THEN PerfChange(p, n); END;
      IF n = 1 THEN
        INC(smallPromotionPages, 1);
      ELSE
        INC(largePromotionPages, n);
      END;
      INC(activePages, n);

      (* these stats must be kept if frag reporting is to be turned on
         at any point *)
      IF RTHeapStats.ReportFragmentation THEN
        INC(activeBytes,ActiveBytesOnPage(p));
        DEC(oldPrevActivePages, n); (* avoid double counting large objects *)
      END;

    END;
    IF DoTimingsXtra THEN Spy.Exit(prom_timer) END;
  END PromotePage;

PROCEDURE InsertFiller (start: RefHeader; n: INTEGER) =
  BEGIN
    IF n = 0 THEN
      (* nothing to do *)
    ELSIF n = ADRSIZE(Header) THEN
      start^ := FillHeader1;
    ELSIF n >= ADRSIZE(Header) + ADRSIZE(INTEGER) THEN
      start^ := FillHeaderN;
      start.size := MIN(n - BYTESIZE(Header), RT0.MaxReferentSize);
      LOOPHOLE(start + ADRSIZE(Header), UNTRACED REF INTEGER)^ := n;
    ELSE
      PutText("GC ERROR >> InsertFiller with negative size\n");
      RTOS.Crash();
      <* ASSERT FALSE *>
    END;
  END InsertFiller;

TYPE CollectorState = {Off, Zero, One, Two, Three, Four, Five};

VAR collectorState := CollectorState.Off;

VAR
  threshold : ARRAY [0 .. 1] OF INTEGER;
  (* start a collection as soon as current space reaches threshold[0] /
     threshold[1] pages; the initial value is 64KB *)

  partialCollection: BOOLEAN;
  (* whether the collection in progress is partial, involving only the newer
     generation *)

  partialCollectionNext: BOOLEAN := FALSE;
  (* whether the next collection should be partial *)

  copyGeneration: Generation := Generation.Younger;

  signalBackground := FALSE;
  (* should signal background collector thread? *)

  signalWeak := FALSE;
  (* should signal weak cleaner thread? *)

PROCEDURE CollectEnough (auto: BOOLEAN; nPages: INTEGER := 0) =
  BEGIN
    IF collectorOn THEN 
      RETURN; 
    END;

    IF Behind(nPages) THEN
      CollectorOn();
      INC(total_cnt); 
      IF auto THEN 
        INC(auto_cnt); 
        IF DoTimingsXtra THEN Spy.Enter(auto_timer) END;
        (*
        IF DoTimings AND NOT nestedAlloc THEN 
          Spy.Move(allocLong_timer, allocGC_timer);
        END;
        *)
        IF DoTimings AND NOT nestedAlloc THEN 
          Spy.Enter(allocGC_timer);
        END;
      ELSE 
        INC(req_cnt); 
        IF DoTimingsXtra THEN Spy.Enter(req_timer) END;
      END;

      IF incremental AND RTHeapDep.VM AND disableVMCount = 0 THEN
        INC(inc_cnt);
        REPEAT CollectSome(); UNTIL NOT Behind(nPages);
      ELSE
        INC(ninc_cnt);
        WHILE collectorState = CollectorState.Off DO CollectSome(); END;
        REPEAT CollectSome(); UNTIL collectorState = CollectorState.Off;
      END;

      IF auto THEN 
        IF DoTimingsXtra THEN Spy.Exit(auto_timer); END;
        IF DoTimings THEN 
          Spy.Exit(allocGC_timer);
        END;
      ELSE 
        IF DoTimingsXtra THEN Spy.Exit(req_timer) END;
      END;

      CollectorOff();
    END;
  END CollectEnough;

VAR
  useHardLimit : BOOLEAN := TRUE;

PROCEDURE Behind (nbPages: INTEGER): BOOLEAN =
  VAR
    large       := 0;
    used        := activePages - large + nbPages;
    allocated   := allocatedPages - large;
    free, current, unallocated, previous := 0;
    thisFree, maxFree     := 0;
    collect     : BOOLEAN;
  BEGIN
    IF disableCount + disableMotionCount > 0
         AND collectorState = CollectorState.Off THEN
      RETURN FALSE;
    END;

    FOR p := p0 TO p1 - 1 DO
      (* find the longest free chunk *)
      IF desc[p - p0].space = Space.Free THEN
        INC(thisFree);
      ELSE
        maxFree := MAX(maxFree, thisFree);
        thisFree := 0;
      END;

      (* count the spaces *)
      CASE desc[p - p0].space OF
      | Space.Current =>
        INC(current);
      | Space.Previous => INC(previous);
      | Space.Free => INC(free); 
      | Space.Unallocated => INC(unallocated);
      END;
    END;
    maxFree := MAX(maxFree, thisFree);

    IF useHardLimit AND HardBehind() THEN
      collect := TRUE;
    ELSE
      (* the original code that uses adjusted thresholds *)
      IF collectorState = CollectorState.Off THEN
        collect := (smallCopyPages + largeCopyPages + smallPromotionPages +
                       largePromotionPages + smallNewPages + largeNewPages) *
                       threshold[1] >= threshold[0];
      ELSE
        collect := (smallNewPages + largeNewPages) * gcRatio DIV 100 >= 
                       smallCopyPages + largeCopyPages;
      END;
    END;

    IF collect THEN
      IF verbose > 0 AND collectorState = CollectorState.Off THEN
        (* print page status *)
        PutText("\nGC pages >> need ");
        PutInt(nbPages); PutText("; max ");
        PutInt(maxFree); PutText("; free ");
        PutInt(free); PutText("; cur ");
        PutInt(current); PutText("; prev ");
        PutInt(previous); PutText("; unal ");
        PutInt(unallocated); PutText("; tot ");
        PutInt(allocatedPages); PutText("\n");

        (* print the triggers *)
        PutText("GC behind >> active ");
        PutInt(activePages); PutText("; large ");
        PutInt(large); PutText("; used ");
        PutInt(used); PutText("; alloc ");
        PutInt(allocated); PutText("\n");
      END;
    END;

    RETURN collect;
  END Behind;

VAR
  delayInit : INTEGER := 1;
  delayCnt  : INTEGER := 0;

PROCEDURE AboveHighWaterMark (): BOOLEAN =
  VAR
    vmFactor: INTEGER;
  BEGIN
    IF RTHeapDep.VM THEN vmFactor := 2; ELSE vmFactor := 1; END;
    RETURN activePages > allocatedPages * gcRatio DIV (2 * gcRatio + vmFactor);
  END AboveHighWaterMark;

PROCEDURE HardBehind () : BOOLEAN =
  BEGIN
    (* check whether we must collect now *)
    IF AboveHighWaterMark() THEN
      IF collectorState # CollectorState.Off THEN
        (* always force collection if in the middle of incremental collection
           and above the safe level. *)
        RETURN TRUE;
      ELSE
        (* Use a back-off scheme to start a collection from scratch
           and above the safe level.  The counters are reset after 
           a collection is successfully finished. *)
        IF verbose > 3 THEN
          PutText("("); PutInt(delayCnt); PutText(" ");
          PutInt(delayInit); PutText(")");
        END;
        <* ASSERT delayCnt >= 0 *>
        IF delayCnt < 0 THEN
          PutText("GC ERROR >> Garbage collector crash (1010)\n");
          RTOS.Crash();
        END;
        IF delayCnt = 0 THEN
          IF verbose > 3 THEN PutText("-"); END;
          delayInit := delayInit * 2;
          delayCnt := delayInit;
          RETURN TRUE;
        ELSE
          IF verbose > 3 THEN PutText("="); END;
          DEC(delayCnt);
          RETURN FALSE;
        END;
      END;
    END;

    (* we do not have to collect now *)
    RETURN FALSE;
  END HardBehind;

<* UNUSED *>
PROCEDURE NewBehind (nbPages: INTEGER): BOOLEAN =
  VAR
    large       : INTEGER;
    used        : INTEGER;
    allocated   : INTEGER;
    collect     : BOOLEAN;
  BEGIN
    IF disableCount + disableMotionCount > 0 AND
      collectorState = CollectorState.Off
     THEN
      RETURN FALSE;
    END;

    large := largePromotionPages + largeCopyPages + largeNewPages;
    used  := activePages - large;
    IF nbPages > 1 THEN
      INC(large, nbPages);
      DEC(used, nbPages);
    ELSE
      INC(used, nbPages);
    END;
    allocated := allocatedPages - large;

    IF collectorState = CollectorState.Off THEN
      IF incremental THEN
        collect := 
            (2 * gcRatio DIV 100 + 2) * used >= allocated * gcRatio DIV 100;
      ELSE
        collect := 2 * used >= allocated;
      END;
    ELSE
      collect := (smallNewPages + largeNewPages) * gcRatio DIV 100 >= 
                     smallCopyPages + largeCopyPages;
    END;

    IF NOT collect AND maxFreeLength < nbPages THEN
      collect := TRUE;
    END;

    IF collect THEN
      IF doVerbose AND verbose > 1 AND collectorState = CollectorState.Off THEN
        (* print page status and the triggers *)
        PutText("\nGC pages >> need "); PutInt(nbPages); 
        PutText("; max "); PutInt(maxFreeLength);
        PutText("; active "); PutInt(activePages); 
        PutText("; large "); PutInt(large);
        PutText("; used "); PutInt(used);
        PutText("; alloc "); PutInt(allocated);
        PutText("\n");
      END;
    END;
    
    RETURN collect;
  END NewBehind;

VAR timeUsedOnEntry: INTEGER;       (* time used when entered collector *)

VAR
  crashedAlready: BOOLEAN := FALSE;

PROCEDURE CollectorOn () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<<ON>>"); END;

    (* ASSERT locked *)
    IF doChecks THEN
      IF crashedAlready THEN
        RETURN;
      END;

      IF NOT (NOT collectorOn) THEN
        PutText("GC ERROR >> collector entered recursively\n");
        IF NOT crashedAlready THEN
          crashedAlready := TRUE;
          RTOS.Crash();
        END;
      END;

      <* ASSERT NOT collectorOn *>
      IF collectorOn THEN
        PutText("GC ERROR >> Garbage collector crash (ddd)\n");
        RTOS.Crash();
      END;
    END;

    collectorOn := TRUE;
    INC(gcCnt);
    IF doVerbose AND verbose > 2 THEN 
      PutText("<<GC ON "); PutInt(gcCnt); PutText(">>"); 
    END;
    IF traceOn THEN GCEnter(); END;

    IF incremental AND RTHeapDep.VM AND disableVMCount = 0 THEN
      (* The VM-synchronized collector doesn't worry about running threads. *)
    ELSE
      ThreadF.SuspendOthers ();
    END;

    (* keep track of time for all forms of GC not only VM synchronized *)
    timeUsedOnEntry := RTHeapDep.TimeUsed();
    IF impureCopyPage # Nil THEN
      IF doChecks THEN
        IF NOT (desc[impureCopyPage - p0].gray) THEN
          PutText("GC ERROR >> Garbage collector crash (1157)\n");
          RTOS.Crash();
        END;
        <* ASSERT desc[impureCopyPage - p0].gray *>
        IF NOT (desc[impureCopyPage - p0].protected) THEN
          PutText("GC ERROR >> Garbage collector crash (1162)\n");
          RTOS.Crash();
        END;
        <* ASSERT desc[impureCopyPage - p0].protected *>
      END;
      Unprotect(impureCopyPage);
    END;
  END CollectorOn;

PROCEDURE CollectorOff () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<<OFF>>"); END;
    (* ASSERT locked *)
    IF doChecks THEN
      IF NOT (collectorOn) THEN
        PutText("GC ERROR >> collector exited more than once\n");
        RTOS.Crash();
      END;
      <* ASSERT collectorOn *>
    END;

    IF impureCopyPage # Nil THEN
      IF doChecks THEN
        IF NOT (desc[impureCopyPage - p0].gray) THEN
          PutText("GC ERROR >> Garbage collector crash (1180)\n");
          RTOS.Crash();
        END;
        <* ASSERT desc[impureCopyPage - p0].gray *>
        IF NOT (NOT desc[impureCopyPage - p0].protected) THEN
          PutText("GC ERROR >> Garbage collector crash (1185)\n");
          RTOS.Crash();
        END;
        <* ASSERT NOT desc[impureCopyPage - p0].protected *>
      END;
      Protect(impureCopyPage, readable := FALSE, writable := FALSE);
    END;
    VAR p := impureCopyStack;
    BEGIN
      WHILE p # Nil DO
        IF desc[p - p0].gray AND NOT desc[p - p0].protected THEN
          Protect(p, readable := FALSE, writable := FALSE);
        END;
        p := desc[p - p0].link;
      END;
    END;

    IF incremental AND RTHeapDep.VM AND disableVMCount = 0 THEN
      (* The VM-synchronized collector doesn't worry about running threads. *)
    ELSE
      ThreadF.ResumeOthers ();
    END;

    (* keep track of time for all forms of GC not only VM synchronized *)
    WITH 
      timeUsedOnExit = RTHeapDep.TimeUsed(),
      cost = cycleCost + faultCost + (timeUsedOnExit - timeUsedOnEntry)
     DO
      IF cost < 0 THEN
        PutText("GC ERROR >> cycle cost less then zero\n");
        PutInt(cycleCost); PutText(" ");
        PutInt(timeUsedOnEntry); PutText(" ");
        PutInt(timeUsedOnExit); PutText(" ");
        PutInt(faultCost); PutText(" ");
        PutInt(cost); PutText("\n");
      ELSE
        cycleCost := cost;
      END;

      (* keep track of the pauses *)
      INC(nPause);
      INC(accPause, cycleCost);
      maxPause := MAX(maxPause,cycleCost);
      IF cycleCost < 100000 THEN
        INC(shortPauses[cycleCost DIV 10000]);
      ELSE
        WITH t = cycleCost DIV 25000 DO
          IF t >= FIRST(longPauses) AND t <= LAST(longPauses) THEN
            INC(longPauses[t]);
          ELSE
            PutText("GC ERROR >> pause: "); 
            PutInt(t); PutText("\n"); 
          END;
        END;
      END;
    END;

    collectorOn := FALSE;
    IF signalBackground OR signalWeak THEN
      signalBackground := FALSE;
      signalWeak := FALSE;
      Broadcast();
    END;

    IF traceOn THEN GCExit(); END;
    IF doVerbose AND verbose > 2 THEN PutText("<<GC OFF>>"); END;
  END CollectorOff;

PROCEDURE CollectSome () =
  BEGIN
    IF doChecks THEN
      IF NOT (disableCount = 0) THEN
        PutText("GC ERROR >> trying to collect when collection disabled\n");
        RTOS.Crash();
      END;
      <* ASSERT disableCount = 0 *>
    END;

    CASE collectorState OF
    | CollectorState.Off => CollectSomeInStateZero();
    | CollectorState.One => CollectSomeInStateOne();
    | CollectorState.Two => CollectSomeInStateTwo();
    | CollectorState.Three => CollectSomeInStateThree();
    | CollectorState.Four => CollectSomeInStateFour();
    | CollectorState.Five => CollectSomeInStateFive();
    | CollectorState.Zero => 
      PutText("GC ERROR >> wrong state in CollectSome\n");
      RTOS.Crash();
    END;
  END CollectSome;

(* Start a collection *)

VAR
  mover      : Mover    := NIL;
  cycleCost  : INTEGER  := 0;    (* running cost of current cycle *)
  faultCost  : INTEGER  := 0;    (* running cost of faulting *)
  cycleLength: CARDINAL := 1;    (* current planned cycle length *)
  cycleL     : CARDINAL := 0;    (* length of current cycle, so far *)
  cycleNews  : CARDINAL;         (* the number of new pages this cycle *)
  minPrefixAvgCost: INTEGER;     (* minimum average cost for a prefix of
                                    this cycle *)
  minCycleL: CARDINAL;           (* the length of that prefix *)

PROCEDURE CollectSomeInStateZero () =
  BEGIN
    INC(colCnt);
    IF doVerbose AND verbose > 0 THEN 
      PutText("<<GC "); PutInt(colCnt); 
      PutText(" "); PutInt(0);
      PutText(" - "); PutInt(activeBytes - prevActiveBytes);
      PutText(" "); PutInt(activeBytes);
      PutText(" - "); PutInt(activePages);
      PutText(" "); PutInt(freePages);
      PutText(">>"); 
    END;

    (* start of collection, take sample *)
    IF RTHeapStats.ReportFragmentation AND
      RTHeapStats.FragOn THEN
      (* activePages is right *)
      RTHeapStats.SampleFrag(currentActiveBytes, 
                             activePages*BytesPerPage, TRUE,FALSE);
    END;

    IF traceOn THEN GCStarted(); END;
    IF doVerbose AND verbose > 2 THEN PutText("<0+>"); END;
    IF DoTimingsXtra THEN Spy.Enter(state0_timer); END;

    IF DoTimingsXtra THEN Spy.Enter(init_timer); END;

    IF doChecks THEN
      IF NOT (disableCount + disableMotionCount = 0) THEN
        PutText("GC ERROR >> Garbage collector crash (1248)\n");
        RTOS.Crash();
      END;
      <* ASSERT disableCount + disableMotionCount = 0 *>
    END;

    collectorState := CollectorState.Zero;

    (* FIXME
    InitAmbiguousReferenceTracing();
    *)

    (* compute some costs relative to previous collection *)
    INC(cycleNews, smallNewPages + largeNewPages);
    VAR prefixAvgCost: INTEGER;
    BEGIN
      IF cycleNews # 0 THEN
        prefixAvgCost := cycleCost DIV cycleNews;
      ELSE
        prefixAvgCost := LAST(INTEGER);
      END;
      IF prefixAvgCost < minPrefixAvgCost THEN
        minPrefixAvgCost := prefixAvgCost;
        minCycleL := cycleL;
      END;
    END;

    (* make generational decisions *)
    IF generational AND RTHeapDep.VM AND disableVMCount = 0 THEN
      copyGeneration := Generation.Older;
      IF partialCollectionNext AND cycleL >= cycleLength THEN
        INC(long_cnt);
      END;
      partialCollection := partialCollectionNext AND cycleL < cycleLength;

      IF NOT partialCollection THEN
        IF minCycleL = cycleLength THEN
          cycleLength := cycleLength + 1;
        ELSE
          cycleLength := MAX(cycleLength - 1, 1);
        END;
      END;
    ELSE
      INC(nogen_cnt);
      copyGeneration := Generation.Younger;
      partialCollection := FALSE;
    END;
    partialCollectionNext := TRUE;

    IF partialCollection THEN
      IF doVerbose AND verbose > 1 THEN PutText("<<PARTIAL>>"); END;
      INC(cycleL);
      INC(part_cnt);
    ELSE
      IF doVerbose AND verbose > 1 THEN PutText("<<FULL>>"); END;
      cycleL := 1;
      cycleCost := 0;
      faultCost := 0;
      cycleNews := 0;
      minPrefixAvgCost := LAST(INTEGER);
      minCycleL := 0;
      INC(full_cnt);
    END;

    IF DoTimingsXtra THEN Spy.Exit(init_timer); END;

    IF DoTimingsXtra THEN Spy.Enter(monitors1_timer) END;
    InvokeMonitors (before := TRUE);
    IF DoTimingsXtra THEN Spy.Exit(monitors1_timer) END;

    IF doPerf AND perfOn THEN PerfBegin(); END;

    (* fill the rest of the current page *)
    InsertFiller(newPtr, newBoundary - newPtr);
    newPage := Nil;
    newStack := Nil;
    newPtr := NIL;
    newBoundary := NIL;

    INC(collections);

    (* flip spaces; newspace becomes oldspace *)
    IF DoTimingsXtra THEN Spy.Enter(flip1_timer) END;
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0].space = Space.Current THEN
        desc[p - p0].space := Space.Previous;
        IF doPerf AND perfOn THEN PerfChange(p, 1); END;
      END;
    END;
    IF DoTimingsXtra THEN Spy.Exit(flip1_timer) END;

    IF doPerf AND perfOn THEN PerfFlip(); END;

    (* The 'new' nextSpace is empty *)
    smallNewPages := 0;
    largeNewPages := 0;
    smallCopyPages := 0;
    largeCopyPages := 0;
    smallPromotionPages := 0;
    largePromotionPages := 0;
    prevActivePages := activePages;
    oldPrevActivePages := activePages;
    activePages := 0;
    prevActiveBytes := activeBytes;
    activeBytes := 0;

    IF DoTimingsXtra THEN Spy.Enter(prepare_timer) END;
    FOR p := p0 TO p1 - 1 DO
      IF desc[p-p0].space = Space.Previous AND NOT desc[p-p0].continued THEN
        IF desc[p - p0].generation = Generation.Older THEN
          IF partialCollection THEN
            IF doChecks THEN
              IF NOT (copyGeneration = Generation.Older) THEN
                PutText("GC ERROR >> Garbage collector crash (1325)\n");
                RTOS.Crash();
              END;
              <* ASSERT copyGeneration = Generation.Older *>
            END;
            IF desc[p - p0].protected THEN
              IF doChecks THEN
                IF NOT (NOT desc[p - p0].pure) THEN
                  PutText("GC ERROR >> Garbage collector crash (1331)\n");
                  RTOS.Crash();
                END;
                <* ASSERT NOT desc[p - p0].pure *>
              END;
              PromotePage(p, Desc{space := Space.Current, generation :=
                                  copyGeneration, pure := FALSE, note :=
                                  Note.OlderGeneration, gray := FALSE,
                                  protected := TRUE, continued := FALSE});
            ELSE
              IF desc[p - p0].pure THEN
                PromotePage(
                  p, Desc{space := Space.Current, generation :=
                          copyGeneration, pure := TRUE, note :=
                          Note.OlderGeneration, gray := FALSE, protected :=
                          FALSE, continued := FALSE});
              ELSE
                PromotePage(
                  p, Desc{space := Space.Current, generation :=
                          copyGeneration, pure := FALSE, note :=
                          Note.OlderGeneration, gray := TRUE, protected :=
                          FALSE, continued := FALSE});
                desc[p - p0].link := impureCopyStack;
                impureCopyStack := p;
              END;
            END;
          ELSE
            IF desc[p - p0].protected THEN Unprotect(p); END;
          END;
        ELSE
          IF doChecks THEN
            IF NOT (NOT desc[p - p0].protected) THEN
              PutText("GC ERROR >> Garbage collector crash (1361)\n");
              RTOS.Crash();
            END;
            <* ASSERT NOT desc[p - p0].protected *>
          END;
        END;
      END;
    END;
    IF DoTimingsXtra THEN Spy.Exit(prepare_timer) END;

    (* now nothing in the previous space is protected or in the older
       generation *)

    (* mark from roots *)
    ThreadF.SuspendOthers();
    BEGIN
      (* Examine the stacks for possible pointers *)
      IF doVerbose AND verbose > 3 THEN PutText("Process stacks\n"); END;
      ambiguousStage := 1;
      IF DoTimingsXtra THEN Spy.Enter(ambState1_timer); END;
      ThreadF.ProcessStacks(NoteStackLocations);
      IF DoTimingsXtra THEN Spy.Exit(ambState1_timer) END;

      (* Examine the ambiguous roots *)
      IF doVerbose AND verbose > 3 THEN PutText("Process strongrefs\n"); END;
      ambiguousStage := 2;
      IF DoTimingsXtra THEN Spy.Enter(ambState2_timer); END;
      VAR 
        prevAlloc := nestedAlloc;
      BEGIN
        nestedAlloc := TRUE;
        RTStrongRef.ProcessRefs(NoteStackLocations);
        nestedAlloc := prevAlloc;
      END;
      IF DoTimingsXtra THEN Spy.Exit(ambState2_timer); END;

      (* Examine all of untraced heap *)
      IF doSweep THEN
        IF doVerbose AND verbose > 3 THEN PutText("Process untraced heap\n"); END;
        ambiguousStage := 3;
        IF DoTimingsXtra THEN Spy.Enter(ambState3_timer); END;
        SweepUntracedHeap(NoteStackLocations);
        IF DoTimingsXtra THEN Spy.Exit(ambState3_timer); END;

        (* FIXME - cannot sweep global data segments until traced roots can *)
        (* be disambiguated - pardy *)
        (*
          (* Examine all of global data segment*)
             ambiguousStage := 4;
             IF DoTimingsXtra THEN Spy.Enter(ambState4_timer); END;
             NoteStackLocations(ADR(RT0u.E_etext), ADR(RT0u.E_end));
             IF DoTimingsXtra THEN Spy.Exit(ambState4_timer); END;
        *)
      END;

      (* Examine the global variables for possible pointers *)
      IF DoTimingsXtra THEN Spy.Enter(walkGlobals_timer); END;
      RTHeapMap.WalkGlobals (mover);
      IF DoTimingsXtra THEN Spy.Exit(walkGlobals_timer); END;
    END;
    ThreadF.ResumeOthers();

    IF doPerf AND perfOn THEN PerfPromotedRoots(); END;

    collectorState := CollectorState.One;
    IF backgroundWaiting THEN signalBackground := TRUE; END;

    IF DoTimingsXtra THEN Spy.Exit(state0_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-0>"); END;
  END CollectSomeInStateZero;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateOne () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<1+>"); END;
    IF DoTimingsXtra THEN Spy.Enter(state1_timer); END;
    IF NOT CopySome() THEN collectorState := CollectorState.Two; END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
    IF DoTimingsXtra THEN Spy.Exit(state1_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-1>"); END;
  END CollectSomeInStateOne;

(* Walk weakly-referenced nodes to determine order in which to do cleanup,
   then cleanup gray nodes.  This should be broken down into parts, since
   it may be a lengthy operation. *)

PROCEDURE CollectSomeInStateTwo () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<2+>"); END;
    IF DoTimingsXtra THEN Spy.Enter(state2_timer); END;
    PreHandleWeakRefs();
    collectorState := CollectorState.Three;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
    IF DoTimingsXtra THEN Spy.Exit(state2_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-2>"); END;
  END CollectSomeInStateTwo;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateThree () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<3+>"); END;
    IF DoTimingsXtra THEN Spy.Enter(state3_timer); END;
    (* recursively copy all objects reachable from promoted objects.  marks
       "marka" and "markb" are cleared when objects move to the new
       space. *)
    IF NOT CopySome() THEN
      PostHandleWeakRefs();      (* must be called with no gray objects *)
      signalWeak := TRUE;
      collectorState := CollectorState.Four;
    END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
    IF DoTimingsXtra THEN Spy.Exit(state3_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-3>"); END;
  END CollectSomeInStateThree;

(* Clean gray nodes *)

PROCEDURE CollectSomeInStateFour () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<4+>"); END;
    IF DoTimingsXtra THEN Spy.Enter(state4_timer); END;
    IF NOT CopySome() THEN collectorState := CollectorState.Five; END;
    IF backgroundWaiting THEN signalBackground := TRUE; END;
    IF DoTimingsXtra THEN Spy.Exit(state4_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-4>"); END;
  END CollectSomeInStateFour;

PROCEDURE CollectSomeInStateFive () =
  BEGIN
    IF doVerbose AND verbose > 2 THEN PutText("<5+>"); END;
    (* before reclaiming free pages, take a sample *)
    IF RTHeapStats.ReportFragmentation AND
      RTHeapStats.FragOn THEN
      (* update pre-end frag *)
      (*  activePages is wrong, need to add from-space *)
      RTHeapStats.SampleFrag(currentActiveBytes, 
                             (activePages+oldPrevActivePages)*BytesPerPage, 
                             FALSE,TRUE);
    END;
    IF DoTimingsXtra THEN Spy.Enter(state5_timer); END;

    (* free all oldspace pages; oldspace becomes freespace *)
    IF DoTimingsXtra THEN Spy.Enter(flip2_timer) END;
    FOR i := 0 TO p1 - p0 - 1 DO
      IF desc[i].space = Space.Previous THEN
        IF traceOn THEN 
          IF NOT desc[i].continued THEN 
            ObjectsDeallocatedOnPage(p0+i); 
          END;
        END;
        desc[i].space := Space.Free;
        INC(freePages);
        desc[i].continued := FALSE;
        IF doChecks THEN
          IF NOT (NOT desc[i].protected) THEN
            PutText("GC ERROR >> Garbage collector crash (1479)\n");
            RTOS.Crash();
          END;
          <* ASSERT NOT desc[i].protected *>
        END;
        IF doPerf AND perfOn THEN PerfChange(p0 + i, 1); END;
      ELSIF moveFromPinned AND desc[i].space = Space.Current AND 
        (desc[i].note = Note.AmbiguousRoot OR desc[i].note = Note.StrongRef)
       THEN
        UntagImmobileObjects(LOOPHOLE(p0 + i, Page));
      END;
    END;
    IF DoTimingsXtra THEN Spy.Exit(flip2_timer) END;

    IF DoTimingsXtra THEN Spy.Enter(free_timer) END;
    RebuildFreelist();
    IF DoTimingsXtra THEN Spy.Exit(free_timer) END;

    (* fill the rest of the current copy pages *)
    InsertFiller(pureCopyPtr, pureCopyBoundary - pureCopyPtr);
    InsertFiller(impureCopyPtr, impureCopyBoundary - impureCopyPtr);
    IF impureCopyPage # Nil THEN
      desc[impureCopyPage - p0].gray := FALSE;
      IF doPerf AND perfOn THEN PerfChange(impureCopyPage, 1); END;
      IF desc[impureCopyPage - p0].generation = Generation.Older THEN
        IF doChecks THEN
          IF NOT (desc[impureCopyPage - p0].space = Space.Current) THEN
            PutText("GC ERROR >> Garbage collector crash (1497)\n");
            RTOS.Crash();
          END;
          <* ASSERT desc[impureCopyPage - p0].space = Space.Current *>
        END;
        Protect(impureCopyPage, readable := TRUE, writable := FALSE);
      END;
      impureCopyPage := Nil;
    END;
    IF doChecks THEN
      IF NOT (impureCopyStack = Nil) THEN
        PutText("GC ERROR >> Garbage collector crash (1506)\n");
        RTOS.Crash();
      END;
      <* ASSERT impureCopyStack = Nil *>
    END;
    pureCopyPage := Nil;
    pureCopyStack := Nil;
    impureCopyPtr := NIL;
    impureCopyBoundary := NIL;
    impureScannedPtr := NIL;
    pureCopyPtr := NIL;
    pureCopyBoundary := NIL;

    IF doPerf AND perfOn THEN PerfEnd(); END;

    IF DoTimingsXtra THEN Spy.Enter(monitors2_timer) END;
    InvokeMonitors(before := FALSE);
    IF DoTimingsXtra THEN Spy.Exit(monitors2_timer) END;

    IF partialCollection THEN
      IF (smallCopyPages + largeCopyPages + smallPromotionPages
                 + largePromotionPages) * threshold[1] >= threshold[0] THEN
        IF doVerbose AND verbose > 2 THEN PutText("<<NEXT FULL>>"); END;
        partialCollectionNext := FALSE;
        INC(next_cnt);
      ELSE
        IF doVerbose AND verbose > 2 THEN PutText("<<NEXT PARTIAL>>"); END;
        partialCollectionNext := TRUE;
      END;
      (* partialCollectionNext := TRUE;*)
    ELSE
      (* adjust threshold to be grow by gcRatio *)
      threshold[0] :=
        (smallCopyPages + largeCopyPages + smallPromotionPages
                + largePromotionPages) * (100 + gcRatio);
      threshold[1] := gcRatio;
      IF doVerbose AND verbose > 4 THEN
        PutText("Threshold: "); PutInt(threshold[1]); PutText(" ");
        PutInt(threshold[0]); PutText("\n");
      END;
      IF doVerbose AND verbose > 2 THEN PutText("<<NEXT PARTIAL>>"); END;
      partialCollectionNext := TRUE;
    END;

    IF doEagerReclamation THEN
      (* forget all the forwarding information *)
      CleanUpForwardHash();
    END;

    collectorState := CollectorState.Off;

    (* reset the delay back-off counters *)
    delayInit := 1;
    delayCnt  := 0;

    IF traceOn THEN GCDone(); END;
    IF DoTimingsXtra THEN Spy.Exit(state5_timer) END;
    IF doVerbose AND verbose > 2 THEN PutText("<-5>"); END;
    IF doVerbose AND verbose > 0 THEN 
      PutText("<<DONE "); PutInt(colCnt);
      PutText(" - "); PutInt(prevActiveBytes - activeBytes);
      PutText(" "); PutInt(activeBytes);
      PutText(" - "); PutInt(activePages);
      PutText(" "); PutInt(freePages);
      PutText(">>"); 
    END;
    prevActiveBytes := activeBytes;
    currentActiveBytes := activeBytes;
    IF RTHeapStats.ReportFragmentation AND
     RTHeapStats.FragOn THEN
      (* update post-end frag *)
      RTHeapStats.SampleFrag(currentActiveBytes, 
                             activePages*BytesPerPage);

    END;

  END CollectSomeInStateFive;

(* CopySome attempts to make progress toward cleaning the new space.  It
   returns FALSE iff there was no more work to do.

   It operates by cleaning the current copy page.  It may also clean some
   number of pages on the stack.  When it returns, there is a new copy
   page. *)

PROCEDURE CopySome (): BOOLEAN =
  VAR
    originalImpureCopyPage := impureCopyPage;
    originalBoundary       := impureCopyBoundary;
    cleanTo                := PageToHeader(impureCopyPage);
  BEGIN
    (* FIXME: do it cheaper *)
    IF ReferentToPage(impureScannedPtr) = impureCopyPage OR
      impureScannedPtr = PageToAddress(impureCopyPage+1)
     THEN
      cleanTo := impureScannedPtr;
    END;

    LOOP
      IF cleanTo < impureCopyPtr THEN
        VAR ptr := impureCopyPtr;
        BEGIN
          CleanBetween(cleanTo, ptr);
          cleanTo := ptr;
          impureScannedPtr := ptr;
        END;
      ELSE
        IF impureCopyStack = Nil THEN RETURN FALSE; END;
        VAR p := impureCopyStack;
        BEGIN
          impureCopyStack := desc[p - p0].link;
          IF doChecks THEN
            IF NOT (NOT desc[p - p0].pure) THEN
              PutText("GC ERROR >> Garbage collector crash (1565)\n");
              RTOS.Crash();
            END;
            <* ASSERT NOT desc[p - p0].pure *>
          END;
          IF desc[p - p0].gray THEN
            IF desc[p - p0].protected THEN Unprotect(p); END;
            IF moveFromPinned AND 
              (desc[p - p0].note = Note.AmbiguousRoot OR 
              desc[p - p0].note = Note.StrongRef)
             THEN
              CleanAmbiguousRootPage(PageToHeader(p), PageToHeader(p + 1));
            ELSE
              CleanBetween(PageToHeader(p), PageToHeader(p + 1));
            END;
            FOR i := 0 TO PageCount(p) - 1 DO
              desc[p + i - p0].gray := FALSE;
            END;
            IF desc[p - p0].generation = Generation.Older THEN
              IF doChecks THEN
                IF NOT (desc[p - p0].space = Space.Current) THEN
                  PutText("GC ERROR >> Garbage collector crash (1577)\n");
                  RTOS.Crash();
                END;
                <* ASSERT desc[p - p0].space = Space.Current *>
              END;
              Protect(p, readable := TRUE, writable := FALSE);
            END;
            IF doPerf AND perfOn THEN PerfChange(p, PageCount(p)); END;
          END;
        END;
      END;
      IF impureCopyPage # originalImpureCopyPage THEN EXIT; END;
    END;

    (* clean-up the rest of the original page *)
    CleanBetween(cleanTo, originalBoundary);
    impureScannedPtr := originalBoundary;

    (* originalImpureCopyPage is now in the stack; mark it not gray *)
    IF originalImpureCopyPage # Nil THEN
      desc[originalImpureCopyPage - p0].gray := FALSE;
      IF desc[originalImpureCopyPage - p0].generation = Generation.Older THEN
        IF doChecks THEN
          IF NOT (desc[originalImpureCopyPage - p0].space = Space.Current) THEN
            PutText("GC ERROR >> Garbage collector crash (1595)\n");
            RTOS.Crash();
          END;
          <* ASSERT desc[originalImpureCopyPage - p0].space = Space.Current *>
        END;
        Protect(
            originalImpureCopyPage, readable := TRUE, writable := FALSE);
      END;
      IF doPerf AND perfOn THEN PerfChange(originalImpureCopyPage, 1); END;
    END;
    RETURN TRUE;
  END CopySome;

PROCEDURE CleanBetween (h, he: RefHeader) =
  BEGIN
    WHILE h < he DO
      IF doChecks THEN
        IF NOT (Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
          PutText("GC ERROR >> Garbage collector crash (1611)\n");
          RTOS.Crash();
        END;
        <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
        IF NOT (NOT h.forwarded) THEN
          PutText("GC ERROR >> Garbage collector crash (1616)\n");
          RTOS.Crash();
        END;
        <* ASSERT NOT h.forwarded *>
      END;
      h.marka := FALSE;
      h.markb := FALSE;
      RTHeapMap.WalkRef (h, mover);
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
  END CleanBetween;

PROCEDURE CleanAmbiguousRootPage (h, he: RefHeader) =
  BEGIN
    PutText("GC ERROR >> Garbage collector crash (eee)\n");
    RTOS.Crash();
    <* ASSERT FALSE *>
    <* NOWARN *> <* ASSERT moveFromPinned *>
    <* ASSERT collectorState = CollectorState.One *>
    <* ASSERT desc[HeaderToPage(h) - p0].note = Note.AmbiguousRoot OR
              desc[HeaderToPage(h) - p0].note = Note.StrongRef *>
    WHILE h < he DO
      <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      IF h.immobile THEN
        <* ASSERT NOT h.forwarded *>
        h.marka := FALSE;
        h.markb := FALSE;
        RTHeapMap.WalkRef (h, mover);
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
  END CleanAmbiguousRootPage;

(* rvr - at the end of every collection, turn off all immobile bits,
         so the next collection starts out fresh.  also overwrite
         forwarded objects with filler so the next collection
         won't try to pin an object that's not there anymore *)

PROCEDURE UntagImmobileObjects(p: Page) =
  VAR h    := PageToHeader(p);
      end  := PageToHeader(p + 1);
      length: INTEGER;
  BEGIN
    PutText("GC ERROR >> Garbage collector crash (fff)\n");
    RTOS.Crash();
    <* ASSERT FALSE *>
    <* NOWARN *> <* ASSERT moveFromPinned *>
    WHILE h < end DO
      <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      length := ADRSIZE(Header) + ReferentSize(h);
      IF NOT h.immobile THEN
        InsertFiller(h, length);
      ELSE
        h.immobile := FALSE;
      END;
      INC(h, length);
    END;
  END UntagImmobileObjects;

PROCEDURE ObjectsPromotedOnPage (p: Page;
                                 ptr: ADDRESS; loc: ADDRESS; src: REFANY) =
  VAR
    h  : RefHeader := PageToHeader(p);
    he : RefHeader := PageToHeader(p+1);
  BEGIN
    WHILE h < he DO
      IF NOT (Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
        PutText("GC ERROR >> Garbage collector crash (1634)\n");
        RTOS.Crash();
      END;
      <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      IF h.typecode # Fill_1_type AND h.typecode # Fill_N_type THEN
        ObjectPromoted(LOOPHOLE(h + ADRSIZE(Header), REFANY), 
                       ambiguousStage, ptr, loc, src);
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
  END ObjectsPromotedOnPage;

PROCEDURE ObjectsDeallocatedOnPage (p: Page) =
  VAR
    h  : RefHeader := PageToHeader(p);
    he : RefHeader := PageToHeader(p+1);
  BEGIN
    WHILE h < he DO
      IF NOT (Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
        PutText("GC ERROR >> Garbage collector crash (1652)\n");
        RTOS.Crash();
      END;
      <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      IF h.typecode # Fill_1_type AND h.typecode # Fill_N_type AND
        NOT h.forwarded
       THEN
        ObjectDeallocated(LOOPHOLE(h + ADRSIZE(Header), Word.T));
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
  END ObjectsDeallocatedOnPage;

(* We maintain a list in weakTable, starting at weakLive0, of weak refs and
   the objects they reference.  This table is not considered a root.  When
   HandleWeakRefs is entered, any object mentioned in that list is a
   candidate for cleanup.

   First, we determine which weakly-referenced objects with non-NIL
   cleanups ("WRNNC objects") are reachable from other WRNNC objects, by
   walking the old space.  All such WRNNC objects are copied to new space,
   and all the objects they reference.

   All the weakly-referenced objects left in the old space can then be
   scheduled for cleanup; we move them from the list starting at weakLive0
   to the list starting at weakDead0 in weakTable.  A separate thread runs
   WeakCleaner, which does the calls to the procedures.

   Note that the refs in weakTable must be updated to point to new
   space. *)
(*
VAR
  v := FALSE;
*)

VAR 
  stacker: Stacker := NIL;

(* PreHandleWeakRefs walks the weakly-references structures in old-space,
   deciding on a cleanup order. *)

PROCEDURE PreHandleWeakRefs () =
  VAR s: Stacker;
  BEGIN
    (* allocate a stack on the side for walking the old space *)
    s := InitStack();
    (* iterate over the weak refs to walk the old space *)
    VAR i := weakLive0;
    BEGIN
      WHILE i # -1 DO
        (* here, all old-space WRNNC objects that have already been scanned
           have marka set, as do all old-space objects reachable from them;
           all old-space WRNNC objects that were reachable from other
           already-scanned WRNNC objects have been promoted to the new
           space. *)
        WITH entry = weakTable[i] DO
          IF entry.p # NIL AND NOT Moved(entry.r) THEN
            (* we haven't seen this WRNNC object before *)
            VAR header := HeaderOf(LOOPHOLE(entry.r, ADDRESS));
            BEGIN
              IF NOT header.marka THEN
                IF doChecks THEN
                  IF NOT (NOT header.markb) THEN
                    PutText("GC ERROR >> Garbage collector crash (1709)\n");
                    RTOS.Crash();
                  END;
                  <* ASSERT NOT header.markb *>
                END;
                (* visit all old-space objects reachable from it; promote
                   all other old-space WRNNC objects reachable from it;
                   promote all old-space objects reachable from it that
                   have "marka" set.  mark all visited nodes with
                   "markb". *)
                WeakWalk1(s, entry.r);
                IF doChecks THEN
                  IF NOT (NOT header.marka) THEN
                    PutText("GC ERROR >> Garbage collector crash (1720)\n");
                    RTOS.Crash();
                  END;
                  <* ASSERT NOT header.marka *>
                  IF NOT (header.markb) THEN
                    PutText("GC ERROR >> Garbage collector crash (1725)\n");
                    RTOS.Crash();
                  END;
                  <* ASSERT header.markb *>
                END;
                (* then change all "markb" to "marka" *)
                WeakWalk2(s, entry.r);
                IF doChecks THEN
                  IF NOT (header.marka) THEN
                    PutText("GC ERROR >> Garbage collector crash (1732)\n");
                    RTOS.Crash();
                  END;
                  <* ASSERT header.marka *>
                  IF NOT (NOT header.markb) THEN
                    PutText("GC ERROR >> Garbage collector crash (1737)\n");
                    RTOS.Crash();
                  END;
                  <* ASSERT NOT header.markb *>
                END;
              END;
            END;
          END;
          i := entry.next;
        END;
      END;
    END;
  END PreHandleWeakRefs;

(* WeakWalk1 starts at a WRNNC object and visits all objects in old space
   reachable from it, using "markb" to keep from visiting them more than
   once.  All other WRNNC objects visited are promoted, as are all objects
   already visited from other WRNNC objects. *)

PROCEDURE WeakWalk1 (s: Stacker; ref: RefReferent) =
  VAR ref0 := ref;
  BEGIN
    IF doChecks THEN
      IF NOT (s.empty()) THEN
        PutText("GC ERROR >> Garbage collector crash (1759)\n");
        RTOS.Crash();
      END;
      <* ASSERT s.empty() *>
    END;
    LOOP
      IF NOT Moved(ref) THEN
        VAR header := HeaderOf(ref);
        BEGIN
          IF header.marka THEN
            IF doChecks THEN
              IF NOT (NOT header.markb) THEN
                PutText("GC ERROR >> Garbage collector crash (1769)\n");
                RTOS.Crash();
              END;
              <* ASSERT NOT header.markb *>
            END;
            Move(NIL, ADR(ref));
          ELSIF NOT header.markb THEN
            IF header.weak AND ref # ref0 THEN
              Move(NIL, ADR(ref));
            ELSE
              header.markb := TRUE;
              RTHeapMap.WalkRef (header, s);
            END;
          END;
        END;
      END;
      IF s.empty() THEN EXIT; END;
      ref := s.pop();
    END;
  END WeakWalk1;

(* WeakWalk2 starts at a WRNNC objects and visits all objects in the old
   space that are reachable from it, changing "markb" to "marka" *)

PROCEDURE WeakWalk2 (s: Stacker;  ref: RefReferent) =
  BEGIN
    IF doChecks THEN
      IF NOT (s.empty()) THEN
        PutText("GC ERROR >> Garbage collector crash (1795)\n");
        RTOS.Crash();
      END;
      <* ASSERT s.empty() *>
    END;
    LOOP
      IF NOT Moved(ref) THEN
        VAR header := HeaderOf(ref);
        BEGIN
          IF header.markb THEN
            header.markb := FALSE;
            header.marka := TRUE;
            RTHeapMap.WalkRef (header, s);
          END;
        END;
      END;
      IF s.empty() THEN EXIT; END;
      ref := s.pop();
    END;
  END WeakWalk2;

PROCEDURE PostHandleWeakRefs () =
  BEGIN
    (* iterate over all weak refs.  if the object hasn't been promoted,
       schedule a cleanup *)
    VAR
      i        := weakLive0;
      previous := -1;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          IF Moved(entry.r) THEN
            (* no cleanup this time; note new address *)
            Move(NIL, ADR(entry.r));
            previous := i;
            i := entry.next;
          ELSE
            (* the weak ref is dead; there are no cleanups *)
            VAR header := HeaderOf(LOOPHOLE(entry.r, ADDRESS));
            BEGIN
              header.weak := FALSE;
            END;
            (* move the entry from the weakLive0 list into the weakDead0 or
               weakFree0 list *)
            VAR next := entry.next;
            BEGIN
              IF previous = -1 THEN
                weakLive0 := next;
              ELSE
                weakTable[previous].next := next;
              END;
              entry.t.a := -1;   (* keep ToRef from succeeding *)
              IF entry.p # NIL THEN
                entry.next := weakDead0;
                weakDead0 := i;
              ELSE
                entry.next := weakFree0;
                weakFree0 := i;
              END;
              i := next;
            END;
          END;
        END;
      END;
    END;
    (* for all entries on the weakDead0 list, including those just placed
       there, note the new address *)
    VAR i := weakDead0;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          IF doChecks THEN
            IF NOT (entry.t.a = -1) THEN
              PutText("GC ERROR >> Garbage collector crash (1869)\n");
              RTOS.Crash();
            END;
            <* ASSERT entry.t.a = -1 *>
          END;
          Move(NIL, ADR(entry.r));
          i := entry.next;
        END;
      END;
    END;
    (* finally, check for objects with final cleanup enabled *)
    VAR
      i        := weakFinal0;
      previous := -1;
    BEGIN
      WHILE i # -1 DO
        WITH entry = weakTable[i] DO
          IF Moved(entry.r) THEN
            (* no cleanup this time; note new address *)
            Move(NIL, ADR(entry.r));
            previous := i;
            i := entry.next;
          ELSE
            (* call the cleanup procedure *)
            LOOPHOLE(entry.p, PROCEDURE (p: REFANY))(
              LOOPHOLE(entry.r, REFANY));
            (* take the entry off the weakFinal0 list and put it on the
               weakFree0 list; on to the next entry *)
            VAR next := entry.next;
            BEGIN
              IF previous = -1 THEN
                weakFinal0 := next;
              ELSE
                weakTable[previous].next := next;
              END;
              entry.next := weakFree0;
              weakFree0 := i;
              i := next;
            END;
          END;
        END;
      END;
    END;
  END PostHandleWeakRefs;

(* The stack for walking the old space is maintained on the heap in the new
   space. *)

TYPE
  Stacker = RTHeapMap.Visitor OBJECT
    data : UNTRACED REF ARRAY OF RefReferent;
    x0   : UNTRACED REF RefReferent;
    x1   : UNTRACED REF RefReferent;
    xA   : UNTRACED REF RefReferent;
    xN   : CARDINAL;
  METHODS
    empty (): BOOLEAN     := StackEmpty;
    pop   (): RefReferent := PopStack;
  OVERRIDES
    apply := PushStack;
  END;

(* InitStack allocates an initial stack of 100 elements. *)

PROCEDURE InitStack (): Stacker =
  BEGIN
    stacker.xN   := NUMBER (stacker.data^);
    stacker.x0   := ADR(stacker.data[0]);
    stacker.x1   := stacker.x0 + stacker.xN * ADRSIZE(RefReferent);
    stacker.xA   := stacker.x0;
    RETURN stacker;
  END InitStack;

(* PushStack pushes an object onto the stack, growing it if necessary. *)

PROCEDURE PushStack (s: Stacker;  cp: ADDRESS) =
  VAR ref: RefReferent := LOOPHOLE(cp, UNTRACED REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      IF s.xA = s.x1 THEN ExpandStack (s); END;
      s.xA^ := ref;
      INC(s.xA, ADRSIZE(RefReferent));
    END;
  END PushStack;

PROCEDURE ExpandStack (s: Stacker) =
  VAR
    newStackN := 2 * s.xN;
    newStack: UNTRACED REF ARRAY OF RefReferent;
  BEGIN
    newStack := NEW(UNTRACED REF ARRAY OF RefReferent, newStackN);
    SUBARRAY(newStack^, 0, s.xN) := SUBARRAY(s.data^, 0, s.xN);
    DISPOSE(s.data);
    s.x0   := ADR(newStack^[0]);
    s.xA   := s.x0 + s.xN * ADRSIZE(RefReferent);
    s.x1   := s.x0 + newStackN * ADRSIZE(RefReferent);
    s.data := newStack;
    s.xN   := newStackN;
  END ExpandStack;

(* PopStack pops an object off the stack. *)

PROCEDURE PopStack (s: Stacker): RefReferent =
  BEGIN
    DEC(s.xA, ADRSIZE(RefReferent));
    RETURN s.xA^;
  END PopStack;

(* StackEmpty tells if the stack is empty. *)

PROCEDURE StackEmpty (s: Stacker): BOOLEAN =
  BEGIN
    RETURN s.xA = s.x0;
  END StackEmpty;

PROCEDURE ActiveBytesOnPage (p: Page; withOverhead: BOOLEAN := FALSE): INTEGER =
  VAR
    h     : RefHeader;
    he    : RefHeader;
    bytes : INTEGER;
  BEGIN
    IF desc[p - p0].space # Space.Current AND
      desc[p - p0].space # Space.Previous
     THEN
      RETURN 0;
    END;
    IF desc[p - p0].continued THEN
      RETURN 0;
    END;
    h  := PageToHeader(p);
    he := PageToHeader(p+1);

    IF p = newPage THEN
      he := newPtr;
    ELSE
      FOR g := FIRST(Generation) TO LAST(Generation) DO
        IF p = pureCopyPage THEN he := pureCopyPtr; EXIT; END;
        IF p = impureCopyPage THEN he := impureCopyPtr; EXIT; END;
      END;
    END;
    bytes := 0;
    WHILE h < he DO
      IF doChecks THEN
        IF NOT (Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
          PutText("GC ERROR >> Garbage collector crash (1652)\n");
          RTOS.Crash();
        END;
        <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
      END;
      IF h.typecode # Fill_1_type AND h.typecode # Fill_N_type AND
        NOT h.forwarded 
       THEN
        INC(bytes, ReferentSize(h));
        IF withOverhead THEN INC(bytes, ADRSIZE(Header)); END;
      END;
      INC(h, ADRSIZE(Header) + ReferentSize(h));
    END;
    RETURN bytes;
  END ActiveBytesOnPage; 

(* Malloc returns the address of "size" bytes of untraced, zeroed storage *)

PROCEDURE Malloc (size: INTEGER (*; tc: INTEGER*)): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      res := Cstdlib.malloc(size);
      IF res = NIL THEN
        RTMisc.FatalError(NIL, 0, "malloc failed, unable to get more memory");
      END;
    END;
    RTOS.UnlockHeap();
    RTMisc.Zero(res, size);
    RETURN res;
  END Malloc;

(* AllocForNew allocates space for a NEW. Assumes the caller has the *)
(* heap lock held. *)
PROCEDURE AllocForNew (dataSize, dataAlignment: CARDINAL): RefReferent =
  VAR
    alignment : INTEGER;
    nextNewPtr: RefHeader;
    size : INTEGER;
  BEGIN
    IF DoTimings AND NOT nestedAlloc THEN Spy.Enter(allocShort_timer); END;

    (* Where would this heap object end if we were to allocate at
       newPtr? *)
    VAR referentTry := newPtr + ADRSIZE(Header);
    BEGIN
      alignment :=
        align[Word.And(LOOPHOLE(referentTry, INTEGER), MaxAlignMask),
              dataAlignment];
      size := alignment + dataSize;
      nextNewPtr := referentTry + size;
    END;
    (* If this is not ok, take the long route *)
    IF nextNewPtr > newBoundary THEN
      nextNewPtr := NIL;         (* clear in case of GC *)
      IF DoTimingsXtra AND NOT nestedAlloc THEN 
        Spy.Enter(allocLong_timer);
      END;
      (*
      IF DoTimings AND NOT nestedAlloc THEN 
        Spy.Move(allocShort_timer, allocLong_timer);
      END;
      *)
      VAR
        res := LongAlloc(dataSize, dataAlignment, currentPtr := newPtr,
                         currentBoundary := newBoundary,
                         currentPage := newPage, stack := newStack,
                         allocMode := AllocMode.New, 
                         pure := FALSE);
      BEGIN
        InsertFiller(newPtr, newBoundary - newPtr);
        INC(activeBytes, dataSize);
        IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocLong_timer); END; 
        IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocShort_timer); END; 
        IF RTHeapStats.ReportFragmentation THEN
          INC(currentActiveBytes, dataSize);
        END;

        RETURN res;
      END;
    END;

    (* Align the referent *)
    IF alignment # 0 THEN
      InsertFiller(newPtr, alignment);
      newPtr := newPtr + alignment;
    END;
    VAR res := LOOPHOLE(newPtr + ADRSIZE(Header), RefReferent);
    BEGIN
      InsertFiller(newPtr, newBoundary - newPtr);
      newPtr := nextNewPtr;
      INC(activeBytes, dataSize);
      IF DoTimings AND NOT nestedAlloc THEN Spy.Exit(allocShort_timer); END;
      IF RTHeapStats.ReportFragmentation THEN
        INC(currentActiveBytes, dataSize);
      END;
      RETURN res;
    END;
  END AllocForNew; 

(* AllocForPureCopy and AllocForImpureCopy allocate space to copy an object
   from oldspace; they have the same logic as AllocForNew. *)

PROCEDURE AllocForPureCopy (dataSize, dataAlignment: CARDINAL):
  RefReferent =
  VAR
    alignment       : INTEGER;
    nextPureCopyPtr : RefHeader;
    res, referentTry: RefReferent;
  BEGIN
    IF doChecks THEN
      IF NOT (collectorOn) THEN
        PutText("GC ERROR >> Garbage collector crash (2067)\n");
        RTOS.Crash();
      END;
      <* ASSERT collectorOn *>
    END;
    (* Where would this heap object end if we were to allocate at
       pureCopyPtr? *)
    referentTry := pureCopyPtr + ADRSIZE(Header);

    (* ---------------- see CheckTypes ---------------------------------
|    WITH a = RTMisc.Align (referentTry, dataAlignment) DO
|      alignment := a - referentTry;
|      nextPureCopyPtr := LOOPHOLE (a + dataSize, RefHeader); END;
       ------------------------------------------------------------------ *)
    VAR tmp := Word.And(LOOPHOLE(referentTry, INTEGER), MaxAlignMask);
    BEGIN
      alignment := align[tmp, dataAlignment];
      nextPureCopyPtr := referentTry + (alignment + dataSize);
    END;

    (* If this is not ok, take the long route *)
    IF nextPureCopyPtr > pureCopyBoundary THEN
      res := LongAlloc(dataSize, dataAlignment, currentPtr := pureCopyPtr,
                       currentBoundary := pureCopyBoundary,
                       currentPage := pureCopyPage, stack := pureCopyStack,
                       allocMode := AllocMode.Copy, pure := TRUE);
      InsertFiller(pureCopyPtr, pureCopyBoundary - pureCopyPtr);
      INC(activeBytes, dataSize);

      IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn AND
        RTHeapStats.SampleAlloc(dataSize) THEN
        ForceFragSample();
      END;
      RETURN res;
    END;

    (* Align the referent *)
    IF alignment # 0 THEN
      InsertFiller(pureCopyPtr, alignment);
      pureCopyPtr := pureCopyPtr + alignment;
    END;

    res := LOOPHOLE(pureCopyPtr + ADRSIZE(Header), RefReferent);
    InsertFiller(pureCopyPtr, pureCopyBoundary - pureCopyPtr);
    pureCopyPtr := nextPureCopyPtr;
    INC(activeBytes, dataSize);
    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn AND
      RTHeapStats.SampleAlloc(dataSize) THEN
      ForceFragSample();
    END;
    RETURN res;
  END AllocForPureCopy;

PROCEDURE AllocForImpureCopy (dataSize, dataAlignment: CARDINAL):
  RefReferent =
  VAR
    alignment        : INTEGER;
    nextImpureCopyPtr: RefHeader;
    res, referentTry : RefReferent;
  BEGIN
    IF doChecks THEN
      IF NOT (collectorOn) THEN
        PutText("GC ERROR >> Garbage collector crash (2117)\n");
        RTOS.Crash();
      END;
      <* ASSERT collectorOn *>
    END;
    (* Where would this heap object end if we were to allocate at
       ImpureCopyPtr? *)
    referentTry := impureCopyPtr + ADRSIZE(Header);

    (* ---------------- see CheckTypes ---------------------------------
|    WITH a = RTMisc.Align (referentTry, dataAlignment) DO
|      alignment := a - referentTry;
|      nextImpureCopyPtr := LOOPHOLE (a + dataSize, RefHeader); END;
       ------------------------------------------------------------------ *)
    VAR tmp := Word.And(LOOPHOLE(referentTry, INTEGER), MaxAlignMask);
    BEGIN
      alignment := align[tmp, dataAlignment];
      nextImpureCopyPtr := referentTry + (alignment + dataSize);
    END;

    (* If this is not ok, take the long route *)
    IF nextImpureCopyPtr > impureCopyBoundary THEN
      res :=
        LongAlloc(dataSize, dataAlignment, currentPtr := impureCopyPtr,
                  currentBoundary := impureCopyBoundary,
                  currentPage := impureCopyPage, stack := impureCopyStack,
                  allocMode := AllocMode.Copy, pure := FALSE);
      InsertFiller(impureCopyPtr, impureCopyBoundary - impureCopyPtr);
      INC(activeBytes, dataSize);
      RETURN res;
    END;

    (* Align the referent *)
    IF alignment # 0 THEN
      InsertFiller(impureCopyPtr, alignment);
      impureCopyPtr := impureCopyPtr + alignment;
    END;

    res := LOOPHOLE(impureCopyPtr + ADRSIZE(Header), RefReferent);
    InsertFiller(impureCopyPtr, impureCopyBoundary - impureCopyPtr);
    impureCopyPtr := nextImpureCopyPtr;
    INC(activeBytes, dataSize);
    RETURN res;
  END AllocForImpureCopy;

TYPE AllocMode = {New, Copy};

PROCEDURE LongAlloc (              dataSize, dataAlignment    : CARDINAL;
                     VAR (*INOUT*) currentPtr, currentBoundary: RefHeader;
                     VAR (*INOUT*) currentPage                : Page;
                     VAR (*INOUT*) stack                      : Page;
                                   allocMode                  : AllocMode;
                                   pure                       : BOOLEAN    ):
  RefReferent =
  VAR
    nbBytes := RTMisc.Upper(ADRSIZE(Header), dataAlignment) + dataSize;
    nbPages := (nbBytes + AdrPerPage - 1) DIV AdrPerPage;
    res     : RefReferent;
    notAfter: SET OF Note;
  BEGIN
    IF allocMode = AllocMode.New THEN
      CollectEnough(TRUE, nbPages);
      notAfter := SET OF Note{Note.Copied};
    ELSE
      notAfter := SET OF Note{Note.Allocated};
    END;

    VAR
      thisPage := FindFreePages(nbPages, notAfter := notAfter);
      (* thisPage points to a block of nbPages contiguous, free pages; just
         what we need! *)
      thisPtr                  := PageToHeader(thisPage);
      thisBoundary             := PageToHeader(thisPage + 1);
      gray                     := allocMode = AllocMode.Copy AND NOT pure;
      generation  : Generation;
      note        : Note;
    BEGIN
      (* maybe we have to put a filler to align this thing *)
      res := RTMisc.Align(thisPtr + ADRSIZE(Header), dataAlignment);
      InsertFiller(thisPtr, res - ADRSIZE(Header) - thisPtr);

      (* allocate the object *)
      thisPtr := LOOPHOLE(res + dataSize, RefHeader);

      WITH ptr = res - ADRSIZE(Header) DO
        InsertFiller(ptr,  thisBoundary - ptr);
      END;

      IF allocMode = AllocMode.New THEN
        generation := Generation.Younger;
        note := Note.Allocated;
      ELSE
        generation := copyGeneration;
        IF doChecks THEN
          IF generation = Generation.Older THEN 
            IF NOT (gray OR pure) THEN
              PutText("GC ERROR >> Garbage collector crash (2206)\n");
              RTOS.Crash();
            END;
            <* ASSERT gray OR pure *>
          END;
        END;
        note := Note.Copied;
      END;

      desc[thisPage - p0] :=
        Desc{space := Space.Current, generation := generation, pure :=
             pure, note := note, gray := gray, protected := FALSE,
             continued := FALSE};
      IF nbPages = 1 THEN
        CASE allocMode OF
        | AllocMode.New => INC(smallNewPages);
        | AllocMode.Copy => INC(smallCopyPages);
        END;
      ELSE
        CASE allocMode OF
        | AllocMode.New => INC(largeNewPages, nbPages);
        | AllocMode.Copy => INC(largeCopyPages, nbPages);
        END;
        FOR i := 1 TO nbPages - 1 DO
          desc[thisPage + i - p0] :=
            Desc{space := Space.Current, generation := generation, pure :=
                 pure, note := note, gray := gray, protected := FALSE,
                 continued := TRUE};
        END;
      END;
      INC(activePages, nbPages);
      DEC(freePages, nbPages);
      IF doPerf AND perfOn THEN PerfChange(thisPage, nbPages); END;

      IF nbPages = 1 THEN
        IF thisBoundary - thisPtr > currentBoundary - currentPtr THEN
          (* more allocation space available on this page; fill and file
             the current page *)
          InsertFiller(currentPtr, currentBoundary - currentPtr);
          IF currentPage # Nil THEN
            IF doChecks THEN
              IF NOT (desc[currentPage - p0].space = Space.Current) THEN
                PutText("GC ERROR >> Garbage collector crash (2244)\n");
                RTOS.Crash();
              END;
              <* ASSERT desc[currentPage - p0].space = Space.Current *>
            END;
            desc[currentPage - p0].link := stack;
            stack := currentPage;
            IF doChecks THEN
              IF allocMode = AllocMode.Copy THEN
                IF NOT (desc[currentPage - p0].gray OR 
                  desc[currentPage - p0].pure) 
                 THEN
                  PutText("GC ERROR >> Garbage collector crash (2254)\n");
                  RTOS.Crash();
                END;
                <* ASSERT desc[currentPage - p0].gray OR desc[currentPage - p0].pure *>
              END;
            END;
          END;
          currentPtr := thisPtr;
          currentBoundary := thisBoundary;
          currentPage := thisPage;
        ELSE
          (* more allocation space available on current page; fill and file
             this page *)
          InsertFiller(thisPtr, thisBoundary - thisPtr);
          desc[thisPage - p0].link := stack;
          stack := thisPage;
        END;
      ELSE
        (* file this page *)
        desc[thisPage - p0].link := stack;
        stack := thisPage;
      END;
    END;
    RETURN res;
  END LongAlloc;

(*--------------------------------------------------*)

VAR
  backgroundWaiting    := FALSE;
  backgroundShouldExit := FALSE;

(* The background thread may be present or not.  If it is present, it
   speeds collection asynchronously.  Because it makes progress slowly, it
   should impose only a small overhead when the mutator is running, but
   quickly complete a collection if the collector pauses. *)

PROCEDURE BackgroundThread (<* UNUSED *> closure: Thread.Closure): REFANY =
  BEGIN
    IF doVerbose AND verbose > 0 THEN
      PutText ("GC >> background thread started\n");
    END;

    LOOP
      backgroundWaiting := TRUE; (* no locks, unfortunately *)
      WHILE collectorState = CollectorState.Off DO Wait(); END;
      backgroundWaiting := FALSE;
      WHILE collectorState # CollectorState.Off DO
        RTOS.LockHeap();
        IF backgroundShouldExit THEN
          backgroundShouldExit := FALSE;
          startedBackground    := TRUE;
          RTOS.UnlockHeap();
          IF doVerbose AND verbose > 0 THEN
            PutText ("GC >> background thread exited\n");
          END;
          RETURN NIL;
        END;
        BEGIN
          IF collectorState # CollectorState.Off THEN
            CollectorOn();
            IF doVerbose AND verbose > 2 THEN PutText("<B+>"); END;
            INC(total_cnt);
            INC(back_cnt); 
            IF DoTimingsXtra THEN Spy.Enter(back_timer); END;
            CollectSome();
            IF DoTimingsXtra THEN Spy.Exit(back_timer); END;
            IF doVerbose AND verbose > 2 THEN PutText("<-B>"); END;
            CollectorOff();
          END;
        END;
        RTOS.UnlockHeap();

        (* we do not pause but instead we run at low priority *)
        (* Thread.Pause(1);       (* one second *) *)
      END;
    END;
    (* never reached *)
  END BackgroundThread;


(* --------------------------------------------------------- collector *)

PROCEDURE StartGC () =
  BEGIN
    StartCollection();
  END StartGC;

PROCEDURE FinishGC () =
  BEGIN
    FinishCollection();
  END FinishGC;

PROCEDURE Crash (): BOOLEAN =
  VAR result: BOOLEAN;
  BEGIN
    PutText ("GC >> crash\n");
    RTOS.LockHeap();        (* left incremented *)
    IF collectorState = CollectorState.Off THEN
      (* no collection in progress *)
      collectorOn := TRUE;       (* left on *)
      result := TRUE;
    ELSIF NOT collectorOn THEN
      CollectorOn();             (* left on *)
      INC(total_cnt); 
      INC(crash_cnt); 
      IF DoTimingsXtra THEN Spy.Enter(crash_timer); END;
      (* finish collection *)
      WHILE collectorState # CollectorState.Off DO CollectSome(); END;
      IF DoTimingsXtra THEN Spy.Exit(crash_timer); END;
      result := TRUE;
    ELSE
      collectorOn := TRUE;       (* left on *)
      result := FALSE;
    END;
    (* unprotect all pages *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0].protected THEN Unprotect(p); END;
    END;
    RETURN result;
  END Crash;

(* --------------------------------------------------------- debugging *)

VAR
  protectedCheck, refCheck: RTHeapMap.Visitor;
  sanityMonitor: MonitorClosure;

PROCEDURE InstallSanityCheck () =
  BEGIN
    RTOS.LockHeap();
    IF refCheck = NIL THEN
      protectedCheck := NEW (RTHeapMap.Visitor,
                             apply := ProtectedOlderRefSanityCheck);
      refCheck := NEW (RTHeapMap.Visitor, apply := RefSanityCheck);
      sanityMonitor := NEW(MonitorClosure, 
                           before := SanityCheck, after := SanityCheck);
      RegisterMonitor(sanityMonitor);
      IF doVerbose AND verbose > 0 THEN
        PutText("GC >> sanity checks installed\n");
      END;
    END;
    RTOS.UnlockHeap();
  END InstallSanityCheck;

PROCEDURE UninstallSanityCheck () =
  BEGIN
    RTOS.LockHeap();
    IF refCheck # NIL THEN
      UnregisterMonitor(sanityMonitor);
      protectedCheck := NIL;
      refCheck       := NIL;
      sanityMonitor  := NIL;
      IF doVerbose AND verbose > 0 THEN
        PutText("GC >> sanity checks uninstalled\n");
      END;
    END;
    RTOS.UnlockHeap();
  END UninstallSanityCheck;

(* SanityCheck checks the heap for correctness when no collection is in
   progress. *)

PROCEDURE SanityCheck (<*UNUSED*> self: MonitorClosure) =
  VAR p := p0;
  BEGIN
    WHILE p < p1 DO
      CASE desc[p - p0].space OF
      | Space.Unallocated => INC(p);
      | Space.Previous =>  
          PutText("GC ERROR >> Garbage collector crash (2389)\n");
          RTOS.Crash();
          <* ASSERT FALSE *>
      | Space.Current =>
          IF NOT (NOT desc[p - p0].gray) THEN
            PutText("GC ERROR >> Garbage collector crash (2394)\n");
            RTOS.Crash();
          END;
          <* ASSERT NOT desc[p - p0].gray *>
          IF NOT (NOT desc[p - p0].continued) THEN
            PutText("GC ERROR >> Garbage collector crash (2399)\n");
            RTOS.Crash();
          END;
          <* ASSERT NOT desc[p - p0].continued *>
          IF desc[p - p0].protected THEN
            IF NOT (desc[p - p0].generation = Generation.Older) THEN
              PutText("GC ERROR >> Garbage collector crash (2405)\n");
              RTOS.Crash();
            END;
            <* ASSERT desc[p - p0].generation = Generation.Older *>
          END;
          (* visit the objects on the page *)
          VAR
            h  := PageToHeader(p);
            he := PageToHeader(p + 1);
          BEGIN
            IF p = newPage THEN he := newPtr; END;
            WHILE h < he DO
              (* check the references in the object *)
              IF desc[p - p0].protected THEN
                RTHeapMap.WalkRef (h, protectedCheck);
              ELSE
                RTHeapMap.WalkRef (h, refCheck);
              END;
              INC(h, ADRSIZE(Header) + ReferentSize(h));
            END;
            IF h > he THEN
              IF NOT (HeaderToPage(h - 1) = p + PageCount(p) - 1) THEN
                PutText("GC ERROR >> Garbage collector crash (2427)\n");
                RTOS.Crash();
              END;
              <* ASSERT HeaderToPage(h - 1) = p + PageCount(p) - 1 *>
            ELSE
              IF NOT (PageCount(p) = 1) THEN
                PutText("GC ERROR >> Garbage collector crash (2433)\n");
                RTOS.Crash();
              END;
              <* ASSERT PageCount(p) = 1 *>
            END;
          END;
          VAR
            n := PageCount(p);
            d := desc[p - p0];
          BEGIN
            d.continued := TRUE;
            d.link := Nil;
            LOOP
              INC(p);
              DEC(n);
              IF n = 0 THEN EXIT; END;
              VAR dd := desc[p - p0];
              BEGIN
                dd.link := Nil;
                IF NOT (dd = d) THEN
                  PutText("GC ERROR >> Garbage collector crash (2453)\n");
                  RTOS.Crash();
                END;
                <* ASSERT dd = d *>
              END;
            END;
          END;
      | Space.Free =>
          IF NOT (NOT desc[p - p0].continued) THEN
            PutText("GC ERROR >> Garbage collector crash (2462)\n");
            RTOS.Crash();
          END;
          <* ASSERT NOT desc[p - p0].continued *>
          INC(p);
      END;
    END;
    IF NOT (p = p1) THEN
      PutText("GC ERROR >> Garbage collector crash (2470)\n");
      RTOS.Crash();
    END;
    <* ASSERT p = p1 *>
  END SanityCheck;

PROCEDURE RefSanityCheck (<*UNUSED*>v: RTHeapMap.Visitor;  cp  : ADDRESS) =
  VAR ref := LOOPHOLE(cp, REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        p  := ReferentToPage(ref);
        h  := HeaderOf(ref);
        tc : INTEGER;
      BEGIN
        IF p0 <= p AND p < p1 THEN
          IF NOT (desc[p - p0].space = Space.Current) THEN
            PutText("GC ERROR >> Garbage collector crash (2488)\n");
            PutText("\ta reference to a non-current page\n");
            PutText("\n\treference: "); PutAddr(cp);
            PutText("\n\treference page: "); 
            PutInt(ReferentToPage(cp)-p0);
            PutText("\tpage number: "); PutInt(p-p0); 
            PutText("\n\tbase page: ");  PutInt(p0); 
            PutText("\n\tpage status: ");  
            RTHeapPages.PutPageStatus(NIL, p);
            PutText("\n");
            RTHeapPages.DumpPageStatus(NIL, TRUE);
            RTOS.Crash();
          END;
          <* ASSERT desc[p - p0].space = Space.Current *>
          IF NOT (NOT desc[p - p0].continued) THEN
            PutText("GC ERROR >> Garbage collector crash (2493)\n");
            PutText("\ta reference to a continued page\n");
            PutText("\n\treference: "); PutAddr(cp);
            PutText("\n\treference page: "); 
            PutInt(ReferentToPage(cp));
            PutText("\tpage number: "); PutInt(p); 
            PutText("\n base page: ");  PutInt(p0); 
            PutText("\n page status: ");  
            RTHeapPages.PutPageStatus(NIL, p);
            PutText("\n");
            RTHeapPages.DumpPageStatus(NIL, TRUE);
            RTOS.Crash();
          END;

          tc := h.typecode;

          <* ASSERT NOT desc[p - p0].continued *>
          IF NOT ((0 < tc AND tc < RT0u.nTypes)
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type)
           THEN
            PutText("GC ERROR >> Garbage collector crash (2504)\n");
            PutText("\ta reference to an object with illegal typecode\n");
            PutText("\n\treference: "); PutAddr(cp);
            PutText("\n\treference page: "); 
            PutInt(ReferentToPage(cp));
            PutText("\tpage number: "); PutInt(p); 
            PutText("\n base page: ");  PutInt(p0); 
            PutText("\n page status: ");  
            RTHeapPages.PutPageStatus(NIL, p);
            PutText("\n");
            RTHeapPages.DumpPageStatus(NIL, TRUE);
            RTOS.Crash();
          END;
          <* ASSERT (0 < tc AND tc < RT0u.nTypes)
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        ELSE
          IF h < LOOPHOLE(10000, ADDRESS) THEN
            PutText("GC ERROR >> Garbage collector crash (2504)\n");
            PutText("\ta illegal reference\n");
            PutText("\n\tlocation: "); PutAddr(cp);
            PutText("\n\theader: "); PutAddr(h);
            PutText("\n\tlocation page: "); 
            PutInt(ReferentToPage(cp)-p0);
            PutText("\tpage number: "); PutInt(p-p0); 
            PutText("\n base page: ");  PutInt(p0); 
            PutText("\n page status: ");  
            RTHeapPages.PutPageStatus(NIL, p);
            PutText("\n");
            RTHeapPages.DumpPageStatus(NIL, TRUE);
            RTOS.Crash();
          END;
          tc := h.typecode;

          (* the compiler generates Text.T that are not in the traced
             heap *)
          IF tc # 1 THEN
            PutText("(1) Illegal traced ref (not to the heap): ");
            PutHex(LOOPHOLE(ref, INTEGER)); PutText(" ");
            PutHex(tc); PutText("\n");
          END;
          IF NOT (tc = 1) THEN
            PutText("GC ERROR >> Garbage collector crash (2519)\n");
            RTOS.Crash();
          END;
          <* ASSERT tc = 1 *>
        END;
      END;
    END;
  END RefSanityCheck;

PROCEDURE ProtectedOlderRefSanityCheck (<*UNUSED*> v  : RTHeapMap.Visitor;
                                                   cp : ADDRESS) =
  VAR ref := LOOPHOLE(cp, REF RefReferent)^;
  BEGIN
    IF ref # NIL THEN
      VAR
        p  := ReferentToPage(ref);
        h  := HeaderOf(ref);
        tc := h.typecode;
      BEGIN
        IF p0 <= p AND p < p1 THEN
          IF NOT (desc[p - p0].space = Space.Current) THEN
            PutText("GC ERROR >> Garbage collector crash (2541)\n");
            RTOS.Crash();
          END;
          <* ASSERT desc[p - p0].space = Space.Current *>
          IF NOT (desc[p - p0].generation = Generation.Older) THEN
            PutText("GC ERROR >> Garbage collector crash (2546)\n");
            RTOS.Crash();
          END;
          <* ASSERT desc[p - p0].generation = Generation.Older *>
          IF NOT (NOT desc[p - p0].continued) THEN
            PutText("GC ERROR >> Garbage collector crash (2551)\n");
            RTOS.Crash();
          END;
          <* ASSERT NOT desc[p - p0].continued *>
          IF NOT ((0 < tc AND tc < RT0u.nTypes)
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type)
           THEN
            PutText("GC ERROR >> Garbage collector crash (2562)\n");
            RTOS.Crash();
          END;
          <* ASSERT (0 < tc AND tc < RT0u.nTypes)
                      OR tc = Fill_1_type
                      OR tc = Fill_N_type *>
        ELSE
          (* the compiler generates Text.T that are not in the traced
             heap *)
          IF tc # 1 THEN
            PutText("(2) Illegal traced ref (not to the heap): ");
            PutHex(LOOPHOLE(ref, INTEGER)); PutText(" ");
            PutHex(tc); PutText("\n");
          END;
          IF NOT (tc = 1) THEN
            PutText("GC ERROR >> Garbage collector crash (2577)\n");
            RTOS.Crash();
          END;
          <* ASSERT tc = 1 *>
        END;
      END;
    END;
  END ProtectedOlderRefSanityCheck;

(* ----------------------------------------------------------------------- *)

PROCEDURE VisitAllRefs (v: RefVisitor; fromGC: BOOLEAN := FALSE) =
  VAR tc: Typecode;
  BEGIN
    TRY
      IF NOT fromGC THEN
        Disable();
      END;
      FOR p := p0 TO p1 - 1 DO
        IF desc[p - p0].space = Space.Current
             AND NOT desc[p - p0].continued THEN
          VAR
            h             := PageToHeader(p);
            he            := PageToHeader(p + 1);
            size: INTEGER;
          BEGIN
            WHILE h < he DO
              IF p = newPage AND h >= newPtr THEN EXIT; END;
              IF p = pureCopyPage AND h >= pureCopyPtr THEN EXIT; END;
              IF p = impureCopyPage AND h >= impureCopyPtr THEN EXIT; END;

              tc := h.typecode;
              IF tc # Fill_1_type AND tc # Fill_N_type AND 
                (tc > RT0u.nTypes OR tc <= 0)
               THEN
                IF h = newPtr OR h = pureCopyPtr OR h = impureCopyPtr THEN
                  (* we were scanning a page with active allocation *)
                  EXIT;
                ELSE
		  PutText("improper typecode "); 
		  PutInt(tc); PutText("\n");
		  IF tc = LAST(Typecode) OR tc = LAST(Typecode) - 1 THEN
		    PutText("last one!\n");
		  END;
		  VAR
		    h1             := PageToHeader(p);
		    he            := PageToHeader(p + 1);
		    size: INTEGER;
		  BEGIN
		    PutText("Page           : "); PutInt(p); 
                    PutText(" "); RTHeapPages.PutPageStatus(NIL, p);  
                    PutText("\n");

		    PutText("newPage        : "); 
                    PutInt(newPage); PutText("\n");
		    PutText("pureCopyPage   : "); 
                    PutInt(pureCopyPage); PutText("\n");
		    PutText("impureCopyPage : "); 
		    PutInt(impureCopyPage); PutText("\n");

		    PutText("p0             : "); 
                    PutInt(p0); PutText("\n");
		    PutText("p1             : "); 
		    PutInt(p1); PutText("\n");

                    PutText("h                  : "); 
                    PutAddr(h); PutText("\n");
                    PutText("newPtr             : "); 
                    PutAddr(newPtr); PutText("\n");
                    PutText("newBoundary        : "); 
                    PutAddr(newBoundary); PutText("\n");
		    PutText("pureCopyPtr        : "); 
                    PutAddr(pureCopyPtr); PutText("\n");
		    PutText("pureCopyBoundary   : "); 
                    PutAddr(pureCopyBoundary); PutText("\n");
		    PutText("impureCopyPtr      : "); 
		    PutAddr(impureCopyPtr); PutText("\n");
		    PutText("impureCopyBoundary : "); 
		    PutAddr(impureCopyBoundary); PutText("\n");

                    RTHeapPages.DumpPageStatus (NIL, TRUE);

		    WHILE h1 < he AND (p # newPage OR h1 < newPtr) DO
		      tc := h1.typecode;
		      PutText("tc: "); PutInt(tc); 
		      PutText("\n");
		      size := ReferentSize(h1);
		      PutText("size: "); PutInt(size); 
		      PutText("\n");
                      IF tc = 0 THEN EXIT; END;
		      INC(h1, ADRSIZE(Header) + size);
		    END;

		  END;
		  RTOSMachine.Debugger();
		END;
              END;
              size := ReferentSize(h);
              IF tc # Fill_1_type AND tc # Fill_N_type THEN
                IF NOT v.visit(
                         tc, LOOPHOLE(h + ADRSIZE(Header), REFANY), size) THEN
                  RETURN;
                END;
              END;
              INC(h, ADRSIZE(Header) + size);
            END;
          END;
        END;
      END;
    FINALLY
      IF NOT fromGC THEN
        Enable();
      END;
    END;
  END VisitAllRefs;

TYPE
  CountClosure = MonitorClosure OBJECT
                   tcs    : REF ARRAY OF Typecode;
                   counts : REF ARRAY OF CARDINAL;
                   visitor: RefVisitor;
                 OVERRIDES
                   after := CountRefsForTypecodes;
                 END;

TYPE
  CountAllClosure = MonitorClosure OBJECT
                      counts : REF ARRAY OF CARDINAL;
                      visitor: RefVisitor;
                    OVERRIDES
                      after := CountRefsForAllTypecodes;
                    END;

TYPE
  CountVisitor =
    RefVisitor OBJECT cl: CountClosure OVERRIDES visit := One; END;

  CountAllVisitor =
    RefVisitor OBJECT cl: CountAllClosure OVERRIDES visit := All; END;

PROCEDURE One (           self: CountVisitor;
                          tc  : Typecode;
               <*UNUSED*> r   : REFANY;
               <*UNUSED*> size: CARDINAL      ): BOOLEAN =
  BEGIN
    FOR i := FIRST(self.cl.tcs^) TO LAST(self.cl.tcs^) DO
      IF self.cl.tcs[i] = tc THEN INC(self.cl.counts[i]); RETURN TRUE; END;
    END;
    RETURN TRUE;
  END One;

PROCEDURE All (           self: CountAllVisitor;
                          tc  : Typecode;
               <*UNUSED*> r   : REFANY;
               <*UNUSED*> size: CARDINAL         ): BOOLEAN =
  BEGIN
    INC(self.cl.counts[tc]);
    RETURN TRUE;
  END All;

PROCEDURE CountRefsForTypecodes (cl: CountClosure) =
  BEGIN
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      cl.counts[i] := 0;
    END;
    VisitAllRefs(cl.visitor);
    FOR i := FIRST(cl.tcs^) TO LAST(cl.tcs^) DO
      PutText("count[");
      PutInt(cl.tcs[i]);
      PutText("] = ");
      PutInt(cl.counts[i]);
      IF i # LAST(cl.tcs^) THEN PutText(",  "); END;
    END;
    PutText("\n");
  END CountRefsForTypecodes;

PROCEDURE CountRefsForAllTypecodes (cl: CountAllClosure) =
  BEGIN
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      cl.counts[i] := 0;
    END;
    VisitAllRefs(cl.visitor);
    FOR i := FIRST(cl.counts^) TO LAST(cl.counts^) DO
      IF cl.counts[i] > 1 THEN
        PutInt(i);
        PutText(": ");
        PutInt(cl.counts[i]);
        IF i # LAST(cl.counts^) THEN PutText(", "); END;
      END;
    END;
    PutText("\n");
  END CountRefsForAllTypecodes;

(* ---------------------------------------------------- showheap hooks *)

VAR
  perfW  : RTPerfTool.Handle;
  perfOn : BOOLEAN := FALSE;

CONST
  EventSize = (BITSIZE(RTHeapEvent.T) + BITSIZE(CHAR) - 1) DIV BITSIZE(CHAR);

<* UNUSED *>
PROCEDURE PerfStart () =
  VAR i, j: Page;
  BEGIN
    IF RTPerfTool.Start("showheap", perfW) THEN
      perfOn := TRUE;
      RTProcess.RegisterExitor(PerfStop);
      PerfGrow(p0, p1 - p0);

      i := p0;
      WHILE i # Nil AND i < p1 DO
        j := i + 1;
        WHILE j < p1 AND desc[j - p0].continued DO INC(j); END;
        IF desc[i - p0].space # Space.Free THEN PerfChange(i, j - i); END;
        i := j;
      END;
    END;
  END PerfStart;

PROCEDURE PerfFlip () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Flip};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfFlip;

PROCEDURE PerfPromotedRoots () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Roots};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfPromotedRoots;

PROCEDURE PerfStop () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Bye};
  BEGIN
    (* UNSAFE, but needed to prevent deadlock if we're crashing! *)
    EVAL RTPerfTool.Send (perfW, ADR(e), EventSize);
    RTPerfTool.Close (perfW);
  END PerfStop;

PROCEDURE PerfAllow (<*UNUSED*> n: INTEGER := 0) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Off, nb :=
                       disableCount + disableMotionCount};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfAllow;

PROCEDURE PerfBegin () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Begin};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfBegin;

PROCEDURE PerfEnd () =
  VAR e := RTHeapEvent.T{kind := RTHeapEvent.Kind.End};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfEnd;

PROCEDURE PerfChange (first: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{kind := RTHeapEvent.Kind.Change, first := first,
                       nb := nb, desc := desc[first - p0]};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfChange;

PROCEDURE PerfGrow (firstNew: Page; nb: CARDINAL) =
  VAR
    e := RTHeapEvent.T{
           kind := RTHeapEvent.Kind.Grow, first := firstNew, nb := nb};
  BEGIN
    perfOn := RTPerfTool.Send (perfW, ADR(e), EventSize);
  END PerfGrow;

(* ----------------------------------------------------------------------- *)

(* RTWeakRef *)

(* weakTable contains four singly-linked lists: for entries in use (rooted
   at index weakLive0), entries with final cleanup (at weakFinal0), dead
   entries awaiting cleanup (at weakDead0), and free entries (at
   weakFree0).

   Entries in use contain the weak ref, the REF, and the procedure.  The
   "a" field of the weak ref is the index in the table; this speeds lookup.
   The "b" field is a unique value, taken from a 32-bit counter.

   Dead entries contain the same dields, but the "a" field of the weak ref
   is set to -1 to keep lookups from succeeding.  When the cleanup
   procedure is to be called, the original weak ref can still be
   reconstructed, since the "a" field was the index. *)

VAR
  weakTable: UNTRACED REF ARRAY OF WeakEntry; (* allocated in "Init" *)
             (* := NEW(UNTRACED REF ARRAY OF WeakEntry, 0); *)
  weakLive0  := -1;              (* the root of the in-use list *)
  weakFinal0 := -1;              (* the root of the thread-cleanup list *)
  weakDead0  := -1;              (* the root of the dead list *)
  weakFree0  := -1;              (* the root of the free list *)

TYPE
  Int32 = BITS 32 FOR [-16_7fffffff-1 .. 16_7fffffff];
  WeakRefAB = ALIGNED RTMachine.PreferredAlignment FOR RECORD
                a: Int32;
                b: Int32;
              END;
  WeakEntry = RECORD
                t: WeakRefAB;    (* the weak ref, if well-formed *)
                r: RefReferent;  (* the traced reference *)
                p: ADDRESS;      (* a WeakRefCleanUpProc or a PROCEDURE(r:
                                    REFANY) *)
                next: INTEGER;   (* the next entry on the list *)
              END;

(* This is WeakRef.FromRef, which returns a new weak ref for an object. *)

VAR startedWeakCleaner := FALSE;

PROCEDURE WeakRefFromRef (r: REFANY; p: WeakRefCleanUpProc := NIL): WeakRef =
  VAR
    start           := FALSE;
    result: WeakRef;
  BEGIN
    IF doChecks THEN
      IF NOT (r # NIL) THEN
        PutText("GC ERROR >> Garbage collector crash (2837)\n");
        RTOS.Crash();
      END;
      <* ASSERT r # NIL *>
    END;
    RTOS.LockHeap();
    BEGIN
      (* create a WeakCleaner thread the first time through *)
      IF p # NIL AND NOT startedWeakCleaner THEN
        start := TRUE;
        startedWeakCleaner := TRUE;
      END;
      (* if necessary, expand weakTable *)
      IF weakFree0 = -1 THEN ExpandWeakTable(); END;
      IF p # NIL THEN
        (* mark the object as having a weak ref with non-nil cleanup *)
        VAR header := HeaderOf(LOOPHOLE(r, ADDRESS));
        BEGIN
          IF doChecks THEN
            IF NOT (NOT header^.weak) THEN
              PutText("GC ERROR >> Garbage collector crash (2855)\n");
              RTOS.Crash();
            END;
            <* ASSERT NOT header^.weak *>
          END;
          header^.weak := TRUE;
        END;
      END;
      (* allocate a new entry *)
      VAR i := weakFree0;
      BEGIN
        weakFree0 := weakTable[i].next;
        (* generate a new weak ref *)
        VAR t := WeakRefAB{a := i, b := Word.Plus(weakTable[i].t.b, 1)};
        BEGIN
          IF doChecks THEN
            IF NOT (t.b # 0) THEN
              PutText("GC ERROR >> Garbage collector crash (2870)\n");
              RTOS.Crash();
            END;
            <* ASSERT t.b # 0 *>
          END;
          (* set up the entry *)
          weakTable[i] :=
            WeakEntry{t := t, r := LOOPHOLE(r, RefReferent), p :=
                      LOOPHOLE(p, ADDRESS), next := weakLive0};
          weakLive0 := i;
          result := LOOPHOLE(t, WeakRef);
        END;
      END;
    END;
    RTOS.UnlockHeap();

    IF start THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := WeakCleaner));
    END;

    RETURN result;
  END WeakRefFromRef;

PROCEDURE ExpandWeakTable () =
  VAR
    newTable := NEW(UNTRACED REF ARRAY OF WeakEntry,
                    2 * NUMBER(weakTable^) + 1);
    (*
    oldTable := weakTable;
    *)
  BEGIN
    RegisterClean(ADR(newTable[0]), BYTESIZE(newTable^), FALSE);
    
    SUBARRAY(newTable^, 0, NUMBER(weakTable^)) := weakTable^;
    FOR i := NUMBER(weakTable^) TO NUMBER(newTable^) - 1 DO
      WITH entry = newTable[i] DO
        entry.t.b := 0;
        entry.next := weakFree0;
        weakFree0 := i;
      END;
    END;
    DISPOSE(weakTable);
    weakTable := newTable;

    (*
    FOR i := FIRST(oldTable^) TO LAST(oldTable^) DO
      oldTable[i].r := NIL;
    END;
    IF NUMBER(oldTable^) # 0 THEN
      UnregisterClean(ADR(oldTable[FIRST(oldTable^)]));
    END;
    DISPOSE(oldTable);
    *)
  END ExpandWeakTable;

(* This is WeakRef.ToRef, which inverts FromRef *)

PROCEDURE WeakRefToRef (READONLY t: WeakRef): REFANY =
  VAR ab: WeakRefAB;  r: REFANY := NIL;
  BEGIN
    LOOPHOLE (ab, WeakRef) := t;
    RTOS.LockHeap();
    (* if the weak ref is not dead, we know the index *)
    WITH entry = weakTable[ab.a] DO
      (* check the weak ref there *)
      IF entry.t = ab THEN
        IF doChecks THEN
          IF NOT (entry.r # NIL) THEN
            PutText("GC ERROR >> Garbage collector crash (2918)\n");
            RTOS.Crash();
          END;
          <* ASSERT entry.r # NIL *>
        END;
        IF collectorState # CollectorState.Off THEN
          VAR p := ReferentToPage(entry.r);
          BEGIN
            IF doChecks THEN
              IF NOT (p # Nil) THEN
                PutText("GC ERROR >> Garbage collector crash (2926)\n");
                RTOS.Crash();
              END;
              <* ASSERT p # Nil *>
            END;
            IF desc[p - p0].space = Space.Previous THEN
              CollectorOn();
              Move(NIL, ADR(entry.r));
              CollectorOff();
            END;
          END;
        END;
        r := LOOPHOLE(entry.r, REFANY);
      END;
    END;
    RTOS.UnlockHeap();
    RETURN r;
  END WeakRefToRef;

(* This is RTHeapRef.RegisterFinalCleanup, which registers final cleanup
   for a heap object. *)

PROCEDURE RegisterFinalCleanup (r: REFANY; p: PROCEDURE (r: REFANY)) =
  BEGIN
    IF doChecks THEN
      IF NOT (r # NIL) THEN
        PutText("GC ERROR >> Garbage collector crash (2950)\n");
        RTOS.Crash();
      END;
      <* ASSERT r # NIL *>
      IF NOT (p # NIL) THEN
        PutText("GC ERROR >> Garbage collector crash (2955)\n");
        RTOS.Crash();
      END;
      <* ASSERT p # NIL *>
    END;
    RTOS.LockHeap();
    BEGIN
      (* if necessary, expand weakTable *)
      IF weakFree0 = -1 THEN ExpandWeakTable(); END;
      (* allocate a new entry *)
      VAR i := weakFree0;
      BEGIN
        weakFree0 := weakTable[i].next;
        (* set up the entry, without a weak ref *)
        weakTable[i].r := LOOPHOLE(r, RefReferent);
        weakTable[i].p := LOOPHOLE(p, ADDRESS);
        weakTable[i].next := weakFinal0;
        weakFinal0 := i;
      END;
    END;
    RTOS.UnlockHeap();
  END RegisterFinalCleanup;

(* WeakCleaner waits for entries to be placed on the dead list, then cleans
   them up and puts them on the free list. *)

PROCEDURE WeakCleaner (<*UNUSED*> closure: Thread.Closure): REFANY =
  VAR
    i   : INTEGER;
    copy: WeakEntry;
  BEGIN
    LOOP
      (* get an entry to handle.  copy its contents, then put it on the
         free list. *)
      WHILE weakDead0 = -1 DO Wait(); END;
      RTOS.LockHeap();
      IF weakDead0 = -1 THEN
        RTOS.UnlockHeap();
      ELSE
        i := weakDead0;
        WITH entry = weakTable[i] DO
          IF doChecks THEN
            IF NOT (entry.t.a = -1) THEN
              PutText("GC ERROR >> Garbage collector crash (2996)\n");
              RTOS.Crash();
            END;
            <* ASSERT entry.t.a = -1 *>
          END;
          CollectorOn();
          Move(NIL, ADR(entry.r));
          CollectorOff();
          copy := entry;
          weakDead0 := entry.next;
          entry.next := weakFree0;
          weakFree0 := i;
        END;
        RTOS.UnlockHeap();
        (* call the registered procedure.  note that collections are
           allowed; the copy is kept on the stack so the object won't be
           freed during the call. *)
        IF copy.p # NIL THEN
          LOOPHOLE(copy.p, WeakRefCleanUpProc)(
            LOOPHOLE(WeakRefAB{a := i, b := copy.t.b}, WeakRef),
            LOOPHOLE(copy.r, REFANY));
        END;
        copy.r := NIL;           (* to help conservative collector *)
      END;
    END;
  END WeakCleaner;

(* ----------------------------------------------------------------------- *)

PROCEDURE FirstPage (p: Page): Page =
  BEGIN
    WHILE desc[p - p0].continued DO DEC(p); END;
    RETURN p;
  END FirstPage;

PROCEDURE PageCount (p: Page): CARDINAL =
  VAR n := 0;
  BEGIN
    IF doChecks THEN
      IF NOT (NOT desc[p - p0].continued) THEN
        PutText("GC ERROR >> Page count called on a continued page\n");
        RTOS.Crash();
      END;
      <* ASSERT NOT desc[p - p0].continued *>
    END;
    REPEAT INC(p); INC(n); UNTIL p >= p1 OR NOT desc[p - p0].continued;
    RETURN n;
  END PageCount;

(* ----------------------------------------------------------------------- *)

PROCEDURE Protect (p: Page; readable, writable: BOOLEAN) =
  VAR n := PageCount(p);
  BEGIN
    IF doChecks THEN
      IF NOT (collectorOn OR (readable AND writable)) THEN
        PutText("GC ERROR >> Garbage collector crash (3048)\n");
        RTOS.Crash();
      END;
      <* ASSERT collectorOn OR (readable AND writable) *>
      IF NOT (RTHeapDep.VM) THEN
        PutText("GC ERROR >> Garbage collector crash (3053)\n");
        RTOS.Crash();
      END;
      <* ASSERT RTHeapDep.VM *>
      <* NOWARN *><* ASSERT TRUE *>
    END;
    RTHeapDep.Protect(p, n, readable, writable);
    VAR protected := NOT (readable AND writable);
    BEGIN
      FOR i := 0 TO n - 1 DO desc[p + i - p0].protected := protected; END;
    END;
    IF doPerf AND perfOn THEN PerfChange(p, n); END;
  END Protect;

PROCEDURE Unprotect (p: Page) =
  BEGIN
    Protect(p, readable := TRUE, writable := TRUE);
  END Unprotect;

PROCEDURE Fault (addr: ADDRESS): BOOLEAN =
  VAR p := Word.RightShift (LOOPHOLE(addr, INTEGER), LogBytesPerPage);
  BEGIN
    INC(fault_cnt);
    IF doVerbose AND verbose > 2 THEN PutText("<F+>"); END;

    IF DoTimings THEN Spy.Enter(fault_timer); END;

    ThreadF.SuspendOthers();

    IF doChecks THEN
      IF NOT (RTHeapDep.VM) THEN
        PutText("GC ERROR >> Garbage collector crash (3076)\n");
        RTOS.Crash();
      END;
      <* ASSERT RTHeapDep.VM *>
      <* NOWARN *><* ASSERT TRUE *>
    END;

    (* otherwise unaccounted for by cycleCost *)
    faultCost := faultCost + RTHeapDep.VMFaultTime(); 

    IF NOT (p0 <= p AND p < p1) OR desc[p - p0].space = Space.Unallocated THEN
      ThreadF.ResumeOthers();
      IF DoTimings THEN Spy.Exit(fault_timer); END;
      IF doVerbose AND verbose > 2 THEN PutText("<-1F>"); END;
      RETURN FALSE;              (* not in heap *)
    END;
    IF NOT desc[p - p0].protected THEN
      ThreadF.ResumeOthers();
      IF DoTimings THEN Spy.Exit(fault_timer); END;
      IF doVerbose AND verbose > 2 THEN PutText("<-2F>"); END;
      RETURN TRUE;               (* was protected, but not any more *)
    END;
    IF doChecks THEN
      IF NOT (NOT desc[p - p0].pure) THEN
        PutText("GC ERROR >> Garbage collector crash (3092)\n");
        RTOS.Crash();
      END;
      <* ASSERT NOT desc[p - p0].pure *>
    END;
    IF desc[p - p0].gray THEN
      INC(gray_cnt);
      CollectorOn();
      IF p = impureCopyPage THEN
        IF CopySome() THEN
          IF doChecks THEN
            IF NOT (NOT desc[p - p0].gray) THEN
              PutText("GC ERROR >> Garbage collector crash (3101)\n");
              RTOS.Crash();
            END;
            <* ASSERT NOT desc[p - p0].gray *>
          END;
        ELSE
          IF desc[p - p0].gray THEN
            IF doChecks THEN
              IF NOT (p = impureCopyPage AND impureCopyStack = Nil) THEN
                PutText("GC ERROR >> Garbage collector crash (3108)\n");
                RTOS.Crash();
              END;
              <* ASSERT p = impureCopyPage AND impureCopyStack = Nil *>
            END;
            InsertFiller(impureCopyPtr, impureCopyBoundary - impureCopyPtr);
            impureCopyPage := Nil;
            impureCopyStack := Nil;
            impureCopyPtr := NIL;
            impureCopyBoundary := NIL;
            FOR i := 0 TO PageCount(p) - 1 DO
              desc[p + i - p0].gray := FALSE;
            END;
            IF desc[p - p0].generation = Generation.Older THEN
              IF doChecks THEN
                IF NOT (desc[p - p0].space = Space.Current) THEN
                  PutText("GC ERROR >> Garbage collector crash (3122)\n");
                  RTOS.Crash();
                END;
                <* ASSERT desc[p - p0].space = Space.Current *>
              END;
              Protect(p, readable := TRUE, writable := FALSE);
            END;
            IF doPerf AND perfOn THEN PerfChange(p, 1); END;
          END;
        END;
      ELSE
        p := FirstPage(p);
        Unprotect(p);
        CleanBetween(PageToHeader(p), PageToHeader(p + 1));
        FOR i := 0 TO PageCount(p) - 1 DO
          desc[p + i - p0].gray := FALSE;
        END;
        IF desc[p - p0].generation = Generation.Older THEN
          IF doChecks THEN
            IF NOT (desc[p - p0].space = Space.Current) THEN
              PutText("GC ERROR >> Garbage collector crash (3140)\n");
              RTOS.Crash();
            END;
            <* ASSERT desc[p - p0].space = Space.Current *>
          END;
          Protect(p, readable := TRUE, writable := FALSE);
        END;
        IF doPerf AND perfOn THEN PerfChange(p, PageCount(p)); END;
      END;
      CollectorOff();
    ELSE
      INC(old_cnt);
      p := FirstPage(p);
      IF doChecks THEN
        IF NOT (desc[p - p0].generation = Generation.Older) THEN
          PutText("GC ERROR >> Garbage collector crash (3152)\n");
          RTOS.Crash();
        END;
        <* ASSERT desc[p - p0].generation = Generation.Older *>
      END;
      Unprotect(p);
    END;
    ThreadF.ResumeOthers();
    IF DoTimings THEN Spy.Exit(fault_timer); END;
    IF doVerbose AND verbose > 2 THEN PutText("<-3F>"); END;
    RETURN TRUE;                 (* was protected, protection cleared *)
  END Fault;

(* ----------------------------------------------------------------------- *)

(* The inner-loop collector action is to pick a gray page and completely
   clean it (i.e., make its referents at least gray, so that the page
   becomes black).  The current gray page, "impureCopyPage" is
   distinguished; it's the page that newly gray objects are copied to.

   To improve locality of referene in the new space, we keep the set of
   gray pages as a stack.  This helps approximate a depth-first copy to
   newspace.  The current page is not a member of the stack, but will
   become one when it becomes full.  The current page is always the page
   that contains newPtr.

   To reduce page faults, we separate the "pure" copy pages (those whose
   objects contain no REFs) from the "impure" ones (those with REFs).  Only
   impure pages become gray, since pure pages can have no REFs into the old
   space (since they have no REFs at all). *)

VAR
  impureCopyPage: Page := Nil;   (* the current impure copy page *)
  impureCopyStack: Page := Nil;  (* threaded through the "link" field; ends
                                    at Nil *)

(* By analogy, we also maintain "pureCopyPage" and "pureCopyStack".  These
   are not used, but maintaining them simplifies the code. *)

VAR
  pureCopyPage: Page;            (* the current pure copy page *)
  pureCopyStack: Page;           (* threaded through the "link" field; ends
                                    at Nil *)

(* By analogy, we also maintain "newPage" and "newStack".  As with
   pureCopyPage and pureCopyStack, these are not used, but maintaining them
   simplifies the code. *)

VAR
  newPage: Page;                 (* the current new page *)
  newStack: Page;                (* threaded through the "link" field; ends
                                    at Nil *)

(* Minimum number of bytes by which to grow the heap.  Setting it higher
   reduces the number of system calls; setting it lower keeps the heap a
   little smaller. *)

(* ----------------------------------------------------------------------- *)

(****** Page-level allocator ******)

(* The freelist is sorted by blocksize, linked through the first page in
   each block, using the "link" field in the "desc" array.  Page allocation
   is best-fit.  For elements of the same blocksize, they are sorted by
   page number, to make the showheap display more easily readable, and to
   slightly reduce fragmentation. *)

(* FindFreePages allocates a run of "n" free pages, which we would prefer
   not be near pages in the current space with notes in notAfter.  The
   allocator can thus be used to separate pages with different notes, since
   they will have different lifetimes.  This is a concern only when
   incremental and generational collection are combined. *)

PROCEDURE FindFreePages (n: INTEGER; notAfter: SET OF Note): Page =
  VAR p: Page;
  BEGIN
    IF collectorState = CollectorState.Off THEN
      p := AllocateFreePagesFromBlock(n, SET OF Note{}, TRUE);
      IF p # Nil THEN RETURN p; END;
    ELSE
      p := AllocateFreePagesFromBlock(n, notAfter, TRUE);
      IF p # Nil THEN RETURN p; END;
      p := AllocateFreePagesFromBlock(n, SET OF Note{}, FALSE);
      IF p # Nil THEN RETURN p; END;
    END;
    GrowHeap(n);
    p := AllocateFreePagesFromBlock(n, SET OF Note{}, TRUE);
    IF doChecks THEN
      IF NOT (p # Nil) THEN
        PutText("GC ERROR >> Garbage collector crash (3237)\n");
        RTOS.Crash();
      END;
      <* ASSERT p # Nil *>
    END;
    RETURN p;
  END FindFreePages;

VAR free: Page;                  (* the head of the freelist *)

VAR
  maxFreeLength: INTEGER;

(* AllocateFreePagesFromBlock finds the first block large enough to satisfy
   the request.  "notAfter" is the set of page notes in the current space
   that the block allocated from must not immediately follow; this is used
   to separate Note.Allocated pages from Note.Copied pages.  If "front" is
   TRUE, the pages will be allocated from the beginning of the block, else
   from the end; this is also used to separate Note.Allocated Pages from
   Note.Copied pages.  If the block is bigger than the request, the
   remainder is left at the right point in the freelist.  If no block
   exists, Nil is returned. *)

PROCEDURE AllocateFreePagesFromBlock (n       : INTEGER;
                                      notAfter: SET OF Note;
                                      front   : BOOLEAN      ): Page =
  VAR
    p                   := free;
    prevP               := Nil;
    prevLength          := 0;
    length    : INTEGER;
  BEGIN
    LOOP
      IF p = Nil THEN RETURN Nil; END;
      length := FreeLength(p);
      IF length >= n
           AND NOT (p > p0 AND desc[(p - 1) - p0].space = Space.Current
                      AND desc[(p - 1) - p0].note IN notAfter) THEN
        EXIT;
      END;
      prevP := p;
      prevLength := length;
      p := desc[p - p0].link;
    END;

    IF length = maxFreeLength AND desc[p - p0].link = Nil THEN
      IF length = n THEN
        IF prevP = Nil THEN
          maxFreeLength := 0;
        ELSE
          maxFreeLength := FreeLength(prevP);
        END;
      ELSE
        IF prevP # Nil AND FreeLength(prevP) > length - n THEN
          maxFreeLength := FreeLength(prevP);
        ELSE
          maxFreeLength := length - n;
        END;
      END;
    END;

    IF length = n THEN
      IF prevP = Nil THEN
        free := desc[p - p0].link;
      ELSE
        desc[prevP - p0].link := desc[p - p0].link;
      END;
      RETURN p;
    ELSE
      VAR
        newP, fragP: Page;
        fragLength : CARDINAL;
      BEGIN
        IF front THEN
          newP := p;
          fragP := p + n;
        ELSE
          newP := p + length - n;
          fragP := p;
        END;
        fragLength := length - n;
        IF fragLength = 1 THEN
          desc[fragP - p0].stacked (* freechunk *) := FALSE;
        ELSE
          desc[fragP - p0].stacked (* freechunk *) := TRUE;
          IF doChecks THEN
            <* ASSERT fragP + 1 < p1 *>
            <* ASSERT desc[fragP + 1 - p0].space = Space.Free *>
          END;
          desc[fragP + 1 - p0].link := fragLength;
        END;
        IF fragLength > prevLength THEN
          IF prevP = Nil THEN
            free := fragP;
          ELSE
            desc[prevP - p0].link := fragP;
          END;
          desc[fragP - p0].link := desc[p - p0].link;
        ELSE
          IF prevP = Nil THEN
            free := desc[p - p0].link;
          ELSE
            desc[prevP - p0].link := desc[p - p0].link;
          END;
          VAR
            pp     := free;
            prevPP := Nil;
          BEGIN
            LOOP
              IF pp = Nil THEN EXIT; END;
              VAR length := FreeLength(pp);
              BEGIN
                IF length > fragLength
                     OR (length = fragLength AND pp > fragP) THEN
                  EXIT;
                END;
              END;
              prevPP := pp;
              pp := desc[pp - p0].link;
            END;
            desc[fragP - p0].link := pp;
            IF prevPP = Nil THEN
              free := fragP;
            ELSE
              desc[prevPP - p0].link := fragP;
            END;
          END;
        END;
        RETURN newP;
      END;
    END;
  END AllocateFreePagesFromBlock;

(* RebuildFreelist rebuilds the free list, from the "desc" array.  It first
   links all free blocks into the free list, then it sorts the free list.
   The sort used is insertion sort, which is quadratic in the number of
   different block sizes, but only linear in the number of pages. *)

PROCEDURE RebuildFreelist () =
  BEGIN
    (* collect all free pages *)
    VAR
      prevP     := Nil;
      prevSpace := Space.Unallocated;
      freeLength: INTEGER;
    BEGIN
      (* link together the first pages of all free blocks *)
      (* find lengths of blocks of free pages *)
      FOR p := p0 TO p1 - 1 DO
        VAR space := desc[p - p0].space;
        BEGIN
          (* first free page in a chain *)
          IF space = Space.Free AND prevSpace # Space.Free THEN
            IF prevP = Nil THEN
              free := p;
            ELSE
              desc[prevP - p0].link := p;
            END;
            prevP := p;
            freeLength := 0;
          END;
          (* a free page *)
          IF space = Space.Free THEN
            INC(freeLength);
          END;
          (* first non-free page *)
          IF space # Space.Free AND prevSpace = Space.Free THEN
            IF freeLength = 1 THEN
              desc[prevP - p0].stacked (* freechunk *) := FALSE;
            ELSE
              desc[prevP - p0].stacked (* freechunk *) := TRUE;
              desc[prevP + 1 - p0].link := freeLength;
            END;
          END;
          prevSpace := space;
        END;
      END;

      (* the last page is free and marks the end of free block *)
      IF prevSpace = Space.Free THEN
        IF freeLength = 1 THEN
          desc[prevP - p0].stacked (* freechunk *) := FALSE;
        ELSE
          desc[prevP - p0].stacked (* freechunk *) := TRUE;
          desc[prevP + 1 - p0].link := freeLength;
        END;
      END;

      IF prevP = Nil THEN
        free := Nil;
      ELSE
        desc[prevP - p0].link := Nil;
      END;
    END;

    (* sort them, using insertion sort *)
    VAR
      n     := 1;                (* smallest block size *)
      p     := free;             (* start of sublist we're examining *)
      prevP := Nil;              (* element before sublist *)
    BEGIN
      LOOP
        VAR
          excess     := Nil;
          prevExcess := Nil;
        BEGIN
          (* separate off blocks over "n" long into excess list *)
          WHILE p # Nil DO
            VAR length := FreeLength(p);
            BEGIN
              IF doChecks THEN
                IF NOT (length >= n) THEN
                  PutText("GC ERROR >> Garbage collector crash (3387)\n");
                  RTOS.Crash();
                END;
                <* ASSERT length >= n *>
              END;
              IF length > n THEN
                IF prevExcess = Nil THEN
                  excess := p;
                ELSE
                  desc[prevExcess - p0].link := p;
                END;
                IF prevP = Nil THEN
                  free := desc[p - p0].link;
                ELSE
                  desc[prevP - p0].link := desc[p - p0].link;
                END;
                prevExcess := p;
              ELSE
                prevP := p;
              END;
            END;
            p := desc[p - p0].link;
          END;
          (* maybe done *)
          IF excess = Nil THEN EXIT; END;
          IF doChecks THEN
            IF NOT (prevExcess # Nil) THEN
              PutText("GC ERROR >> Garbage collector crash (3412)\n");
              RTOS.Crash();
            END;
            <* ASSERT prevExcess # Nil *>
          END;
          (* link longer blocks onto end *)
          IF prevP = Nil THEN
            free := excess;
          ELSE
            desc[prevP - p0].link := excess;
          END;
          desc[prevExcess - p0].link := Nil;
          p := excess;
        END;
        (* find smallest element size of remaining bocks *)
        n := LAST(CARDINAL);
        VAR pp := p;
        BEGIN
          REPEAT
            VAR length := FreeLength(pp);
            BEGIN
              IF length < n THEN n := length; END;
            END;
            pp := desc[pp - p0].link;
          UNTIL pp = Nil;
        END;
      END;
      maxFreeLength := n;
    END;
  END RebuildFreelist;

PROCEDURE PrintFreeList () =
  VAR
    p := free;
    i := 0;
  BEGIN
    LOOP
      IF p = Nil THEN EXIT; END;
      PutInt(i); PutText(": ");
      PutInt(p-p0); PutText(" - ");
      PutInt(FreeLength(p)); PutText("\n");
      p := desc[p - p0].link;
    END;
  END PrintFreeList;

(* FreeLength returns the number of free pages starting at page p. *)

PROCEDURE FreeLength (p: Page): CARDINAL =
  VAR result := 1;
  BEGIN
    IF p < p0 OR p >= p1 THEN
      PutText("GC ERROR >> fucked free\n");
      RTOSMachine.Debugger();
    END;
    IF doChecks THEN
      IF NOT (desc[p - p0].space = Space.Free) THEN
        PutText("GC ERROR >> Garbage collector crash (3446)\n");
        RTOS.Crash();
      END;
      <* ASSERT desc[p - p0].space = Space.Free *>
    END;
    IF desc[p - p0].stacked (* freechunk *) THEN
      IF doChecks THEN
        IF p + 1 > p1 OR
          desc[p + 1 - p0].space # Space.Free
         THEN
          PutText("GC ERROR >> Garbage collector crash (3447)\n");
          RTOS.Crash();
        END;
        <* ASSERT p + 1 < p1 *>
        <* ASSERT desc[p + 1 - p0].space = Space.Free *>
      END;
      result := desc[p + 1 - p0].link;
    END;
    RETURN result;
  END FreeLength;

(* GrowHeap adds a block of at least "minNewPages" free pages to the heap,
   and links it into the free list. *)

VAR fragment0, fragment1: ADDRESS := NIL;
VAR InitialBytes: INTEGER;

CONST
  MinNewBytes  = 262144;         (* grow the heap by at least 256K *)
  MinNewFactor = 20;            (* grow the heap by at least 20% *)

PROCEDURE GrowHeap (pp: INTEGER) =
  VAR
    newChunk    : ADDRESS;
    newSideSpan : INTEGER;
    firstNewPage: Page;
    lastNewPage : Page;
    newP0       : Page;
    newP1       : Page;
  BEGIN
    IF doVerbose AND verbose > 0 THEN
      PutText("GrowHeap: "); PutInt(pp); PutText("\n");
    END;

    IF allocatedPages = 0 THEN
      pp := MAX(pp, (InitialBytes + BytesPerPage - 1) DIV BytesPerPage);
    ELSE
      pp := MAX(pp, (MinNewBytes + BytesPerPage - 1) DIV BytesPerPage); 
      pp := MAX(pp, (allocatedPages * MinNewFactor + 50) DIV 100);
    END;
    VAR bytes := pp * BytesPerPage;
    BEGIN
      IF allocatedPages # 0 THEN
        (* we are crashing, allow nested allocation, dump stats and crash *)
        PutText("\nGC ERROR >>> ran out of traced heap\n");
        nestedAlloc := TRUE;
        DumpAll(pp);
        PutText("\nGC ERROR >>> crashing the runtime\n");
        RTOS.Crash();
      END;

      newChunk := RTOS.GetMemory(bytes);
      IF newChunk = NIL OR newChunk = LOOPHOLE(-1, ADDRESS) THEN
        PutText("GC ERROR >>> Could not extend the traced heap\n");
        PutText("collection threshold:\n");
        PutInt(threshold[0]); PutText(" ");
        PutInt(threshold[1]); PutText("\n");
        RTHeapPages.DumpPageStatus (NIL, TRUE);
        RTOS.Crash();
      END;
      IF fragment1 = newChunk THEN
        newChunk := fragment0;
        bytes := bytes + (fragment1 - fragment0);
      END;
      VAR excess := (-LOOPHOLE(newChunk, INTEGER)) MOD BytesPerPage;
      BEGIN
        INC(newChunk, excess);
        DEC(bytes, excess);
      END;
      VAR pages := bytes DIV BytesPerPage;
      BEGIN
        firstNewPage := Word.RightShift(LOOPHOLE(newChunk, INTEGER), 
                                        LogBytesPerPage);
        lastNewPage := firstNewPage + pages - 1;
        fragment0 := LOOPHOLE((firstNewPage + pages)*BytesPerPage, ADDRESS);
        fragment1 := newChunk + bytes;
      END;
    END;
    (* determine the new boundaries of the allocated pages *)
    IF p0 = Nil THEN
      newP0 := firstNewPage;
      newP1 := lastNewPage + 1;
    ELSIF firstNewPage < p0 THEN
      newP0 := firstNewPage;
      newP1 := p1;
    ELSIF p1 <= lastNewPage THEN
      newP0 := p0;
      newP1 := lastNewPage + 1;
    ELSE
      newP0 := p0;
      newP1 := p1;
    END;
    (* extend the side arrays if necessary *)
    newSideSpan := newP1 - newP0;
    IF desc = NIL OR newSideSpan # NUMBER(desc^) THEN
      WITH newDesc = NEW(UNTRACED REF ARRAY OF Desc, newSideSpan) DO
        IF desc # NIL THEN
          FOR i := FIRST(desc^) TO LAST(desc^) DO
            newDesc[i + p0 - newP0] := desc[i];
          END;
          FOR i := p1 TO firstNewPage - 1 DO
            newDesc[i - newP0].space := Space.Unallocated;
          END;
          FOR i := lastNewPage + 1 TO p0 - 1 DO
            newDesc[i - newP0].space := Space.Unallocated;
          END;
          DISPOSE(desc);
        END;
        desc := newDesc;
      END;
    END;
    p0 := newP0;
    p1 := newP1;
    FOR i := firstNewPage TO lastNewPage DO
      desc[i - p0].space := Space.Free;
      INC(freePages);
    END;
    IF doPerf AND perfOn THEN
      PerfGrow(firstNewPage, lastNewPage - firstNewPage + 1);
    END;
    INC(allocatedPages, lastNewPage - firstNewPage + 1);
    RebuildFreelist();
    IF RTHeapStats.ReportFragmentation THEN
      RTHeapStats.InitSampleArray(allocatedPages*BytesPerPage);
    END;
    (*
    PutText("\n--------------- GROW HEAP done ----------\n");
    RTHeapPages.DumpPageStatus(NIL, TRUE);
    PutText("\n-----------------------------------------\n");
    *)
  END GrowHeap;

(*** SYNCHRONIZATION ***)

(* Wait() waits on a condition, implemented compatibly with RTOS.LockHeap().
   Wait() will be called only from background threads.  The caller
   will be in a critical section.  *)

VAR
  mutex     :   MUTEX;	(* Defer NEW until boot. *)
  condition :   Thread.Condition; (* Defer NEW until boot. *)

PROCEDURE Wait () =
  BEGIN
    (* This procedure may not be called from inside the collector. *)
    LOCK mutex DO Thread.Wait(mutex, condition); END;
  END Wait;

(* Broadcast() broadcasts a condition, implemented compatibly with
   RTOS.LockHeap().  Broadcast will not be called from inside the collector, so
   that references to the heap will succeed. *)

PROCEDURE Broadcast () =
  BEGIN
    Thread.Broadcast(condition);
  END Broadcast;

(*** INITIALIZATION ***)

CONST MaxAlignment  = 8;
CONST MaxAlignMask  = 2_0111;     (* bit mask to capture MaxAlignment *)
TYPE  MaxAlignRange = [0 .. MaxAlignment - 1];

VAR align: ARRAY MaxAlignRange, [1 .. MaxAlignment] OF CARDINAL;
(* align[i,j] == RTMisc.Align (i, j) - i *)

PROCEDURE Init () =
  BEGIN
    PutText("GC >> using the copying collector\n");
    nestedAlloc := FALSE;
    next_cnt := 1;
    verbose := 0;
    disableCount := 1;
    delayInit := 1;
    delayCnt  := 0;
    maxFreeLength := LAST(INTEGER);

    (* get the number of bytes for initial allocation *)
    InitialBytes := (RTMem.GetMaxGCSize() DIV BytesPerPage - 2) * BytesPerPage;
    (*
    InitialBytes := 2500 * 8 * 1024;
    *)

    VAR
      str := RTParams.RawValue("S");
      ptr : UNTRACED REF CHAR := str;
      n := 0;
      c: CHAR;
    BEGIN
      IF ptr # NIL THEN
        LOOP
          c := ptr^;
          IF c < '0' OR c > '9' THEN EXIT; END;
          n := n * 10 + ORD(c) - ORD('0');
          INC(ptr, BYTESIZE(CHAR));
        END;
        InitialBytes := n * BytesPerPage;
        IF doVerbose AND verbose > 0 THEN
          PutText("GC >> heap size set by a boot parameter to ");
          PutInt(InitialBytes); PutText(" bytes\n");
        END;
      END;
    END;

    threshold[0] := (InitialBytes DIV 4 DIV BytesPerPage - 1) * 100;
    threshold[1] := gcRatio;

    PutText("GC >> initial heap size: ");  PutInt(InitialBytes); 
    PutText(" ("); PutHex(InitialBytes); PutText(") bytes\n");

    weakTable := NEW(UNTRACED REF ARRAY OF WeakEntry, 0);

    (* initialize the alignment array *)
    FOR i := FIRST(align) TO LAST(align) DO
      FOR j := FIRST(align[0]) TO LAST(align[0]) DO
        align[i, j] := RTMisc.Upper(i, j) - i;
      END;
    END;
    RTTypeSecurity.Init();    
    RTRefCount.Init ();

    (* allocates untraced heap to collect stats on traced heap *)
    RTHeapStats.Init ();

    RTAllocator.Init();
    RTHeapTrace.Init();

    RTHeapDebug.Init();
    RTHeapPages.Init();



    mutex := NEW(MUTEX);
    condition := NEW(Thread.Condition);
    RTutils.Init();
    RTStrongRef.Init();
    mover := NEW (Mover);
    stacker := NEW (Stacker);
    stacker.data := NEW(UNTRACED REF ARRAY OF RefReferent, 100);

    IF doEagerReclamation THEN
      fwdTable := NEW(ForwardHashUtbl.Default).init(25000);
      fwdIter  := fwdTable.iterate();
    END;

    ResetStat();
    disableCount := 0;
    IF doVerbose AND verbose > 0 THEN
      PutText ("GC >> initialized...\n"); 
    END;
  END Init;

PROCEDURE ResetStat() =
  BEGIN
    auto_cnt  := 0;
    req_cnt   := 0;
    back_cnt  := 0;
    crash_cnt := 0;
    full_cnt  := 0;
    part_cnt  := 0;
    high_cnt  := 0;
    long_cnt  := 0;
    next_cnt  := 0;
    manu_cnt  := 0;
    nogen_cnt := 0;
    fault_cnt := 0;
    total_cnt := 0;
    inc_cnt   := 0;
    ninc_cnt  := 0;
    fault_cnt := 0;
    gray_cnt  := 0;
    old_cnt   := 0;

    FOR i := FIRST(shortPauses) TO LAST(shortPauses) DO
      shortPauses[i] := 0;
    END;
    FOR i := FIRST(longPauses) TO LAST(longPauses) DO
      longPauses[i] := 0;
    END;

    RT0u.total_traced_bytes := 0;
    RT0u.total_untraced_bytes := 0;

    IF RTHeapStats.ReportFragmentation THEN
      RTHeapStats.ResetSamples();
      (* force new initial sample *)
      ForceFragSample();
    END;

    RTHeapTrace.Reset();
    RTHisto.ResetHistogram();
  END ResetStat;

(* add a new frag sample *)
PROCEDURE ForceFragSample() =
  BEGIN
    IF RTHeapStats.ReportFragmentation THEN
      IF collectorState = CollectorState.Off THEN
        (* activePages is right *)
        RTHeapStats.SampleFrag(currentActiveBytes, 
                               activePages*BytesPerPage);
      ELSE
        (* collecting, activePages needs to be added to old space *)
        RTHeapStats.SampleFrag(currentActiveBytes, 
                               (oldPrevActivePages+activePages)*BytesPerPage);
      END;
    END;
  END ForceFragSample;


PROCEDURE GetStatistics(VAR s: Statistics; traverseHeap: BOOLEAN := TRUE) =
  BEGIN
    s.collectorState := ORD(collectorState);
    s.allocatedPages := allocatedPages;
    s.bytesPerPage := BytesPerPage;
    s.minAddress := LOOPHOLE(MinAddress(), Word.T);
    s.maxAddress := LOOPHOLE(MaxAddress(), Word.T);
    s.threshold  := threshold[0] DIV threshold[1];
    s.threshold0 := threshold[0];
    s.gcRatio    := gcRatio;
    s.activePages := activePages;
    s.maxPause := maxPause;
    s.avePause := accPause DIV nPause;
    s.nPause   := nPause;
    s.freePages := allocatedPages - activePages;
    s.smallNewPages := smallNewPages;
    s.largeNewPages := largeNewPages;
    s.smallCopyPages := smallCopyPages;
    s.largeCopyPages := largeCopyPages;
    s.smallPromotionPages := smallPromotionPages;
    s.largePromotionPages := largePromotionPages;
    s.totalTracedBytes := RT0u.total_traced_bytes;
    s.totalUntracedBytes := RT0u.total_untraced_bytes;
    s.autoCnt := auto_cnt;
    s.reqCnt := req_cnt;
    s.backCnt := back_cnt;
    s.crashCnt := crash_cnt;
    s.fullCnt := full_cnt;
    s.partCnt := part_cnt;
    s.highCnt := high_cnt;
    s.longCnt := long_cnt;
    s.nextCnt := next_cnt;
    s.manuCnt := manu_cnt;
    s.nogenCnt := nogen_cnt;
    s.faultCnt := fault_cnt;
    s.totalCnt := total_cnt;
    s.incCnt := inc_cnt;
    s.nincCnt := ninc_cnt;
    s.faultCnt := fault_cnt;
    s.grayCnt := gray_cnt;
    s.oldCnt := old_cnt;
    FOR i := FIRST(shortPauses) TO LAST(shortPauses) DO
      s.shortPauses[i] := shortPauses[i];
    END;
    FOR i := FIRST(longPauses) TO LAST(longPauses) DO
      s.longPauses[i] := longPauses[i];
    END;

    s.ambRefCnt := ambiguousReferencesCnt;
    s.strRefCnt := strongReferencesCnt;
    s.unsafeAmbRefCnt := unsafeAmbiguousReferencesCnt;
    s.uniqAmbRefCnt := uniqueAmbiguousReferencesCnt;
    s.uniqStrRefCnt := uniqueStrongReferencesCnt;
    s.unsafeUniqRefCnt := unsafeUniqueAmbiguousReferencesCnt;

    s.allocCnt := allocCnt;
    s.deallocCnt := deallocCnt;
    s.promoteCnt := promoteCnt;
    s.moveCnt := moveCnt;
  END GetStatistics;

PROCEDURE GetMoreStatistics (VAR s: Statistics) =
  BEGIN
    PutText("GC ERROR >> GetMoreStatistics not merged yet\n");
  END GetMoreStatistics;

PROCEDURE RTInit() =
  BEGIN
    (* set up timing stuff *)
    IF DoTimings OR DoTimingsXtra THEN 
      auto_timer   := Spy.Create("GC-auto", TRUE);
      req_timer    := Spy.Create("GC-manl", TRUE);
      back_timer   := Spy.Create("GC-back", TRUE);
      crash_timer  := Spy.Create("GC-crash", TRUE);
      state0_timer := Spy.Create("GC-state 0", TRUE);
      state1_timer := Spy.Create("GC-state 1", TRUE);
      state2_timer := Spy.Create("GC-state 2", TRUE);
      state3_timer := Spy.Create("GC-state 3", TRUE);
      state4_timer := Spy.Create("GC-state 4", TRUE);
      state5_timer := Spy.Create("GC-state 5", TRUE);
      ambState1_timer   := Spy.Create("GC-stack scan", TRUE);
      ambState2_timer   := Spy.Create("GC-strongrefs", TRUE);
      ambState3_timer   := Spy.Create("GC-untraced scan", TRUE);
      ambState4_timer   := Spy.Create("GC-global scan", TRUE);
      monitors1_timer   := Spy.Create("GC-sanity before", TRUE);
      monitors2_timer   := Spy.Create("GC-sanity after", TRUE);
      flip1_timer       := Spy.Create("GC-flip before", TRUE);
      flip2_timer       := Spy.Create("GC-flip after", TRUE);
      free_timer        := Spy.Create("GC-free", TRUE);
      prepare_timer     := Spy.Create("GC-prepare", TRUE);
      init_timer        := Spy.Create("GC-init", TRUE);
      walkGlobals_timer := Spy.Create("GC-walk global", TRUE);
      pause_timer       := Spy.Create("GC-pause", TRUE);
      prom_timer        := Spy.Create("GC-promote", TRUE);
      move_timer        := Spy.Create("GC-move", TRUE);
      fault_timer       := Spy.Create("GC-fault", TRUE);
      
      allocTraced_timer   := Spy.Create("Alloc-traced", TRUE);
      allocUntraced_timer := Spy.Create("Alloc-untraced", TRUE);
      allocShort_timer    := Spy.Create("Alloc-short", TRUE);
      allocLong_timer     := Spy.Create("Alloc-long", TRUE);
      allocGC_timer       := Spy.Create("Alloc-gc", TRUE);
      allocInit_timer     := Spy.Create("Alloc-init", TRUE);

    
      nsl1Timer := Spy.Create("NSL-1", TRUE);
      nsl2Timer := Spy.Create("NSL-2", TRUE);
      nsl3Timer := Spy.Create("NSL-3", TRUE);
      nsl4Timer := Spy.Create("NSL-4", TRUE);

      (*
      Spy.Reset();
      *)
    END;
    IF RTParams.IsPresent("nogc") THEN disableCount := 1; END;
    IF RTParams.IsPresent("novm") THEN disableVMCount := 1; END;
    IF RTParams.IsPresent("noincremental") THEN incremental := FALSE; END;
    IF RTParams.IsPresent("nogenerational") THEN generational := FALSE; END;
    IF RTParams.IsPresent("paranoidgc") THEN InstallSanityCheck(); END;
  END RTInit;

VAR
  nPause   : INTEGER := 0;
  accPause : INTEGER := 0;
  maxPause : INTEGER := 0;
  shortPauses : ARRAY [0..100] OF INTEGER;
  longPauses  : ARRAY [0..10000] OF INTEGER;
  
VAR
  auto_cnt  : CARDINAL; (* number of collections started automatically *)
  req_cnt   : CARDINAL; (* number of collections started manually *)
  back_cnt  : CARDINAL; (* of background collections *)
  crash_cnt : CARDINAL; (* number of collections started by Crash *)
  full_cnt  : CARDINAL; (* number of full collections *)
  part_cnt  : CARDINAL; (* number of partial collections *)
  high_cnt  : CARDINAL;
  long_cnt  : CARDINAL;
  next_cnt  : CARDINAL;
  manu_cnt  : CARDINAL;
  nogen_cnt : CARDINAL;
  total_cnt : CARDINAL;
  inc_cnt   : CARDINAL;
  ninc_cnt  : CARDINAL;
  fault_cnt : CARDINAL;
  gray_cnt  : CARDINAL;
  old_cnt   : CARDINAL;

VAR
  auto_timer   : Spy.T;        (* collections started automatically *)
  req_timer    : Spy.T;        (* collections started manually *)
  back_timer   : Spy.T;        (* background collections *)
  crash_timer  : Spy.T;        (* collections started by Crash *)
  state0_timer : Spy.T;        (* states 0, 1, 2, 3, 4, 5 *)
  state1_timer : Spy.T;
  state2_timer : Spy.T;
  state3_timer : Spy.T;
  state4_timer : Spy.T;
  state5_timer : Spy.T;
  ambState1_timer   : Spy.T;   (* scanning stacks *)
  ambState2_timer   : Spy.T;   (* strongrefs *)
  ambState3_timer   : Spy.T;   (* scanning of the untraced heap *)
  ambState4_timer   : Spy.T;   (* global data segments *)
  monitors1_timer   : Spy.T;   (* sanity checks before GC *)
  monitors2_timer   : Spy.T;   (* sanity checks after GC *)
  flip1_timer       : Spy.T;   (* make new space into prev space *)
  flip2_timer       : Spy.T;   (* make old space into free space *)
  free_timer        : Spy.T;   (* rebuild free list *)
  prepare_timer     : Spy.T;   (* prepare all pages before collection *)
  init_timer        : Spy.T;
  walkGlobals_timer : Spy.T;   (* prepare all pages before collection *)

  (* allocShort encompasses full allocation.
     allocLong encompasses LongAlloc (may include GC).
     allocGC measures ONLY GC.
     
     Normal allocation time is therefore allocShort - allocGC
     Long allocation time is allocLong - allocGC
  *)
  allocShort_timer  : Spy.T;   
  allocLong_timer   : Spy.T;   
  allocGC_timer     : Spy.T;   
  prom_timer        : Spy.T;
  move_timer        : Spy.T;
  fault_timer       : Spy.T;
  nsl1Timer : Spy.T;
  nsl2Timer : Spy.T;
  nsl3Timer : Spy.T;
  nsl4Timer : Spy.T;

VAR
  doSweep := FALSE;
  showUnsafeAmbiguous := FALSE;
  anchorUnsafeAmbiguous := FALSE;

PROCEDURE SetSweepUntraced(on: BOOLEAN) =
  BEGIN
    doSweep := on;
  END SetSweepUntraced; 

PROCEDURE ShowUnsafeAmbiguous(on: BOOLEAN) =
  BEGIN
    showUnsafeAmbiguous := on
  END ShowUnsafeAmbiguous;

PROCEDURE AnchorUnsafeAmbiguous(on: BOOLEAN) = 
  BEGIN
    anchorUnsafeAmbiguous := on;
  END AnchorUnsafeAmbiguous;

PROCEDURE Verbose (level: INTEGER) = 
  BEGIN
    verbose := level;
  END Verbose; 

PROCEDURE CheckAmbiguous (i: INTEGER) =
  VAR
    amb_spl: RTOSMachine.InterruptLevel;
  BEGIN
    amb_spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);
    PutText("CheckAmbiguous: "); PutInt(i); PutText("\n");
    
    InitAmbiguousReferenceTracing();

    (* Examine the strong references *)
    PutText("CheckAmbiguous: stage 1\n");
    ambiguousStage := 1;
    VAR 
      prevAlloc := nestedAlloc;
    BEGIN
      nestedAlloc := TRUE;
      RTStrongRef.ProcessRefs(NoteStackLocations);
      nestedAlloc := prevAlloc;
    END;

    (* Examine the stacks for possible pointers *)
    PutText("CheckAmbiguous: stage 2\n");
    ambiguousStage := 2;
    ThreadF.ProcessStacks(FindAmbiguous);

    (* Examine all of untraced heap *)
    IF doSweep THEN
      PutText("CheckAmbiguous: stage 3\n");
      ambiguousStage := 3;
      SweepUntracedHeap(FindAmbiguous);

      (* FIXME - cannot sweep global data segments until traced roots can *)
      (* be disambiguated - pardy *)
      (* 
         (* Examine all of global data segment*)
            PutText("CheckAmbiguous: stage 4\n");
            ambiguousStage := 4;
            FindAmbiguous(ADR(RT0u.E_etext), ADR(RT0u.E_end));
      *)
    END;

    PutText("CheckAmbiguous: "); PutInt(i); 
    PutText(" (done)\n");

    RTOSMachine.RestoreInterruptMask(amb_spl);
  END CheckAmbiguous;

PROCEDURE FindAmbiguous (start, stop: ADDRESS) =
  VAR
    fp                                := start;
    firstAllocatedAddress             := PageToAddress(p0);
    firstNonAllocatedAddress          := PageToAddress(p1);
    p                       : ADDRESS;
    pp                      : Page;
  BEGIN
    (*
      PutText("FindAmbiguous: "); 
      PutHex(LOOPHOLE(start, INTEGER)); PutText(" ");
      PutHex(LOOPHOLE(stop, INTEGER)); PutText("\n");
    *)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF firstAllocatedAddress <= p AND p < firstNonAllocatedAddress THEN
        pp := Word.RightShift (LOOPHOLE(p, INTEGER), LogBytesPerPage);
        IF desc[pp - p0].space = Space.Current THEN
          NoteAmbiguousReference(fp, p);
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END FindAmbiguous; 

PROCEDURE DumpClean () =
  BEGIN
    PutText("Clean regions:\n"); 
    IF cleanRegions = NIL THEN
      PutText("  NIL\n"); 
    ELSE
      FOR i := 0 TO cleanRegionsCnt - 1 DO
        PutInt(i, 3); PutText(" ");
        PutAddr(cleanRegions[i].start); PutText(" "); 
        PutAddr(cleanRegions[i].end); PutText("\n"); 
      END;
    END;
  END DumpClean;

PROCEDURE PrintCloseClean (p: ADDRESS) =
  BEGIN
    PutText("Close clean regions for: "); 
    PutAddr(p); PutText("\n"); 
    IF cleanRegions = NIL THEN
      PutText("  NIL\n"); 
    ELSE
      FOR i := 0 TO cleanRegionsCnt - 1 DO
        IF (i>0 AND cleanRegions[i-1].end < p AND cleanRegions[i].start > p) OR
          (cleanRegions[i].start <= p AND cleanRegions[i].end >= p) OR
          (i<cleanRegionsCnt AND cleanRegions[i].start < p AND 
          cleanRegions[i+1].end > p)
         THEN
          PutInt(i, 5); PutText(" ");
          PutAddr(cleanRegions[i].start); PutText(" "); 
          PutAddr(cleanRegions[i].end); PutText("\n"); 
        END
      END;
      PutText("\n"); 
    END;
  END PrintCloseClean;

PROCEDURE IsOnStack (p: ADDRESS): BOOLEAN =
  BEGIN
    IF cleanRegions = NIL THEN
      RETURN FALSE;
    END;
    FOR i := 0 TO cleanRegionsCnt - 1 DO
      IF cleanRegions[i].start <= p AND cleanRegions[i].end >= p THEN
        RETURN cleanRegions[i].stack;
      ELSIF cleanRegions[i].start > p THEN
        RETURN FALSE;
      END;
    END;
    RETURN FALSE;
  END IsOnStack; 

PROCEDURE RegisterClean (start: ADDRESS; size: INTEGER; stack: BOOLEAN) =
  VAR
    pos: INTEGER;
    end: ADDRESS := start + size - ADRSIZE(ADDRESS);
    spl: RTOSMachine.InterruptLevel;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);

    IF traceClean THEN
      PutText("RegisterClean: ");
      PutAddr(start); PutText(" ");
      PutAddr(end); PutText(" ");
      PutBoolean(stack); PutText("\n");
      PrintCloseClean(start);
    END;

    (* check whether the region is non-empty *)
    IF size <= 0 THEN
      PutText("GC ERROR >> empty clean region\n");
      RTOS.Crash();
    END;

    (* check whether the region is within untraced heap *)
    IF start < RT0u.kmembase OR end > RT0u.kmemlimit THEN
      PutText("GC ERROR >> clean region outside of the untraced heap\n");
      RTOS.Crash();
    END;

    (* create the list if not created yet *)
    IF cleanRegions = NIL THEN
      cleanRegions := NEW(RegionVector, 100);
    END;

    (* reallocate the array to accommodate new information *)
    IF cleanRegionsCnt = NUMBER(cleanRegions^) THEN
      VAR 
        size := NUMBER(cleanRegions^);
        new := NEW(RegionVector, 2 * size);
      BEGIN
        SUBARRAY(new^, 0, size) := SUBARRAY(cleanRegions^, 0, size);
        FOR i := FIRST(cleanRegions^) TO LAST(cleanRegions^) DO
          cleanRegions[i].start := NIL;
          cleanRegions[i].end   := NIL;
          cleanRegions[i].stack := FALSE;
        END;
        DISPOSE(cleanRegions);
        cleanRegions := new;
      END;
    END;

    (* find the index in the sorted table for the new region *)
    (* reject overlapping regions *)
    pos := 0;
    IF cleanRegionsCnt # 0 THEN
      WHILE pos < cleanRegionsCnt AND cleanRegions[pos].start < start DO
        INC(pos);
      END;
      IF (pos < cleanRegionsCnt AND cleanRegions[pos].start = start) OR
        (pos > 0 AND cleanRegions[pos-1].end >= start) OR
        (pos < cleanRegionsCnt-1 AND cleanRegions[pos].start <= end)
       THEN
        PutText("\nGC ERROR >> overlapping clean regions: ");
        PutInt(pos); PutText(" "); 
        PutInt(cleanRegionsCnt); PutText("\nthis: "); 
        PutAddr(start); PutText(" "); 
        PutAddr(end); 
        XXX(start);
        IF pos > 0 THEN 
          PutText("\nprev: "); PutAddr(cleanRegions[pos-1].start); 
          PutText(" "); PutAddr(cleanRegions[pos-1].end); 
          PutText("\n"); 
          XXX(cleanRegions[pos-1].start);
        END;
        IF pos < cleanRegionsCnt THEN 
          PutText("\nnext: "); PutAddr(cleanRegions[pos].start);
          PutText(" "); PutAddr(cleanRegions[pos].end);
          PutText("\n"); 
          XXX(cleanRegions[pos].start);
        END;
        PutText("\n"); 
        IF pos < cleanRegionsCnt AND cleanRegions[pos].start = start THEN PutText("ONE\n"); END;
        IF pos > 0 AND cleanRegions[pos-1].end >= start THEN PutText("TWO\n"); END;
        IF pos < cleanRegionsCnt-1 AND cleanRegions[pos].start <= end THEN PutText("THREE\n"); END;
        RTOS.Crash();
      END;

      (* shift all regions by 1 *)
      FOR i := cleanRegionsCnt TO pos+1 BY -1  DO
        cleanRegions[i] := cleanRegions[i-1];
      END;
    END;

    (* add new region *)
    cleanRegions[pos].start := start;
    cleanRegions[pos].end := end;
    cleanRegions[pos].stack := stack;
    INC(cleanRegionsCnt);

    IF traceClean THEN
      PrintCloseClean(start);
    END;
    RTOSMachine.RestoreInterruptMask(spl);
  END RegisterClean;

PROCEDURE UnregisterClean (start: ADDRESS; size: INTEGER) =
  VAR
    spl: RTOSMachine.InterruptLevel;
    found := FALSE;
  BEGIN
    spl := RTOSMachine.SetInterruptMask(RTOSMachine.InterruptClass.High);

    IF traceClean THEN
      PutText("UnregisterClean: ");
      PutAddr(start); PutText("\n");
      PrintCloseClean(start);
    END;

    FOR i := 0 TO cleanRegionsCnt-1 DO
      IF start = cleanRegions[i].start THEN
        (*
          PutText("UnregisterClean - found: ");
          PutAddr(start); PutText(" ");
          PutAddr(cleanRegions[i].end); PutText("\n");
        *)
        IF size # -1 AND 
          cleanRegions[i].end # cleanRegions[i].start+size-ADRSIZE(ADDRESS)
         THEN
          PutText("ERROR >> UnregisterClean: wrong size for ");
          PutAddr(start); PutText(": ");
          PutInt(size); PutText(" instead of ");
          PutInt(cleanRegions[i].end - cleanRegions[i].start); 
          PutText("\n");
          XXX(start);
          RTOS.Crash();
        END;
        FOR j := i TO cleanRegionsCnt-2 DO
          cleanRegions[j] := cleanRegions[j+1];
        END;
        DEC(cleanRegionsCnt);
        found := TRUE;
        EXIT;
      END;
    END;

    IF NOT found THEN
      PutText("UnregisterClean - not found: ");
      PutAddr(start); PutText("\n");
      RTOS.Crash();
    END;
    IF traceClean THEN
      PrintCloseClean(start);
    END;
    RTOSMachine.RestoreInterruptMask(spl);
  END UnregisterClean;

PROCEDURE XXX(<* UNUSED *>a: ADDRESS) =
  BEGIN
  END XXX;

PROCEDURE SweepUntracedHeap (proc: PROCEDURE (start, end: ADDRESS)) =
  VAR
    start: ADDRESS;
  BEGIN
    start := RT0u.kmembase;
    FOR i := 0 TO cleanRegionsCnt-1 DO
      proc(start, cleanRegions[i].start-ADRSIZE(ADDRESS));
      start := cleanRegions[i].end+ADRSIZE(ADDRESS);
    END;
    proc(start, RT0u.kmemlimit);
  END SweepUntracedHeap;

PROCEDURE SweepUntracedHeapCl (cl: SweepClosure) : BOOLEAN =
  VAR
    start: ADDRESS;
  BEGIN
    start := RT0u.kmembase;
    FOR i := 0 TO cleanRegionsCnt-1 DO
      IF NOT cl.apply(start, cleanRegions[i].start-ADRSIZE(ADDRESS)) THEN
        RETURN FALSE;
      END;
      start := cleanRegions[i].end+ADRSIZE(ADDRESS);
    END;
    RETURN cl.apply(start, RT0u.kmemlimit);
  END SweepUntracedHeapCl;

PROCEDURE InitAmbiguousReferenceTracing() =
  BEGIN
    ambiguousReferencesCnt := 0;
    strongReferencesCnt := 0;
    unsafeAmbiguousReferencesCnt := 0;
    uniqueAmbiguousReferencesCnt := 0;
    uniqueStrongReferencesCnt := 0;
    unsafeUniqueAmbiguousReferencesCnt := 0;
    tabledReferencesCnt := 0;
  END InitAmbiguousReferenceTracing;

PROCEDURE NoteAmbiguousReference(fp: ADDRESS; p: ADDRESS) =
  VAR
    ref  := GetReference(p); (* the object pointed to by the pointer *)
  BEGIN
    IF doVerbose AND verbose > 3 THEN
      PutText("---> ambiguous reference ");
      PutHex(LOOPHOLE(fp, INTEGER)); PutText(" ");
      PutHex(LOOPHOLE(p, INTEGER)); PutText(" ");
      PutHex(LOOPHOLE(ref, INTEGER)); PutText(" ");
      PutInt(ambiguousReferencesCnt); PutText(" ");
      PutInt(uniqueAmbiguousReferencesCnt); PutText("\n");
    END;

    (* total number of ambiguous roots noted by the GC *)
    IF ambiguousStage = 1 THEN
      INC(ambiguousReferencesCnt);
    ELSIF ambiguousStage = 2 THEN
      INC(strongReferencesCnt);
    END;

    (* if untraced heap scan then the pointers are unsafe unless strongref-ed*)
    IF ambiguousStage > 2 THEN
      IF NOT StrongRef.Exists(LOOPHOLE(p,REFANY)) THEN 
        (* total number of unsafe pointers *)
        INC(unsafeAmbiguousReferencesCnt);

        IF showUnsafeAmbiguous THEN 
          VAR
            tc  := TYPECODE(ref);
            ptr := RTType.Get(tc);
          BEGIN
            PutText("GC WARNING >> unsafe pointer at location "); 
            PutHex(LOOPHOLE(fp, INTEGER)); 
            PutText(" : ");
            PutHex(LOOPHOLE(p, INTEGER)); 
            PutText(" to object "); 
            PutHex(LOOPHOLE(ref, INTEGER));
            PutText(" of type "); PutString(ptr.name); 
            PutText(" (tc: "); PutInt(tc); 
            PutText(", size "); 
            PutInt(ReferentSize(HeaderOf(LOOPHOLE(ref,ADDRESS))));
            PutText(")\n");
          END;
        END;
      END;
    END;

    (* check for duplicates *)
    FOR i := 0 TO tabledReferencesCnt-1 DO
      IF ambiguousReferences[i] = LOOPHOLE(ref,ADDRESS) THEN RETURN; END;
    END;

    (* unique unsafe *)
    IF ambiguousStage > 2 THEN
      INC(unsafeUniqueAmbiguousReferencesCnt);
    END;

    (* create the list if not created yet *)
    IF ambiguousReferences = NIL THEN
      ambiguousReferences := NEW(UNTRACED REF ARRAY OF ADDRESS, 100);
    END;

    (* reallocate the array to accommodate new information *)
    IF tabledReferencesCnt = NUMBER(ambiguousReferences^) THEN
      VAR 
        size := NUMBER(ambiguousReferences^);
        new : UNTRACED REF ARRAY OF ADDRESS;
      BEGIN
        new := NEW(UNTRACED REF ARRAY OF ADDRESS, 2 * size);
        SUBARRAY(new^, 0, size) := SUBARRAY(ambiguousReferences^, 0, size);
        DISPOSE(ambiguousReferences);
        ambiguousReferences := new;
      END;
    END;

    (* add new unique value and update their counter *)
    ambiguousReferences[tabledReferencesCnt] := LOOPHOLE(ref,ADDRESS);
    IF ambiguousStage = 1 THEN
      INC(uniqueAmbiguousReferencesCnt);
    ELSIF ambiguousStage = 2 THEN
      INC(uniqueStrongReferencesCnt);
    END;
    INC(tabledReferencesCnt);
  END NoteAmbiguousReference;

PROCEDURE GetReference (p: ADDRESS): REFANY =
  VAR
    page : Page;
    h    : RefHeader;
    he   : RefHeader;
    last : REFANY;
    href : Word.T;
    addr : Word.T;
  BEGIN
    addr := LOOPHOLE(p, Word.T);
    page := Word.RightShift (LOOPHOLE(p, INTEGER), LogBytesPerPage);
    IF page = Nil THEN
      RETURN NIL;
    END;
    he := PageToHeader(page+1);
    WHILE desc[page-p0].continued DO
      DEC(page);
    END;
    h := PageToHeader(page);
    last := LOOPHOLE(h + ADRSIZE(Header), REFANY);

    (* FIXME
    IF TYPECODE(last) = 0 THEN 
      PutText(
          "GC ERROR >> GetReference: first object on a page has typecode 0\n");
      RTOS.Crash();
    END;
    *)

    (* scan the non-continued page to find the beginning of the object *)
    (* there is only one object on the continued page *)
    IF HeaderToPage(he) - HeaderToPage(h) = 1 THEN
      WHILE h < he DO
        IF doChecks THEN
          IF NOT(Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
            PutText("GC ERROR >> Garbage collector crash (1008)\n");
            RTOS.Crash();
          END;
          <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
        END;
        IF h.typecode # Fill_1_type AND h.typecode # Fill_N_type THEN
          href := LOOPHOLE(h, INTEGER);
          IF href > addr THEN EXIT; END;
          IF h.typecode = 0 THEN EXIT; END;
          last := LOOPHOLE(h + ADRSIZE(Header), REFANY);
        END;
        INC(h, ADRSIZE(Header) + ReferentSize(h));
      END;
    END;
    IF TYPECODE(last) = 0 THEN 
      PutText("GC ERROR >> GetReference found object with typecode 0\n");
      RTOS.Crash();
    END;
    RETURN last;
  END GetReference;

PROCEDURE TypeName (ref: ADDRESS): TEXT =
  VAR header: RTHeapMap.ObjectPtr;
  VAR tc: RT0.Typecode;
  BEGIN
    IF ref >= MinAddress() AND ref <= MaxAddress() THEN
      ref := LOOPHOLE(GetReference(ref), ADDRESS);
      IF ref # NIL THEN
        header := ref - ADRSIZE (RT0.RefHeader);
        tc := header.typecode;
        IF RTType.IsValid(tc) THEN
          IF (0 < tc) AND (tc < RT0u.nTypes) THEN
            WITH tname = RTTypeSRC.TypecodeName(tc) DO
              IF Text.Equal(tname, "<anon type>") THEN
                RETURN "Anonymous type";
              ELSE
                RETURN tname;
              END;
            END;
          END;
        END;
      END;
    END;
    RETURN "<OBJECT-NOT-FOUND>";
  END TypeName;

(* rvr -- immobilizing objects (and their pages) that contain arbitrary
          address-like values found on the stack is too conservative.
          If an ambiguous "root" points into filler space, there's no
          object of interest and no reason to pin the page.  Similarly,
          no valid mutator pointers should address header areas.  It
          would even be possible to restrict pins to proper object
          boundaries.  This last is not enforced here because mutator
          pointers might reasonably be using C-style address arithmetic.
*)

PROCEDURE ImmobilizeObjectContaining(p: ADDRESS; ppage: Page): BOOLEAN =
  VAR
    h, hnext : RefHeader;
    result : BOOLEAN := FALSE;
    space := desc[ppage - p0].space;
    note := desc[ppage - p0].note;
  BEGIN
    <* ASSERT FALSE *>
    <* NOWARN *> <* ASSERT moveFromPinned *>
    (* ???? *)
    IF (space = Space.Previous OR space = Space.Current) AND 
      (note = Note.AmbiguousRoot OR note = Note.StrongRef)
     THEN
      ppage := FirstPage(ppage);
      <* ASSERT desc[ppage - p0].space = space *>
      <* ASSERT desc[ppage - p0].note = note *>
      h := PageToHeader(ppage);
      hnext := h + ADRSIZE(Header) + ReferentSize(h);
      WHILE LOOPHOLE(hnext, ADDRESS) < p DO
        h := hnext;
        hnext := h + ADRSIZE(Header) + ReferentSize(h);
      END;
      IF h.typecode # Fill_1_type AND h.typecode # Fill_N_type THEN
        <* ASSERT NOT h.forwarded *>
        result := TRUE;
        h.immobile := TRUE;
      END;
    END;
    RETURN result;
  END ImmobilizeObjectContaining;

PROCEDURE CheckAllRefs () =
  VAR tc: Typecode;
  BEGIN
    IF collectorState # CollectorState.Off OR allocatedPages = 0 THEN
      RETURN;
    END;
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0].space = Space.Current
        AND NOT desc[p - p0].continued THEN
        VAR
          h             := PageToHeader(p);
          he            := PageToHeader(p + 1);
          size: INTEGER;
        BEGIN
          WHILE h < he DO 
            IF p = newPage AND h >= newPtr THEN EXIT; END;
            IF p = pureCopyPage AND h >= pureCopyPtr THEN EXIT; END;
            IF p = impureCopyPage AND h >= impureCopyPtr THEN EXIT; END;

            tc := h.typecode;
            IF tc # Fill_1_type AND tc # Fill_N_type AND 
              (tc > RT0u.nTypes OR tc <= 0)
             THEN
              IF h = newPtr OR h = pureCopyPtr OR h = impureCopyPtr THEN
                (* we were scanning a page with active allocation *)
                EXIT;
              ELSE
                PutText("improper typecode "); 
                PutInt(tc); PutText("\n");
                IF tc = LAST(Typecode) OR tc = LAST(Typecode) - 1 THEN
                  PutText("last one!\n");
                END;
                VAR
                  h1            := PageToHeader(p);
                  he            := PageToHeader(p + 1);
                  size: INTEGER;
                BEGIN
                  PutText("Page           : "); PutInt(p); 
                  PutText(" "); RTHeapPages.PutPageStatus(NIL, p);  
                  PutText("\n");

                  PutText("newPage        : "); 
                  PutInt(newPage); PutText("\n");
                  PutText("pureCopyPage   : "); 
                  PutInt(pureCopyPage); PutText("\n");
                  PutText("impureCopyPage : "); 
                  PutInt(impureCopyPage); PutText("\n");

                  PutText("p0             : "); 
                  PutInt(p0); PutText("\n");
                  PutText("p1             : "); 
                  PutInt(p1); PutText("\n");

                  PutText("h                  : "); 
                  PutAddr(h); PutText("\n");
                  PutText("newPtr             : "); 
                  PutAddr(newPtr); PutText("\n");
                  PutText("newBoundary        : "); 
                  PutAddr(newBoundary); PutText("\n");
                  PutText("pureCopyPtr        : "); 
                  PutAddr(pureCopyPtr); PutText("\n");
                  PutText("pureCopyBoundary   : "); 
                  PutAddr(pureCopyBoundary); PutText("\n");
                  PutText("impureCopyPtr      : "); 
                  PutAddr(impureCopyPtr); PutText("\n");
                  PutText("impureCopyBoundary : "); 
                  PutAddr(impureCopyBoundary); PutText("\n");
                  
                  RTHeapPages.DumpPageStatus (NIL, TRUE);
                  
                  WHILE h1 < he AND (p # newPage OR h1 < newPtr) DO
                    PutText("h    : "); 
                    PutAddr(h1); PutText(" ");
                    tc := h1.typecode;
                    PutText("tc   : "); 
                    PutInt(tc); PutText(" ");
                    size := ReferentSize(h1);
                    PutText("size : "); 
                    PutInt(size); PutText("\n");
                    IF tc = 0 THEN EXIT; END;
                    INC(h1, ADRSIZE(Header) + size);
                  END;

                END;
                RTOSMachine.Debugger();
              END;
            END;
            size := ReferentSize(h);
            INC(h, ADRSIZE(Header) + size);
          END;
        END;
      END;
    END;
  END CheckAllRefs; 

PROCEDURE ReturnMem(<*UNUSED*>a: ADDRESS) =
  BEGIN
  END ReturnMem;

PROCEDURE CheckSanity(<*UNUSED*>msg: TEXT): BOOLEAN =
  BEGIN
  END CheckSanity;

PROCEDURE LocateHeaderOf(<*UNUSED*>a: ADDRESS): RefHeader =
  BEGIN
  END LocateHeaderOf;

PROCEDURE DistributeMemory() =
  BEGIN
  END DistributeMemory;

(*-------------------------------------------------------- eager reclamation *)

VAR
  fwdTable : ForwardHashUtbl.T;
  fwdIter  : ForwardHashUtbl.Iterator;
  (* a hash table from old headers to new ones *)

PROCEDURE AddForwardHash(old, new: RefHeader) =
  BEGIN
    IF fwdTable.put(old, new) THEN
      PutText("GC ERROR >> AddForwardHash: duplicate header\n");
      RTOS.Crash();
    END;
  END AddForwardHash;

PROCEDURE CleanUpForwardHash() =
  VAR
    old, new: ADDRESS;
  BEGIN
    fwdIter.reset();
    WHILE fwdIter.next(old, new) DO
      IF NOT fwdTable.delete(old, new) THEN
        PutText("GC ERROR >> CleanUpForwardHash: could not remove\n");
        RTOS.Crash();
      END;
    END;
  END CleanUpForwardHash; 

PROCEDURE ForwardHash(old: ADDRESS): RefHeader =
  VAR
    new: ADDRESS;
  BEGIN
    IF NOT fwdTable.get(old, new) THEN
      RETURN NIL;
    ELSE
      RETURN new;
    END;
  END ForwardHash; 

(* FIXME: make it cheaper *)
PROCEDURE InsertFreePage(thisPage: Page) =
  VAR
    p          := free;
    prevP      := Nil;
    nextP      := Nil;
    l, length     : INTEGER;
    mPrev, mNext := Nil;
    remove: BOOLEAN;
  BEGIN
    (* make the page free *)
    desc[thisPage - p0].space := Space.Free;
    desc[thisPage - p0].continued := FALSE;
    desc[thisPage - p0].stacked (* freechunk *) := FALSE;
    desc[thisPage - p0].link := Nil;
    INC(freePages);

    (* find the chunks to merge with this page and remove them from the list *)
    (* FIXME
    p := free;
    LOOP
      IF p = Nil THEN EXIT; END;
      nextP := desc[p - p0].link;
      IF p = thisPage + 1 THEN
        (* will prepend thisPage to mNext *)
        mNext := p;
        remove := TRUE;
      ELSIF thisPage = p + length THEN
        (* will append thisPage to mPrev *)
        mPrev := p;
        remove := TRUE;
      END;
      IF remove THEN
        desc[p-p0].link := Nil;
        IF prevP # Nil THEN
          desc[prevP-p0].link := nextP;
        ELSE
          free := nextP;
        END;
        remove := FALSE;
      END;
      prevP := p;
      p := nextP;
    END;

    (* adjust the lengths *)
    IF mPrev # Nil THEN
      length := FreeLength(mPrev) + 1;
      IF mNext # Nil THEN
        (* merging two ranges *)
        INC(length, FreeLength(mNext));

        (* FIXME: this should not be necessary *)
        desc[mNext - p0].stacked (* freechunk *) := FALSE;
        desc[mNext + 1 - p0].link := Nil;
      END;
      desc[mPrev - p0].stacked (* freechunk *) := TRUE;
      desc[mPrev + 1 - p0].link := length;
      thisPage := mPrev;
    ELSIF mNext # Nil THEN
      length := FreeLength(mNext) + 1;
      desc[thisPage - p0].stacked (* freechunk *) := TRUE;
      desc[thisPage + 1 - p0].link := length;
      desc[mNext - p0].stacked := FALSE;
    ELSE (* mPrev = Nil AND mNext = Nil *)
      (* single separate page *)
    END;
    *)
    (* insert into the list *)
    p := free;
    prevP := Nil;
    LOOP
      IF p = Nil THEN EXIT; END;
      nextP := desc[p - p0].link;
      l := FreeLength(p);
      IF l > length OR (l = length AND p > thisPage) THEN
        EXIT;
      END;
      prevP := p;
      p := nextP;
    END;
    IF prevP # Nil THEN
      desc[thisPage - p0].link := desc[prevP - p0].link;
      desc[prevP - p0].link := thisPage;
    ELSE
      desc[thisPage - p0].link := free;
      free := thisPage;
    END;
    IF desc[thisPage - p0].link = Nil THEN
      maxFreeLength := length;
    END;
  END InsertFreePage;

(*---------------------------------------------------------------------------*)

PROCEDURE DumpAll (pp: INTEGER := 0) =
  BEGIN
    VAR
      large       : INTEGER;
      used        : INTEGER;
      allocated   : INTEGER;
      free, current, unallocated, previous := 0;
      thisFree, maxFree     := 0;
    BEGIN
      FOR p := p0 TO p1 - 1 DO
	(* find the longest free chunk *)
	IF desc[p - p0].space = Space.Free THEN
	  INC(thisFree);
	ELSE
	  maxFree := MAX(maxFree, thisFree);
	  thisFree := 0;
	END;

	(* count the spaces *)
	CASE desc[p - p0].space OF
	| Space.Current =>
	  INC(current);
	| Space.Previous => INC(previous);
	| Space.Free => INC(free); 
	| Space.Unallocated => INC(unallocated);
	END;
      END;
      maxFree := MAX(maxFree, thisFree);

      large := largePromotionPages + largeCopyPages + largeNewPages;
      used  := activePages - large;
      allocated := allocatedPages - large;

      (* print page status *)
      PutText("\nGC pages >> need ");
      PutInt(pp); PutText("; max ");
      PutInt(maxFree); PutText("; free ");
      PutInt(free); PutText("; cur ");
      PutInt(current); PutText("; prev ");
      PutInt(previous); PutText("; unal ");
      PutInt(unallocated); PutText("; tot ");
      PutInt(allocatedPages); PutText("\n");

      (* print the triggers *)
      PutText("GC behind >> active ");
      PutInt(activePages); PutText("; large ");
      PutInt(large); PutText("; used ");
      PutInt(used); PutText("; alloc ");
      PutInt(allocated); PutText("\n");
      PutText("\n" );
    END;

    (* print the summary of the page usage *)
    PutText("\nGC >>> page dump\n");
    RTHeapPages.DumpPageStatus (NIL, TRUE);

    PutText("\nGC >>> freelist\n");
    PrintFreeList();

    (* unprotect all pages before you touch any objects there *)
    FOR p := p0 TO p1 - 1 DO
      IF desc[p - p0].protected THEN Unprotect(p); END;
    END;

    (* print the biggest hogs in the heap *)
    PutText("\nGC >>> allocation by bytes\n");
    RTutils.Heap(TRUE, RTutils.HeapPresentation.ByByteCount, 
		 10, NIL, TRUE);

    (* dump the allocation per site *)
    RTHeapTrace.Dump();

  END DumpAll;

PROCEDURE GetHeader (p: ADDRESS): RefHeader =
  VAR
    page : Page;
    h    : RefHeader;
    he   : RefHeader;
    last : RefHeader;
    href : Word.T;
    addr : Word.T;
  BEGIN
    addr := LOOPHOLE(p, Word.T);
    page := Word.RightShift (LOOPHOLE(p, INTEGER), LogBytesPerPage);
    IF page = Nil THEN
      RETURN NIL;
    END;
    he := PageToHeader(page+1);
    WHILE desc[page-p0].continued DO
      DEC(page);
    END;
    h := PageToHeader(page);
    last := h;

    (* scan the non-continued page to find the beginning of the object *)
    (* there is only one object on the continued page *)
    IF HeaderToPage(he) - HeaderToPage(h) = 1 THEN
      WHILE h < he DO
        IF doChecks THEN
          IF NOT(Word.And (LOOPHOLE (h, INTEGER), 3) = 0) THEN
            PutText("GC ERROR >> Garbage collector crash (1008)\n");
            RTOS.Crash();
          END;
          <* ASSERT Word.And (LOOPHOLE (h, INTEGER), 3) = 0 *>
        END;
        href := LOOPHOLE(h, INTEGER);
        IF href > addr THEN EXIT; END;
        IF h.typecode = 0 THEN EXIT; END;
        last := h;
        INC(h, ADRSIZE(Header) + ReferentSize(h));
      END;
    END;
    RETURN last;
  END GetHeader;

PROCEDURE DumpPointer (cp: ADDRESS) =
  VAR 
    h := GetHeader(cp);
    p := AddressToPage(h);
  BEGIN
    IF cp = NIL THEN
      PutText("NIL");
    END;
    PutAddr(cp); PutText(", h: ");
    PutAddr(h); PutText(", tc: ");
    PutInt(h.typecode); PutText(", size: ");
    PutInt(h.size); PutText(", I:");
    PutBoolean(h.immobile); PutText(" F:");
    PutBoolean(h.forwarded); PutText(" A:");
    PutBoolean(h.marka); PutText(" B:");
    PutBoolean(h.markb); PutText(" Z:");
    PutBoolean(h.Z); PutText(" V:");
    PutBoolean(h.V); PutText(", page: ");
    PutInt(p); PutText(" (");
    PutInt(p - p0); PutText(") ");
    RTHeapPages.PutPageStatus(NIL, p); PutText("\n");
  END DumpPointer;

<* UNUSED *>
PROCEDURE DumpPage (p: Page) =
  BEGIN
    VAR
      i  := PageToHeader(p);
      ii := PageToHeader(p+1);
    BEGIN
      PutText("Page: "); PutInt(p - p0); 
      PutText(" "); RTHeapPages.PutPageStatus(NIL, p);  
      PutText("\n");
      IF desc[p-p0].space = Space.Current OR
        desc[p-p0].space = Space.Previous
       THEN
        WHILE i < ii DO
          PutAddr(i); PutText(" "); 
          PutInt(i.typecode); PutText(" "); 
          PutInt(ADRSIZE(Header) + ReferentSize(i)); PutText(" "); 
          PutBoolean(i.forwarded);
          IF i.forwarded THEN
            PutText(" "); 
            PutAddr(LOOPHOLE(i + ADRSIZE(ADDRESS), UNTRACED REF ADDRESS)^);
          END;
          PutText("\n"); 
          INC(i, ADRSIZE(Header) + ReferentSize(i));
        END;
        PutAddr(i); PutText("\n"); 
      END;
    END;
  END DumpPage;

PROCEDURE MoveFromPinned (<* UNUSED *>allowMovement:BOOLEAN) =
  BEGIN
  END MoveFromPinned;

PROCEDURE SetGCRatio (<* UNUSED *>ratio: INTEGER) =
  BEGIN
  END SetGCRatio;

BEGIN
  (* SPIN - mainbody moved to RTInit() and is run directly by RTLinker *)
END RTCollector.

