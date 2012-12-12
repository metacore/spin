(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(*
 * Coding conventions: s: CARDINAL is an index in the space table
 *                     v: CARDINAL is an index in the VA Table.
 *                     0 is used instead of FIRST() in many cases because
 *                          FIRST does not work on dynamically allocated
 *                          arrays.
 *)

MODULE VMTaskSupport;

IMPORT Space, Translation, Word, Fmt;
IMPORT VMHandlers;
IMPORT Textify;
IMPORT HandlerUtils;
IMPORT VMError;
FROM HandlerUtils IMPORT Debug;
FROM HandlerUtils IMPORT Print;

IMPORT FastList;


TYPE VATableNode = RECORD
  beginaddr: Word.T;
  size: Word.T;
  inheritance: Word.T;
  nodeInUse: BOOLEAN;
END;

TYPE SpaceNode = RECORD
  space: Space.T;
  vatable: REF ARRAY OF VATableNode;
  vaTableSize: CARDINAL;
  vaTableNext: CARDINAL;
  heapPointer: Word.T;
  nodeInUse: BOOLEAN;
  freePageList: REF FastList.T;
END;

TYPE FreePageNode = FastList.T OBJECT
  beginaddr: Word.T;
END;

CONST
  HeapStart = 16_160000000;

  SpaceTableInitSize = 10;
  VATableInitSize = 20;


VAR
  SpaceTable: REF ARRAY OF SpaceNode;
  SpaceTableSize: CARDINAL;
  SpaceTableMutex: MUTEX;
  SpaceTableNext: CARDINAL;

  FreePageNodeFreeList: REF FastList.T := NEW(REF FastList.T);

  PageAllocCount: CARDINAL := 0;
  PageDeallocCount: CARDINAL := 0;


(*
 * Print out PageAllocCount and PageDeallocCount.
 *)
PROCEDURE MemUsage() =
  BEGIN
    HandlerUtils.Print("Page Allocation Data: PageAllocCount: " & 
      Fmt.Int(PageAllocCount) &
      " - " & Fmt.Int(PageDeallocCount) & " = " &
      Fmt.Int(PageAllocCount - PageDeallocCount) & "\n");
    HandlerUtils.Print("VM Page Size: " & 
      Fmt.Int(VMHandlers.VM_PAGE_SIZE) & "\n");
  END MemUsage;

PROCEDURE VMAllocAnywhere(space: Space.T; VAR addr: Word.T; VAR size: Word.T) =
  VAR
    adjSize: Word.T;
    adjAddr: Word.T;
  BEGIN
    (*
     * Get size and make sure it is page-aligned.  (Round up if not.) 
     *)
    adjSize := size;
    HandlerUtils.RoundUpToPage(adjSize);

    (* adjAddr := addr; *)
    (*
     * Must determine an address at which to allocate memory.  This
     * will be the current value of the global variable HeapPointer.
     * HeapPointer should be incremented by the size of the space
     * allocated in this call.
     *)
    IF GetHeapPointer(space, adjSize, adjAddr) THEN
      (* reusing a previously deallocated page *)
      addr := adjAddr;
      size := adjSize;
    END;
    IF adjAddr = -1 THEN
      HandlerUtils.PrintError("GetHeapPointer failed.\n");
      addr := 0;
    END;


    (*
     * There was not a reusable region for this allocation.  A pointer
     * at which to allocate new memory was returned through addr.  Proceed
     * to allocate there.
     *)
    TRY
      Space.Allocate(space, adjAddr, adjSize, FALSE);
    EXCEPT
    | VMError.E(ec) =>
      HandlerUtils.PrintError("\tVMAllocAnywhere: Failure exception in " & 
        "Space.Allocate.\n");
      HandlerUtils.Print("VMAllocAnywhere: tried to allocate in space " &
        Textify.Ref(space) & " at addr 0x" & Fmt.Unsigned(adjAddr) &
        " size 0x" & Fmt.Unsigned(adjSize) & ":" & VMError.Message(ec) &"\n");
      addr := 0;
    ELSE
      HandlerUtils.PrintError("Space.Allocate exception.\n");
      addr := 0;
    END;

    (* This allocation must be "registered" by this procedure's caller. *)
    addr := adjAddr;
    size := adjSize;
    (*
    Print("VMAllocAnywhere: space = " & Textify.Ref(space) &
      " addr = 0x" & Fmt.Unsigned(addr) & " size = 0x" & 
      Fmt.Unsigned(size) & "\n");
      *)
  END VMAllocAnywhere;

(*
 * This procedure only called from within this module.  It assumes that
 * the mutex on the space table has already been obtained.
 *)
PROCEDURE RegisterNewSpacePrivate(space: Space.T) : CARDINAL =
  VAR
    i: CARDINAL;
  BEGIN
    (*
     * Make sure this space isn't already registered.
     *)
    FOR i := 0 TO SpaceTableNext - 1 DO
      IF SpaceTable[i].space = space THEN
        (* Note this return will unlock the mutex. *)
        RETURN i;
      END;
    END;

    (*
     * If there is no more room in space table, re-allocate it.
     *)
    IF SpaceTableNext >= SpaceTableSize THEN
      ReallocSpaceTable();
    END;

    (*
     * Register this space.
     *)
    SpaceTable[SpaceTableNext].space := space;
    SpaceTable[SpaceTableNext].vatable := NEW(REF ARRAY OF VATableNode,
                                              VATableInitSize);
    SpaceTable[SpaceTableNext].vaTableSize := VATableInitSize;
    SpaceTable[SpaceTableNext].vaTableNext := 0;
    SpaceTable[SpaceTableNext].heapPointer := HeapStart;
    SpaceTable[SpaceTableNext].nodeInUse := TRUE;
    SpaceTable[SpaceTableNext].freePageList := NEW(REF FastList.T);
    
    (*
     * Update SpaceTableNext.  But first save it's value for return.
     *)
    i := SpaceTableNext;
    INC(SpaceTableNext);
    RETURN i;
  END RegisterNewSpacePrivate;

PROCEDURE RegisterNewSpace(space: Space.T) =
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("RegisterNewSpace: NIL space.\n");
      RETURN;
    END;

    (*
     * Acquire the mutex on the space table.
     *)
    LOCK SpaceTableMutex DO
      EVAL RegisterNewSpacePrivate(space);
    END;

    IF Debug THEN Print("RegisterNewSpace: registered space 0x" &
      Textify.Ref(space) & "\n");
    END;
  END RegisterNewSpace;

(*
 * RegisterMemAllocPrivate enters the actual data into the vatable 
 * structure.  It is isolated in this procedure in case any of the 
 * defaults or the structure of the vatable node changes.
 *
 * Assumptions:  The Space Table mutex is held.  s is the index to
 *               the Space Table for the space being updated.  v is
 *               the index to the vatable within that space.  The
 *               vatable has already been expanded if necessary.  This
 *               procedure does not handle incrementing vaTableNext.
 *)
PROCEDURE RegisterMemAllocPrivate(s: CARDINAL; v: CARDINAL; addr: Word.T;
  size: Word.T) =
  BEGIN
    SpaceTable[s].vatable[v].beginaddr := addr;
    SpaceTable[s].vatable[v].size := size;
    SpaceTable[s].vatable[v].inheritance := VMHandlers.VM_INHERIT_COPY; 
                                                       (*default*)
    SpaceTable[s].vatable[v].nodeInUse := TRUE;

    INC(PageAllocCount, size DIV VMHandlers.VM_PAGE_SIZE);
    IF Debug THEN Print("PageAllocCount: " & Fmt.Int(PageAllocCount) &
      " - " & Fmt.Int(PageDeallocCount) & " = " &
      Fmt.Int(PageAllocCount - PageDeallocCount) & "\n");
    END;
  END RegisterMemAllocPrivate;    

PROCEDURE RegisterMemAlloc(space: Space.T; addr: Word.T; size: Word.T) =
  VAR
    s, v: CARDINAL;
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("RegisterMemAlloc: NIL space.\n");
      RETURN;
    END;

    (*
     * Acquire mutex on Space Table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find slot in Space Table for space.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Check to make sure there is room in the VATable.
           *)
          IF SpaceTable[s].vaTableNext > SpaceTable[s].vaTableSize - 1 THEN
            ReallocVATable(s);
          END;
          (*
           * Record the data for this allocation.
           *)
          v := SpaceTable[s].vaTableNext;
          INC(SpaceTable[s].vaTableNext);
          RegisterMemAllocPrivate(s, v, addr, size);
          (*
           * All done.  Why bother scanning through rest of Space Table?
           *)
          IF addr = 16_16000 THEN
            Print("RegisterMemAlloc: space " & Textify.Ref(space) & 
              " addr 0x" & Fmt.Unsigned(addr) &
              " size 0x" & Fmt.Unsigned(size) & "\n");
          END;
          IF Debug THEN Print("RegisterMemAlloc: addr 0x" & 
            Fmt.Unsigned(addr) & " size 0x" & Fmt.Unsigned(size) & "\n");
          END;
          RETURN;
        END;
      END;

      (*
       * If this point is reached, space was not in Space Table.  Add it.
       *)
      s := RegisterNewSpacePrivate(space);
      IF Debug THEN Print("RegisterNewSpace: registered space 0x" &
        Textify.Ref(space) & "\n");
      END;

      (*
       * Now update data.  Note that since we have the mutex, there shouldn't
       * be any way the vatable could have filled up, but we don't want to
       * make any assumptions about future coding decisions (like alloc the
       * table on demand).
       *)
      IF SpaceTable[s].vaTableNext > SpaceTable[s].vaTableSize - 1 THEN
        ReallocVATable(s);
      END;
      (*
       * Record the data for this allocation.
       *)
      RegisterMemAllocPrivate(s, SpaceTable[s].vaTableNext, addr, size);
      INC(SpaceTable[s].vaTableNext);
    END; (* mutex *)

    IF Debug THEN Print("RegisterMemAlloc: addr 0x" & Fmt.Unsigned(addr) & 
			" size 0x" & Fmt.Unsigned(size) & "\n");
    END;

  END RegisterMemAlloc;

PROCEDURE RegisterMemDealloc(space: Space.T; addr: Word.T; size: Word.T)
  : BOOLEAN =
  VAR
    freePage: FreePageNode;
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("RegisterMemDealloc: NIL space.\n");
      RETURN FALSE;
    END;

    (*
     * Acquire mutex on Space Table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find space in Space Table.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Find this region in the VATable.
           *)
          FOR v := 0 TO SpaceTable[s].vaTableNext - 1 DO
            IF SpaceTable[s].vatable[v].beginaddr = addr THEN
              (*
               * Mark it unused.
               *)
              SpaceTable[s].vatable[v].nodeInUse := FALSE;
              IF Debug THEN Print("PageDeallocCount: " & 
                Fmt.Int(PageAllocCount) &
                " - " & Fmt.Int(PageDeallocCount) & " = " &
                Fmt.Int(PageAllocCount - PageDeallocCount) & "\n");
              END;
              IF Debug THEN Print("RegisterMemDealloc: addr 0x" & 
                Fmt.Unsigned(addr) & " size 0x" & Fmt.Unsigned(size) & "\n")
              END;
              (*
               * Determine if it can be reused.  Many allocations are for
               * out-of-band device reads and VM reads.  These usually occur
               * a page at a time, and are deallocated immediately. Perhaps
               * these pages can be reused, saving valuable space.  Return
               * false if it can be reused (and hence should not be 
               * dealloc'ed.  Note this RETURN releases the mutex.
               *)
              IF addr >= HeapStart AND size = VMHandlers.VM_PAGE_SIZE THEN
                (* it can be reused *)
                freePage := FastList.Dequeue(FreePageNodeFreeList);
                IF freePage = NIL THEN
                  freePage := NEW(FreePageNode);
                END;
                freePage.beginaddr := addr;
                FastList.Enqueue(freePage, SpaceTable[s].freePageList);
                IF Debug THEN Print("RegisterMemDealloc: registered addr 0x" & 
                  Fmt.Unsigned(addr) & " size 0x" & Fmt.Unsigned(size) & 
				    " for reuse\n");
                END;
                RETURN FALSE;
              ELSE
                (*
                 * Return TRUE to indicate that the region should be 
                 * deallocated.  Note this RETURN releases the mutex.
                 *)
                INC(PageDeallocCount, size DIV VMHandlers.VM_PAGE_SIZE);
                RETURN TRUE;
              END;
            END;
          END;
          (*
           * If this point is reached, the space was located in the Space
           * Table but the region was not located in the va table.  Since
           * there is only once node per space in the Space Table, there
           * is no sense in continuing the search.
           *)
          HandlerUtils.PrintError("RegisterMemDealloc: region at addr 0x" &
            Fmt.Unsigned(addr) & " not found in space.\n");
          RETURN TRUE;
        END; (* IF this is the space *)
      END; (* WHILE *)
    END; (* LOCK *)
    HandlerUtils.PrintError("RegisterMemDealloc: space not found.\n");
    RETURN TRUE;
  END RegisterMemDealloc;

(*
 * ReallocSpaceTable is called when room runs out in the current space
 * table.  It is called only from within this module.
 *)
PROCEDURE ReallocSpaceTable() =
  VAR
    NewTable: REF ARRAY OF SpaceNode;
    newi: CARDINAL;
  BEGIN
    (*
     * The mutex is already held upon entrance to this procedure.
     *
     * Basically, we're going to allocate a new space table which is 
     * double the size of the existing space table.  Then when copying
     * the current table over we're going to just not copy any nodes
     * which are no longer in use.  Finally, we're going to leave the
     * old space table (which was dynamically allocated) unreferenced
     * for the collector to pick up.
     *)
    NewTable := NEW(REF ARRAY OF SpaceNode, MAX(2 * SpaceTableSize, 1));

    newi := 0;
    FOR oldi := 0 TO SpaceTableSize - 1 DO
      IF SpaceTable[oldi].nodeInUse THEN
        NewTable[newi] := SpaceTable[oldi];
        INC(newi);
      END;
    END;

    SpaceTable := NewTable;
    SpaceTableSize := MAX(2 * SpaceTableSize, 1);
    SpaceTableNext := newi;

    IF Debug THEN Print("ReallocSpaceTable:  new SpaceTableSize = " &
      Fmt.Int(SpaceTableSize) & "\n");
    END;
  END ReallocSpaceTable;

PROCEDURE ReallocVATable(s: CARDINAL) =
  VAR
    NewTable: REF ARRAY OF VATableNode;
    newi: CARDINAL;
  BEGIN
    (*
     * The mutex is already held upon entrance to this procedure.
     *
     * Basically, we're going to allocate a new VA Table which is double
     * the size of the existing VA table at index s of the Space Table.
     * Then, when copying the current table over we're going to not copy
     * any nodes which are no longer in use.  Finally, we're going to
     * leave the old VA Table (which was dynamically allocated) unreferenced
     * for the collector to pick up.
     *)
    NewTable := NEW(REF ARRAY OF VATableNode, 
                    MAX(2 * SpaceTable[s].vaTableSize, 1));

    newi := 0;
    FOR oldi := 0 TO SpaceTable[s].vaTableSize - 1 DO
      IF SpaceTable[s].vatable[oldi].nodeInUse THEN
        NewTable[newi] := SpaceTable[s].vatable[oldi];
        INC(newi);
      END;
    END;

    SpaceTable[s].vatable := NewTable;
    SpaceTable[s].vaTableSize := MAX(2 * SpaceTable[s].vaTableSize, 1);
    SpaceTable[s].vaTableNext := newi;

    IF Debug THEN Print("ReallocVATable: new VATableSize = " &
      Fmt.Int(SpaceTable[s].vaTableSize) & "\n");
    END;
  END ReallocVATable;

PROCEDURE GetHeapPointer(space: Space.T; size: Word.T; VAR addr: Word.T) : BOOLEAN =
  VAR
    i: CARDINAL;
    freePage: FreePageNode;
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("GetHeapPointer: NIL space.\n");
      addr := -1;
      RETURN FALSE;
    END;

    (*
     * We will bump up the heap pointer by the size, so make sure size
     * is aligned on page boundaries.
     *)
    HandlerUtils.RoundUpToPage(size);

    (*
     * Acquire the mutex on the HeapPointerTable.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find the space.
       *)
      FOR i := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[i].space = space THEN
          (*
           * Many allocations are for out-of-band device reads and VM reads.
           * These are usually a page at a time and are then deallocated 
           * right away.  Perhaps these pages can be reused.  Check if any 
           * are available that may suit this case.
           *)
          IF size = VMHandlers.VM_PAGE_SIZE THEN 
            freePage := FastList.Dequeue(SpaceTable[i].freePageList);
            IF freePage # NIL THEN (* we have a winner *)
              addr := freePage.beginaddr;
              (* allow this FreePageNode to be reused. *)
              FastList.Enqueue(freePage, FreePageNodeFreeList);
              RETURN TRUE;
            END;
          END;

          addr := SpaceTable[i].heapPointer;
          SpaceTable[i].heapPointer := 
              Word.Plus(SpaceTable[i].heapPointer, size);
          RETURN FALSE;
        END;
      END;

      (*
       * If we didn't find the space, that means it has not been 
       * registered.  Register it now.
       *)
      i := RegisterNewSpacePrivate(space);

      addr := SpaceTable[i].heapPointer;
      SpaceTable[i].heapPointer := 
          Word.Plus(SpaceTable[i].heapPointer, size);
      RETURN FALSE;
    END;
  END GetHeapPointer;

PROCEDURE GetRegion(space: Space.T; VAR addr: Word.T; VAR size: Word.T) 
  RAISES {BadSpace,BadAddr} =
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("GetRegion: NIL space.\n");
      RAISE BadSpace;
    END;

    (*
     * Acquire mutex on Space Table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find space in Space Table.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Find this region in the VATable using addr as the key.
           * This search may be more correct if a comparison to size
           * is also used.
           *)
          FOR v := 0 TO SpaceTable[s].vaTableNext - 1 DO
            IF SpaceTable[s].vatable[v].beginaddr <= addr AND
             addr <= Word.Plus(SpaceTable[s].vatable[v].beginaddr,
                               SpaceTable[s].vatable[v].size) AND
             SpaceTable[s].vatable[v].nodeInUse = TRUE THEN
              (*
               * Set the addr and size and return.
               * RETURN releases the mutex.
               *)
              addr := SpaceTable[s].vatable[v].beginaddr;
              size := SpaceTable[s].vatable[v].size;
              RETURN;
            END;
          END;
          (*
           * If this point is reached, we located the space node in the 
           * space table but not the va node in the vatable.  There's no
           * point searching any further in the space table since there
           * is only one node per space.
           *)
          HandlerUtils.PrintError("GetRegion: region at addr 0x" &
            Fmt.Unsigned(addr) & " not found in space.\n");
          RAISE BadAddr; (* indicates error to caller *)
        END; (* IF this is the space *)
      END; (* FOR all nodes in Space Table *)
    END; (* LOCK *)
    (*
     * If this point is reached, the space was not found in the space
     * table.
     *)
    HandlerUtils.PrintError("GetRegion: space not found.\n");
    RAISE BadSpace; (* indicates error *)
  END GetRegion;

(* dangerous to put something this big on stack - used in DupTask *)
VAR buf: ARRAY[0..2*VMHandlers.VM_PAGE_SIZE-1] OF CHAR;

PROCEDURE DupTask(parentspace: Space.T; childspace: Space.T) RAISES {Failure} =
  VAR
    sp, sc: CARDINAL; (* indices in Space Table *)
    vp, vc: CARDINAL; (* indices in VATable *)
    docopy: BOOLEAN;
    copysize, amountcopied: Word.T;
    copyaddr: Word.T;
  BEGIN
    (*
     * Note:  we pretty much have to use WHILE loops all over the place
     * in this procedure since FOR loop variables have limited scope.
     *)
    IF Debug THEN Print("  DupTask.\n"); END;
    (*
     * Check arg validity.
     *)
    IF parentspace = NIL THEN
      HandlerUtils.PrintError("DupTask: NIL parent space.\n");
      RAISE Failure;
    END;
    IF childspace = NIL THEN
      HandlerUtils.PrintError("DupTask: NIL child space.\n");
      RAISE Failure;
    END;

    (*
     * Acquire mutex on the Space Table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find the parent and child spaces.  Both spaces
       * MUST be registered before this procedure is called.
       *
       * Since the parent was created earlier, it will have a lower
       * index than the child.  This is probably a stupid
       * assumption which I will come to regret -- watch out for bugs.
       *)
      sp := 0;
      WHILE sp < SpaceTableNext DO
        IF SpaceTable[sp].space = parentspace THEN
          EXIT;
        END;
        INC(sp);
      END;
      IF sp = SpaceTableNext THEN
        HandlerUtils.PrintError("DupTask: cannot find parent space.\n");
        RETURN;
      END;

      (* start search at parent, since child must be younger. *)
      sc := sp + 1;
      WHILE sc < SpaceTableNext DO
        IF SpaceTable[sc].space = childspace THEN
          EXIT;
        END;
        INC(sc);
      END;
      IF sc >= SpaceTableNext THEN
        HandlerUtils.PrintError("DupTask: cannot find child space.\n");
        RETURN;
      END;

      (*
       * First print out a table of what we're going to be duplicating.
       *)
      IF Debug THEN
        Print("  DupTask: data to copy:\n");
        vp := 0;
        WHILE vp < SpaceTable[sp].vaTableNext DO
          IF SpaceTable[sp].vatable[vp].nodeInUse THEN
            IF SpaceTable[sp].vatable[vp].inheritance = 
              VMHandlers.VM_INHERIT_COPY THEN
              Print("node " & Fmt.Int(vp) & 
                " beginaddr 0x" & 
                Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) &
                " size 0x" &
                Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) &
                " inheritance VMHandlers.VM_INHERIT_COPY\n");
            ELSE
              Print("node " & Fmt.Int(vp) &
                " beginaddr 0x" & 
                Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) &
                " size 0x" &
                Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) &
                " inheritance not VMHandlers.VM_INHERIT_COPY\n");
            END;
          END;
          INC(vp);
        END;
      END;

      (*
       * For everything in the parent space marked VMHandlers.VM_INHERIT_COPY, 
       * allocate space in the child, read, and write.
       *)
      IF Debug THEN Print("DupTask: SpaceTable[sp].vaTableNext = " &
        Fmt.Int(SpaceTable[sp].vaTableNext) & "\n");
      END;
      IF Debug THEN Print("DupTask: SpaceTable[sc].vaTableSize = " &
        Fmt.Int(SpaceTable[sc].vaTableSize) & "\n");
      END;
      vp := 0;
      WHILE vp < SpaceTable[sp].vaTableNext DO
        (*
         * Check if the VATable node is in use, and if it's 
         * VMHandlers.VM_INHERIT_COPY.
         *)
        IF SpaceTable[sp].vatable[vp].nodeInUse AND
          SpaceTable[sp].vatable[vp].inheritance = 
          VMHandlers.VM_INHERIT_COPY AND
          (* the following is a hack and I hate it *)
          SpaceTable[sp].vatable[vp].beginaddr # 16_0 THEN
          (*
           * Allocate space.
           *)
          IF Debug THEN Print("  alloc  : addr  0x" &
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) &
              " size = 0x" & 
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) & 
              " space = 0x" &
              Textify.Ref(childspace) & "\n");
          END;
          TRY
	    Space.Allocate(childspace, 
			   SpaceTable[sp].vatable[vp].beginaddr,
			   SpaceTable[sp].vatable[vp].size);
            docopy := TRUE;
          EXCEPT
          | VMError.E(ec) =>
            HandlerUtils.PrintError("DupTask: Failure exception in " & 
              "Space.Allocate: addr 0x" & 
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) &
              " size 0x" & Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) &
              ":" & VMError.Message(ec) &				    
              "\n");
            docopy := FALSE;
            (* RAISE Failure;  - Try moving on for now. *)
          ELSE
            HandlerUtils.PrintError("DupTask: unknown exception " & 
              "allocating space at addr = 0x" &
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) &
              " of size = 0x" & 
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) & "\n");
            docopy := FALSE;
          END;

          (*
           * If allocation was successful, we must register the new region
           * in the childspace.  Much of the register procedure is locating
           * the proper slots in the data structure, but we already know
           * this, and we also possess the mutex.  Call the private register
           * procedure.  We do have to make sure there is enough room in
           * the vatable before calling.
           *)
          IF docopy THEN
            IF SpaceTable[sc].vaTableNext >= SpaceTable[sc].vaTableSize THEN
              ReallocVATable(sc);
            END;
            vc := SpaceTable[sc].vaTableNext;
            RegisterMemAllocPrivate(sc, 
                                    vc, 
                                    SpaceTable[sp].vatable[vp].beginaddr,
				    SpaceTable[sp].vatable[vp].size);
            INC(SpaceTable[sc].vaTableNext);
            (*
             * Keep track of heap pointer also.
             *)
            IF Word.Plus(SpaceTable[sc].vatable[vc].beginaddr,
                         SpaceTable[sc].vatable[vc].size) > 
                         SpaceTable[sc].heapPointer THEN
              SpaceTable[sc].heapPointer := 
                  Word.Plus(SpaceTable[sc].vatable[vc].beginaddr,
                            SpaceTable[sc].vatable[vc].size);
            END;
          END;

          (*
           * If allocation was successful, copy over data by first reading
           * it into a temporary buffer and then writing it, two pages (size
           * of buffer) at a time.
           *)
          IF docopy THEN
            amountcopied := 0;
            copyaddr := SpaceTable[sp].vatable[vp].beginaddr;
            WHILE amountcopied < SpaceTable[sp].vatable[vp].size DO
              copysize := MIN(NUMBER(buf),
                              SpaceTable[sp].vatable[vp].size - amountcopied);
              (* IF Debug THEN Print("inc copy addr 0x" & 
                Fmt.Unsigned(copyaddr) &
                " size 0x" & Fmt.Unsigned(copysize) & "\n"); END; *)
              TRY
		Translation.Read(parentspace, copyaddr, SUBARRAY(buf, 0, copysize));
		Translation.Write(childspace, SUBARRAY(buf, 0, copysize), copyaddr);
              EXCEPT
              | VMError.E(ec) =>
                HandlerUtils.PrintError("\tDupTask: VM error " & 
                  "exception copying data from addr 0x" &
                  Fmt.Unsigned(copyaddr) & " of size 0x" &
                  Fmt.Unsigned(copysize) & ":" & VMError.Message(ec) & "\n");
                RAISE Failure;
              ELSE
                HandlerUtils.PrintError("DupTask: exception copying " & 
                  "data from addr 0x" &
                  Fmt.Unsigned(copyaddr) & " of size 0x" &
                  Fmt.Unsigned(copysize) & "\n");
              END;
              INC(amountcopied, copysize);
              INC(copyaddr, copysize);
              (* IF Debug THEN Print("         addr 0x" & 
                Fmt.Unsigned(copyaddr) &
                " size 0x" & Fmt.Unsigned(copysize) & "\n"); END; *)
            END;
            (* IF Debug THEN Print("copied : addr 0x" & 
              Fmt.Unsigned(SpaceTable[sp].vatable[vp].beginaddr) & 
              " size 0x" & Fmt.Unsigned(SpaceTable[sp].vatable[vp].size) & 
              "\n"); END; *)
          END; (* IF allocation successful *)
        END; (* IF parent node in use and VMHandlers.VM_INHERIT_COPY *)
        INC(vp);
      END; (* WHILE all the parent nodes *)
    END; (* mutex *)
  END DupTask;

(*
 * Set the inheritance attribute of a region of memory.  The region will
 * be identified by space, start virtual address, and size.  
 *)
PROCEDURE SetInheritance(space: Space.T; addr: Word.T; size: Word.T;
  inheritance: Word.T) =
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("SetInheritance: NIL space.\n");
      RETURN;
    END;

    (*
     * Acquire mutex on space table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find the space.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Look for the vatable node and update inheritance attribute.
           *)
          FOR v := 0 TO SpaceTable[s].vaTableNext - 1 DO
            IF SpaceTable[s].vatable[v].beginaddr = addr AND
              SpaceTable[s].vatable[v].size = size THEN
              SpaceTable[s].vatable[v].inheritance := inheritance;
              (* Note this return releases the mutex. *)
              IF Debug THEN Print("SetInheritance: addr 0x" & 
                Fmt.Unsigned(addr) & " size 0x" & Fmt.Unsigned(size) & 
                " inheritance 0x" & Fmt.Unsigned(inheritance) & "\n");
              END;
              RETURN;
            END;
          END;
          HandlerUtils.PrintError("SetInheritance: unable to " & 
            "locate region of space.\n");
          RETURN;
        END; (* IF this is the space *)
      END; (* FOR all nodes in space table *)
    END; (* mutex *)
    HandlerUtils.PrintError("SetInheritance: unable to locate space.\n");
  END SetInheritance;

PROCEDURE ClearSpace(space: Space.T) RAISES {Failure} =
  VAR
    count: CARDINAL := 0;
  BEGIN
    IF Debug THEN Print("  ClearSpace: space = 0x" & 
      Textify.Ref(space) & "\n");
    END;
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("ClearSpace: NIL space.\n");
      RAISE Failure;
    END;

    (*
     * Acquire mutex on space table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find the space.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Move down vatable deallocating space and marking nodes as
           * no longer in use.
           *)
          FOR v := 0 TO SpaceTable[s].vaTableNext - 1 DO
            IF SpaceTable[s].vatable[v].nodeInUse THEN
              TRY
		Space.Deallocate(space,
				 SpaceTable[s].vatable[v].beginaddr,
				 SpaceTable[s].vatable[v].size);
                IF Debug THEN Print("ClearTask: addr 0x" & 
                  Fmt.Unsigned(SpaceTable[s].vatable[v].beginaddr) & 
                  " size 0x" & 
                  Fmt.Unsigned(SpaceTable[s].vatable[v].size) & "\n")
                END;
                INC(PageDeallocCount, 
                    SpaceTable[s].vatable[v].size DIV VMHandlers.VM_PAGE_SIZE);
                IF Debug THEN Print("ClearTask pagecount: " & 
                  Fmt.Int(PageAllocCount) &
                  " - " & Fmt.Int(PageDeallocCount) & " = " &
                  Fmt.Int(PageAllocCount - PageDeallocCount) & "\n");
                END;
                INC(count); (* how many deallocations in this call *)
              EXCEPT
              | VMError.E(ec) =>
                HandlerUtils.PrintError("ClearTask: Failure exception " & 
                  "deallocating addr 0x" &
                  Fmt.Unsigned(SpaceTable[s].vatable[v].beginaddr) &
                  " size 0x" & 
                  Fmt.Unsigned(SpaceTable[s].vatable[v].size) &
		  ":" & VMError.Message(ec) & "\n");
                (*RAISE Failure; - try moving on for now *)
              ELSE
                HandlerUtils.PrintError("ClearSpace: exception " & 
                  "deallocating addr 0x" &
                  Fmt.Unsigned(SpaceTable[s].vatable[v].beginaddr) &
                  " size 0x" & 
                  Fmt.Unsigned(SpaceTable[s].vatable[v].size) & "\n");
              END;
              SpaceTable[s].vatable[v].nodeInUse := FALSE;
            END;
          END; (* FOR nodes in VATable *)
        END; (* IF this is the space *)
      END; (* FOR nodes in SpaceTable *)
    END; (* mutex *)
    IF Debug THEN Print("ClearSpace deallocations: " & 
      Fmt.Int(count) & "\n");
    END;
  END ClearSpace;

PROCEDURE RegisterSpaceDestroy(space: Space.T) =
  BEGIN
    (*
     * Check arg validity.
     *)
    IF space = NIL THEN
      HandlerUtils.PrintError("RecordSpaceDestroy: NIL space.\n");
      RETURN;
    END;

    (*
     * Acquire the mutex on the space table.
     *)
    LOCK SpaceTableMutex DO
      (*
       * Find the space.
       *)
      FOR s := 0 TO SpaceTableNext - 1 DO
        IF SpaceTable[s].space = space THEN
          (*
           * Mark node as no longer in use.  Mark the space as NIL since 
           * this field is used in comparisons on searches.  Then set the 
           * vatable reference to NIL, leaving the dynamically allocated 
           * vatable memory to be collected.
           *)
          SpaceTable[s].nodeInUse := FALSE;
          SpaceTable[s].space := NIL;
          SpaceTable[s].vatable := NIL;
          (*
           * Don't need to clear any of the other data (eg vaTableNext)
           * because:
           * 1.  This node will never be reused.  It will be left for 
           *     collection if the Space Table is ever reallocated.
           * 2.  Even if this node were reused, RegisterNewSpace would
           *     reinitialize all these other fields.
           *)
          IF Debug THEN Print("RegisterSpaceDestroy: space = 0x" &
            Textify.Ref(space) & "\n");
          END;
        END;
      END;
    END;
  END RegisterSpaceDestroy;

BEGIN
  (*
   * Set up space tables.
   *)
  SpaceTableMutex := NEW(MUTEX);
  SpaceTable := NEW(REF ARRAY OF SpaceNode, SpaceTableInitSize);
  SpaceTableSize := SpaceTableInitSize;
  FOR i := 0 TO SpaceTableSize - 1 DO
    SpaceTable[i].space := NIL; (* don't want any misunderstandings *)
    SpaceTable[i].nodeInUse := FALSE;
  END;
END VMTaskSupport.
