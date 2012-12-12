(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Tue Dec 22 16:58:21 PST 1992 by rustan     *)
(*      modified on Fri Sep 18 14:58:21 PDT 1992 by rustan     *)

(* rvr - all memory requests come through RTAllocator, which calls
         AllocForNew or Malloc as required, as in the mostly-copying
         design.  The allocation routines below are now unused...*)

UNSAFE MODULE RTHeapMAS EXPORTS RTHeapMAS, RTHeapRep, RTCollector;

IMPORT RT0, RT0u, RTMisc, Word, ThreadF, RTMem, RTIO, RTHeapTrace;
IMPORT RTHeapMap, RTOS, RTMachine, Cstdlib, RTAllocator, RTHeapPages;
IMPORT RTutils, RTStrongRef, RTHeapDebug, RTTypeSRC, RTType, RTRefCount;
FROM RT0 IMPORT Typecode, GcStatus;
FROM RTIO IMPORT PutText, PutInt, PutAddr, Flush;

(* This module implements the memory management part of the run-time 
   system for Modula-3. Both the TRACED and UNTRACED referents are 
   processed here. 
*)

VAR
  MEMORY_Start: ADDRESS := NIL;
  MEMORY_End: ADDRESS := NIL;
  LONG_Start: ADDRESS := NIL;

CONST
  MAX_SHORT_SIZE = 8 * 1024;
  SHORT_TO_LONG  = 512; (* 1024 is 100% *)

  (* There are AddrPerAlign  addressable units per alignment *)
  (* AddrPerAlign satisfies, for any address a:
       ( INT(a) MOD AddrPerAlign = 0 ) = ( any object can start at address a )
     where INT(a) means LOOPHOLE( a, INTEGER ).  *)

CONST
  AddrPerAlign = RTMachine.PointerAlignment;

(*-------------------------------------------------- low-level allocation ---*)

TYPE
  (* NOTE.  It is assumed that the first two fields, i.e., 'size' and
     'typecode', in Header and Descriptor are the same.

     The 'size' field is the number of addressable units in the node,
     including the Header or Descriptor record itself.  'size' is
     invariantly at least MinMemoryNodeSize and a multiple of AddrPerAlign.

     'typecode' is the typecode of the variable stored in the node,
     and is TcFree if the node is free.

     After these two fields the Header and Descriptor records may
     differ in any way (e.g., their sizes are not required to be the same). *)

  (* NOTE.  The following type must be in synch with _refHeader defined in
     M3Runtime.h, except that the latter should be padded to so that it
     is of size HeaderSize.
     Also, the type needs to be in synch with the compiler's TextExpr
     module. *)

  (* Note, the RecursivelyMarked value in the following type must be in
     synch with the compiler's TextExpr module *)
  RefDescriptor = UNTRACED REF Descriptor;
  Descriptor = RECORD
    header: RT0.RefHeader;
    nextFree  : RefDescriptor; (* next memory block on the free list *)
  END;

CONST
  (* HeaderSize is the smallest multiple of AddrPerAlign that is at
     least ADRSIZE( Header ).
     NOTE.  Changing this value will also require a change in M3Runtime.h
     (see comment on RefHeader and Header above). *)
  HeaderSize = (( ADRSIZE( Header ) + AddrPerAlign - 1 ) DIV AddrPerAlign)
               * AddrPerAlign;
  DescriptorSize = ADRSIZE( Descriptor );
  (* MinMemoryNodeSize is the minimum size of memory available in a node.
     This minimum exists so that any used node can be converted into a
     descriptor node at any time. *)
  MinMemoryNodeSize = MAX( HeaderSize,
                           ((DescriptorSize+AddrPerAlign-1) DIV AddrPerAlign)
                           * AddrPerAlign );
  MinShortMemoryNodeSize = MinMemoryNodeSize;
  MinLongMemoryNodeSize = MAX_SHORT_SIZE;

  TcFree = LAST( Typecode );

VAR
  (* head of linked list of free memory nodes,
     sorted in order of increasing memory address *)
  short_freelist: RefDescriptor := NIL;
  long_freelist: RefDescriptor := NIL;
  short_free: INTEGER := 0;
  long_free: INTEGER := 0;

VAR
  (* marker object for RTHeapMap.Walkglobals to use *)
  theMarker  : Marker  := NIL;
  theCleaner : Cleaner := NIL;

PROCEDURE Init () =
  BEGIN
    RTIO.PutText("GC >> using the mark-and-sweep collector\n");

    verbose := 0;

    InitHeap();

    IF doSanity AND NOT CheckSanity("after InitHeap") THEN
      RTOS.Crash();
    END;

    RTRefCount.Init();
    RTAllocator.Init();
    RTHeapTrace.Init();
    RTHeapDebug.Init();
    RTHeapPages.Init();
    RTutils.Init();
    RTStrongRef.Init();

    IF doSanity AND NOT CheckSanity("after initializations") THEN
      RTOS.Crash();
    END;

    theMarker  := NEW (Marker);
    theCleaner := NEW (Cleaner);
  END Init;

PROCEDURE InitHeap() =
  (* Called as the first things from RTMain.Run, so this procedure
     cannot depend on anything that can't be statically initialized
     by the C compiler.  This procedure initializes the heap.  After
     this procedure returns, NEW can be called.  Note, though, that
     the garbage collector may not be in place until later during the
     initializations.  Thus, if there's not enough memory for the
     initializations, the garbage collector might not be invoked.
     That is very reasonable, however, since if the initialization
     cannot be carried to completion, then it is not likely that
     there would be enough space for anything else either. *)
  BEGIN
    (* InitHeap can only be called once.  If not called before, then
       freelist = NIL.  (But not necessarily the other way around.)  *)
    <* ASSERT short_freelist = NIL *>
    <* ASSERT long_freelist = NIL *>

    (* get the number of bytes for initial allocation *)
    InitialBytes := (RTMem.GetMaxGCSize() DIV BytesPerPage - 2) * BytesPerPage;
    (*    InitialBytes := 8 * 1024 * 1024;*)

    IF verbose > 1 THEN
      RTIO.PutText("GC >> initial heap size: ");  RTIO.PutInt(InitialBytes); 
      RTIO.PutText(" ("); RTIO.PutHex(InitialBytes); RTIO.PutText(") bytes\n");
    END;

    GrowHeap(0);

    IF MEMORY_End - MEMORY_Start < 2 * MinMemoryNodeSize THEN
      RTMisc.FatalError( NIL, 0, "gc: heap too small at initialization");
    END;

    short_freelist := MEMORY_Start;
    short_freelist.header.size := 
        (MEMORY_End-MEMORY_Start) * SHORT_TO_LONG DIV 1024;
    <* ASSERT short_freelist.header.size MOD AddrPerAlign = 0 *>
    short_freelist.header.typecode := TcFree;
    short_freelist.header.refcount := 0;
    short_freelist.header.forwarded := FALSE;
    short_freelist.header.gcStatus := GcStatus.Untraced;
    short_freelist.nextFree := NIL;
    INC(short_free, short_freelist.header.size);

    LONG_Start := MEMORY_Start + short_freelist.header.size;
    long_freelist := LONG_Start;
    long_freelist.header.size :=
        (MEMORY_End - MEMORY_Start) - short_freelist.header.size;
    <* ASSERT long_freelist.header.size MOD AddrPerAlign = 0 *>
    long_freelist.header.typecode := TcFree;
    long_freelist.header.refcount := 0;
    long_freelist.header.forwarded := FALSE;
    long_freelist.header.gcStatus := GcStatus.Untraced;
    long_freelist.nextFree := NIL;
    INC(long_free, long_freelist.header.size);
  END InitHeap;

FUNCTIONAL PROCEDURE MinAddress(): ADDRESS =
  BEGIN
    RETURN MEMORY_Start;
  END MinAddress;

FUNCTIONAL PROCEDURE MaxAddress(): ADDRESS =
  BEGIN
    RETURN MEMORY_End - 1;
  END MaxAddress;

(* GrowHeap adds a block of at least "minNewPages" free pages to the heap,
   and links it into the free list.  For now, we only support growing the
   heap one time, at initialization. *)

VAR
  InitialBytes: INTEGER;

  (*InitialBytes = 262144;    (* initial heap size is 256K *)*)
  (*InitialBytes = 67108864;  (* initial heap size is 8K pages (@ 8192 per)*)*)

CONST
  MinNewBytes  = 262144;     (* grow the heap by at least 256K *)
  MinNewFactor = 20;         (* grow the heap by at least 20% *)

PROCEDURE GrowHeap (pp: INTEGER) =
  VAR
    newChunk    : ADDRESS;
  BEGIN
    IF allocatedPages = 0 THEN
      pp := MAX(pp, (InitialBytes + BytesPerPage - 1) DIV BytesPerPage);
    ELSE
      pp := MAX(pp, (MinNewBytes + BytesPerPage - 1) DIV BytesPerPage); 
      pp := MAX(pp, (allocatedPages * MinNewFactor + 50) DIV 100);
    END;
    VAR bytes := (pp + 1) * BytesPerPage;
    BEGIN
      IF allocatedPages # 0 THEN
        RTIO.PutText("\nGC ERROR >>> ran out of traced heap\n");
        RTOS.Crash();
      END;
      newChunk := RTOS.GetMemory(bytes);
      IF newChunk = NIL OR newChunk = LOOPHOLE(-1, ADDRESS) THEN
        RTIO.PutText("GC ERROR >>> Could not extend the traced heap\n");
        RTOS.Crash();
      END;

      MEMORY_Start := newChunk;
      MEMORY_End := MEMORY_Start + bytes;
      PutText("GC >> heap: "); PutAddr(MEMORY_Start); 
      PutText(" "); PutAddr(MEMORY_End); PutText("\n");
      PutText("GC >> minimum size: "); PutInt(MinMemoryNodeSize);
      PutText("\n");
      Flush();
    END;
  END GrowHeap;

PROCEDURE GetMem (size: CARDINAL;(*FIXME*)<*UNUSED*>dataAlignment: CARDINAL): ADDRESS =
  (* ASSUMES the heap was locked with LockHeap *)
  (* Returns the address of a memory block of size at least 'size'
     addressable units, aligned for 'dataAlignment'. *)
  (* Returns NIL if no memory is available *)
  (* This procedure is called only by AllocForNew. *)
  VAR d: RefDescriptor;
      back: RefDescriptor := NIL;
      remaining: CARDINAL;
      newDescriptor: RefDescriptor;
      adrSizeRequested := MAX((( size + AddrPerAlign - 1 )
                                DIV ( AddrPerAlign )) * AddrPerAlign
                               + HeaderSize,
                               MinMemoryNodeSize );
      sizeUsed: CARDINAL;
      useShort: BOOLEAN;
  BEGIN
    IF verbose > 3 THEN
      PutText("GetMem: "); PutInt(size);
      PutText(" "); PutInt(adrSizeRequested);
      PutText("\n"); Flush();
    END;

    useShort := size < MAX_SHORT_SIZE;

    IF useShort THEN
      d := short_freelist;
    ELSE
      d := long_freelist;
    END;

    WHILE d # NIL AND d.header.size < adrSizeRequested DO
      back := d;  d := d.nextFree
    END;

    IF d = NIL THEN
      RETURN NIL; (* no memory *)
    END;

    (* ASSERT <FORALL d :: d.size MOD AddrPerAlign = 0 AND
                           d.size >= MinMemoryNodeSize>
       (Only one d is actually checked here, and only the first conjunct) *)
    <* ASSERT d.header.size MOD AddrPerAlign = 0 *>

    remaining := d.header.size - adrSizeRequested;
    IF (useShort AND remaining >= MinShortMemoryNodeSize) OR 
      (NOT useShort AND remaining >= MinLongMemoryNodeSize)
     THEN
      (* create new descriptor for the remaining memory *)
      newDescriptor := d + adrSizeRequested;
      newDescriptor.header.size := remaining;
      newDescriptor.header.typecode := TcFree;
      newDescriptor.header.refcount := 0;
      newDescriptor.header.forwarded := FALSE;
      newDescriptor.nextFree := d.nextFree;
      <* ASSERT (newDescriptor < LONG_Start) = (newDescriptor.nextFree < LONG_Start) OR newDescriptor.nextFree = NIL *>
      newDescriptor.header.gcStatus := GcStatus.Untraced;
      IF back = NIL THEN 
        IF useShort THEN
          short_freelist := newDescriptor
        ELSE
          long_freelist := newDescriptor;
          <* ASSERT long_freelist >= LONG_Start OR long_freelist = NIL *>
        END;
      ELSE
        back.nextFree := newDescriptor;
        <* ASSERT (back < LONG_Start) = (back.nextFree < LONG_Start) OR back.nextFree = NIL *>
      END;
      sizeUsed := adrSizeRequested
    ELSE
      (* not enough room for another descriptor in the remaining memory *)
      IF back = NIL THEN
        IF useShort THEN
          short_freelist := d.nextFree;
        ELSE
          long_freelist := d.nextFree;
          <* ASSERT long_freelist >= LONG_Start OR long_freelist = NIL *>
        END
      ELSE
        back.nextFree := d.nextFree;
        <* ASSERT (back < LONG_Start) = (back.nextFree < LONG_Start) OR back.nextFree = NIL *>
      END;
      sizeUsed := d.header.size
    END;

    WITH h = LOOPHOLE(d, RefHeader) DO
      h.size := sizeUsed;
      h.gcStatus := GcStatus.NotMarked;
      h.forwarded := FALSE;
      h.typecode := TcFree;
      h.refcount := 0;
    END;

    IF useShort THEN
      DEC(short_free, sizeUsed);
    ELSE
      DEC(long_free, sizeUsed);
    END;
    
    IF verbose > 3 THEN
      PutText("GetMem done: "); PutInt(sizeUsed);
      PutText("\n");
      Flush();
    END;
    IF d.header.refcount # 0 THEN
      PutText("when GetMem done\n"); 
      RTOS.Crash();
    END;
    RETURN d + HeaderSize
  END GetMem;

<* UNUSED *>
PROCEDURE FindPointersToRange(start, stop: ADDRESS) =
  VAR 
    h    : RefHeader := MEMORY_Start;
    next : ADDRESS := h + h.size;
    ptr  : UNTRACED REF ADDRESS := MEMORY_Start;
    ref  : ADDRESS;
  BEGIN
    WHILE ptr < MEMORY_End DO
      ref := ptr^;
      IF ref >= start AND ref < stop THEN
        PutText("#");
        RETURN;
      END;
      
      INC(ptr, ADRSIZE(ADDRESS));
      IF ptr = start THEN
        ptr := stop;
      END;
      IF ptr = next OR ptr = stop THEN
        REPEAT
          h := next;
          next := h + h.size;
        UNTIL h = stop OR h.typecode # TcFree;
        ptr := LOOPHOLE(h, ADDRESS);
      END;
    END;
  END FindPointersToRange;

<* UNUSED *>
PROCEDURE FindFunnyPointers() =
  VAR 
    h    : RefHeader := MEMORY_Start;
    h1   : RefHeader;
    ptr  : UNTRACED REF ADDRESS;
    ref  : ADDRESS;
    stop : ADDRESS;
  BEGIN
    WHILE h < MEMORY_End DO
      IF h.typecode = TcFree THEN
        ptr  := LOOPHOLE(h, ADDRESS) + 2 * ADRSIZE(ADDRESS);
        stop := h + h.size;
        WHILE ptr < stop DO
          ref := ptr^;
          IF MEMORY_Start <= ref AND ref < MEMORY_End THEN
            h1 := LocateHeaderOf(ref);
            IF h1.typecode # TcFree THEN
              PutText("@");
            END;
          END;
          ptr^ := LOOPHOLE(16_abcde, ADDRESS);
          INC(ptr, ADRSIZE(ADDRESS));
        END;
      END;
      h := h + h.size;
    END;
  END FindFunnyPointers; 

(* 
 * Return a memory block that was allocated by a call to GetMem.  Upon
 * return from this procedure, the location pointed to by 'a' must not
 * be used. 
 *
 * Note, the running time of this procedure is linear in the number of
 * free nodes on the heap. 
 *)

PROCEDURE ReturnMem(a: ADDRESS) =
  VAR 
    addr : ADDRESS;
    back : RefDescriptor := NIL;
    p    : RefDescriptor := NIL;
  BEGIN
    (* RTIO.PutText("X");*)
    addr := a - HeaderSize;
    BEGIN
      IF LOOPHOLE(a, RefDescriptor).header.size < MAX_SHORT_SIZE THEN
        p := short_freelist;
      ELSE
        p := long_freelist;
      END;
      WHILE p # NIL AND p < addr DO back := p;  p := p.nextFree END;
      <* ASSERT p # addr *>
    END;
    ReturnMemAux(addr, back);
  END ReturnMem;

PROCEDURE ReturnMemAux(addr: ADDRESS; back: RefDescriptor) =
  (* Performs the same function as ReturnMem, except that its
     parameters are different.  Here, 'addr' is the address of
     a Header in the heap, and 'back' is the address of the
     free node preceding 'addr' in order of increasing addresses
     (or NIL, if no free node precedes 'addr'). *)
  VAR size: CARDINAL;
      d: RefDescriptor;
      useShort: BOOLEAN;
  BEGIN
    IF addr = TrackRef THEN
      RTIO.PutText("<R "); RTIO.PutAddr(addr); RTIO.PutText("\n");
    END;
    IF NOT ((addr < LONG_Start) = (back < LONG_Start) OR back = NIL) THEN
      PutText("\n");
      PutAddr(addr); PutText(" "); 
      PutAddr(back); PutText(" "); 
      PutAddr(LONG_Start); PutText(" "); 
      PutText("\n");
    END;
    <* ASSERT (addr < LONG_Start) = (back < LONG_Start) OR back = NIL *>    

    d := addr;
    IF d = NIL THEN
      RTIO.PutText("GC-MAS >> NIL pointer in ReturnMemAux\n");
      RTOS.Crash();
    END;
    IF RefCount THEN
      IF d.header.refcount # 0 THEN
        RTIO.PutText("GC-MAS >> non-zero reference count ReturnMemAux\n");
        RTIO.PutAddr(addr+ADRSIZE(RT0.RefHeader)); RTIO.PutText(" ");
        RTIO.PutInt(d.header.typecode); RTIO.PutText(" ");
        RTIO.PutText(RTTypeSRC.TypecodeName(d.header.typecode)); 
        RTIO.PutText(" ");
        RTIO.PutInt(d.header.refcount); RTIO.PutText(" ");
        RTIO.PutBoolean(d.header.V); RTIO.PutText(" ");
        RTIO.PutBoolean(d.header.Z); RTIO.PutText("\n");
        RTOS.Crash();
      END;
    END;

    size := d.header.size;
    useShort := addr < LONG_Start;
    IF useShort THEN
      INC(short_free, size);
    ELSE
      INC(long_free, size);
    END;
    IF back = NIL THEN
      (* there is no previous node; the new node will be the first one
         on the free list *)
      d.header.size := size;
      d.header.typecode := TcFree;
      d.header.forwarded := FALSE;
      d.header.refcount := 0;
      d.header.gcStatus := GcStatus.Untraced;
      IF useShort THEN
        d.nextFree := short_freelist;
        <* ASSERT (d < LONG_Start) = (d.nextFree < LONG_Start) OR d.nextFree = NIL *>
        short_freelist := d
      ELSE
        d.nextFree := long_freelist;
        <* ASSERT (d < LONG_Start) = (d.nextFree < LONG_Start) OR d.nextFree = NIL *>
        long_freelist := d;
        <* ASSERT long_freelist >= LONG_Start OR long_freelist = NIL *>
      END;
    ELSIF back + back.header.size = addr THEN
      (* merge this node with the previous *)
      d := back;
      INC(d.header.size, size);
      (* d.nextFree and d.typecode are already set up properly *)
    ELSE
      (* the previous node on the list is not adjacent *)
      d.header.size := size;
      d.header.typecode := TcFree;
      d.header.forwarded := FALSE;
      d.header.refcount := 0;
      d.header.gcStatus := GcStatus.Untraced;
      d.nextFree := back.nextFree;
      <* ASSERT (d < LONG_Start) = (d.nextFree < LONG_Start) OR d.nextFree = NIL *>
      back.nextFree := d;
      <* ASSERT (back < LONG_Start) = (back.nextFree < LONG_Start) OR back.nextFree = NIL *>
    END;
    (* check if next node is adjacent *)
    IF d.nextFree # NIL AND d + d.header.size = d.nextFree THEN
      (* merge this node with the next *)
      WITH next = d.nextFree DO
        INC(d.header.size, next.header.size);
        next := next.nextFree
      END
      (* d.typecode is already set up properly *)
    END
  END ReturnMemAux;

(*-------------------------------------------------- low-level collection ---*)

CONST
  PtrBufferSize = 128;
  GcStackSize = PtrBufferSize * ADRSIZE(ADDRESS) +
                256 * ADRSIZE(Word.T);
  MaxMapDepth = 5;
  MaxQSortDepth = 12;

VAR
  needMoreRecursion: BOOLEAN;

<* UNUSED *>
PROCEDURE Sort( VAR m: ARRAY [0..PtrBufferSize-1] OF ADDRESS; c: CARDINAL ) =
  (* sorts 'c' elements starting with 0 of 'm' *)
  BEGIN
    QSort( m, c, 0, MaxQSortDepth );
    ISort( m, c )
  END Sort;

PROCEDURE ISort( VAR m: ARRAY [0..PtrBufferSize-1] OF ADDRESS; c: CARDINAL ) =
  (* sorts, using Insertion Sort, 'c' elements starting with 0 of 'm' *)
  VAR a: ADDRESS;
      j: CARDINAL;
  BEGIN
    (* invariant (using the artificial i = c upon termination of the loop):
       SUBARRAY( m, 0, i ) is a sorted  *)
    FOR i := 1 TO c-1 DO
      a := m[i];  j := i;
      (* invariant:  (FORALL k : j < k <= i : a < m[k]) *)
      WHILE j # 0 AND a < m[j-1] DO
        m[j] := m[j-1];  DEC( j )
      END;
      (* invariant AND ( j=0 OR m[j-1] <= a )  ==>
         invariant AND (FORALL k : 0 <= k < j : m[k] <= a) *)
      IF j # i THEN m[j] := a END  (* j = i ==> j # 0 AND m[j] <= a *)
    END
  END ISort;

PROCEDURE QSort( VAR m: ARRAY [0..PtrBufferSize-1] OF ADDRESS;
                 c: CARDINAL; s: CARDINAL; depth: CARDINAL ) =
  (* May sort, using QuickSort, 'c' elements starting with 's' of 'm'.
     In any case, 'm' returns as a permutation of what it was passed in as. *)
  BEGIN
    WHILE c > 2 DO
      (* invoke the Dutch Flag program, using "<white" as red, ">white"
         as blue. *)
      VAR white := m[ s + (c DIV 2) ];
          w, k: CARDINAL := s;
          b: CARDINAL := s + c;
      BEGIN
        WHILE k # b DO
          IF      m[k] < white THEN  (* m[k] = red *)
            VAR t := m[k]; BEGIN m[k] := m[w];  m[w] := t END;
            INC( w );  INC( k )
          ELSIF   m[k] > white THEN  (* m[k] = blue *)
            DEC( b );
            VAR t := m[k]; BEGIN m[k] := m[b];  m[b] := t END
          ELSE                       (* m[k] = white *)
            INC( k )
          END
        END;
        <* ASSERT w # b *>
        IF w-s >= s+c-b THEN
          IF depth # 0 THEN QSort( m, s+c-b, b, depth-1 ) END;
          c := w-s
        ELSE
          IF depth # 0 THEN QSort( m, w-s, s, depth-1 ) END;
          c := s+c-b;  s := b
        END
      END
    END
  END QSort;

PROCEDURE LocateHeaderOf(a: ADDRESS): RefHeader =
  VAR h: RefHeader := MEMORY_Start;
  BEGIN
    IF a < MEMORY_Start OR a >= MEMORY_End THEN
      RETURN NIL;
    END;
    WHILE h + h.size <= a DO
      h := h + h.size;
    END;
    RETURN h;
  END LocateHeaderOf;

PROCEDURE CheckSanity (msg: TEXT): BOOLEAN =
  VAR 
    h: RefHeader := MEMORY_Start;
    cnt: INTEGER := 0;
  PROCEDURE PutError(error: TEXT) =
    BEGIN
      PutText("GC ERROR >> ");
      PutText(msg); PutText(" : ");
      PutText(error); PutText("\ncnt ");
      PutInt(cnt); PutText(", addr ");
      PutAddr(h); PutText(", tc ");
      PutInt(h.typecode); PutText(", refcount ");
      PutInt(h.refcount); PutText(", status ");
      PutInt(ORD(h.gcStatus)); PutText(", size ");
      PutInt(h.size); PutText("\n");
      Flush();
      (*
      DumpHeap(FALSE);
      *)
    END PutError;
  BEGIN
    WHILE h < MEMORY_End DO
      IF h.size < MinMemoryNodeSize THEN
        PutError("illegal size");
        RETURN FALSE;
      END;
      VAR
        tc := h.typecode;
      BEGIN
        IF (tc <= 0 OR tc >= RT0u.nTypes) AND tc # TcFree THEN
          PutError("illegal typecode");
          RETURN FALSE;
        END;
      END;
      IF h.gcStatus = GcStatus.Unused OR
        h.gcStatus = GcStatus.Untraced AND h.typecode # TcFree THEN
        PutError("illegal gcStatus");
        RETURN FALSE;
      END;
      IF h.refcount # 0 THEN
        PutError("illegal refcount");
      END;
      (*
      CheckMarkSanity(h);
      *)
      h := h + h.size;
      INC(cnt);
    END;
    RETURN TRUE;
  END CheckSanity; 

(*
<* UNUSED *> THIS IS UNFINISHED
PROCEDURE CheckMarkSanity(h: RefHeader) =
  VAR
    ptr  : UNTRACED REF ADDRESS := LOOPHOLE(h, ADDRESS) + 2 * ADRSIZE(ADDRESS);
    stop : ADDRESS := h + h.size; (* ???? *)
    ref  : ADDRESS;
    h1   : RefHeader;
  BEGIN
    WHILE ptr < stop DO
      ref := ptr^;
      IF MEMORY_Start <= ref AND ref < MEMORY_End THEN
        h1 := LocateHeaderOf(ref);
        CASE h.typecode OF
        | Unused => 
          PutText("<1>");
        | Untraced => 
          (* a reference from free to used (should have been wiped out) *)
          IF h1.gcStatus # Untraced THEN PutText("<2>"); END; 
        | NotMarked =>
          (* an unmarked objects right after collection *)
          IF collectorState = XXX THEN PutText("<3>"); END;
          (* a reference from used to free *)
          IF h1.gcStatus = Untraced THEN PutText("<4>"); END; 
        | Marked =>
          (* a partially marked object after full scan *)
          IF collectorState > XXX THEN PutText("<3>"); END;
          (* a reference from used to free *)
          IF h1.gcStatus = Untraced THEN PutText("<4>"); END; 
        | RecursivelyMarked =>
          (* a reference from black to white *)
          IF h1.gcStatus = NotMarked THEN PutText("<4>"); END; 
          (* a reference from black to grey *)
          IF h1.gcStatus = Marked THEN PutText("<4>"); END; 
          (* a reference from used to free *)
          IF h1.gcStatus = Untraced THEN PutText("<4>"); END; 
        END;
        IF h.typecode # TcFree THEN
          PutText("@");
        END;
      END;
      ptr^ := LOOPHOLE(16_abcde, ADDRESS);
      INC(ptr, ADRSIZE(ADDRESS));
    END;
    END CheckMarkSanity;
*)

TYPE 
  Marker = RTHeapMap.Visitor OBJECT 
    depth : INTEGER := 0;
  OVERRIDES apply := MarkReferent;
  END;

  Cleaner = RTHeapMap.Visitor OBJECT 
  OVERRIDES apply := CleanReferent;
  END;

PROCEDURE CleanReferent (<* UNUSED *>self: Cleaner; a: ADDRESS) =
  (* REQUIRES 'a' is a pointer to a location that contains a pointer to a 
     traced variable in the heap *)
  VAR
    refref := LOOPHOLE(a, UNTRACED REF ADDRESS);
    ref    := refref^;
  BEGIN
    IF ref = NIL THEN RETURN; END;

    WITH h = LOOPHOLE(ref-ADRSIZE(RT0.RefHeader),UNTRACED REF RT0.RefHeader) DO
      IF h = TrackRef OR h.typecode = TrackTC THEN
        WITH from = LocateHeaderOf(a) DO
          RTIO.PutText("{"); RTIO.PutAddr(h); RTIO.PutText(" ");
          RTIO.PutInt(h.typecode); RTIO.PutText(" ");
          RTIO.PutAddr(a); RTIO.PutText(" ");
          RTIO.PutAddr(from); RTIO.PutText(" ");
          RTIO.PutInt(from.typecode); RTIO.PutText("}");
        END;
      END;
    END;
    IF RefCount THEN
      RTRefCount.Decrement(ref);
    END;
  END CleanReferent; 

VAR
  markedCnt, markedBytes : INTEGER;
  doneCnt, doneBytes     : INTEGER;

PROCEDURE MarkReferent (self: Marker; a: ADDRESS) =
  (* REQUIRES 'a' is a pointer to a location that contains a pointer to a 
     traced variable in the heap *)
  (* Mark the header of the referent of 'a' and recursively mark the headers 
     of all variables to which the referent contains a pointer.
     'depth' indicates the number of levels of references that may be
     recursively marked. *)
  VAR
    refref := LOOPHOLE(a, UNTRACED REF ADDRESS);
    ref    := refref^;
    h      : RefHeader;
  BEGIN
    IF ref = NIL THEN RETURN; END;
    h := ref - HeaderSize;
    IF TrackRef = NIL THEN
      Mark(h, self, NIL);
    ELSE
      WITH from = LocateHeaderOf(a) DO
        IF from = NIL THEN
          Mark(h, self, a);
        ELSE
          Mark(h, self, from);
        END;
      END;
    END;
  END MarkReferent;

PROCEDURE Mark (h: RefHeader; marker: Marker; from: RefHeader) =
  BEGIN
    IF h.forwarded THEN
      RTIO.PutText("M: "); RTIO.PutAddr(h); 
      IF h # NIL THEN
        RTIO.PutText(" "); RTIO.PutInt(h.typecode); 
        RTIO.PutText(" "); RTIO.PutInt(ORD(h.gcStatus)); 
        RTIO.PutText(" "); RTIO.PutString(RTType.Get(h.typecode).name); 
      END;
      RTIO.PutText(" "); RTIO.PutAddr(from); 
      IF from # NIL AND from >= MEMORY_Start AND from < MEMORY_End THEN
        RTIO.PutText(" "); RTIO.PutInt(from.typecode); 
        RTIO.PutText(" "); RTIO.PutInt(ORD(from.gcStatus)); 
        RTIO.PutString(RTType.Get(from.typecode).name); 
      END;
      RTIO.PutText("\n");
      IF traceOn THEN
        RTHeapTrace.PutRefDesc(LOOPHOLE(h+ADRSIZE(RT0.RefHeader), REFANY)); 
        RTIO.PutText(" ");
        RTIO.PutAddr(RTHeapTrace.GetAllocationPC(
                         LOOPHOLE(h+ADRSIZE(RT0.RefHeader), REFANY))); 
        RTIO.PutText("\n");
      END;
      RTOS.Crash();
      RETURN;
    END;

    IF h.gcStatus = GcStatus.Untraced THEN
      PutText("GC ERROR >> GcStatus.Untraced found in Mark : ");
      PutAddr(h); PutText("\n");
      DumpHeap(FALSE);
    END;
    IF h.gcStatus = GcStatus.Untraced THEN
      PutText("GC ERROR >> h.gcStatus = GcStatus.Untraced\n");
      RTOS.Crash();
    END;
    <* ASSERT h.gcStatus # GcStatus.Untraced *>
    IF h.gcStatus # GcStatus.RecursivelyMarked THEN
      IF marker.depth = 0 THEN
        IF h.gcStatus = GcStatus.NotMarked THEN
          INC(markedCnt);
          INC(markedBytes, h.size);
        END;
        h.gcStatus := GcStatus.Marked;
        needMoreRecursion := TRUE;
        RETURN
      END;

      IF h.gcStatus = GcStatus.NotMarked THEN
        INC(doneCnt);
        INC(doneBytes, h.size);
      END;
      h.gcStatus := GcStatus.RecursivelyMarked;

      (* this object is marked, mark what it points to *)
      DEC(marker.depth);
      RTHeapMap.WalkRef (h, marker);
      INC(marker.depth);
    END;
  END Mark;

PROCEDURE MarkThrough() =
  (* call Mark on all nodes in the heap whose gcStatus is Marked *)
  VAR h: RefHeader;
  BEGIN
    LOOP
      needMoreRecursion := FALSE;
      h := MEMORY_Start;
      WHILE h < MEMORY_End DO
        IF h.gcStatus = GcStatus.Marked THEN
          theMarker.depth := MaxMapDepth;
          Mark(h, theMarker, NIL);
        END;
        IF h.size = 0 THEN
          PutText("GC ERROR >> zero size object found during marking\n");
          EXIT;
        END;
        INC(h, h.size);
      END;
      IF NOT needMoreRecursion THEN EXIT END
    END
  END MarkThrough;

PROCEDURE MarkFromStacks (start, stop: ADDRESS) =
  (* Marks (does not recursively mark) traced variables for which
     there exists a potential reference to in a stack.  A word
     on the stack is assumed to be a reference to a variable if
     it contains the value of any of the addresses occupied by
     the variable.  Since this procedure does not recursively
     mark (to conserve stack space, as this procedure puts a
     large array on its stack) the variables, procedure MarkThrough
     needs to be run after this procedure's completion. *)

  VAR
    fp := start;
    p  : ADDRESS;
    h  : RefHeader;
  BEGIN
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF MEMORY_Start <= p AND p < MEMORY_End THEN
        h := LocateHeaderOf(p);
        IF h.typecode # TcFree AND NOT h.forwarded THEN
          IF h.gcStatus = GcStatus.Untraced THEN
            RTIO.PutText("FUCK 1\n");
            RTOS.Crash();
          END;
          IF h.gcStatus = GcStatus.NotMarked THEN
            INC(markedCnt);
            INC(markedBytes, h.size);
          END;
          h.gcStatus := GcStatus.Marked;
        END;
      END;
      INC(fp, RTMachine.PointerAlignment);
    END;
  END MarkFromStacks;

PROCEDURE RecycleGarbage (): BOOLEAN =
  (* Recycles all nodes in the heap whose gcStatus is NotMarked, by
     putting them onto the free list.  Sets the gcStatus of all marked
     nodes back to NotMarked.  Returns TRUE if any memory was
     recycled, and FALSE otherwise. *)
  VAR h: RefHeader;
      back: RefDescriptor; (* INVARIANT: back = NIL OR
                                         back is a node on the free list *)
      recycledAny: BOOLEAN;
  BEGIN
    IF verbose > 2 THEN PutText("Recycle: clean refcounts\n"); Flush(); END;
    IF RefCount THEN
      h := MEMORY_Start;
      WHILE h < MEMORY_End DO
        IF h.gcStatus = GcStatus.NotMarked THEN
          (* DO NOT COLLECT CYCLES *)
          (*
            IF h.refcount # 0 OR h.V OR h.Z THEN
            h.gcStatus := GcStatus.RecursivelyMarked;
            ELSE
            (* IF h.refcount # 0 OR h.V THEN RTIO.PutText("C1"); END;*)
               (* IF NOT h.forwarded THEN RTIO.PutText("C2"); END;*)
                  END;
          *)
          IF h.forwarded THEN
            RTHeapMap.WalkRef (h, theCleaner);
          ELSE
            h.gcStatus := GcStatus.RecursivelyMarked;
          END;
        ELSIF h.gcStatus = GcStatus.RecursivelyMarked THEN
          IF h.forwarded THEN RTIO.PutText("W1"); END;
        END;
        INC(h, h.size);
      END;
      IF verbose > 2 THEN PutText("Recycle: free zeros\n"); Flush(); END;
      (* RTRefCount.FreeZeroObjects();*)
    END;

    IF verbose > 2 THEN PutText("Recycle: scan\n"); Flush(); END;
    h           := MEMORY_Start;
    back        := NIL;
    recycledAny := FALSE;
    WHILE h < MEMORY_End DO
      IF h.gcStatus = GcStatus.NotMarked THEN
        VAR p: RefDescriptor; BEGIN
          IF back = NIL AND h < LONG_Start THEN 
            p := short_freelist;
          ELSIF back < LONG_Start AND h >= LONG_Start THEN
            back := NIL;
            p := long_freelist;
          ELSE 
            p := back.nextFree;
          END;
          WHILE p # NIL AND p < h DO back := p;  p := p.nextFree END
        END;
        IF traceOn THEN
          RTHeapTrace.ObjectDeallocated(LOOPHOLE(h+ADRSIZE(RT0.RefHeader), 
                                                 Word.T));
        END;
        IF RefCount THEN
          IF NOT h.forwarded THEN RTIO.PutText("W2"); END;
        END;
        ReturnMemAux(h, back);
      ELSIF h.gcStatus = GcStatus.RecursivelyMarked THEN
        IF RefCount THEN
          IF h.forwarded THEN RTIO.PutText("W3"); END;
        END;
        h.gcStatus := GcStatus.NotMarked
      END;
      recycledAny := TRUE;
      (* Note, h.size on the next line may be different than it was before
         the above call to ReturnMemAux. *)
      INC(h, h.size);
    END;
    RETURN recycledAny;
  END RecycleGarbage;

(*------------------------------------------------------------ collection ---*)

PROCEDURE Collect ((*bytes: INTEGER*)) =
  BEGIN
    IF verbose > 2 THEN
      PutText("\n<<GC: alloc: "); (* PutInt(bytes); *)
      PutText(" short: "); PutInt(short_free); 
      PutText(" long: "); PutInt(long_free); 
      PutText(" cnt: "); PutInt(cnt); 
      PutText(" total: "); (* PutInt(bytes); *)
      PutText(" max: "); PutInt(maxSize);
      PutText(" ..\n"); 
      Flush();
    END;

    PutText("<<GC..");
    IF verbose > 1 THEN PutText("Collect\n"); Flush(); END;

    IF disableCount > 0 THEN
      PutText("Disabled\n");
      RETURN;
    END;

    IF doSanity AND NOT CheckSanity("start Collect") THEN RTOS.Crash(); END;

    ThreadF.SuspendOthers();

    markedCnt   := 0;
    markedBytes := 0;
    doneCnt     := 0;
    doneBytes   := 0;

    IF verbose > 1 THEN PutText("Collect: Process Refcounts\n"); Flush(); END;
    IF RefCount THEN
      RTRefCount.ProcessOutstanding (TRUE);
    END;
    PutText("#");

    (* ASSERT <FORALL h : h is a header of an allocated heap item :
       h.gcStatus IN { Untraced, NotMarked }> *)
    IF verbose > 1 THEN PutText("Collect: Stacks\n"); Flush(); END;
    ThreadF.ProcessStacks(MarkFromStacks);
    PutText("#");
    IF verbose > 2 THEN
      PutText("\t<<Stacks: "); 
      PutInt(markedCnt); PutText(" "); PutInt(markedBytes); PutText(" ");
      PutInt(doneCnt); PutText(" "); PutInt(doneBytes); PutText("\n");
      Flush();
    END;

    IF verbose > 1 THEN PutText("Collect: Strongrefs\n"); Flush(); END;
    RTStrongRef.ProcessRefs(MarkFromStacks);
    PutText("#");
    IF verbose > 2 THEN
      PutText("\t<<Strongrefs: "); 
      PutInt(markedCnt); PutText(" "); PutInt(markedBytes); PutText(" ");
      PutInt(doneCnt); PutText(" "); PutInt(doneBytes); PutText("\n");
      Flush();
    END;

    IF verbose > 1 THEN PutText("Collect: WalkGlobals\n"); Flush(); END;
    theMarker.depth := 0;
    RTHeapMap.WalkGlobals(theMarker);
    PutText("#");
    (* ASSERT <FORALL h : h is a header of an allocated heap item :
       h.gcStatus IN { Untraced, NotMarked, Marked }> *)

    IF verbose > 2 THEN
      PutText("\t<<Globals: ");
      PutInt(markedCnt); PutText(" "); PutInt(markedBytes); PutText(" ");
      PutInt(doneCnt); PutText(" "); PutInt(doneBytes); PutText("\n");
      Flush();
    END;

    IF verbose > 1 THEN PutText("Collect: MarkThrough\n"); Flush(); END;
    MarkThrough();
    PutText("#");
    (* ASSERT <FORALL h : h is a header of an allocated heap item :
       h.gcStatus IN { Untraced, NotMarked, RecursivelyMarked }> *)

    IF verbose > 2 THEN
      PutText("\t<<All: ");
      PutInt(markedCnt); PutText(" "); PutInt(markedBytes); PutText(" ");
      PutInt(doneCnt); PutText(" "); PutInt(doneBytes); PutText("\n");
      Flush();
    END;

    IF RefCount THEN
      IF verbose > 1 THEN PutText("Collect: Process Refcounts\n"); Flush();END;
      RTRefCount.ProcessOutstanding (FALSE);
      PutText("#");
    END;

    IF verbose > 1 THEN PutText("Collect: RecycleGarbage\n"); Flush(); END;
    IF NOT RecycleGarbage() THEN
      PutText("Collect: RecycleGarbage returned FALSE\n");
    END;
    PutText("#");

    ThreadF.ResumeOthers();

    IF doSanity AND NOT CheckSanity("end of Collect") THEN RTOS.Crash(); END;

    IF verbose > 2 THEN
      PutText(".. "); PutInt(short_free); PutText(" ");
      PutInt(long_free); PutText(" done>>\n"); Flush();
    END;
    IF verbose > 1 THEN PutText("Collect: done\n"); Flush(); END;
    PutText("..GC>>\n");
    RETURN;
  END Collect;

(*------------------------------------------------------------ allocation ---*)

VAR
  bytes, cnt, maxSize: INTEGER;

PROCEDURE AllocForNew(dataSize, alignment: CARDINAL ): ADDRESS =
  (* Returns the address of a memory block of size at least 'size'
     addressable units, and which is to be used as storage for a variable
     that needs 'alignment'. *)
  (* Always returns a non-NIL value.  If no memory is available, this
     procedure does not return. *)
  (* All procedures below should call this procedure rather than the
     lower-level GetMem, since it is this procedure that properly
     handles memory failures. *)
  VAR res: ADDRESS;
  BEGIN
    RTOS.LockHeap();

    IF RefCount THEN
      RTRefCount.Check();
    END;

    INC(cnt);
    INC(bytes, dataSize);
    maxSize := MAX(dataSize, maxSize);

    IF verbose > 3 THEN
      PutText("AllocForNew "); PutInt(dataSize); PutText("\n");
    END;

    IF doSanity AND NOT CheckSanity("before GetMem") THEN RTOS.Crash(); END;

    res := GetMem(dataSize, alignment);

    IF doSanity AND NOT CheckSanity("after GetMem") THEN RTOS.Crash(); END;

    IF res = NIL THEN
      Collect((*dataSize*));

      IF doSanity AND NOT CheckSanity("after Collect") THEN RTOS.Crash(); END;

      res := GetMem(dataSize, alignment);

      IF doSanity AND NOT CheckSanity("after second GetMem") THEN RTOS.Crash(); END;
      IF res = NIL THEN
        RTMisc.FatalError( NIL, 0, "gc: out of memory" );
      END;
    END;

    IF doSanity AND NOT CheckSanity("returning from AllocForNew") THEN RTOS.Crash(); END;

    IF verbose > 3 THEN
      PutText("AllocForNew done\n");
    END;
    RTOS.UnlockHeap();
    RETURN res
  END AllocForNew;

PROCEDURE AllocForNewBody(<* UNUSED *>dataSize, alignment: CARDINAL ): ADDRESS =
  BEGIN
    RTIO.PutText("AllocForNewBody called\n");
    RTOS.Crash();
  END AllocForNewBody;

(* Malloc returns the address of "size" bytes of untraced, zeroed storage *)

PROCEDURE Malloc (size: INTEGER): ADDRESS =
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

(*------------------------------------------------------------- fault     ---*)

PROCEDURE Fault (<* UNUSED *>addr: ADDRESS): BOOLEAN =
  BEGIN
    (* PutText("GC ERROR >> fault in the mark-and-sweep collector\n");*)
    RETURN FALSE;
  END Fault;

(*------------------------------------------------------------- debugging ---*)

PROCEDURE DumpHeap(detail: BOOLEAN := TRUE) =
  CONST GcS = ARRAY GcStatus OF TEXT { "Unused", "Untraced", "Not Marked",
                                       "Marked", "Recursively Marked" };
  VAR h: RefHeader := MEMORY_Start;
  BEGIN
    PutText( "\n-----  Heap  -----\n" );
    WHILE h < MEMORY_End DO
      PutText( "Address: " );  PutAddr( h );
      PutText( "   " );
      PutText( "Size: " );  PutInt( h.size );  PutText( "   " );
      IF h.typecode = TcFree THEN
        PutText( "FREE\n" )
      ELSE
        PutText( "Typecode: " );  PutInt( h.typecode );  PutText( "   " );
        PutText( "GcStatus: " );  PutText( GcS[ h.gcStatus ] );
        IF detail THEN
          <* ASSERT h.size MOD AddrPerAlign = 0 *>
          <* ASSERT h.size >= HeaderSize *>
          <* ASSERT h.size >= MinMemoryNodeSize *>
          FOR i := 0 TO ((h.size - HeaderSize) DIV ADRSIZE(Word.T)) - 1 DO
            IF i MOD 8 = 0 THEN PutText( "\n" ) END;
            PutText( "    " );
            PutInt( LOOPHOLE( h+HeaderSize+i*ADRSIZE(Word.T),
                              UNTRACED REF Word.T )^ )
          END
        END;
        PutText( "\n" )
      END;
      IF h.size = 0 THEN
        PutText("size = ZERO\n");
        EXIT;
      END;
      INC( h, h.size )
    END;
    Flush();
  END DumpHeap;

<* UNUSED *>
PROCEDURE DumpFreeList(d: RefDescriptor (* := freelist *)) =
  BEGIN
    PutText( "\n-----  Free list  -----\n" );
    WHILE d # NIL DO
      PutText( "Address: " );  PutAddr( d );
      PutText( "   " );
      PutText( "Size: " );  PutInt( d.header.size );
      IF d.header.typecode # TcFree THEN
        PutText("   ");  PutText( "Typecode: " );  PutInt(d.header.typecode)
      END;
      PutText( "\n" );
      <* ASSERT d.header.size >= MinMemoryNodeSize *>
      <* ASSERT d.nextFree = NIL OR d < d.nextFree *>
      d := d.nextFree
    END
  END DumpFreeList;

(*---------------------------------------------------------------------------*)

PROCEDURE GetRealPointer (ref: ADDRESS): ADDRESS =
  BEGIN
    <* ASSERT FALSE *>
  END GetRealPointer;

BEGIN
END RTHeapMAS.
