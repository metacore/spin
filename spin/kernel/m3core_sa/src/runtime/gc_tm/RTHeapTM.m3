(* Copyright (C) 1990, Digital Equipment Corporation *)
(* All rights reserved. *)
(* See the file COPYRIGHT for a full description. *)

(* Last modified on Tue Dec 22 16:58:21 PST 1992 by rustan *)
(*      modified on Fri Sep 18 14:58:21 PDT 1992 by rustan *)

(* HISTORY
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added pause histogram and fragmentation reporting.  A few bug
 *	fixes.  This is the code base used for PLDI measurements.
 *
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Lots of changes.  Made fully incremental. Still needs some
 *      tuning and lots of cleanup.
 *
 * 31-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added initial code for incrementality.  Also hardcoded heap size
 *	to 20480000.
 *
 * 06-May-97  Przemek Pardyak (pardy) at the University of Washington
 *      Added some tracing calls.
 *
 * 05-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added full VM remapping, and support for the gc stat command.
 *
 * 31-Mar-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.  TreadMill style collection.  Not yet incremental.
 *
 *)

UNSAFE MODULE RTHeapTM EXPORTS RTHeapTM, RTHeapRep, RTCollector;

IMPORT RT0, RT0u, RTMisc, Word, ThreadF, RTMem, RTIO, RTHeapTrace;
IMPORT RTHeapMap, RTOS, RTMachine, Cstdlib, RTHeapStats, RTParams;
IMPORT RTAllocator, RTHeapPages,RTutils, RTStrongRef, RTHeapDebug;
IMPORT RTHeapVM;
IMPORT Spy;
IMPORT RTCollectorSRC;
IMPORT RTRefCount;
(*IMPORT RTHeapDep;*) (* for profiling and measuring marking *)
IMPORT RTHisto;


FROM RTCollectorSRC IMPORT DoTimings;
FROM RT0 IMPORT Typecode, GcStatus;
FROM RTIO IMPORT PutText, PutInt, PutAddr, Flush;


(* There are AddrPerAlign addressable units per alignment *)
(* AddrPerAlign satisfies, for any address a:
     ( INT(a) MOD AddrPerAlign = 0 ) = ( any object can start at address a )
   where INT(a) means LOOPHOLE( a, INTEGER ).  *)

CONST AddrPerAlign = RTMachine.PointerAlignment;

(* XXX machine dependent! *)

CONST 
  (* to get descriptor from half ptr *)
  DescMask = 16_ffffffff00000000;
  (* to get ptr from desc *)
  PtrMask =  16_00000000ffffffff;
  MaxThreads = 2000; (* max threads under alpha that can be processed *)

(*-------------------------------------------------- low-level allocation
   ---*)

TYPE
  (* NOTE.  It is assumed that the first two fields, i.e., 'size' and
     'typecode', in Header and Descriptor are the same.

     The 'size' field is the number of addressable units in the node,
     including the Header or Descriptor record itself.  'size' is
     invariantly at least MinMemoryNodeSize and a multiple of AddrPerAlign.

     'typecode' is the typecode of the variable stored in the node, and is
     TcFree if the node is free.

     After these two fields the Header and Descriptor records may differ in
     any way (e.g., their sizes are not required to be the same). *)

  (* Note, header may change, but the typecode must always be where it is *)
  (* The program gets a ptr to just after the header *)
  RefDescriptor = UNTRACED REF Descriptor;  
  (* 2 half ptrs, OR with 0xffffffff00000000 to get a ptr to a refdescriptor *)
  Ptr = BITS 32  FOR [0 .. 16_ffffffff];
  Ptrs = RECORD next, prev : Ptr END;

  Descriptor =
    RECORD
    ptrs  : Ptrs;
      header: RT0.RefHeader;
      (* The header fields :
      bs1       : BITS  1 FOR BOOLEAN    := FALSE;
      gcStatus  : BITS  3 FOR GcStatus   := GcStatus.NotMarked;
      typecode  : BITS 20 FOR Typecode   := 0;    
      site      : BITS  8 FOR [0 .. 255] := 0;
      size      : BITS 32 FOR [0 .. 16_ffffffff];
      *)

      (* nextFree/prevFree aren't strictly necessary but it is
         convenient to initialize them to the properly masked ptrs *)

      (* also note that the allocator returns a pointer just after the
         header.  That is, the following fields will get overwritten
         by the mutator
         *)

      nextFree: RefDescriptor;   (* next memory block on the free list *)
      prevFree: RefDescriptor;   (* prev memory block on the free list *)
    END;

CONST
  (* HeaderSize is the smallest multiple of AddrPerAlign that is at least
     ADRSIZE( Header ).  *)
  HeaderSize = ((ADRSIZE(Header) + AddrPerAlign - 1) DIV AddrPerAlign)
                 * AddrPerAlign;
  DescriptorSize = ADRSIZE(Descriptor);
  PtrSize = BYTESIZE(Ptrs);
  Offset = PtrSize + HeaderSize;   (* total offset from header to payload *)
  (* MinMemoryNodeSize is the minimum size of memory available in a node.
     This minimum exists so that any used node can be converted into a
     descriptor node at any time. *)
  MinMemoryNodeSize = MAX(HeaderSize+PtrSize, 
                          ((DescriptorSize + AddrPerAlign - 1)
                           DIV AddrPerAlign) * AddrPerAlign);


  TcFree = LAST(Typecode);


(* -------------Segregated free list stuff *)
(* The following vars determine the free lists used *)
(* the last list (the reserve pool) must be managed by the old code *)
CONST
  NumFreeLists = 10;              (* 9 sub page lists and the rest for the
                                     reserve pool *)
  HalfReserve = TRUE;            (* devote half the memory to reserve
                                    pool *)
  (* this allows us to manually set the size of the reservepool.  big hack*)
  (* This overrides the HalfReserve options.  Use -1 to use the half
     reserve option above *)
  ReservePct  = 50;              (* Use 72% of memory initially for reservepool*)
  RP = NumFreeLists - 1;(* just for clarity *)
  StartSize  = 32;              (* size of the smallest objects *)

  Sanity     = TRUE;             (* other sanity checks enable/disable *)
  CheckDuringAlloc = FALSE;      (* sanity check during allocation. *really* slow. *)
 
VAR
  MEMORY_Start: ADDRESS := NIL;
  MEMORY_End  : ADDRESS := NIL;
  TMVerbose   : INTEGER;
  ReserveSize := -1 ;            (* -1  => not yet set *)

VAR
  Finish : BOOLEAN;             (* indicate that we must finish the colln next
                                   chance we get *)
  BytesMarked : INTEGER;        (* # of bytes marked in the colln so far *)
  BytesAlloced: INTEGER;        (* # of bytes allocated since end of last colln *)
  NumAllocations : INTEGER;     (* # allocs since last collection *)
  NumResAllocations : INTEGER;  (* # allocs from respool since last collection *)
  ResBytesAlloced : INTEGER;    (* # bytes alloced from res pool since last colln *)
  ToMark      : INTEGER;        (* # of bytes to be marked at each increment,
                                   determined at beginning of each collection *)
  TotalFreeBytes : INTEGER; 
  TotalElems : INTEGER;         (* # of elems in segregated freelists 
                                   (EXCLUDING reserve pool) *)

  (* ---------- control vars for allocation/collection ---------- *)
  Balance := TRUE;              (* perform list balancing *)
  ResFirstFit := TRUE;          (* first or best fit in reserve pool *)


  (* ---------- freelists and info about them ---------- *)

  InitialSizes: INTEGER;                                  (* not useful *)
  FreeListLocations: UNTRACED REF ARRAY OF ADDRESS;
  ElemsPerPage : ARRAY [0..NumFreeLists-1] OF Page;

  (* map of the ptrs for treadmill *)
               (* <---next                  prev--> *)
  (*BOTTOM--white--FREE--live/black--SCAN--live/gray--TOP--ecru--BOTTOM *)


  Free   : UNTRACED REF ARRAY OF RefDescriptor;
  (* last Free thing, for fast append *)
  Bottom :  UNTRACED REF ARRAY OF RefDescriptor; 

  (* last ecru descriptor *) 
  Top :  UNTRACED REF ARRAY OF RefDescriptor; 

  (* gray, marked but not yet scanned *)
  (* scanning moves objects between gray and black *)
  Scan : UNTRACED REF ARRAY OF RefDescriptor; 

  (* WHEN THINGS ARE EMPTY
     LIST            IS EMPTY WHEN...
     ----------------------------------------------------------------------
     free/white      Free[i] = Bottom[i]
     ecru            Top[i] = Bottom[i].  o.w. top pts to head of ecru list.
     live/gray       Scan[i] points to Top[i], 
                        o.w. Scan[i] points to first gray object
     live/black      Scan[i] = Free[i]
  *)

  (* The ecru list is empty when Top[i] to Bottom[i].  During non
     incremental allocation, Top[i] follows Free[i], since there
     are no black objects.  Then during the scanning phase, crap gets
     moved from ecru to grey/black.  *)


  (* --------------- Marking Colors -------------- *)
  (* 
     new objects are always allocated with WhiteStatus.
     At the end of collection, the colors are flipped.
     Gray is the usual GcStatus.Marked
  *)
     
  WhiteStatus : GcStatus;
  BlackStatus : GcStatus;
  GrayStatus  := GcStatus.Marked;


  (* --------------- Page Lists ------------------ *)
  (* PAGE COLLECTION
     works as follows :
     When a new object is allocated and the page is not already dirty
     (i.e. desc.space = Space.Current ), the page is marked Current
     and moved to the AllocatedPages list.

     During marking phase, when an object is marked black, the page it is
     on is also marked black by moving it to the BlackPages list.  The
     space field is set to Space.Previous (for lack of a better choice).

     At the end of collection, all pages left in the allocated pages
     list are moved back into the freelist. Black and Allocated are then
     flipped.

     Because there is no way to tell if a page is dirty or not, the
     pages that are returned to the FreePagesList must have their
     Space field reset to "free" manually during the reclaim phase. 

  *)
  (* 
     FreePageList is the page number of the first free page for a 
     list of a specific size 
     
     FreePages counts the number of free pages in a list.  A free page
     is a page without any live objects on it.  Pages are moved here from
     ReclaimAndFlip.

     TotalPages keeps track of all pages in a list 

     AllocatedPages contains pages with (possibly) live objects
     on them.  Pages are moved here by AddObjToDesc.

     BlackPages : a page is black if it has a black object on it.
     whenver an object is colored black, its page is similarly
     colored, and moved into this black list.

     BottomPage is the last page in the free page list, for fast appends.

     NOTE: It is necessary to manually sweep the Space bits because
     it is hard to tell when a page is or is not already in the 
     AllocatedPages list during object allocation time.

  *)
     
  FreePageList: ARRAY [0 .. NumFreeLists - 1] OF Page;
  FreePages   : ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  TotalFreePages : INTEGER;
  TotalPages  : ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  AllocatedPages : ARRAY [0 .. NumFreeLists - 1] OF Page;
  BlackPages : ARRAY [0 .. NumFreeLists - 1] OF Page;
  BottomPage :  ARRAY [0 .. NumFreeLists - 1] OF Page;


  (* ---------- Stats ---------- *)
  (* # of bytes left *)
  BytesFree: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  (* Elements left on the list *)
  ElemsFree: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  
  (* bytes counted live during collection *)
  BytesLive: ARRAY [0 .. NumFreeLists - 1] OF INTEGER; 
  ActiveBytes : INTEGER;
  CountedBytes  : INTEGER;
  (* # of nodes total in this list, updated when memory moves from list to 
     list *)
  FreeListElems: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;

  (* number of collections triggered *)
  TriggerList: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  
  (* number of balances triggered *)
  BalanceTriggerList : ARRAY [0 .. NumFreeLists - 1] OF INTEGER;

  (* # of allocations per list *)
  Allocations: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;

  (* # of live objects per list *)
  LiveObjects: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;

  (* Total number of allocations *)
  TotalAllocatedBytes : INTEGER;

  (* list of total storage capacities for each list, updated when blocks
     are moved from list to list *)
  ListCaps: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;

  (*---------- Collection stats ---------------*)
  (* number of collections since last gc reset *)
  NumCollections: INTEGER;
  FreeBytesAtCollection : INTEGER; (* send via autoCnt *)
  FreePagesAtCollection : INTEGER; (* send via reqCnt *)


VAR 
  (* This essentially tacks on fwd/back ptrs to the desc array.  For
     cache locality, this should be put into the desc array *)
  pagelinks : UNTRACED REF ARRAY OF PageLink;

  (* keep track of its size *)
  descsize : INTEGER;

  (* amount to grow by, should be about half the original heap size *)
  descgrow : INTEGER;

  (* to support remapping, we need to know how many pages there are, so 
     we know when to grow the desc array *)
  descpages : INTEGER;
 


(* ------------------- Timers ------------------ *)
VAR
  BalanceTimer : Spy.T;         (* cost of balancing *)
  MovePageTimer:Spy.T;          (* movepage (part of balancing )*)
  TotalAllocationTimer : Spy.T; (* total allocation time *)
  AllocTimer : Spy.T;           (* time for the allocation, which will
                                   get moved to balance or gcalloc timer
                                   as needed *)
  BalanceAllocTimer : Spy.T;
  GCAllocTimer : Spy.T;
  IncGCAllocTimer : Spy.T;
  SmallObjTimer : Spy.T;        (* time to get subpage obj *)
  BigObjTimer : Spy.T;          (* time to get reservepool obj *)
  PageUpdateTimer:Spy.T;        (* during an allocation *)
  ReturnMemTimer : Spy.T;       (* time to return and coalesce one object 
                                   to the reservepool*)
  RecycleTimer : Spy.T;         (* Total time to recycle garbage *)
  CollectionTimer : Spy.T;      (* total collection time *)
  StackScanTimer : Spy.T;
  StrongRefTimer : Spy.T;
  GlobalTimer : Spy.T;
  ReclaimTimer : Spy.T;         (* reclaim and flip *)
  MarkTimer : Spy.T;            (* time to mark through *)
  (*
  MTMG : Spy.T;                 (* make gray *)
  MTWalkRef : Spy.T;            (* scanning an obj *) (* sub MTMG *)
  MTFL : Spy.T;                 (* Markthrough on free lists *)
  MTRP : Spy.T;                 (* Markthrough on res pool *)
  *)
  MPFR : Spy.T;                 (* MovePageFromReserve *)
  AugResTimer : Spy.T;          (* Augment reserve *)
  WBTimer : Spy.T;              (* Write barrier *)

  CollectStartTimer : Spy.T;    
  CollectSomeTimer : Spy.T;
  CollectFinishTimer : Spy.T;   
  CollectRescanTimer : Spy.T;

CONST
  (* XXX for treadmill, cannot turn this off, as some places assume
     page alignment.
  *)
  AllocateAligned = TRUE;             (* all allocations are page aligned *)
  Stats = TRUE; 
  NodeSizes   = ARRAY [0 .. NumFreeLists - 1] OF INTEGER {32,64,128,256,512,1024,2048,4096,8192,0};
  


(* ----------------------------------------*)

(* Most of the sanity checking code breaks when using implicit
   collection (because object status bits are not valid unless the
   object is allocated, and some statistics can no longer be easily
   computed).  

   All sanity checking does work if the collector is compiled with
   the Implicit flag set to FALSE.
*)

VAR
  FLSC            := FALSE;      (* The master flag.  If this is off, 
                                    all the heavyweight sanity checking
                                    is off *)
  PLSC            := TRUE;       (* Page List Sanity Checking *)
  DescSanity      := FALSE;       (* check page desc array *)
  TMSanity        := TRUE;       (* check treadmill structures *)
  RPSC            := FALSE;       (* reserve pool sanity checking *)


VAR
  (* marker object for RTHeapMap.Walkglobals to use *)
  theMarker: Marker := NIL;

PROCEDURE PutCR () =
  BEGIN
    PutText("\n");
  END PutCR;

PROCEDURE PutBool (tf : BOOLEAN) =
  BEGIN
    IF tf THEN
      PutText("TRUE");
    ELSE
      PutText("FALSE");
    END;
  END PutBool;

(* ------------------------------- Accessors -----*)

PROCEDURE InstallSanity (tf : BOOLEAN)=
  BEGIN
    SetFLSC(tf);
    SetPLSC(tf);
    SetRPSC(tf);
    SetDesc(tf);
    SetTMSanity(tf);
  END InstallSanity;

<*UNUSED*>
PROCEDURE SetBalance (tf: BOOLEAN) =
  BEGIN
    Balance := tf;
  END SetBalance;
<*UNUSED*>
PROCEDURE SetMyverbose (i :INTEGER) = 
  BEGIN
    TMVerbose := i;
  END SetMyverbose;


PROCEDURE SetFLSC (tf : BOOLEAN) = 
  BEGIN
    FLSC := tf;
  END SetFLSC;
PROCEDURE SetPLSC (tf : BOOLEAN) = 
  BEGIN
    PLSC := tf;
  END SetPLSC;
PROCEDURE SetRPSC (tf : BOOLEAN) = 
  BEGIN
    RPSC := tf;
  END SetRPSC;
PROCEDURE SetTMSanity (tf : BOOLEAN) = 
  BEGIN
    TMSanity := tf;
  END SetTMSanity;
PROCEDURE SetDesc (tf : BOOLEAN) = 
  BEGIN
    DescSanity := tf;
  END SetDesc;


FUNCTIONAL PROCEDURE GetPage (a : ADDRESS) : INTEGER = 
  BEGIN
    RETURN Word.RightShift(LOOPHOLE (a, INTEGER),LogBytesPerPage);
  END GetPage;

(* If only we could inline... *)
FUNCTIONAL PROCEDURE PtrToDesc (p : INTEGER) : RefDescriptor =
  BEGIN
    IF p = 0 THEN RETURN NIL; END;
    RETURN LOOPHOLE (Word.Or(p, DescMask)  , RefDescriptor) ;
  END PtrToDesc;

FUNCTIONAL PROCEDURE DescToPtr (d : RefDescriptor) : Ptr =
  BEGIN
    RETURN Word.And(LOOPHOLE(d, Word.T), PtrMask);
  END DescToPtr; 

(* --------------------------------------------- Initialization ------ *)

(* Initialization proceeds as follows :
   - Allocate global arrays
   - compute sizes, GrowHeap
   - compute list sizes (InitListSizes)
   - generate segregated freelists (GenerateSubPageLists)
   - init reservepool
   - init stats
   - init page descriptor array (InitPages)

   - allocate and activate timers (InitTimers)
   - other crud
   - RTHeapVM
*)


PROCEDURE Init () =
  BEGIN
    InitHeap();
    InitTimers();
    PutText("Timers allocated\n");Flush();
    RTAllocator.Init();
    PutText("Allocator initialized\n");Flush();
    RTHeapTrace.Init();
    PutText("RTHeapTrace initialized\n");Flush();
    RTHeapDebug.Init();
    PutText("RTHeapDebug initialized\n");Flush();
    RTHeapPages.Init();
    PutText("RTHeapPages initialized\n");Flush();
    RTutils.Init();
    PutText("RTutils initialized\n");Flush();
    RTStrongRef.Init();
    PutText("Strongrefs initialized\n");Flush();
    IF Remapping THEN
      RTHeapVM.Init();
    END;
    theMarker := NEW(Marker);
    PutText("GC >> Traced heap initialized\n");
    Flush();
    Spy.Init();
    Spy.Reset();
    (*RTHeapDep.ProfileOn();*) 
  END Init;

PROCEDURE InitHeap () =
  (* Called as the first things from RTMain.Run, so this procedure cannot
     depend on anything that cannot be statically initialized by the C
     compiler.  This procedure initializes the heap.  After this procedure
     returns, NEW can be called.  Note, though, that the garbage collector
     may not be in place until later during the initializations.  Thus, if
     there is not enough memory for the initializations, the garbage
     collector might not be invoked.  That is very reasonable, however,
     since if the initialization cannot be carried to completion, then it
     is not likely that there would be enough space for anything else
     either. *)
  VAR
    listSizes: INTEGER;
    currAddr : ADDRESS;
  BEGIN
    PutText("COLLECTOR SETUP - \n");
    PutText("\nImplicit : ");
    PutBool(Implicit);
    PutText("\nIncremental : ");
    PutBool(Incremental);
    PutText("\nRemapping : ");
    PutBool(Remapping);
    PutText("\nWRITEBARRIER : ");
    PutBool(RTRefCount.WRITEBARRIER);
    PutText("\nREPORT FRAG : ");
    PutBool(RTHeapStats.ReportFragmentation);
    PutText("\nFL Sanity : ");
    PutBool(FLSC);
    IF FLSC AND Implicit THEN
      PutText("WARNING - Implicit collection and sanity checking do NOT WORK TOGETHER\n");
      RTOS.Crash();
    END;
    PutCR();
    AllocateArrays();
    verbose := 0;
    TMVerbose := 0;
    NumCollections := 0;
    FOR i := 0 TO NumFreeLists - 1 DO 
      Free[i] := NIL; 
      Scan[i] := NIL; (* no collections in progress *)
      Top[i] := NIL; (* nothing allocated yet *)
      Bottom[i] := NIL;
    END;


    listSizes := StartSize;

    (* figure out what sizes have been hardcoded*)
    (* list sizes are assumed to be powers of 2 *)
    (* things are made faster by making NodeSizes CONST*)
    FOR i := 0 TO NumFreeLists - 1 DO
      listSizes := listSizes * 2;
    END;
   
    (* this will sort of ensure that i can use TRACED_END as where to
       start remapping pages because there is no left over memory
       XXX This assumes tracedend is/was page aligned !
    *)

    InitialBytes :=  (RTMem.GetMaxGCSize() DIV 
                      BytesPerPage-1) * BytesPerPage;

    (* XXX for same as copying *)
    InitialBytes := 20480000; 
    VAR
      str := RTParams.RawValue("S");
      ptr : UNTRACED REF CHAR := str;
      n := 0;
      c: CHAR;
    BEGIN
      RTIO.PutText("#");
      IF ptr # NIL THEN
        LOOP
          c := ptr^;
          RTIO.PutChar(c); RTIO.PutChar(':');
          IF c < '0' OR c > '9' THEN EXIT; END;
          n := n * 10 + ORD(c) - ORD('0');
          INC(ptr, BYTESIZE(CHAR));
        END;
        InitialBytes := n * BytesPerPage;
      END;
    END;
    
    (* do initial heap grow *)
    GrowHeap(0);

    PutText("Initial bytes : ");
    PutInt(InitialBytes);
    PutCR();

    IF MEMORY_End - MEMORY_Start < NumFreeLists * MinMemoryNodeSize THEN
      RTMisc.FatalError(NIL, 0, "gc: heap too small at initialization");
    END;

    (* compute a list size for each list by divving up the memory given.
       then iterate generating the free lists *)
    listSizes :=
      (MEMORY_End - MEMORY_Start) * (1024 DIV NumFreeLists) DIV 1024;
    <* ASSERT listSizes MOD AddrPerAlign = 0 *>

    InitialSizes := listSizes;
    currAddr := MEMORY_Start;

    (* If ReservePct is set, reserve that percent of memory for the reserve 
       pool *)

    IF ReservePct > 0 THEN
      ReserveSize := ((MEMORY_End-MEMORY_Start) * ReservePct) DIV 100;
    END;

    IF ReserveSize > 0 THEN
      listSizes := MEMORY_End - MEMORY_Start;
      DEC(listSizes, ReserveSize);
    ELSIF HalfReserve THEN
      (* default - just take up half the memory *)
      listSizes := (MEMORY_End - MEMORY_Start) DIV 2;
    END;
    (* align the other lists *)
    listSizes := listSizes * (1024 DIV NumFreeLists - 1) DIV 1024;

    PutText("GC >> Reserve pool size = ");
    PutInt(ReserveSize);    PutCR();
    Flush();

    InitListSizes(listSizes);

    (* ---------- Generate sub page freelists -------- *)
    (* this updates currAddr to end of sub page freelists *)
    GenerateSubPageLists(currAddr);
    
    (* ----------- Init reserve pool ------------- *)
    InitReservePool(currAddr);

    (* end marker *)
    FreeListLocations[NumFreeLists] := MEMORY_End;

    InitRandomStats();
    ActiveBytes := 0;
    PutText("Free list locations : list/address/page#\n");
    FOR i := 0 TO NumFreeLists DO
      PutInt(i);
      PutText("/");
      PutAddr(FreeListLocations[i]);
      PutText("/");
      PutAddr(LOOPHOLE(GetPage(FreeListLocations[i]), ADDRESS));
      PutCR();
    END;

    (* --------- page desc init--------- *)
    InitPages();

    PutText("Pages initialized\n");
    PageListSanityCheck("Init heap pages");

    (* initial meanings for the colors *)
    WhiteStatus := GcStatus.NotMarked;
    BlackStatus := GcStatus.RecursivelyMarked;
    FOR i:=0 TO RP DO
      BytesFree [i] := CountFreeBytes(i);
      ListCaps[i] := BytesFree[i];
    END;
    COLLECTING := 0;


    (* initialize TotalElems *)
    TotalElems := 0;
    TotalFreePages := 0;
    FOR i:=0 TO RP-1 DO
      INC(TotalElems, CountElems(i));
      INC(TotalFreePages, FreePages[i]);
    END;

    FOR i:=0 TO RP DO
      INC(TotalFreePages, FreePages[i]);
    END;
    TotalFreePages := CountFreePages();
    (*
    FreeListSanityCheck("Initheap");    
    *)
    InitPolicy();
    PutText("Incremental policy initialized\n");
    PutText("tcfree = "); PutInt(TcFree); PutCR();
      RTHeapStats.InitSampleArray(InitialBytes);
  END InitHeap;


PROCEDURE ResetStats() =
  BEGIN
    NumCollections := 0; (* avoid div by zero *)
    FOR i:= 0 TO NumFreeLists-1 DO
      TriggerList[i] := 0;
      BalanceTriggerList[i] := 0;
      Allocations[i] := 0;
    END;

    TotalAllocatedBytes := 0;
    RT0u.total_traced_bytes := 0;
    FreeBytesAtCollection := 0;
    FreePagesAtCollection := 0;

    IF RTHeapStats.ReportFragmentation THEN
      RTHeapStats.ResetSamples();
      (* force a valid first sample *)
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes);
    END;

    RTHisto.ResetHistogram();

  END ResetStats;

PROCEDURE InitTimers () =
  BEGIN
    IF DoTimings THEN

      (*pause_timer := Spy.Create("GC: Pause", TRUE);*)
      TotalAllocationTimer := Spy.Create("Alloc-GetMem", TRUE);
      AllocTimer := Spy.Create("GetMem-Normal", TRUE);
      BalanceAllocTimer := Spy.Create("GetMem-Balanced", TRUE);
      GCAllocTimer := Spy.Create("GetMem-GC", TRUE);
      IncGCAllocTimer := Spy.Create("GetMem-INC-GC", TRUE);

      SmallObjTimer := Spy.Create("GetMem-Small", TRUE);
      BigObjTimer := Spy.Create("GetMem-Big", TRUE);
      PageUpdateTimer := Spy.Create("GetMem-PageUpdate", TRUE);

      BalanceTimer := Spy.Create("BA-Balance", TRUE);
      MovePageTimer := Spy.Create("BA-MovePage", TRUE);
      MPFR := Spy.Create("BA-FromReserve", TRUE);
      AugResTimer := Spy.Create("BA-ToReserve", TRUE);
      CollectionTimer := Spy.Create("GC-Collection", TRUE);

      CollectStartTimer := Spy.Create("GC-START", TRUE);
      CollectSomeTimer := Spy.Create("GC-SOME", TRUE);
      CollectFinishTimer := Spy.Create("GC-FINISH", TRUE);
      CollectRescanTimer := Spy.Create("GC-RESCAN", TRUE);
      StackScanTimer := Spy.Create("GC-ScanStacks", TRUE);
      StrongRefTimer := Spy.Create("GC-StrongRef", TRUE);
      GlobalTimer := Spy.Create("GC-Globals", TRUE);
      MarkTimer := Spy.Create("GC-MarkThrough", TRUE);
      ReclaimTimer := Spy.Create("GC-ReclaimAndFlip", TRUE);
      ReturnMemTimer := Spy.Create("GC-ReturnToReserve", TRUE);
      RecycleTimer := Spy.Create("GC-Free", TRUE);
      WBTimer := Spy.Create("GC-WriteBarrier", TRUE);

      (* These really slow things down.
      MTMG := Spy.Create("MT: MakeGray", TRUE);
      MTWalkRef := Spy.Create("MT: WalkRef", TRUE);
      MTFL := Spy.Create("MT: Freelists", TRUE);
      MTRP := Spy.Create("MT: ResPool", TRUE);
      *)
      (*Spy.Activate (pause_timer, TRUE);*)
      Spy.Activate (TotalAllocationTimer, TRUE);
      Spy.Activate (AllocTimer, TRUE);
      Spy.Activate (BalanceAllocTimer, TRUE);
      Spy.Activate (GCAllocTimer, TRUE);
      Spy.Activate (IncGCAllocTimer, TRUE);
      Spy.Activate (SmallObjTimer, TRUE);
      Spy.Activate (BigObjTimer, TRUE);
      Spy.Activate (PageUpdateTimer, TRUE);
      Spy.Activate (BalanceTimer, TRUE);
      Spy.Activate (MovePageTimer, TRUE);
      Spy.Activate (CollectionTimer, TRUE);
      Spy.Activate (StackScanTimer, TRUE);
      Spy.Activate (StrongRefTimer, TRUE);
      Spy.Activate (GlobalTimer, TRUE);
      Spy.Activate (MarkTimer, TRUE);
      Spy.Activate (ReclaimTimer, TRUE);
      Spy.Activate (RecycleTimer, TRUE);
      Spy.Activate (ReturnMemTimer, TRUE);
      (*
      Spy.Activate (MTMG, TRUE);
      Spy.Activate (MTWalkRef, TRUE);
      Spy.Activate (MTFL, TRUE);
      Spy.Activate (MTRP, TRUE);
      *)
      Spy.Activate (MPFR, TRUE);
      Spy.Activate (WBTimer, TRUE);
      Spy.Activate (AugResTimer, TRUE);
      Spy.Activate (CollectStartTimer,TRUE);
      Spy.Activate (CollectSomeTimer ,TRUE);
      Spy.Activate (CollectFinishTimer,TRUE);
      Spy.Activate (CollectRescanTimer,TRUE);


      MFS:= Spy.Create("MarkFromStacks",TRUE);
      Spy.Activate(MFS,TRUE);

    END;

  END InitTimers;


(* Keep everything untraced to ensure they are not scanned *)
PROCEDURE AllocateArrays() =
  BEGIN
    Free := NEW(UNTRACED REF ARRAY OF RefDescriptor, NumFreeLists);
    Bottom := NEW(UNTRACED REF ARRAY OF RefDescriptor, NumFreeLists);
    Top := NEW(UNTRACED REF ARRAY OF RefDescriptor, NumFreeLists);
    Scan := NEW(UNTRACED REF ARRAY OF RefDescriptor, NumFreeLists);
    FreeListLocations := NEW(UNTRACED REF ARRAY OF ADDRESS, NumFreeLists+1);
  END AllocateArrays;


PROCEDURE InitListSizes (listSizes : INTEGER) =
  VAR
    pages : INTEGER;
    remainder: INTEGER;
  BEGIN
    FOR i := 0 TO NumFreeLists - 2 DO
      FreeListElems[i] := listSizes DIV NodeSizes[i] ;
      PutText ("List/elems "); PutInt(i); PutText("/");
      PutInt(FreeListElems[i]); Flush();
      IF AllocateAligned THEN
        (* align to pages *)
        pages :=  FreeListElems[i] * NodeSizes[i] DIV BytesPerPage;
        remainder := FreeListElems[i] * NodeSizes[i] MOD BytesPerPage;
        (* here we assume that the listsizes are only powers of two,
           we are guaranteed to find a good # of pages to satisfy alignment
        *)
        WHILE remainder # 0 DO
          INC(pages);
          FreeListElems[i] := pages * BytesPerPage DIV NodeSizes[i];
          remainder  := FreeListElems[i] * NodeSizes[i] MOD BytesPerPage
        END;
        PutText("  ");PutInt(pages);   PutText(" pages ");
      END;
      PutText(" Nodesize = ");
      PutInt(NodeSizes[i]);PutCR();Flush();
      ElemsPerPage[i] := BytesPerPage DIV NodeSizes[i];
    END;

  END InitListSizes;


PROCEDURE GenerateSubPageLists (VAR currAddr : ADDRESS) =
  VAR
    last : RefDescriptor;
  BEGIN
    
    FOR i := 0 TO NumFreeLists - 2 DO
      (* initialize list head *)
      Free[i] := currAddr;
      BytesFree[i] := FreeListElems[i] * NodeSizes[i];
      ElemsFree[i] := FreeListElems[i];
      (* now generate the free list *)
      last := NIL;
      FOR elem := 1 TO FreeListElems[i] DO
        WITH addr = LOOPHOLE(currAddr, RefDescriptor) DO
          addr.header.size := NodeSizes[i];
          addr.header.typecode := TcFree;
          addr.header.gcStatus := GcStatus.Untraced;
          addr.nextFree := addr + NodeSizes[i];
          addr.prevFree := last;
          addr.ptrs.next := DescToPtr(addr.nextFree);
          addr.ptrs.prev := DescToPtr(addr.prevFree);
          last := addr;
          currAddr := currAddr + NodeSizes[i];
        END;
      END;
      (* terminate the list *)
      last.nextFree := Free[i];
      last.ptrs.next := DescToPtr(Free[i]);
      Bottom[i] := last; (* for fast append at end of collection *)
      Top[i] := last; (* nothing allocated yet *)
      Scan[i] := last; (* nothing scanned/toscan *)
      
      (* fix up the first ptr *)
      Free[i].ptrs.prev := DescToPtr(last);
      
      (* ASSERT LOOPHOLE(currAddr, INTEGER) MOD BytesPerPage = 0 *)
      IF AllocateAligned THEN
        IF LOOPHOLE(currAddr, INTEGER) MOD BytesPerPage # 0 THEN
          PutText("Init : alignment fault!\n");
          Flush();
          RTOS.Crash();
        END;
      END;
    END;

  END GenerateSubPageLists;

PROCEDURE InitReservePool(currAddr:ADDRESS) = 
  VAR
  BEGIN
    Free[RP] := currAddr;
    (* because the last pool is going to be a reserve pool, allocations out
       of it will use the old code to perform coalescing etc *)
    WITH h = LOOPHOLE(Free[RP], RefDescriptor) DO
      (* swipe the remaining memory *)        
      h.header.size := MEMORY_End - currAddr;
      <* ASSERT h.header.size >= ReserveSize *>
      h.header.typecode := TcFree;
      h.nextFree := NIL;
      h.prevFree := NIL;
      h.ptrs.next := 0;
      h.ptrs.prev := 0;
      h.header.gcStatus := GcStatus.Untraced;
      (* NodeSizes[RP] := h.header.size;*)
      BytesFree[RP] := h.header.size;
      FreeListElems[RP] := 1;
    END;
    Bottom[RP] := NIL; (* black *)
    Top[RP] := NIL;(* ecru *)
    Scan[RP] := NIL; (* gray *)

  END InitReservePool;

PROCEDURE InitPages () =
  VAR
  BEGIN
    (* initialize the descriptors for the reserve pool *)
    FOR p := GetPage(FreeListLocations[RP])+1-p0 TO LAST(desc^) DO
      desc[p].continued := TRUE;
      (*desc[p].space := Space.Free;*)(* initialized in GrowHeap *)
    END;

    pagelinks := NEW(UNTRACED REF ARRAY OF PageLink, descsize);

    FOR i := 0 TO NumFreeLists-1 DO 
      (* for each list, just dump the pages into the appropriate list *)
      FreePageList[i] := GetPage(FreeListLocations[i]);
      FreePages[i] := BytesFree[i] DIV BytesPerPage;
      TotalPages[i] :=       FreePages[i];
      FOR p := 0 TO FreePages[i]-1 DO
        pagelinks [FreePageList[i] + p - p0].next := FreePageList[i]+p+1;
        pagelinks [FreePageList[i] + p - p0].prev := FreePageList[i]+p-1;
      END;
      (* set end points *)
      pagelinks[FreePageList[i]-p0].prev := 0;
      pagelinks[FreePageList[i] + FreePages[i] - 1 - p0].next := 0;
      
      BlackPages[i] := 0; (* no collection in progress *)
      AllocatedPages[i] := 0; (* nothing allocated yet *)
      BottomPage[i] := FreePageList[i] + FreePages[i] -1;
    END;

  END InitPages;

PROCEDURE InitRandomStats () =
  BEGIN
    FOR i := 0 TO NumFreeLists - 1 DO
      FreeListLocations[i] := Free[i];

      Allocations[i] := 0;
      LiveObjects[i] := 0;

      TriggerList[i]:=0;
      BalanceTriggerList[i]:=0;
      ListCaps[i] := BytesFree[i];

      IF verbose = 1 THEN
        PutText("nodeSize: ");
        PutInt(NodeSizes[i]);
        PutText("   elements : ");
        PutInt(FreeListElems[i]);
        PutText("  startaddr : ");
        PutAddr(Free[i]);
        PutText("\n");
      END;
    END;
    NumAllocations :=0;
    BytesMarked := 0;
    BytesAlloced := 0;
    NumResAllocations := 0;
    ResBytesAlloced := 0;
    TotalFreeBytes := (MEMORY_End - MEMORY_Start);

    PutCR();
    Finish := FALSE;
    IF MeasureMarking THEN
      InitMMG();
    END;

    StackBytes := 0;
  END InitRandomStats;



(* GrowHeap adds a block of at least "minNewPages" free pages to the heap,
   and links it into the free list.  For now, we only support growing the
   heap one time, at initialization. *)


VAR InitialBytes: INTEGER;

CONST
  MinNewBytes  = 262144;         (* grow the heap by at least 256K *)
  MinNewFactor = 20;             (* grow the heap by at least 20% *)

PROCEDURE GrowHeap (pp: INTEGER) =
  VAR newChunk: ADDRESS;
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
      PutText("GC >> heap: ");
      PutAddr(MEMORY_Start);
      PutText(" ");
      PutAddr(MEMORY_End);
      PutText("\n");
      PutText("GC >> minimum size: ");
      PutInt(MinMemoryNodeSize);
      PutText("\n");
      PutText("GC >> Ptr/Header/Descriptor sizes : ");
      PutInt(PtrSize);
      PutText("/");
      PutInt(HeaderSize);
      PutText("/");
      PutInt(DescriptorSize);
      PutCR();
      Flush();
    END;

    (* If we are supporting page alignment, set up p0, p1 (boundaries
       the traced heap), initialize the page descriptor array *)

    <* ASSERT LOOPHOLE(MEMORY_Start, INTEGER) MOD BytesPerPage = 0 *>
    <* ASSERT LOOPHOLE(MEMORY_End, INTEGER) MOD BytesPerPage = 0 *>
    p0 := GetPage(MEMORY_Start);
    p1 := GetPage(MEMORY_End);
    (* XXX HACK HACK HACK
       this allows the size of the heap to be set manually via InitialBytes
    *)
    p1 := GetPage(RTMem.GetTracedEnd());

    (* the array we allocate grows by descgrow whenever more remapped. *)
    (* Space.Unallocated means that the desc entry is not used yet, or 
       the page that was there has been unmapped *)
    descsize := p1-p0;
    descpages := p1-p0;
    descgrow := descsize;
    INC(descsize , descgrow);
    desc := NEW(UNTRACED REF ARRAY OF Desc, descsize);
    FOR i := 0 TO GetPage(MEMORY_End)-p0-1 DO
      desc[i] := Desc{space := Space.Free, 
                      generation := Generation.Younger,
                      pure := FALSE,
                      continued := FALSE,
                      protected := FALSE,
                      note := Note.Allocated,
                      gray := FALSE
                      };
      
      INC(freePages);
    END;
    (* initialize the other unused entries *)
    FOR i := GetPage(MEMORY_End)-p0 TO descsize-1 DO
      desc[i] := Desc{space := Space.Unallocated, 
                      generation := Generation.Younger,
                      pure := FALSE,
                      continued := FALSE,
                      protected := FALSE,
                      note := Note.Allocated,
                      gray := FALSE
                      };
      
    END;

    <* ASSERT activePages = 0 *>

  END GrowHeap;

(* grows the desc array *)
(* if pages are added, someone else must move p1 and MEMORY_End *)
PROCEDURE GrowDesc () =
  VAR
    newdesc : UNTRACED REF ARRAY OF Desc;
    newpl : UNTRACED REF ARRAY OF PageLink;
  BEGIN
    (* alloc new array, copy the old info *)
    descsize := descsize + descgrow;
    newdesc := NEW(UNTRACED REF ARRAY OF Desc, descsize);
    FOR i := 0 TO LAST(desc^) DO
      newdesc[i] := desc[i];
    END;

    (* init the new entries *)
    FOR i := NUMBER(desc^) TO LAST(newdesc^) DO
      newdesc[i] := Desc{space := Space.Unallocated, 
                               generation := Generation.Younger,
                               pure := FALSE,
                               continued := FALSE,
                               protected := FALSE,
                               note := Note.Allocated,
                               gray := FALSE
                           };
    END;
    DISPOSE(desc);
    desc := newdesc;

    (* do the same for the pagelinks array *)
    newpl := NEW (UNTRACED REF ARRAY OF PageLink, descsize);
    FOR i := 0 TO LAST(pagelinks^) DO
      newpl[i] := pagelinks[i];
    END;
    (* don't care about unused pagelinks...yet *)
    DISPOSE (pagelinks);
    pagelinks := newpl;
  END GrowDesc;


(* --------------------------------------------- Allocation  ------ *)

(* Malloc returns the address of "size" bytes of untraced, zeroed
   storage *)

PROCEDURE Malloc (size: INTEGER): ADDRESS =
  VAR res: ADDRESS;
  BEGIN
    RTOS.LockHeap();
    BEGIN
      res := Cstdlib.malloc(size);
      IF res = NIL THEN
        RTMisc.FatalError(
          NIL, 0, "malloc failed, unable to get more memory");
      END;
    END;
    RTOS.UnlockHeap();
    RTMisc.Zero(res, size);
    RETURN res;
  END Malloc;


PROCEDURE TMAlloc (dataSize : CARDINAL;
                   tc       : Typecode) : ADDRESS=
  VAR
    res : RefDescriptor;
    tries:INTEGER; (* number of tries *)
    list : INTEGER;
    (* this only works because memory is already broken up into 
       pieces that will always satisfy alignment requirements *)
    (*adrSizeRequested := MAX(dataSize + Offset, MinMemoryNodeSize);*)
    adrSizeRequested := dataSize + Offset;
    NotYetCollected := TRUE;    (* ensure we only collect once per allocation *)
  BEGIN
    IF CheckDuringAlloc THEN
      (* this substantially slows down the system *)
      FreeListSanityCheck("before GetMem");
    END;

    Spy.Enter(TotalAllocationTimer); 
    Spy.Enter(AllocTimer);


    list := GetList(adrSizeRequested); 
    (* first attempt *)
    res := GetMem(adrSizeRequested, list, tc);
    IF res = NIL THEN
      IF traceOn THEN 
        GCEnter(); 
        GCDone();
      END;
      (* try a balance, but if we fail, do a collection *)
      IF Balance AND (list # RP OR Remapping) THEN
        Spy.Move(AllocTimer, BalanceAllocTimer);

        IF Stats THEN
          INC(BalanceTriggerList[list]);
        END;
        IF NOT BalanceLists(list, adrSizeRequested) THEN
          (* Balancing failed *)
          IF Stats THEN
            (* charge this trigger to GC *)
            DEC(BalanceTriggerList[list]); 
            INC(TriggerList[list]);
          END;

          (* failed to move any memory there, so do a collection *)
          (* this is pretty catastrophic *)
          Spy.Move(BalanceAllocTimer,GCAllocTimer);
          (* do a full collection, or finish one in progress.  Note
             that we only really need to call CollectInternal, but in
             order to have useful verbose output, we need to
             distinguish if we're catastrophically calling
             CollectInternal, which means that we completely failed to
             start a collection soon enough *)
          CollectInternal();
          NotYetCollected := FALSE;
        END;
      ELSE
        (* not doing a balance, or remapping, just collect *)
        IF Stats THEN
          INC(TriggerList[list]);
        END;
        Spy.Move(AllocTimer,GCAllocTimer);

        CollectInternal();
        NotYetCollected := FALSE;  
      END;
      (* we have either done a collection or a balance.  try again  *)
      res := GetMem(adrSizeRequested,list,tc);
      IF res = NIL THEN
        (* try a balance! *)
        EVAL BalanceLists(list, adrSizeRequested);
          
        (* now go up the lists until we get the memory we need *)
        tries := 0;
        WHILE res = NIL AND tries+list < RP DO
          IF verbose >= 1 THEN
            PutText("<<<Retrying tries/size ");
            PutInt(tries); PutText("/"); PutInt(dataSize);
            PutCR();Flush(); 
          END;
          res := GetMem(adrSizeRequested, list+tries,tc);
          INC(tries);
        END;

        (* desperation move if we're fucked *)
        IF res = NIL AND list < RP THEN 
          MovePageFromReserve(list);
          res := GetMem(adrSizeRequested,list, tc);
        END;
        (* this check is moved here so that it isn't exercised on
           the common case path *)
        IF res = NIL THEN
          PutText("********* Failed to satisfy request for ");
          PutInt(dataSize);  PutText(" (aligned "); PutInt(adrSizeRequested);
          PutText(") bytes\n"); Flush();
          
          (* print the biggest hogs in the heap *)
          PutText("\nGC >>> allocation by bytes\n");
          RTutils.Heap(TRUE, RTutils.HeapPresentation.ByByteCount, 
                       10, NIL, TRUE);

          (* dump the allocation per site *)
          RTHeapTrace.Dump();

          RTMisc.FatalError(NIL, 0, "gc: out of memory");
        END;
      END;
      IF traceOn THEN GCExit(); END;
    END;

    Spy.Enter(IncGCAllocTimer);
    IF Incremental THEN

      INC(BytesAlloced,res.header.size);  
      (* buffer zone is modified by GetMem and GetMemFromReserve *)

      IF COLLECTING # 0 THEN
        (* XXX inlined Behind() *)
        IF NotYetCollected AND BytesMarked  < Word.LeftShift(BytesAlloced, RatioPower) THEN
          IF Finish THEN
            (* uh oh...better make sure the thing just allocated 
               gets put on the black list!! *)
            (* however, if we allocated out of the reserve pool, then we
               need to extract the thing out from the Top list.  *)
            IF list = RP THEN
              VAR
                prev  : RefDescriptor;
              BEGIN
                IF res = Top[list] THEN
                  (* empty, advance TOP *)
                  Top[list] := PtrToDesc(Top[list].ptrs.next);
                  IF Top[list] # NIL THEN
                    Top[list].ptrs.prev := 0;
                  END;
                ELSE
                  (* prev guaranteed non nil since res # top *)
                  prev := PtrToDesc(res.ptrs.prev);
                  prev.ptrs.next := res.ptrs.next;
                  IF res.ptrs.next # 0 THEN
                    PtrToDesc(res.ptrs.next).ptrs.prev := res.ptrs.prev;
                  END;
                END;
              END;
            END;
            MakeBlack(res);
            res.header.gcStatus := BlackStatus;
            INC(BytesLive[list], res.header.size);
            INC(LiveObjects[list]);

            CollectFinish();
          ELSE
            IF CollectSome() (*AND TotalFreePages < 10*) THEN
              Finish := TRUE;
            END;
          END;
        END;

      ELSE
        IF BufferZone > BytesPerPage AND TotalFreePages <= Word.RightShift(BufferZone, LogBytesPerPage) THEN
          (*IF TriggerCollection() THEN*)
          CollectStart();

        END;
      END;
    END;
    Spy.Exit(IncGCAllocTimer);

    Spy.Exit(AllocTimer);
    Spy.Exit(TotalAllocationTimer); 

    IF RTHeapStats.ReportFragmentation AND 
      RTHeapStats.FragOn AND 
      RTHeapStats.SampleAlloc(dataSize)
     THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes);
    END;

    IF RTHeapStats.ReportFragmentation THEN
      INC(ActiveBytes, dataSize);
    END;
    (* hardclock();*)
    RETURN res+Offset;
  END TMAlloc;

PROCEDURE AllocForNew (<*UNUSED*> dataSize:CARDINAL;
           <*UNUSED*> alignment: CARDINAL): ADDRESS =
  VAR
  BEGIN
    RTOS.Crash();
    RETURN NIL;
  END AllocForNew;


(* figure out which freelist something lives on *)
(* return list's index *)
(* XXX really need to do something faster, but with just 10 lists it is
   probably not worth it *)
(* XXX  compiler support - statically determine which list to use *)
FUNCTIONAL PROCEDURE GetList (size: CARDINAL): INTEGER =
  BEGIN
    (* XXX unrolled loop manually *)
    IF size <= NodeSizes[0] THEN RETURN 0 END;
    IF size <= NodeSizes[1] THEN RETURN 1 END;
    IF size <= NodeSizes[2] THEN RETURN 2 END;
    IF size <= NodeSizes[3] THEN RETURN 3 END;
    IF size <= NodeSizes[4] THEN RETURN 4 END;
    IF size <= NodeSizes[5] THEN RETURN 5 END;
    IF size <= NodeSizes[6] THEN RETURN 6 END;
    IF size <= NodeSizes[7] THEN RETURN 7 END;
    IF size <= NodeSizes[8] THEN RETURN 8 END;
    RETURN 9;

  END GetList;


(* Attempts to get an object that satisfies requested size. Returns
   NIL on failure. ASSUMES adrSizeRequested already takes into account
   the alignment and header requirements for the allocation request *)
(* ASSUMES the heap was locked with LockHeap *) 
(* This procedure is called only by AllocForNew. *)

PROCEDURE GetMem (adrSizeRequested: INTEGER;
                  list            : INTEGER;
  (*tries           : INTEGER:=0;*)
                  tc              : Typecode:=TcFree  ) : ADDRESS =
  VAR
    d            : RefDescriptor;
    page         : Page ;
  BEGIN
    (*INC(list,tries); *)(* go to a bigger list if necessary *)

    IF list = RP THEN
      (* ------The Reserve Pool ----------*)
      (*Spy.Enter(BigObjTimer);*)
      d := GetMemFromReserve (adrSizeRequested);
      (*Spy.Exit(BigObjTimer);*)

      IF d = NIL THEN 
        RETURN NIL;
      END;

    ELSE
      (* -----------Normal free lists----------- *)
      (*Spy.Enter(SmallObjTimer);*)

      IF Free[list] = Bottom[list] THEN
        (*Spy.Exit(SmallObjTimer);*)
        RETURN NIL;
      END;

      DEC(BytesFree[list], NodeSizes[list]);
      DEC(ElemsFree[list]);

      (* this code will always allocate white *)
      (* TOP will point to the right thing, even if the ecru list was empty *)
      d := Bottom[list];
      Bottom[list] := LOOPHOLE(Word.Or(d.ptrs.prev,DescMask), RefDescriptor);
      d.header.gcStatus := WhiteStatus;

      (* update incremental stats *)
      INC(BufferZone,TriggerIncrement[list]);
      (*Spy.Exit(SmallObjTimer);*)
    END;

    IF Stats THEN
      INC(Allocations[list]);
      (*INC(LiveObjects[list]);*)
    END;
    (*d.header.gcStatus := WhiteStatus;*)
    d.header.typecode := tc;

    (* mark off the pages as being dirty or continued *)
    (* minor hack : check the descriptor here instead of incurring proc call on
       every allocation *)
    page := Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage) - p0;
    IF desc[page].space = Space.Free (*OR list = RP *)THEN
      AddObjToDesc(d, list, page);
      DEC(BytesAlloced,d.header.size); (* sub out the stuff we allocated 
                                          from the stuff added by losing 
                                          the page *)
    END;
    (*
    IF verbose > 2 THEN
      PutText("<GM");PutInt(list);
      IF verbose > 2 THEN
        PutText("/");PutAddr(d);
      END;
      PutText(">");
    END;
    *)
    RETURN d; 
  END GetMem;


(* XXX should get rid of reliance on nextFree and prevFree *)
PROCEDURE GetMemFromReserve (VAR adrSizeRequested : INTEGER) : RefDescriptor =
  VAR
    d            : RefDescriptor;
    back         : RefDescriptor := NIL; (* XXX anachronism - should remove *)
    select,selectback       : RefDescriptor; (* for choosing best fit *)
    newDescriptor: RefDescriptor; (* descriptor of the remainder we create *)
    page         : Page ;
    remainder    : INTEGER;
    tempremainder: INTEGER; 
  BEGIN

    (* search the list for a node just larger than what we need and
       then cut out the amount that we need *)
    (* if page aligning is on, round up to page boundary *)

    IF AllocateAligned THEN
      remainder :=  adrSizeRequested MOD BytesPerPage;
      INC(adrSizeRequested , BytesPerPage - remainder);
      (* ASSERT adrSizeRequested MOD BytesPerPage = 0 *)
    END;

    d := Free[RP];

    IF ResFirstFit THEN
      (* --------------- FIRST FIT ---------------- *)
      back := NIL;
      WHILE d # NIL AND d.header.size < adrSizeRequested DO
        back := d;
        d := d.nextFree;
      END;
    ELSE
      (* --------------- BEST FIT ---------------- *)
      (* search for object that gives the smallest remainder *)

      remainder := LAST(INTEGER);

      back := NIL; 
      selectback := NIL;
      select := NIL;
      (* scan entire free list *)
      WHILE d # NIL AND d < MEMORY_End DO
        tempremainder := d.header.size - adrSizeRequested;
        IF tempremainder >= 0 AND tempremainder < remainder THEN
          select := d;
          selectback := back;
          remainder := tempremainder;
        END;
        back:= d;
        d := d.nextFree;
      END;
      d := select;
      back := selectback;
    END;
    (* ----------------------------------------------- *)
    (* ASSERT d = ptr to the target, back is the thing before it 
       ( can eliminate after doubly linked lists work ) *)
    
    IF d = NIL THEN

      IF verbose > 2 THEN
        PutText("reserve pool exhausted size req ");
        PutInt(adrSizeRequested);
        PutText("  unused bytes in reserve = ");
        PutInt(BytesFree[RP]);
      END;
      RETURN NIL;
    END;

    (* create remainder if necessary *)
    remainder := d.header.size - adrSizeRequested;

    (* after this IFTHENELSE, newDescriptor is the thing that is left 
       behind (i.e. the remainder block)  *)
    IF remainder < MinMemoryNodeSize THEN
      (* ----------- SMALL OR NO REMAINDER ---------- *)
      newDescriptor := d.nextFree;
      IF newDescriptor # NIL THEN
        (* skip thing before this *)
        newDescriptor.prevFree := d.prevFree;
        newDescriptor.ptrs.prev := d.ptrs.prev;
      END;
      (* leave header size intact *)
      (* however, we're reducing the number of elements *)
      DEC(ElemsFree[RP]);

      IF Sanity AND AllocateAligned AND remainder # 0 THEN
        PutText("\nGC ERROR >>> Should never have a small remainder with page aligning on\n");
        PutText("d/d.size/adrsizereq/remainder : ");
        PutAddr(d); PutText("/");
        PutInt(d.header.size); PutText("/");
        PutInt(adrSizeRequested); PutText("/");
        PutInt(remainder); PutCR();
        Flush(); 
        FLSC := TRUE; FreeListSanityCheck("remainder fault");
        RTOS.Crash();
      END;

    ELSE
      (* ----------- BIG REMAINDER---------- *)
      newDescriptor := d + adrSizeRequested;
      newDescriptor.header.size := remainder;
      newDescriptor.header.typecode := TcFree;
      newDescriptor.nextFree := d.nextFree;
      newDescriptor.prevFree := d.prevFree;
      newDescriptor.ptrs.next := d.ptrs.next;
      newDescriptor.ptrs.prev := d.ptrs.prev;
      newDescriptor.header.gcStatus := GcStatus.Untraced;


      (* change the original header size *)
      d.header.size := adrSizeRequested;
      IF AllocateAligned THEN
        page := Word.RightShift( LOOPHOLE(newDescriptor, INTEGER), 
                                 LogBytesPerPage) - p0;
        desc[page].continued := FALSE;
        
        IF desc[page].space # Space.Free THEN
          PutText("GC ERROR >> Res pool page not free! nd/nd.size\n");
          PutAddr(newDescriptor); PutText("/");
          PutInt(newDescriptor.header.size); PutCR();

          IF desc[page].space = Space.Unallocated THEN
            PutText("UNALLOCATED\n");
          END;
          IF desc[page].space = Space.Previous THEN
            PutText("PREVIOUS\n");
          END;
          IF desc[page].space = Space.Current THEN
            PutText("CURRENT\n");
          END;

          
          page := GetPage(d) - p0;
          IF desc[page].space # Space.Free THEN
            PutText("the original object was not free either, dammit\n");
          END;

          IF NOT Implicit AND d.header.typecode # TcFree THEN
            PutText("The actual object ain't free either... \n");
          END;

          RTOS.Crash();
          Flush();
        END;
      END;
    END;
    (* now update the free list *)
    IF newDescriptor # NIL THEN 
      (* got a real remainder *)

      IF newDescriptor.prevFree # NIL THEN
        (* deleting out of middle *)
        newDescriptor.prevFree.nextFree := newDescriptor;
        PtrToDesc(newDescriptor.ptrs.prev).ptrs.next := 
            Word.And(LOOPHOLE(newDescriptor,Word.T), PtrMask);
      ELSE
        (* we deleted out of the head *)
        Free[RP] := newDescriptor;
      END;

      (* update back ptr from next *)
      IF newDescriptor.nextFree # NIL THEN
        newDescriptor.nextFree.prevFree := newDescriptor;
        PtrToDesc(newDescriptor.ptrs.next).ptrs.prev := 
            Word.And(LOOPHOLE(newDescriptor, Word.T), PtrMask);
      END;

    ELSE
      (* no remainder...either at the end of the list or the list is empty *)
      IF back = NIL THEN
        (* free list is empty *)
        Free[RP] := NIL;
      ELSE
        (* cut off the end *)
        back.nextFree := NIL;
        back.ptrs.next := 0;
      END;
    END;
    (* add new object to allocated list *)


    IF Top[RP] = NIL THEN
      (* empty, nothing allocated *)
      Top[RP] := d;
      d.ptrs.next := 0;
      d.ptrs.prev := 0;
    ELSE
      d.ptrs.prev :=0;
      (*d.ptrs.next := DescToPtr(Top[list]);*)
      d.ptrs.next := Word.And(LOOPHOLE(Top[RP], Word.T), PtrMask);
      (*Top[list].ptrs.prev := DescToPtr(d);*)
      Top[RP].ptrs.prev := Word.And(LOOPHOLE(d, Word.T), PtrMask);
      Top[RP] := d;
    END;
    d.header.gcStatus := WhiteStatus;
    (* ----------stats for reserve pool-------------*)
    DEC(BytesFree[RP], d.header.size);

    INC(BufferZone, Word.RightShift(d.header.size, RatioPower));
    RETURN d;
  END GetMemFromReserve;

  (* mark a page as dirty...move to the allocated list.  *)

  (* Minor optimization :don't do the continued check unless we're allocating 
     from the reserve pool.  This assumes the small lists are always
     sub page sizes.
  *)

  (* ASSUMPTIONS : page was free, and not black 
                   if res pool, and start page is free, then all pages of obj
                   are free
  *)
  
PROCEDURE AddObjToDesc(d : RefDescriptor; list : INTEGER; page : Page) = 
  VAR 
    rempage : Page;
    nextobj : RefDescriptor;
  BEGIN
    (*Spy.Enter(PageUpdateTimer);*)
    (* we must rely on the fact that hte space bit is always correct, which 
       means that pages cannot be reclaimed implicitly *)

      (* not already marked as un free *)
      DEC (freePages);
      UnfreePage(page,list); 
      (* mark as current *)
      desc[page] := Desc{space := Space.Current,
                         generation := Generation.Younger,
                         pure := FALSE,
                         continued := FALSE,
                         protected := FALSE,
                         note := Note.Allocated,
                         gray := FALSE
      };

      AddPageToAllocated(page, list);
    (*END;*)

    IF AllocateAligned AND list = RP THEN
      (*
      IF  desc[page].continued # FALSE THEN
        PutText("GC ERROR >> res page start is continued!\n");
        Flush();RTOS.Crash();
      END;
      *)
      nextobj := d+d.header.size;
      rempage := Word.RightShift(LOOPHOLE(nextobj,INTEGER), LogBytesPerPage) - p0;

      (* mark continued pages *)

      IF LOOPHOLE(nextobj, INTEGER) MOD BytesPerPage = 0 THEN
        DEC(rempage);
      END;

      IF rempage > page THEN
        FOR i := page+1 TO rempage DO
          (* this page no longer free *)
          UnfreePage(i, list);
          AddPageToAllocated(i, list);
          desc[i] := Desc{space := Space.Current,
                          generation := Generation.Younger,
                          pure := FALSE,
                          continued := TRUE,
                          protected := FALSE,
                          note := Note.Allocated,
                          gray := FALSE
          };
        END;
      END;
    END;
    (*Spy.Exit(PageUpdateTimer);*)
  END AddObjToDesc;


(* should only be called when reclaiming objects from reserve pool *)
(* because i never explicitly reclaim pages for the reservepoool, might
   as well do it here *)
PROCEDURE DelObjFromDesc(d : RefDescriptor; list : INTEGER) = 
  VAR
    p := Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage);
    page := p - p0;
    rempage : Page;
    nextobj : RefDescriptor;
  BEGIN
    (* set space to free *)
    desc[page] := Desc{space := Space.Free,
                          generation := Generation.Younger,
                          pure := FALSE,
                          continued := FALSE,
                          protected := FALSE,
                          note := Note.Allocated,
                          gray := FALSE
          };


    
    IF Sanity AND BlackPages[list] = p THEN
      PutText("GC ERROR >> RES POOL PAGE BLACK");
      RTOS.Crash();
    END;

    IF Sanity AND BottomPage[list] = p THEN
      PutText("GC ERROR >> RES POOL PAGE BOTTOM");
      RTOS.Crash();
    END;
    INC (freePages);
    FreePage(p, page, list);

    (* now check if it was continued... *)
    (* cannot remove continued field because it lets us know its
       part of a contig block, even if it is freed *)
    (* need to unref it though *)
    IF TRUE OR list = RP THEN
      nextobj := d+d.header.size;
      rempage := Word.RightShift(LOOPHOLE((d+d.header.size), INTEGER), 
                                 LogBytesPerPage) - p0 - 1;

      IF rempage > page THEN
        FOR i := page+1 TO rempage DO
          (*desc[i].space := Space.Free;*)
          desc[i] := Desc{space := Space.Free,
                          generation := Generation.Younger,
                          pure := FALSE,
                          continued := TRUE,
                          protected := FALSE,
                          note := Note.Allocated,
                          gray := FALSE
          };

          INC (freePages);
          FreePage(i+p0, i, list);
        END;
      ELSE
        (* FIXME: IS THIS AN ERROR OR NOT???
        PutText("GC ERROR >> DOFD : only one page??? page/rempage/size/typecode ");
        PutInt(page);   PutText("/");
        PutInt(rempage);PutText("/");
        PutInt(d.header.size);PutText("/");
        PutInt(d.header.typecode);
        PutCR();Flush();
        *)
      END;
    END;
  END DelObjFromDesc; 

(* run whenever a page becomes free (from the allocated page list.
   update the freepage list *)
(* take page index, not real page *)
(* page = page index, p is actual page number *)
PROCEDURE FreePage (p : Page; page : Page; list : INTEGER) = 
  VAR
  BEGIN
    IF AllocatedPages[list] = p THEN
      (* advance the allcoated page list ptr *)
      AllocatedPages[list] := pagelinks[page].next;

      IF AllocatedPages[list] # 0 THEN
        pagelinks[AllocatedPages[list]-p0].prev := 0;
      END;
    ELSE
      (* unlink *)
      IF pagelinks[page].prev # 0 THEN
        pagelinks[pagelinks[page].prev-p0].next := pagelinks[page].next;
      END;
      IF pagelinks[page].next # 0 THEN
        pagelinks[pagelinks[page].next-p0].prev := pagelinks[page].prev;
      END;
    END;

    (* insert at head of the freepage list *)
    pagelinks[page].next := FreePageList[list];
    pagelinks[page].prev := 0;
    
    (* update head of list to point back to me *)
    IF FreePageList[list] # 0 THEN
      pagelinks[FreePageList[list]-p0].prev := p;
    ELSE
      (* list was empty, reset Bottom *)
      (* next is already 0 *)
      BottomPage[list] := p;
    END;
    (* change actual head *)
    FreePageList[list] := p;
    
    INC(FreePages[list]);
    INC(TotalFreePages);
  END FreePage;

(* delete page from free page list *)
PROCEDURE UnfreePage (p : Page; list:INTEGER) =
  VAR
  BEGIN
    (* first update the thing before *)
    IF pagelinks[p].prev # 0 THEN
      (* middle or end of list *)
      pagelinks[pagelinks[p].prev-p0].next := pagelinks[p].next;
      
    ELSE
      (* head of list *)
      IF Sanity AND FreePageList[list] # p+p0 THEN
        PutText("Unfree : free head is wrong!!!  list/page/freepagelist ");
        PutInt(list);PutText("/");
        PutInt(p);PutText("/");
        PutInt(FreePageList[list]-p0);
        PutCR();Flush();
        RTOS.Crash();
      END;

      FreePageList[list] := pagelinks[p].next;
    END;  

    (* now update the thing after to point at my prev *)
    IF pagelinks[p].next # 0 THEN
      pagelinks[pagelinks[p].next -p0].prev := pagelinks[p].prev;
    END;

    IF p = BottomPage[list]-p0 THEN
      IF Sanity AND pagelinks[p].next # 0 THEN
        PutText("Unfree : bottom.next not 0!!!!  list ");
        PutInt(list);
        PutCR();Flush();
        RTOS.Crash();
      END;

      BottomPage[list] := pagelinks[BottomPage[list]-p0].prev;
      (* XXX *)
      IF BottomPage[list] # 0 THEN 
        pagelinks[BottomPage[list]-p0].next := 0;
      END; 
    END;

    DEC(FreePages[list]);
    DEC(TotalFreePages);
    (* during a collection this represents the fact that 
       we lost a whole page *)
    INC(BytesAlloced,BytesPerPage); 
    IF list # RP THEN
      DEC(BufferZone,BytesPerPage);
    END;

  END UnfreePage;



(* ----------------------------------------------- Collection Routines *)


(* implicitly reclaim all objects and pages for a given freelist, 
   flipping any bits as necessary *)
PROCEDURE ReclaimAndFlip(list : INTEGER) :BOOLEAN =
  VAR
    last : INTEGER;
  BEGIN
    
    (* ---------------- Reclaim pages -----------------*)
    (* 
       move unmarked pages back to the freelist.  
       The space field is initialized back to Free.
       This is necessary so we don't unnecessarily incur the 
       cost of moving a page to the AllocatedPages list when an object
       gets allocated
    *)

    IF BottomPage[list] # 0 THEN
      (* not empty *)
      pagelinks[BottomPage[list]-p0].next := AllocatedPages[list];
      (* in this case last will be fine *)
    ELSE
      (* empty, insert at head *)
      FreePageList[list] := AllocatedPages[list];
      (* last should be alright since we go through loop, allocpages is not nil 
         
      *)
    END;

    IF AllocatedPages[list] # 0 THEN
      pagelinks[AllocatedPages[list]-p0].prev := BottomPage[list];
    END;
    last := -1;
    WHILE AllocatedPages[list] # 0 DO
      last := AllocatedPages[list]-p0;
      
      IF Sanity AND desc[AllocatedPages[list]-p0].space # Space.Current THEN
        PutText("GC ERROR >> RAF-  space not current! list/page/space ");
        PutInt(list);
        PutText("/");
        PutInt(last); (* i bet it's a circular end *)
        PutText("/");
        IF desc[AllocatedPages[list]-p0].space = Space.Free THEN
          PutText("free");
        ELSE
          PutText("other, prolly previous");
        END;
        PutCR(); 
      END;

      AllocatedPages[list] := pagelinks[last].next;
      desc[last].space := Space.Free;

      INC(FreePages[list]);
      INC(freePages);
      INC(TotalFreePages);
    END;
    IF last # -1 THEN
      BottomPage[list] := last + p0;
      pagelinks[last].next := 0;
    END;(* else, leave as is, we didn't return anything *)

    (* swapping page spaces *)
    AllocatedPages[list]:=BlackPages[list];
    (* have to sweep black pages' dirty bit *)
    WHILE BlackPages[list] # 0 DO
      IF Sanity AND desc[BlackPages[list]-p0].space # Space.Previous THEN
        PutText("GC ERROR>>> RAF - Nonblack page in the blackpage list!!!\n");
        PutInt(list);
        PutText("/");
        IF desc[BlackPages[list]-p0].space = Space.Free THEN
          PutText("FREE");
        END;
        IF desc[BlackPages[list]-p0].space = Space.Current THEN
          PutText("CURRENT");
        END;

        RTOS.Crash();
      END;
      desc[BlackPages[list]-p0].space := Space.Current;
      BlackPages[list] := pagelinks[BlackPages[list]-p0].next;
    END;


    (* ---------------- Reclaim objects -----------------*)
    (* move the objects...note we are losing locality by
       not inserting objects close to other objects on the same page *)
    (* ASSERT Top[list] =Scan[list] *)
    
    (* only move bottom if something was collected *)
    IF Top[list] # Bottom[list] THEN

      (* FOR DEBUGGING...keep track of # of elements, sanity check
         everything.  Only done if the Implicit flag is FALSE *)

      IF NOT Implicit THEN
        VAR p:= PtrToDesc(Bottom[list].ptrs.next);
        BEGIN
          WHILE p # PtrToDesc(Top[list].ptrs.next) DO
            (*PutText(".");*)
            (*INC(ElemsFree[list]);*) (* no longer needed *)
            IF p.header.gcStatus # WhiteStatus THEN
              PutText("GC ERROR >> RAF - not white and still in ecru list ! p/white/stat/size ");
              PutAddr(p); PutText("/");
              PutText(GcS[WhiteStatus]); PutText("/");
              PutText(GcS[p.header.gcStatus]);PutText("/");
              PutInt(p.header.size);
              PutCR();Flush();
            END;


            p.header.gcStatus := GcStatus.Untraced;
            (*p.header.typecode := TcFree;*)

            p := PtrToDesc(p.ptrs.next);
          END;
        END;
      END;
      
      (* move bottom to top, move top to just behind FREE, so everything new
         is allocated grey *)
      Bottom[list] := Top[list];
      
      (* top = freelists.prev, the last ecru object.  *)
      Top[list] := LOOPHOLE(Word.Or(Free[list].ptrs.prev, DescMask),RefDescriptor);
      IF Top[list].header.gcStatus # BlackStatus AND Top[list]#Bottom[list] THEN
        PutText ("GC ERROR >> new top not black1!!!  list/addr/status/blackstatus");
        PutInt(list);PutText("/");
        PutAddr(Top[list]);PutText("/");
        PutText(GcS[Top[list].header.gcStatus]);PutText("/");
        PutText(GcS[BlackStatus]);
        PutCR();Flush();
        RTOS.Crash();
      END;
      (* everything else is implicitly in the freelist now *)
      (*--------- sanity check live objects ---------*)
      IF NOT Implicit THEN
        VAR p := PtrToDesc(Bottom[list].ptrs.next);
        BEGIN
          WHILE p # PtrToDesc(Top[list].ptrs.next) DO
            (*PutText(":");*)
            IF p.header.gcStatus # BlackStatus THEN
              PutText("GC ERROR >> RAF- new ecru list not all black! p/curr/status ");
              PutAddr(p); PutText("/");
              PutText(GcS[BlackStatus]); PutText("/");
              PutText(GcS[p.header.gcStatus]);
              PutCR(); Flush();
            END;
            p := PtrToDesc(p.ptrs.next);
          END;

        END;
      END;

      RETURN TRUE;
    ELSE
      (* NOTHING COLLECTED *)
      (* reset top to free.prev *)
      Top[list] := LOOPHOLE(Word.Or(Free[list].ptrs.prev, DescMask),
                            RefDescriptor);
      
      (* sanity check that everything is live *)
      IF NOT Implicit THEN
        VAR p := PtrToDesc(Bottom[list].ptrs.next);
        BEGIN
          WHILE p # PtrToDesc(Top[list].ptrs.next) DO
            IF p.header.gcStatus # BlackStatus THEN
              PutText("GC ERROR >> new ecru list not all black! p/black/status ");
              PutAddr(p);
              PutText("/");
              PutText(GcS[BlackStatus]);
              PutText("/");
              PutText(GcS[p.header.gcStatus]);
              PutCR();
              Flush();
            END;
            p := PtrToDesc(p.ptrs.next);
          END;
        END;
      END;
      RETURN FALSE;
    END;
  END ReclaimAndFlip;

(* run at end of collection to recompute pressure on freelists *)
PROCEDURE UpdateStatsEOC () =
  VAR
  BEGIN
    FOR i := 0 TO NumFreeLists - 1 DO
      Scan[i] :=NIL;
    END;
    FOR i := 0 TO NumFreeLists-2 DO
      (* reserve pool bytes free is already maintained by
         ReturnMemToReserve *)
      BytesFree[i]  := ListCaps[i] - BytesLive[i];
      ElemsFree[i] := FreeListElems[i] - LiveObjects[i];
    END;

      ActiveBytes := CountedBytes;



    Finish := FALSE;
    
    IF Incremental THEN
      BytesMarked :=0;
      BytesAlloced := 0;
      RecomputeBufferZone();
    END;
  END UpdateStatsEOC;


(* assumes page already unlinked from wherever it used to be *)
PROCEDURE AddPageToAllocated(p : Page; list : INTEGER) =
  VAR
    page := p+p0;
  BEGIN
    pagelinks[p].next := AllocatedPages[list]; (* nil terminated if it was head *)
    pagelinks[p].prev := 0;
    
    IF AllocatedPages[list] # 0 THEN
      pagelinks[AllocatedPages[list]-p0].prev := page;
    END;
    AllocatedPages[list] := page;
  END AddPageToAllocated;

<* UNUSED *>
PROCEDURE AllocatePageBlack(p:Page; list : INTEGER) =
  VAR
    page := p+p0;
  BEGIN
    RTOS.Crash();
    pagelinks[p].next := BlackPages[list]; (* nil terminated if it was head *)
    pagelinks[p].prev := 0;
    
    IF BlackPages[list] # 0 THEN
      pagelinks[BlackPages[list]-p0].prev := page;
    END;
    BlackPages[list] := page;
  END AllocatePageBlack;


(* called during collections *)
(* unlinks pages itself from allocated list*)
PROCEDURE AddPageToBlack(p    : Page;  (* page index *)
                         page : Page;  (* actual page number *)
                         list : INTEGER) =
  VAR
  BEGIN
    IF AllocatedPages[list] = page THEN
      (* advance the allcoated page list ptr *)
      AllocatedPages[list] := pagelinks[p].next;

      IF AllocatedPages[list] # 0 THEN
        pagelinks[AllocatedPages[list]-p0].prev := 0;
      END;

    ELSE
      (* unlink *)
      IF pagelinks[p].prev # 0 THEN
        pagelinks[pagelinks[p].prev-p0].next := pagelinks[p].next;
      END;
      IF pagelinks[p].next # 0 THEN
        pagelinks[pagelinks[p].next-p0].prev := pagelinks[p].prev;
      END;
    END;
    pagelinks[p].next := BlackPages[list]; (* nil terminated if it was head *)
    pagelinks[p].prev := 0;
    
    IF BlackPages[list] # 0 THEN
      pagelinks[BlackPages[list]-p0].prev := page;
    END;
    BlackPages[list] := page;
    (* mark the page as blackened...have to sweep later *)
    desc[p].space := Space.Previous;
  END AddPageToBlack;


(* This routine is only called for the reservepool! It returns the
   object pointed to by addr to the reserve pool, inserting it at the
   appropriate location and coalescing with adjacent blocks if possible *)

PROCEDURE ReturnMemToReserve (addr: ADDRESS )=
  VAR
    size: CARDINAL;
    d   : RefDescriptor;
    back: RefDescriptor;
    list : INTEGER;
  BEGIN
    Spy.Enter(ReturnMemTimer);
    size := LOOPHOLE(addr, RefDescriptor).header.size;

    BEGIN
      list := RP;
      DEC(LiveObjects[list]);
      INC(BytesFree[list], size);
      INC (ElemsFree[list]);
      d := addr;
      (*---------------- insert in sorted order---------------------- *)
      VAR p: RefDescriptor;
      BEGIN
        p := Free[list];
        back := NIL;
        WHILE p # NIL AND p < addr DO back := p; p := p.nextFree; END;
        IF p = addr THEN
          PutText(
              "GC ERROR >> Something that was collected was still in the Reserve pool!\n");
          PutText("crap already there addr/size/in/in.size/innext/inprev ");
          PutAddr(addr); PutText("/");
          PutInt(LOOPHOLE(addr,RefDescriptor).header.size);
          PutText("/"); PutAddr(p); PutText("/");
          PutInt(p.header.size); PutText("/");
          PutAddr(p.nextFree); PutText("/");
          PutAddr(p.prevFree); PutCR(); Flush();
          FLSC := TRUE; PLSC := TRUE; DescSanity := TRUE; TMSanity := TRUE;
          RPSC := TRUE;
          FreeListSanityCheck("collection screwup");
          Flush();
          RTOS.Crash();
        END;
        (* initialize the header *)
        d.header.gcStatus := GcStatus.Untraced;
        IF back = NIL THEN
          d.nextFree := Free[list];
          d.ptrs.next := Word.And(LOOPHOLE(d.nextFree, Word.T), PtrMask); 

          IF d.nextFree # NIL THEN
            d.nextFree.prevFree := d;
            PtrToDesc(d.ptrs.next).ptrs.prev := Word.And(LOOPHOLE(d, Word.T), PtrMask);
          END;
          Free[list] := d;
          d.prevFree := NIL;
          d.ptrs.prev := 0;
        ELSE
          back.nextFree := d;
          back.ptrs.next := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
          d.nextFree := p;
          d.prevFree := back;

          d.ptrs.next := Word.And(LOOPHOLE(p, Word.T), PtrMask); 
          d.ptrs.prev := Word.And(LOOPHOLE(back, Word.T), PtrMask); 
          IF p # NIL THEN
            p.prevFree := addr;
            p.ptrs.prev := Word.And(LOOPHOLE(addr, Word.T), PtrMask); 
          END;
        END;
        (* ----------- Coalescing time -------------- *)
        (* make sure we have udpated the page descriptor array first.   *)
        DelObjFromDesc(d, list);
        d.header.typecode := TcFree;(* XXX put this here so the typcode is kept
                                       long enough for us to print it out *)
        (* check back ptr *)
        IF back # NIL THEN
          IF back + back.header.size = d THEN
            (* coalsce with prev guy *)

            back.header.size := back.header.size + d.header.size;
            back.nextFree := d.nextFree;
            back.ptrs.next := d.ptrs.next;

            IF d.nextFree # NIL THEN
              d.nextFree.prevFree := back;
              PtrToDesc(d.ptrs.next).ptrs.prev := Word.And(LOOPHOLE(back, Word.T), 
                                                           PtrMask);
            END;

            (* Must update page descriptor because this object became
               continued *)
            (*desc[GetPage(d)-p0].continued := TRUE;*)

            desc[Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage)-p0] :=
                Desc{space := Space.Free, 
                     generation := Generation.Younger,
                     pure := FALSE,
                     continued := TRUE,
                     protected := FALSE,
                     note := Note.Allocated,
                     gray := FALSE
            };

            d := back;       (* for next check *)
            DEC (ElemsFree[list]);

          END;
        END;

        (* check next*)
        IF d + d.header.size = d.nextFree THEN
          (* coalesce with the next guy! *)
          (* Must update page descriptor because this object became
             continued *)
          (*desc[GetPage(d+d.header.size)-p0].continued := TRUE;*)
          (*
          desc[Word.RightShift(LOOPHOLE(d+d.header.size,INTEGER), 
                               LogBytesPerPage)-p0].continued := TRUE;
                               *)
          desc[Word.RightShift(LOOPHOLE(d+d.header.size,INTEGER), 
                               LogBytesPerPage)-p0] := 
                Desc{space := Space.Free, 
                     generation := Generation.Younger,
                     pure := FALSE,
                     continued := TRUE,
                     protected := FALSE,
                     note := Note.Allocated,
                     gray := FALSE
            };
          d.header.size := d.header.size + d.nextFree.header.size;
          d.nextFree := d.nextFree.nextFree;
          d.ptrs.next := Word.And(LOOPHOLE(d.nextFree, Word.T), PtrMask); 
          IF d.nextFree # NIL THEN
            d.nextFree.prevFree := d;
            PtrToDesc(d.ptrs.next).ptrs.prev := Word.And(LOOPHOLE(d, Word.T), 
                                                         PtrMask); 
          END;
          (* my prevfree remains intact *)
          DEC (ElemsFree[list]);

        END;
        (* ------end coalescing crud -------- *)
      END;

    END;

    Spy.Exit(ReturnMemTimer);

  END ReturnMemToReserve;



(* -------------------------------------------- Page Balancing *)


(* wrapper to itearte over min pressure lists until we successfully move
   memory to the target or max pressured list *)
(* "needed" will be used later for moving crud from/to reservepool *)
PROCEDURE BalanceLists (target: INTEGER; needed : INTEGER): BOOLEAN =
  VAR
    min := 0;                    (* index into pressures *)
    max := target;                    (* actual list that has max pressure! *)
    (* sorted list in decreasing pressure *)
    (*pressures: ARRAY [0 .. NumFreeLists - 1] OF INTEGER;*)
    ret : BOOLEAN;
  BEGIN

    IF Sanity THEN
      FreeListSanityCheck("balance start");
    END;

    Spy.Enter(BalanceTimer);

    IF Remapping AND target = RP THEN
      (* special stuff has to be done for the res pool *)
      ret :=  AugmentReservePool(needed);
      Spy.Exit(BalanceTimer); 
      RETURN ret;
    END;
    max := target; 
    min := 0;
    WHILE min <= NumFreeLists-2 DO
      IF min = max THEN
        (* skip max if we hit it *)
        INC(min);
      ELSIF FreePages[min] > 1 THEN
        (* move a page if there's more than one page left 
           in the victim list *)
        MovePage(min,max);
        Spy.Exit(BalanceTimer); 
        FreeListSanityCheck("balancesucc");
        RETURN TRUE;
      ELSE
        (* retrying balance *)
        INC(min);
      END;
    END;
    (* if we got here then we failed.  Let's try the reservepool as a 
       last resort *)

    (* only do this if remapping is on, because then we know that memory
       can make its way back to the reservepool *)
    IF Remapping AND FreePages[RP] > 1 THEN
      MovePageFromReserve(max);
      Spy.Exit(BalanceTimer); 
      FreeListSanityCheck("mpfrsucc");
      RETURN TRUE;
    END;
    Spy.Exit(BalanceTimer); 
    RETURN FALSE;
  END BalanceLists;


(* src and dest are lists *)
(* move a page by extracting all elements on the victim page
   and re-fragmenting the page according to the 
   destination's node size*)

PROCEDURE MovePage(src : INTEGER; dest : INTEGER) =
  VAR
    srcPage : Page;
    srcPageIndex : Page;
    srcPageAddr : RefDescriptor;
    dhead : Page;
  BEGIN
    Spy.Enter(MovePageTimer);
    srcPageAddr := RemovePage(src, srcPage, srcPageIndex);


    (* ------- update free page list --------- *)
    (* get page number of the previous head of dest list *)
    dhead := FreePageList[dest];
    (* change page link of moving page to point to it *)
    pagelinks[srcPageIndex].next := dhead;
    pagelinks[srcPageIndex].prev := 0;
    IF dhead #0 THEN
      pagelinks[dhead-p0].prev   := srcPage;
    ELSE
      (* inserting into empty list *)
      BottomPage[dest] := srcPage;
    END;
    
    (* set the head of the dest page list to point to the moved page *)
    FreePageList[dest] := srcPage;
    
    FragmentPage(dest, srcPageAddr);

    (* incremental collector really needs these stats to be correct *)
    IF Stats OR Incremental THEN
      DEC(FreePages[src]);
      DEC(TotalPages[src]);
      DEC(BytesFree[src], BytesPerPage);
      DEC(ElemsFree[src], ElemsPerPage[src]);
      DEC(FreeListElems[src], ElemsPerPage[src]);
      DEC(ListCaps[src], BytesPerPage);
      
      INC(TotalPages[dest]);
      INC(FreePages[dest]);
      INC(BytesFree[dest], BytesPerPage);
      INC(ElemsFree[dest], ElemsPerPage[dest]);
      INC(FreeListElems[dest], ElemsPerPage[dest]);
      INC(ListCaps[dest], BytesPerPage);

    END;
    Spy.Exit(MovePageTimer);
  END MovePage;


(* ------------------------------------------------ Page Remapping Code *)

(* This code needs to be cleaned up and optimized.  
*)

VAR 
  (* list of freelists and the number of pages to swipe from each *)
  targets : ARRAY[0..NumFreeLists-2] OF INTEGER;

(* find and map enough pages to the reservepool *)
PROCEDURE AugmentReservePool(needed : INTEGER) : BOOLEAN =
  VAR
    numpages  : INTEGER;
    min       : INTEGER;
    movedpageaddr : ADDRESS;
    movedpageindex : Page;
    movedpage : Page;
    newpage   : Page; (* page # of the newly mapped page *)
    newpageindex : Page; (* page index of the newly mapped page *)
    start     : ADDRESS := NIL; (* start of new block in reservepool *)
    totake    : INTEGER;
    numallocated := 0; (* number of pages remapped so far *)
    toSteal : INTEGER; (* number of pages to steal from free lists *)
    fromWithin :=0;    (* #pages remapped from within *)
    movedElems : INTEGER; (* # elems stolen from seg freelists *)
  BEGIN
    Spy.Enter(AugResTimer);
    (* how many pages are needed*)
    numpages := Word.RightShift(needed, LogBytesPerPage);
    IF needed MOD BytesPerPage >= 0 THEN
      INC(numpages);
    END;

    (* First thing to try - remap WITHIN reservepool.  This reduces
       fragmentation and the likelihood of causing another balance on
       the poor free lists *)
    IF FreePages[RP] > 0 THEN
      fromWithin :=  RemapWithinReserve(numpages,start);

      IF fromWithin >= numpages THEN
        (* there were enough pages in the res pool *)
        Spy.Exit(AugResTimer);
        FreeListSanityCheck ("RWRsucc");
        RETURN TRUE;
      END;
      numallocated := fromWithin;
    ELSE
      (*start :=  RTMem.GetTracedEnd();*)
      start :=  RTHeapVM.GetEnd();
    END;
    
    (* initialize the targets lsit *)
    (* using this saves us from dequeueing and re-queueing pages in the 
       case of a failure *)
    FOR i := 0 TO NumFreeLists-2 DO
      targets[i] := 0;
    END;
    (* find the free pages *)
    min := 0;
    toSteal := numpages - numallocated;
    WHILE min <= NumFreeLists-2 AND numallocated < numpages DO
      IF FreePages[min] >  1 THEN
        (* take as much out as possible *)
        totake := FreePages[min] - 1;
        IF numallocated + totake  > numpages THEN
          (* just take what is needed *)
          totake := numpages - numallocated;
        END;

        INC(targets[min], totake);
        DEC(FreePages[min], totake);
        INC(numallocated , totake);
      END;
      (* try next *)
      INC(min);
    END;

    (* if we didn't get all the pages we needed ... *)
    IF numallocated < numpages THEN
      (* failure, restore FreePage numbers *)
      FOR i := 0 TO NumFreeLists-2 DO
        INC(FreePages[i], targets[i]);
      END;
      (* if we remapped any from within reserve, need to create an object for it *)
      IF fromWithin#0 THEN
        numallocated := fromWithin;
        WITH h = LOOPHOLE(start, RefDescriptor) DO
          h.ptrs.next := DescToPtr(Free[RP]); (* XXX speed up! *)
          h.ptrs.prev := 0;
          h.nextFree := Free[RP];
          h.prevFree := NIL;

          h.header.size := numallocated * BytesPerPage;
          h.header.gcStatus := GcStatus.Untraced;
          h.header.typecode := TcFree;

          IF Free[RP]  #NIL THEN
            Free[RP].prevFree := h;
            Free[RP].ptrs.prev := DescToPtr(h);
          END;
          (* inc elemcount? *)
          Free[RP] := h;

          (* HACK for arbitrary sized heap !*)
          (*INC(MEMORY_End,h.header.size);*)
          MEMORY_End := start + h.header.size;

          (* set first page to be not continued *)
          (* original...doesn't work if heap size artificially limited *)
          desc[p1-p0] := Desc{space := Space.Free, 
                              generation := Generation.Younger,
                              pure := FALSE,
                              continued := FALSE,
                              protected := FALSE,
                              note := Note.Allocated,
                              gray := FALSE
          };
        END;
        (* advance the last page ptr *)
        INC(p1, numallocated);
        INC(descpages, numallocated);
      END;



      Spy.Exit(AugResTimer);
      RETURN FALSE; (* this will incur a collection *)
    END;
    
    (* need to know if we have to grow the page desc array *)
    IF numpages + descpages  >  descsize THEN
      GrowDesc();
    END;
    
    (* get the pages indicated by targets *)
    FOR i := 0 TO NumFreeLists-2 DO
      FOR p := 1 TO targets[i] DO
        (* got a candidate *)
        movedpageaddr := RemovePage(i, movedpage, movedpageindex);
        (* later should add the page to a list of "holes" in the
           virtual heap so we can fill them up later *)
        (*desc[GetPage(movedpage)-p0].space := Space.Unallocated;*)

        desc[movedpageindex]
           := Desc{space := Space.Unallocated,
                                   generation := Generation.Younger,
                                   pure := FALSE,
                                   continued := FALSE,
                                   protected := FALSE,
                                   note := Note.Allocated,
                                   gray := FALSE
                                   };

        (* remap *)
        newpage := Word.RightShift(LOOPHOLE(RTHeapVM.MovePageToReservePool(movedpageaddr), INTEGER), LogBytesPerPage);
        newpageindex := newpage- p0;
        (* update new desc, pagelink entries *)
        desc[newpageindex] := Desc{space := Space.Free, 
                                   generation := Generation.Younger,
                                   pure := FALSE,
                                   continued := TRUE,
                                   protected := FALSE,
                                   note := Note.Allocated,
                                   gray := FALSE
                                   };

        pagelinks[newpageindex].next := FreePageList[RP];
        pagelinks[newpageindex].prev := 0; (* new head of list *)
        IF FreePageList[RP] # 0 THEN
          (* not empty, set prev *)
          pagelinks[FreePageList[RP]-p0].prev := newpage;
        ELSE
          (* empty, so better make sure we set bottom page *)
          BottomPage[RP] := newpage;
        END;
        FreePageList[RP] := newpage;
      END;
      movedElems := ElemsPerPage[i]*targets[i];
      DEC(TotalPages[i], targets[i]);
      DEC(BytesFree[i],BytesPerPage*targets[i]);
      DEC(ElemsFree[i],movedElems);
      DEC(FreeListElems[i], movedElems);
      DEC(ListCaps[i], BytesPerPage * targets[i]);
      (* elements were stolen from the freelists, so decrement TotalElems *)
      DEC(TotalElems, movedElems);
    END;

    (* create the new block for the res pool *)

    WITH h = LOOPHOLE(start, RefDescriptor) DO
      h.ptrs.next := DescToPtr(Free[RP]); (* XXX speed up! *)
      h.ptrs.prev := 0;
      h.nextFree := Free[RP];
      h.prevFree := NIL;

      h.header.size := numpages * BytesPerPage;
      h.header.gcStatus := GcStatus.Untraced;
      h.header.typecode := TcFree;

      IF Free[RP]  #NIL THEN
        Free[RP].prevFree := h;
        Free[RP].ptrs.prev := DescToPtr(h);
      END;
      (* inc elemcount? *)
      Free[RP] := h;

      INC (FreePages[RP], toSteal);
      INC(TotalPages[RP], toSteal);
      INC(BytesFree[RP], toSteal*BytesPerPage);
      INC(ListCaps[RP], toSteal*BytesPerPage);

      (* HACK for arbitrary sized heap !*)
      (*INC(MEMORY_End,h.header.size);*)
      MEMORY_End := start + h.header.size;

      (* set first page to be not continued *)
      (* original...doesn't work if heap size artificially limited *)
      desc[p1-p0] := Desc{space := Space.Free, 
                      generation := Generation.Younger,
                      pure := FALSE,
                      continued := FALSE,
                      protected := FALSE,
                      note := Note.Allocated,
                      gray := FALSE
                      };
    END;


    (* advance the last page ptr *)
    INC(p1, numpages);
    INC(descpages, numpages);
    Spy.Exit(AugResTimer);
    RETURN TRUE;
  END AugmentReservePool;


PROCEDURE RemapWithinReserve(numpages : INTEGER; VAR start : ADDRESS) : INTEGER =
  VAR
    newpage : Page; (* page # of newly mapped page *)
    newpageaddr : ADDRESS;
    newpageindex : Page; (* page index of the newly mapped page *)
    nummoved :INTEGER; (* number of pages moved *)
    objpages :INTEGER;
    d : RefDescriptor; (* current object *)
    pageaddr : ADDRESS; (* pointer within object*)
    pagenum : Page;     (* page number of target *)
    pageindex : Page;  (* page index of target *)
  BEGIN
    (* remember where the block starts *)
    (*start := RTMem.GetTracedEnd();*)
    start := RTHeapVM.GetEnd();
    (* dequeue objects until we have enough *)
    nummoved := 0;
    d := Free[RP];
    WHILE nummoved < numpages DO
      IF d = NIL THEN
        (*PutText("RemapWithinRes : Ran out of objects\n");*)
        RETURN nummoved;
      END;
      objpages := Word.RightShift(d.header.size, LogBytesPerPage);
      pageaddr := d;

      (* dequeue object *)
      IF d.nextFree # NIL THEN
        d.nextFree.prevFree := NIL;
        d.nextFree.ptrs.prev := 0;
      END;

      (*
      IF d.prevFree # NIL THEN
        PutText("HUH? why wasnt d head of freelist? \n");
        d.prevFree.nextFree := d.nextFree;
        d.prevFree.ptrs.next := d.ptrs.next;
      END;
      *)
      d:= d.nextFree; (* get value before i remap the page *)
      Free[RP]:=d;
      pagenum := Word.RightShift(LOOPHOLE(pageaddr, INTEGER), LogBytesPerPage);
      pageindex := pagenum-p0;
      (* need to know if we have to grow the heap *)
      IF objpages + descpages  >  descsize THEN
        GrowDesc();
      END;
    
      FOR i:= 1 TO objpages DO
        desc[pageindex] := Desc{space := Space.Unallocated,
                                   generation := Generation.Younger,
                                   pure := FALSE,
                                   continued := FALSE,
                                   protected := FALSE,
                                   note := Note.Allocated,
                                   gray := FALSE
                                   };
        (* REMOVE PAGE FROM RESERVE *)
        (* *********** *)

        IF FreePageList[RP] = 0 THEN
          PutText("RWR : FPL emptied\n");
          BottomPage[RP] := 0;
        END;

        IF pagelinks[pageindex].prev # 0 THEN
          pagelinks[ pagelinks[pageindex].prev-p0].next := pagelinks[pageindex].next;
        ELSE
          (* should be head*)
          IF FreePageList[RP] # pagenum THEN
            PutText("WAS NOT FREEPAGELIST!\n");
            RTOS.Crash();
          END;

          FreePageList[RP] := pagelinks[pageindex].next;
        END;

        IF pagelinks[pageindex].next # 0 THEN
          pagelinks[ pagelinks[pageindex].next-p0].prev := pagelinks[pageindex].prev;
        ELSE
          (* bottom! *)
          BottomPage[RP] := pagelinks[pageindex].prev;
        END;
        (* *********** *)
        (*newpage := GetPage(RTHeapVM.MovePageToReservePool(pageaddr));*)

        newpageaddr := RTHeapVM.MovePageToReservePool(pageaddr);
        newpage := Word.RightShift(LOOPHOLE(newpageaddr,INTEGER), LogBytesPerPage);
        newpageindex := newpage - p0;
        (* update new desc, pagelink entries *)
        desc[newpageindex] := Desc{space := Space.Free, 
                                   generation := Generation.Younger,
                                   pure := FALSE,
                                   continued := TRUE,
                                   protected := FALSE,
                                   note := Note.Allocated,
                                   gray := FALSE
                                   };
        (* re insert *)
        pagelinks[newpageindex].next := FreePageList[RP];
        pagelinks[newpageindex].prev := 0; (* new head of list *)
        IF FreePageList[RP] # 0 THEN
          (* not empty, set prev *)
          pagelinks[FreePageList[RP]-p0].prev := newpage;
        ELSE
          (* empty, set bottom *)
          BottomPage[RP] := newpage;
        END;
        FreePageList[RP] := newpage;
        INC(pageaddr, BytesPerPage);
        INC(pagenum);
        INC(pageindex);
      END;
      INC(descpages, objpages);
      INC(nummoved, objpages);
      DEC(FreeListElems[RP]);
      DEC(ElemsFree[RP]);
    END;
    (* create a new block and enqueue *)
    (* create the new block for the res pool *)
    WITH h = LOOPHOLE(start, RefDescriptor) DO
      (*h.ptrs.next := DescToPtr(Free[RP]);*)
      h.ptrs.next := Word.And(LOOPHOLE(Free[RP], Word.T),
                              PtrMask);
      h.ptrs.prev := 0;
      h.nextFree := Free[RP];
      h.prevFree := NIL;

      h.header.size := nummoved * BytesPerPage;
      h.header.gcStatus := GcStatus.Untraced;
      h.header.typecode := TcFree;
      
      (* inc elemcount? *)
      Free[RP] := h;

      (* set first page to be not continued *)
      desc[p1-p0] := Desc{space := Space.Free, 
                      generation := Generation.Younger,
                      pure := FALSE,
                      continued := FALSE,
                      protected := FALSE,
                      note := Note.Allocated,
                      gray := FALSE
                      };
      (* INC(MEMORY_End,h.header.size);*)
      (*MEMORY_End := PageToAddress(newpage) + h.header.size;*)
      MEMORY_End := start + h.header.size;
      INC(FreeListElems[RP]);
      INC(ElemsFree[RP]);
    END;
    (* advance the last page ptr *)
    INC(p1, nummoved);


    RETURN nummoved;
  END RemapWithinReserve;


(* Because different structures are used, we have a separate routine to 
   move memory from res pool to free lists.  this should be fixed or cleaned up.
*)
PROCEDURE MovePageFromReserve (dest : INTEGER) =
  VAR
    srcPage : Page; (* the page # to move *)
    srcPageIndex : Page; (* page index of srcPage *)
    srcPageAddr : RefDescriptor; (* desc of that page *)
    p, d : RefDescriptor;
    newDescriptor : RefDescriptor; (* desc of remainder we create, if any *)
    remainder : INTEGER;
    dhead, page : Page;
  BEGIN
    Spy.Enter(MPFR);
    (* just grab the first thing...it should be at least 8192 bytes *)
    (* this will work because we check for the # of freepages before calling
       this routine *)
    d := Free[RP];
    IF d.header.size < BytesPerPage THEN
      RTOS.Crash();
    END;

    remainder := d.header.size - BytesPerPage;
    IF remainder # 0 THEN
      newDescriptor := d+BytesPerPage;
      newDescriptor.header.size := remainder;
      newDescriptor.header.typecode := TcFree;
      newDescriptor.nextFree := d.nextFree;
      newDescriptor.prevFree := d.prevFree;
      newDescriptor.ptrs.next := d.ptrs.next;
      newDescriptor.ptrs.prev := d.ptrs.prev;
      newDescriptor.header.gcStatus := GcStatus.Untraced;
      d.header.size := BytesPerPage;
      page := Word.RightShift( LOOPHOLE(newDescriptor, INTEGER), 
                               LogBytesPerPage) - p0;
      desc[page].continued := FALSE;
          
      IF desc[page].space # Space.Free THEN
        PutText("GC ERROR >> MPFR -  pool page not free! nd/nd.size\n");
        PutAddr(newDescriptor); PutText("/");
        PutInt(newDescriptor.header.size); PutCR();
        Flush();
      END;
    ELSE
      (* just skip *)
      newDescriptor := d.nextFree;
      IF newDescriptor # NIL THEN
        (* skip thing before this *)
        newDescriptor.prevFree := d.prevFree;
        newDescriptor.ptrs.prev := d.ptrs.prev;
      END;
      (* leave header size intact *)
      (* however, we're reducing the number of elements *)
      DEC(ElemsFree[RP]);
    END;

    IF newDescriptor # NIL THEN 
      (* got a real remainder *)

      IF newDescriptor.prevFree # NIL THEN
        (* deleting out of middle *)
        newDescriptor.prevFree.nextFree := newDescriptor;
        PtrToDesc(newDescriptor.ptrs.prev).ptrs.next := Word.And(LOOPHOLE(newDescriptor,Word.T), PtrMask);
      ELSE
        (* we deleted out of the head *)
        Free[RP] := newDescriptor;
      END;

      (* update back ptr from next *)
      IF newDescriptor.nextFree # NIL THEN
        newDescriptor.nextFree.prevFree := newDescriptor;
        PtrToDesc(newDescriptor.ptrs.next).ptrs.prev := Word.And(LOOPHOLE(newDescriptor, Word.T), PtrMask);
      END;

    ELSE
      (* no remainder...either at the end of the list or the list is empty *)
        (* free list is empty, should not happen! *)
        RTOS.Crash();
        Free[RP] := NIL;

    END;


    (* now gotta move the page to the target pool *)
    srcPageAddr := d;
    srcPage := GetPage(d);
    srcPageIndex := srcPage - p0;
    (* extract page from free page list  *)
    IF pagelinks[srcPageIndex].prev # 0 THEN
      pagelinks[ pagelinks[srcPageIndex].prev-p0].next :=
          pagelinks[srcPageIndex].next;
    ELSE
      (* this must be the head ! *)
      IF srcPage # FreePageList[RP] THEN
        PutText("srcpage was not head!!!\n");
        RTOS.Crash();
      END;

      FreePageList[RP] := pagelinks[srcPageIndex].next;
    END;

    IF pagelinks[srcPageIndex].next # 0 THEN
      pagelinks[ pagelinks[srcPageIndex].next-p0].prev :=
          pagelinks[srcPageIndex].prev;
    ELSE
      (* this must be bottom *)
      (* agh, i don't keep track of bottom page for reservepool! *)
      (*
      IF (srcPage # BottomPage[RP]) THEN
        PutText("srcpage was not at bottom!\n");
        RTOS.Crash();
        END;
        *)
      BottomPage[RP] := pagelinks[srcPageIndex].prev;
    END;

    p := d;

    (* get page number of the previous head of dest list *)
    dhead := FreePageList[dest];
    (* change page link of page being moved to point to it *)
    pagelinks[srcPageIndex].next := dhead;
    pagelinks[srcPageIndex].prev := 0;
    IF dhead #0 THEN
      pagelinks[dhead-p0].prev   := srcPage;
    ELSE
      (* inserting into empty list *)
      BottomPage[dest] := srcPage;
    END;
    (* set the head of the dest page list to point to the moved page *)
    FreePageList[dest] := srcPage;

    FragmentPage(dest, srcPageAddr);
    (* stats *)
    DEC(BytesFree[RP], BytesPerPage);

    DEC (FreePages[RP]);
    DEC (TotalPages[RP]);
    DEC (ListCaps[RP], BytesPerPage);

    INC(TotalPages[dest]);
    INC(FreePages[dest]);
    INC(BytesFree[dest], BytesPerPage);
    INC(ElemsFree[dest], ElemsPerPage[dest]);
    INC(FreeListElems[dest], ElemsPerPage[dest]);
    INC(TotalElems,ElemsPerPage[dest]);
    INC(ListCaps[dest], BytesPerPage);

    Spy.Exit(MPFR);


  END MovePageFromReserve;


PROCEDURE FragmentPage(dest : INTEGER; srcPageAddr : ADDRESS) =
  VAR
    p : RefDescriptor;
    start : RefDescriptor;
    last : RefDescriptor;
  BEGIN

    (* --------- fragment the page ------------ *)
    (* reinitialize the page (i.e. fragment it)  *)

    p := srcPageAddr;
    start := p;
    last := NIL;
    FOR i:=0 TO ElemsPerPage[dest] - 1 DO
      p.header.size := NodeSizes[dest];
      p.header.typecode := TcFree;
      p.header.gcStatus := GcStatus.Untraced;

      (* only for precomputation*)
      p.nextFree := p + p.header.size;
      p.ptrs.next := Word.And(LOOPHOLE(p.nextFree, Word.T), PtrMask);
      p.ptrs.prev := Word.And(LOOPHOLE(last, Word.T), PtrMask); 
      
      last := p;
      p := p.nextFree;
    END;

    (*------ tie up the endpoint prev/next pointers --------*)
    (* insert before head *)
    last.ptrs.next := Word.And(LOOPHOLE(Free[dest], Word.T), PtrMask);
    (* fix my head so my back ptr is at the allocated stuff *)
    start.ptrs.prev := Free[dest].ptrs.prev;
    (* fix thing before head to point at me *)
    LOOPHOLE(Word.Or(Free[dest].ptrs.prev, DescMask),RefDescriptor).ptrs.next := 
        Word.And(LOOPHOLE(start, Word.T), PtrMask);
    
    (* fix thing after the tail to point at 'last' *)
    Free[dest].ptrs.prev := Word.And(LOOPHOLE(last, Word.T), PtrMask);
    Free[dest] := start;


  END FragmentPage;


(* RemovePage removes a page from a free list, unlinking all objects on it *)
PROCEDURE RemovePage (src : INTEGER; VAR srcPage: Page; VAR srcPageIndex : Page):  ADDRESS =
  VAR
    srcPageAddr : RefDescriptor;
    p : RefDescriptor;
    nextp : Page;
  BEGIN
    srcPage := FreePageList[src];
    srcPageIndex := srcPage-p0;

    (* update the next page in the list so it's at the head *)
    nextp := pagelinks[srcPageIndex].next;
    FreePageList[src] := nextp;
    (* if last page was moved (bad idea) then update bottompage *)
    IF nextp = 0 THEN
      BottomPage[src] := 0;
    END;
    (* update pagelinks to splice me out*)
    IF nextp # 0 THEN
      pagelinks[nextp-p0].prev := 0;
    END;

    srcPageAddr := LOOPHOLE(Word.LeftShift(srcPage,LogBytesPerPage),ADDRESS);
    p:= srcPageAddr;

    (* extract every element on this page from the free list it resides in *)
    FOR i:=1 TO ElemsPerPage[src] DO
      IF Free[src] = p THEN
        IF Sanity AND p = Bottom[src] THEN
          PutText("RP - list became empty!! ");
          PutInt(src);
          RTOS.Crash();
        END;
        Free[src] := LOOPHOLE(Word.Or(Free[src].ptrs.next, DescMask),
                                   RefDescriptor);
      END;

      (*
      IF COLLECTING AND p = Scan[src] AND p # Bottom[src] THEN
        PutText("GC ERROR >> REMOVEPAGE : should never be gray! page/Scan[src]/top[src]/currwhite/stat\n");
        PutAddr(srcPageAddr); PutText("/");
        PutAddr(Scan[src]); PutText("/");
        PutAddr(Top[src]); PutText("/");
        PutText(GcS[WhiteStatus]); PutText("/");
        PutText(GcS[p.header.gcStatus]); PutCR();
        Flush(); RTOS.Crash();
      END;
      *)
      IF p = Top[src] THEN
        (* Top must equal Bottom because this page is theoretically
           free, so no element on the page can be in the ecru or black
           lists.  So if i'm pointing at Top, the ecru list must be 
           empty *)
        IF Top[src] # Bottom[src] THEN 
          PutText("GC ERROR >> RP top#bottom p/top/bot/free/scan ");
          PutAddr(p); PutText("/");
          PutAddr(Top[src]); PutText("/");
          PutAddr(Bottom[src]); PutText("/");
          PutAddr(Free[src]); PutText("/");
          PutAddr(Scan[src]); PutCR();Flush();
          RTOS.Crash();
        END;
      END;

      IF p = Bottom[src] THEN
        (* we're extracting Bottom, move bottom pointer up *)
        Bottom[src] := LOOPHOLE(Word.Or(Bottom[src].ptrs.prev, DescMask), 
                                RefDescriptor);
      END;

      IF p=Scan[src] THEN 
        IF NOT Implicit AND p.header.gcStatus # GcStatus.Untraced THEN
          (* XXX This test is not necessary *)
          PutText("GC ERROR>> scan must be untraced! \n");
          RTOS.Crash();
        END;
        IF p # Top[src] THEN
          PutText("How can p be Scan AND NOT TOP at the same time???\n");
          RTOS.Crash();
        END;

        Scan[src] := Bottom[src];
      END;
      IF p = Top[src] THEN
        Top[src] := Bottom[src]; 
      END;

      IF NOT Implicit AND (p.header.gcStatus # GcStatus.Untraced ) THEN
        PutText("GC ERROR >> RP: live object on freepage. list/page/obj ");
        PutInt(src); PutText("/");
        PutAddr(srcPageAddr); PutText("/");
        PutAddr(p); PutCR(); Flush();
        RTOS.Crash();
      END;

      (* unlink the element *)
      LOOPHOLE(Word.Or(p.ptrs.prev, DescMask), RefDescriptor).ptrs.next :=
                 p.ptrs.next;
      LOOPHOLE(Word.Or(p.ptrs.next, DescMask), RefDescriptor).ptrs.prev :=
                 p.ptrs.prev;
      (* move onto next element on this page *)
      INC(p, p.header.size);
    END;
    RETURN srcPageAddr;
  END RemovePage;


(*-------------------------------------------------- low-level collection
   ---*)

VAR 
  needMoreRecursion: BOOLEAN;
  ReserveCollection: BOOLEAN;


(* returns the header of the object pointed to by a *)
PROCEDURE LocateHeaderOf (a: ADDRESS): RefHeader =
  VAR 
    h: RefDescriptor;
    page : INTEGER;
  BEGIN
    IF AllocateAligned THEN
      (* look for page.  if it is continued then move backwards until 
         we find a page that is not continued *)
      page := Word.RightShift(LOOPHOLE(a, INTEGER), LogBytesPerPage);

      IF desc[page-p0].space = Space.Unallocated THEN
        (* assume it's a false ambiguous ptr and return *)
        RETURN NIL;
      END;

      (* the following line works because the ONLY time you have a referent on
         the page boundary is if you are on a continued page *)
      (*
      IF LOOPHOLE(a, INTEGER) MOD BytesPerPage = 0 THEN

        IF NOT desc[page-p0].continued THEN
          (* this means that we're at the head of an object...which 
             is impossible, but return it anyways just to be safe *)
          RETURN a + PtrSize; 
        END;

        DEC(page);
      END;
      *)
      WHILE desc[page-p0].continued DO
        DEC(page);
      END;
      (* we've found the page *)
      h := LOOPHOLE(Word.LeftShift(page, LogBytesPerPage), ADDRESS);

      WHILE h + h.header.size < a DO h := h + h.header.size; END;
      RETURN h + PtrSize;
    ELSE
      (* slower method that doesn't assume things are page aligned,
         and assumes that the only well-known headers are the heads
         of the free lists *)
      FOR i := 1 TO NumFreeLists DO
        IF a < FreeListLocations[i] THEN
          h := FreeListLocations[i - 1];
          WHILE h + h.header.size < a DO h := h + h.header.size; END;
          RETURN h + PtrSize;
        END;
      END;
    END;

    RETURN NIL;
  END LocateHeaderOf;



TYPE
  Marker = RTHeapMap.Visitor OBJECT
             depth: INTEGER := 0;
           OVERRIDES
             apply := MarkReferent;
           END;



PROCEDURE MarkReferent (self: Marker; a: ADDRESS) =
  (* REQUIRES 'a' is a pointer to a location that contains a pointer to a
     traced variable in the heap *)
  (* Mark the header of the referent of 'a' and recursively mark the
     headers of all variables to which the referent contains a pointer.
     'depth' indicates the number of levels of references that may be
     recursively marked. *)
  VAR
    refref            := LOOPHOLE(a, UNTRACED REF ADDRESS);
    ref               := refref^;
    h     : RefDescriptor;
  BEGIN
    IF ref = NIL THEN RETURN; END;
    h := ref - Offset;
    IF h < MEMORY_Start OR h >=MEMORY_End THEN
      IF h.header.typecode #1 THEN
        PutText("not text and outside memory!!!");
        PutAddr(h-PtrSize); PutCR();Flush();
        RTOS.Crash();
      END;
      RETURN;
    END;
    IF h.header.gcStatus # BlackStatus THEN
      Mark(h, self);
    END;
  END MarkReferent;


(* inserts an object into the gray list, to be scanned *)
(* can change this to make it depth or breadth first...right now it's 
   depth first because it inserts at the head of the queue. *)
(* assumes the object is white.  Check outside this procedure to avoid
   the spurious procedure call into here during collection *)
PROCEDURE MakeGray(d : RefDescriptor) =
  VAR
    prev : RefDescriptor;
    next : RefDescriptor;
    list : INTEGER;
    dptr : Ptr;
  BEGIN
    IF MeasureMarking THEN
      INC(MarkBytes, d.header.size);
      INC(MarkRef);
    END;
    (*
    IF NOT Remapping AND d >= FreeListLocations[RP] THEN

      (* this is a hack to address the problem where there is somehow
         a traced reference to a single page in the reservepool.
         There should not be references to single pages (which only
         form because the reserve pool was fragmented, so single pages
         are currently always unused), which implies that this is
         ambiguous.  I'm not convinced of this.
      *)

      list := RP;
    ELSE
      list := GetList(d.header.size);
    END;
    *)
      list := GetList(d.header.size);
    INC(ObjsMarked);

    (* XXX adds overhead! *)
    IF RTHeapStats.ReportFragmentation THEN
      INC(CountedBytes, ReferentSize(d+PtrSize));
    END;
    IF list=RP THEN
      (* extract me *)
      IF d = Top[list] THEN
        (* empty, advance TOP *)
        Top[list] := PtrToDesc(Top[list].ptrs.next);
        IF Top[list] # NIL THEN
          Top[list].ptrs.prev := 0;
        END;
      ELSE
        prev := PtrToDesc(d.ptrs.prev);
        prev.ptrs.next := d.ptrs.next;
        IF d.ptrs.next # 0 THEN
          PtrToDesc(d.ptrs.next).ptrs.prev := d.ptrs.prev;
        END;
      END;

      (* move into gray list, i.e. after SCAN *)
      IF Scan[list] = NIL THEN
        Scan[list] := d;
        d.ptrs.next := 0;
        d.ptrs.prev := 0;
      ELSE (* FOR BFS Put at end *)
        d.ptrs.prev :=0;
        d.ptrs.next := Word.And(LOOPHOLE(Scan[list], Word.T), PtrMask); 
        Scan[list].ptrs.prev := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
        Scan[list] := d;
      END;

      RETURN;
    END;
    INC(BytesLive[list],NodeSizes[list]);
    INC(LiveObjects[list] );
    (* put me between SCAN and TOP *)

    (* splice d out *)
    (* move top back if i'm moving what it points to *)
    IF d = Top[list] THEN
      (* XXX this does not do DFS, but it is faster than moving to
         head of queue *)
      IF Top[list] = Bottom[list] THEN
        (* this is an error because Top is being marked but that
           means something in the free list is getting marked     *)
        PutText("GC ERROR >>> MG  : Top = bottom! list/top/gcstatus ");
        PutInt(list); PutText("/");
        PutAddr(Top[list]); PutText("/");
        PutText(GcS[d.header.gcStatus]); PutCR();
        RTOS.Crash();
      END;
      Top[list] := LOOPHOLE(Word.Or(d.ptrs.prev, DescMask), RefDescriptor);

      (* if scan was empty, it would be pointing at d right now *)
      (* if scan wasn't empty, then it doesn't matter, it will hit d later *)

      RETURN;
    END;

    IF d = Scan[list] THEN
      (* moving this guy might be a bad idea, he's already gray *)

      RETURN;
    END;


    (* precompute the ptr of d *)
    dptr := Word.And(LOOPHOLE(d, Word.T), PtrMask); 

    prev := LOOPHOLE(Word.Or(d.ptrs.prev, DescMask), RefDescriptor);
    next := LOOPHOLE(Word.Or(d.ptrs.next, DescMask), RefDescriptor);
    
    (*
    IF LOOPHOLE(prev,INTEGER) = DescMask OR LOOPHOLE(next,INTEGER) = DescMask THEN
      PutText("freelist ptr nil.  list/typecode/size/ptr/top/free/bottom/scan\n");
      PutInt(list);PutText("/");
      PutInt(d.header.typecode);PutText("/");
      PutInt(d.header.size);PutText("/");
      PutAddr(Top[list]);PutText("/");
      PutAddr(Free[list]);PutText("/");
      PutAddr(Bottom[list]);PutText("/");
      PutAddr(Scan[list]);
      PutCR();Flush();
      RTOS.Crash();
    END;
    *)
    prev.ptrs.next := d.ptrs.next;
    next.ptrs.prev := d.ptrs.prev;
      
    IF Scan[list] # Top[list] THEN
      (* not empty *)
      (* insert before SCAN*)
      d.ptrs.prev := Scan[list].ptrs.prev;
      LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask),RefDescriptor).ptrs.next
                                     := dptr;

      Scan[list].ptrs.prev := dptr;
      d.ptrs.next := Word.And(LOOPHOLE(Scan[list], Word.T), PtrMask); 
    ELSE
      (* empty *)
      (* insert after TOP *)
      Scan[list] := d;
      d.ptrs.next := Top[list].ptrs.next;
      LOOPHOLE(Word.Or(Top[list].ptrs.next, DescMask),RefDescriptor).ptrs.prev
                         := dptr;
      d.ptrs.prev := Word.And(LOOPHOLE(Top[list], Word.T), PtrMask); 
      Top[list].ptrs.next := dptr;
    END;

  END MakeGray;


(* Like MakeGray, assumes the object being pointed to is not already black. 
*)
PROCEDURE MakeBlack(d : RefDescriptor) =
  VAR
    prev : RefDescriptor;
    next : RefDescriptor;
    list : INTEGER;
    page,p : Page;
  BEGIN
    IF d.header.gcStatus = BlackStatus THEN
      RETURN;
    END;
    (*
    IF NOT Remapping AND d >= FreeListLocations[RP] THEN

      (* this is a hack to address the problem where there is somehow
         a traced reference to a single page in the reservepool.
         There should not be references to single pages (which only
         form because the reserve pool was fragmented, so single pages
         are currently always unused), which implies that this is
         ambiguous.  I'm not convinced of this.
      *)

      list := RP;
    ELSE
      list := GetList(d.header.size);
    END;
    *)
    list := GetList(d.header.size);
    IF list=RP THEN
      (* extract me *)
      (* move into black list, i.e. after SCAN *)
      
      IF Bottom[list] = NIL THEN
        Bottom[list] := d;
        d.ptrs.next := 0;
        d.ptrs.prev := 0;
      ELSE
        d.ptrs.prev :=0;
        d.ptrs.next := Word.And(LOOPHOLE(Bottom[list], Word.T), PtrMask); 
        Bottom[list].ptrs.prev := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
        Bottom[list] := d;
      END;
      RETURN;
    END;


    IF d = Top[list] THEN
      (* have to advance the Top ptr*)
      Top[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask),
                            RefDescriptor); 
       (* continue to make the object black *)
    END;

    IF d = Scan[list] THEN
      IF ReserveCollection THEN
        (* we have to manually advance SCAN because this object
           is being blackened by the reservepool marking loop
        *)
        Scan[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask),
                               RefDescriptor);
      ELSE
        RETURN; (* it will get blackened upon leaving this call *)
      END;
    END;


    (* splice d out *)
    prev := LOOPHOLE(Word.Or(d.ptrs.prev, DescMask), RefDescriptor);
    next := LOOPHOLE(Word.Or(d.ptrs.next, DescMask), RefDescriptor);
    prev.ptrs.next := d.ptrs.next; 
    next.ptrs.prev := d.ptrs.prev;
    (* insert after SCAN *)
    d.ptrs.prev := Word.And(LOOPHOLE(Scan[list], Word.T), PtrMask); 
    d.ptrs.next := Scan[list].ptrs.next;
    Scan[list].ptrs.next := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
    LOOPHOLE(Word.Or(d.ptrs.next, DescMask), RefDescriptor).ptrs.prev 
                     := Scan[list].ptrs.next;

    (* mark the page *)
    page := Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage);
    p := page-p0;
    IF desc[p].space # Space.Previous THEN
      AddPageToBlack(page-p0, page, list);
    END;
  END MakeBlack;


(* this routine is only used during allocation during incremental collection.
   d has no pointers, so blacken it immediately.  This may be a little
   conservative if the thing allocated is being assigned to a local variable.
*)
<* UNUSED *>
PROCEDURE MakeBlackFromWhite(d : RefDescriptor) =
  VAR
    prev : RefDescriptor;
    next : RefDescriptor;
    list : INTEGER;
    page,p : Page;
  BEGIN
    FreeListSanityCheck("MBFW BEFORE");
    IF d.header.gcStatus = BlackStatus THEN
      RETURN;
    END;
    IF NOT Remapping AND d >= FreeListLocations[RP] THEN

      (* this is a hack to address the problem where there is somehow
         a traced reference to a single page in the reservepool.
         There should not be references to single pages (which only
         form because the reserve pool was fragmented, so single pages
         are currently always unused), which implies that this is
         ambiguous.  I'm not convinced of this.
      *)

      list := RP;
    ELSE
      list := GetList(d.header.size);
    END;

    IF list=RP THEN
      (* extract me *)
      IF d = Top[list] THEN
        (* empty, advance TOP *)
        Top[list] := PtrToDesc(Top[list].ptrs.next);
        IF Top[list] # NIL THEN
          Top[list].ptrs.prev := 0;
        END;
      ELSE
        prev := PtrToDesc(d.ptrs.prev);
        prev.ptrs.next := d.ptrs.next;
        IF d.ptrs.next # 0 THEN
          PtrToDesc(d.ptrs.next).ptrs.prev := d.ptrs.prev;
        END;
      END;

      (* move into black list, i.e. after SCAN *)
      
      IF Bottom[list] = NIL THEN
        Bottom[list] := d;
        d.ptrs.next := 0;
        d.ptrs.prev := 0;
      ELSE
        d.ptrs.prev :=0;
        d.ptrs.next := Word.And(LOOPHOLE(Bottom[list], Word.T), PtrMask); 
        Bottom[list].ptrs.prev := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
        Bottom[list] := d;
      END;
      RETURN;
    END;


    IF d = Scan[list] THEN
      IF ReserveCollection THEN
        (* we have to manually advance SCAN because this object
           is being blackened by the reservepool marking loop
        *)

        PutText("MBFW : NOT POSSIBLE1");
        RTOS.Crash();
        Scan[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask),
                               RefDescriptor);
      ELSE
        (* d = scan and we're not collecting from res pool *)
        (* possible if Scan is empty...move it back *)
        IF Top[list] # Scan[list] THEN
          PutText("MBFW : TOP # SCAN!!!!\n");
          RTOS.Crash();
        END;
        IF d = Top[list] THEN
          (* have to advance the Top ptr*)
          Top[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask),
                                RefDescriptor); 
          (* continue to make the object black *)
        END;
        Scan[list] := Top[list];
        page := Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage);
        p := page-p0;

        IF desc[p].space # Space.Previous THEN
          AddPageToBlack(page-p0, page, list);
        END;
        INC(BytesLive[list],NodeSizes[list]);
        INC(LiveObjects[list] );
        
        (* we're in front of scan, so we're black *)
        RETURN; (* it will get blackened upon leaving this call *)
      END;
    END;
    IF d = Top[list] THEN
      (* have to advance the Top ptr*)
      Top[list] := LOOPHOLE(Word.Or(Top[list].ptrs.prev, DescMask),
                            RefDescriptor); 
      (* continue to make the object black *)
    END;
    
    IF d = Bottom[list] THEN
      PutText("makeblackfromwhite - can't be free?!\n");Flush();
      RTOS.Crash();
    END;
    (* splice d out *)
    prev := LOOPHOLE(Word.Or(d.ptrs.prev, DescMask), RefDescriptor);
    next := LOOPHOLE(Word.Or(d.ptrs.next, DescMask), RefDescriptor);
    prev.ptrs.next := d.ptrs.next; 
    next.ptrs.prev := d.ptrs.prev;
    (* insert after SCAN *)
    d.ptrs.prev := Word.And(LOOPHOLE(Scan[list], Word.T), PtrMask); 
    d.ptrs.next := Scan[list].ptrs.next;
    Scan[list].ptrs.next := Word.And(LOOPHOLE(d, Word.T), PtrMask); 
    LOOPHOLE(Word.Or(d.ptrs.next, DescMask), RefDescriptor).ptrs.prev 
                     := Scan[list].ptrs.next;

    (* mark the page *)
    page := Word.RightShift(LOOPHOLE(d,INTEGER), LogBytesPerPage);
    p := page-p0;

    IF desc[p].space # Space.Previous THEN
      AddPageToBlack(page-p0, page, list);
    END;
    INC(BytesLive[list],NodeSizes[list]);
    INC(LiveObjects[list] );
  END MakeBlackFromWhite;

PROCEDURE Mark (h: RefDescriptor; marker: Marker) =
  BEGIN
    (* ASSERT h.header.gcStatus # GcStatus.Untraced *)
    IF NOT Implicit AND h.header.gcStatus = GcStatus.Untraced THEN
      PutText("GC ERROR >> Mark:UNTRACED d/size/tc/tcfree ");
      PutAddr(h); PutText("/");
      PutInt(h.header.size);  
      PutText("/");
      PutInt(h.header.typecode);
      PutText("/");
      PutInt(TcFree);
      PutCR();
      Flush(); RTOS.Crash();
    END;
    IF (h.header.typecode <= 0 OR h.header.typecode >= RT0u.nTypes) THEN
      PutText("GC ERROR >> Mark:ILLEGAL TYPECODE d/typecode ");
      PutAddr(h-PtrSize); PutText("/");
      PutInt(h.header.typecode);  PutCR();
      Flush(); RTOS.Crash();
    END;
    IF (h < MEMORY_Start OR h >= MEMORY_End) THEN
      IF h.header.typecode # 1 THEN
        PutText("GC ERROR >> Mark:h not in MEMORY h/lastdescriptor/lastref/typecode ");
        PutAddr(h-PtrSize);  PutText("/");
        PutInt(h.header.typecode); PutCR();
        Flush(); RTOS.Crash();
      ELSE
        RETURN; (* don't want to add the kernel texts into the heap *)
      END;
    END;
    
    

    IF h.header.gcStatus # BlackStatus THEN
      IF marker.depth = 0 THEN
        IF h.header.gcStatus = WhiteStatus THEN
          (* check if we need to scan it.  if not, just make it
             black *)
          (*
          IF Incremental THEN
            
            VAR
              td : RT0.TypeDefn := RTType.Get(h.header.typecode);
            BEGIN

              IF td.gc_map = NIL AND td.parent = NIL THEN
                MakeBlackFromWhite(h);
                h.header.gcStatus := BlackStatus;
                needMoreRecursion := TRUE;
                INC(BytesMarked,h.header.size);
                RETURN;
              ELSE
                MakeGray(h); 
                h.header.gcStatus := GrayStatus;
                needMoreRecursion := TRUE;
                RETURN;
              END;
            END;
          END;
          *)
          MakeGray(h); 
          h.header.gcStatus := GrayStatus;
          needMoreRecursion := TRUE;
        END;
        RETURN;
      END;
      (* not yet depth 0, scan the object *)

      IF ReserveCollection THEN
        MakeBlack(h);
      END; (* otherwise MarkThrough will move Scan forward 
              to implicitly blacken the thing *)

      h.header.gcStatus := BlackStatus;

      (* this object is marked, mark what it points to *)
      (*Spy.Enter(MTWalkRef);*)

      IF MeasureMarking THEN
        EnterMG();
      END;


      DEC(marker.depth);
      RTHeapMap.WalkRef(h+PtrSize, marker);
      INC(marker.depth);

      IF MeasureMarking THEN
        ExitMG();
      END;


      (*Spy.Exit(MTWalkRef);*)
      (* h is black. whee. *)
    END;
  END Mark;


(* iterate on the gray list (stuff between SCAN and TOP) 
   scanning and marking those objects until all gray lists are
   empty *)
PROCEDURE MarkThrough () =
  VAR 
    h: RefDescriptor;
    page : Page;(* page of the markee *)
    p : Page;    (* page index *)
  BEGIN
    LOOP
      (*Spy.Enter(MTFL);*)
      needMoreRecursion := FALSE;
      FOR list := 0 TO NumFreeLists-2 DO
        WHILE Scan[list] # Top[list] DO
          theMarker.depth := 1;
          Mark(Scan[list], theMarker);

          (*----- check if the page needs to be moved ------ *)
          page := Word.RightShift(LOOPHOLE(Scan[list], INTEGER),
                                         LogBytesPerPage);
          p := page - p0;
          IF desc[p].space # Space.Previous THEN
            AddPageToBlack(p,page, list);
          END;
          (* implicit make-black *)
          Scan[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask), 
                                 RefDescriptor);
        END;
      END;
      (*Spy.Exit(MTFL);*)
      (*Spy.Enter(MTRP);*)
      (* --------- Reclaim reserve pool -----------*)
      ReserveCollection := TRUE;
      h := Scan[RP];
      WHILE h # NIL DO
        Scan[RP] := PtrToDesc(Scan[RP].ptrs.next);
        IF Scan[RP] # NIL THEN
          Scan[RP].ptrs.prev := 0;
        END;
        
        IF h.header.size = 0 THEN
          PutText("GC ERROR >> zero size object found during marking at ");
          PutAddr(Scan[RP]); PutCR(); RTOS.Crash();
        END;
        theMarker.depth := 1; 
        Mark(h , theMarker);
        h := Scan[RP];        
      END;
      Scan[RP] := NIL;
      ReserveCollection := FALSE;
      (*Spy.Exit(MTRP);*)

      IF NOT needMoreRecursion THEN EXIT END

    END;
  END MarkThrough;

VAR
  StackBytes : INTEGER;
  MFS : Spy.T;

PROCEDURE MarkFromStacks (start, stop: ADDRESS) =
  (* Marks (does not recursively mark) traced variables for which there
     exists a potential reference to in a stack.  A word on the stack is
     assumed to be a reference to a variable if it contains the value of
     any of the addresses occupied by the variable.  Since this procedure
     does not recursively mark (to conserve stack space, as this procedure
     puts a large array on its stack) the variables, procedure MarkThrough
     needs to be run after this procedures completion. *)

  VAR
    fp            := start;
    p : ADDRESS;
    h : RefHeader;
  BEGIN
    INC(StackBytes, LOOPHOLE(stop-start,INTEGER));
    (*Spy.Enter(MFS);*)
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF p < MEMORY_Start OR p >= MEMORY_End THEN
        INC(fp, RTMachine.PointerAlignment);
      ELSE
        h := LocateHeaderOf(p);
        IF h # NIL AND h.typecode # TcFree AND h.gcStatus = WhiteStatus THEN
          MakeGray(h-PtrSize);
          h.gcStatus := GrayStatus;
        END;
        INC(fp, RTMachine.PointerAlignment);
      END;
    END;
    (*Spy.Exit(MFS);*)
  END MarkFromStacks;


PROCEDURE RecycleGarbage (): BOOLEAN =
  VAR
    h,hnext: RefDescriptor;
    recycledAny: BOOLEAN := FALSE;
  BEGIN
    (*FreeListSanityCheck("recycle garbage start");*)
    Spy.Enter(RecycleTimer);
    h := Top[RP];
    IF Scan[RP] # NIL THEN
      PutText("didn't finish scanning resrevepool!!!\n");
      RTOS.Crash();
    END;

    WHILE h # NIL DO
      (* need to remember next ptr since the ptrs may change after 
         returnmemtoreserve due to coalescing *)
      hnext := PtrToDesc(h.ptrs.next); 
      IF h.header.gcStatus # WhiteStatus THEN
        PutText("GC ERROR >> reserve pool has non white obj in Top. h/curr/stat ");
        PutAddr(h); PutText("/");
        PutText(GcS[WhiteStatus]);  PutText("/");
        PutText(GcS[h.header.gcStatus]);   PutCR();
        Flush();    RTOS.Crash();
      END;
      ReturnMemToReserve(h);
      recycledAny := TRUE;
      h := hnext;
    END;

    (* swap spaces *)
    Top[RP] := Bottom[RP];
    Bottom[RP] := NIL;

    (* reclaim memory from each free list *)
    Spy.Enter(ReclaimTimer);
    FOR i := 0 TO NumFreeLists-2 DO
      EVAL  ReclaimAndFlip(i);
    END;
    Spy.Exit(ReclaimTimer);

    (* flip meaning of white and black *)
    IF WhiteStatus = GcStatus.NotMarked THEN
      WhiteStatus := GcStatus.RecursivelyMarked;
      BlackStatus := GcStatus.NotMarked;
    ELSE
      WhiteStatus := GcStatus.NotMarked;
      BlackStatus := GcStatus.RecursivelyMarked;
    END;
    Spy.Exit(RecycleTimer);
    RETURN recycledAny;
  END RecycleGarbage;

(* Perform a full collection - used for non incremental case *)
PROCEDURE CollectInternal () =
  VAR
  BEGIN
    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes, TRUE,FALSE);

    END;
    CountedBytes := 0;
    IF Incremental AND COLLECTING # 0 THEN
      CollectFinish();
      RETURN;
    END;
    


    (* uncomment this to use incremental code even if 
       we're not incremental *)
    (*
    CollectStart();
    WHILE NOT CollectSome() DO
    END;
    CollectFinish();
    RETURN;
    *)

    FOR  i := 0 TO RP DO
      BytesLive[i] := 0;
    END;

    INC(NumCollections);
    FreeListSanityCheck("collect start");
    COLLECTING := 1;
    IF DoTimings THEN Spy.Enter(CollectionTimer); END;
    ThreadF.SuspendOthers();

    (* init live objects stats so we know how free the lists are *)
    (* increment in MakeBlack *)
    FOR i := 0 TO NumFreeLists-2 DO
      LiveObjects[i] := 0;
    END;

    (* init scan ptrs *)
    FOR i := 0 TO NumFreeLists-2 DO
      Scan[i] := Top[i]; (* nothing gray *)
    END;
    
  
    IF TMVerbose > 1 THEN
      PutText("C:SS\n");Flush();
    END;


    IF DoTimings THEN
      Spy.Enter(StackScanTimer);
    END;
    ThreadF.ProcessStacks(MarkFromStacks);
    IF DoTimings THEN
      Spy.Exit(StackScanTimer);
    END;

    IF TMVerbose > 1 THEN
      PutText("C:SR\n");Flush();
    END;


    IF DoTimings THEN
      Spy.Enter(StrongRefTimer);
    END;
    RTStrongRef.ProcessRefs(MarkFromStacks);
    IF DoTimings THEN
      Spy.Exit(StrongRefTimer);
    END;
    IF TMVerbose > 1 THEN
      PutText("C:G\n");Flush();
    END;


    theMarker.depth := 0;
    IF DoTimings THEN
      Spy.Enter(GlobalTimer);
    END;
    RTHeapMap.WalkGlobals(theMarker);
    IF DoTimings THEN
      Spy.Exit(GlobalTimer);
    END;

    IF TMVerbose > 1 THEN
      PutText("C:MT\n");Flush();
    END;

    IF DoTimings THEN
      Spy.Enter(MarkTimer);
    END;
    MarkThrough();
    IF DoTimings THEN
      Spy.Exit(MarkTimer);
    END;
    IF TMVerbose > 1 THEN
      PutText("C:RG\n");Flush();
    END;
    IF NOT RecycleGarbage() THEN
      (* this means that nothing was returned to the reserve pool *)
      (*PutText("Collect: RecycleGarbage returned FALSE\n");*)
    END;


    UpdateStatsEOC();
    Spy.Exit(CollectionTimer); 
    ThreadF.ResumeOthers();

    IF verbose >= 1 THEN
      PutText("Collect: done\n");
      Flush();
    END;
    COLLECTING := 0;
    FreeListSanityCheck("Collect");
    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes, FALSE,TRUE);
    END;
    RETURN;
  END CollectInternal;




(* -------------------------------------------------- Incremental --------- *)

(* Incremental collection stages
   - Start 
     - scan roots atomically
     - compute ToMark based on BytesAlloced, BytesMarked and free memory 
     - cap max objects to be marked by measuring how many objs it takes to do 50 ms 
       work
   - CollectSome
     - scan ToMark elements, and finish collection if possible
   - FinishCollection
     - atomically recycle garbage.
*)

CONST
  GcRatio = 8; 
  RatioPower = 3;


(* this will maintain how many pages will be needed to ensure we
   finish collection in time*)

(* incrementally modify the value as follows : whenever GcRatio pages
   are unfreed, add 1 page to PagesNeeded *)

VAR 
  PagesNeeded : INTEGER;
  NextIncrement : INTEGER;

  (* This is how much memory we need in order to guarantee finishing
     collection in time.  
     recompute at end of collection, and update
     with every allocation thereafter.  *)
  (* this is the value "N" in TriggerCollection *)
  (* when totalfreepages * 8192 <= Bufferzone, start the collection *)
  BufferZone : INTEGER;

  (* amount to change the bufferzone by given an allocation
     from a list.  precomputed so we don't do a division every time *)
  TriggerIncrement : ARRAY [0 .. NumFreeLists - 1] OF INTEGER;
  (* ditto, except this is the amount to modify the bufferzone 
     in the case of a balanced allocation *)
  TriggerBalancedIncrement : ARRAY [0 .. NumFreeLists - 1] OF INTEGER;


PROCEDURE InitPolicy () =
  VAR
  BEGIN
      PagesNeeded := TotalFreePages DIV GcRatio;
      NextIncrement := 0;

      (* precompute the increments to BufferZone that we will incur *)
      (* doesn't apply to reserve pool allocations *)
      (* XXX assumes GcRatio doesn't change *)
      FOR i := 0 TO RP-1 DO
        TriggerIncrement[i] := NodeSizes[i] DIV GcRatio + NodeSizes[i];

        (* we gain a page in the list, so that increases internal
           fragmentation, and increases the amount of slack needed for the
           list.  Naturally, someone else will have lost a page  *)
        TriggerBalancedIncrement[i] := - BytesPerPage;
      END;
      (* something special must be done for list 8 (page size) as well because
         htere is no new  memory "preallocated" via internal fragmentation. *)
      TriggerBalancedIncrement[RP-1] := 0;
      (*TriggerIncrement[RP-1] := NodeSizes[i] DIV GcRatio;*)
      RecomputeBufferZone();

      (* XXX*) 
      BufferZone := 0;
  END InitPolicy;

<*UNUSED*>
PROCEDURE UpdateStatsAllocation (list : INTEGER)=
  VAR
  BEGIN
    IF Incremental THEN
      IF list # RP THEN
        INC(BufferZone,TriggerIncrement[list]);
      ELSE
        (* net change in TFP is just the size of the allocation.  change in
           BZ is the size of allocation / GcRatio *)
      END;
    END;
  END UpdateStatsAllocation;

<*UNUSED*>
PROCEDURE UpdateStatsBalance (list : INTEGER)=
  VAR
  BEGIN
    IF Incremental THEN
      IF list # RP THEN
        INC(BufferZone,TriggerBalancedIncrement[list]);
      END;
    END
  END UpdateStatsBalance;

PROCEDURE RecomputeBufferZone () =
  VAR
    A, F, N : INTEGER;
  BEGIN
    BufferZone := 0;
    FOR i := 0 TO RP DO
      (* compute Ax *)
      A := ListCaps[i] - BytesFree[i];
      (* compute Fx (fragged) *)
      F := BytesFree[i] - (BytesPerPage * FreePages[i]);
      (* compute N, extra mem needed *)
      N := ((A DIV GcRatio) - F);
      (* sum up free bytes needed *)
      INC(BufferZone,  N);
    END;
  END RecomputeBufferZone;

(*
  let x be a list
  
  Ax = allocated memory in the list(=total-free)
  Fx = free memory in the list
  Nx = Amount of memory needed to guarantee marking will complete
     = R * Ax
  (Nx-Fx)/pagesize = number of extra free pages needed to ensure marking will complete

  problem : Fx may change if pages are moved away...Fx is actually part of Free page list
  - just use FreePages[x]...all extra free elems is fragmentation.

  - need to update the ratio depending if TotalFreePages is dwindling faster 
  
  *)

(*FUNCTIONAL*)
<* UNUSED *>
PROCEDURE TriggerCollection () : BOOLEAN =
  VAR
  BEGIN
      
      RETURN BufferZone > BytesPerPage AND TotalFreePages <= Word.RightShift(BufferZone, LogBytesPerPage);
      
      IF TotalFreePages <= Word.RightShift(BufferZone, LogBytesPerPage) THEN
          PutText("Trigger : TFP/BZ ");
          PutInt(TotalFreePages);
          PutText("/");
          (*PutInt(Word.RightShift(BufferZone, LogBytesPerPage));*)
          PutInt(BufferZone DIV BytesPerPage);
          PutCR();
        RETURN TRUE;
      END;
      RETURN FALSE;

      (* if freepages < total live memory*gcratio then start collection *)
      VAR
        A, F, N, FP : INTEGER;
      BEGIN
        FP := 0;
        FOR i := 0 TO RP DO
          (* compute Ax *)
          A := ListCaps[i] - BytesFree[i];
          (* compute Fx (fragged) *)
          F := BytesFree[i] - (BytesPerPage * FreePages[i]);
          (* compute N, extra mem needed *)
          N := ((A DIV GcRatio) - F) DIV BytesPerPage;
          (* sum up free pages needed *)
          INC(FP,  N);
        END;
        IF TotalFreePages <= FP THEN
          PutText("Trigger : TFP/FP ");
          PutInt(TotalFreePages);
          PutText("/");
          PutInt(FP);
          PutCR();
        END;
        RETURN TotalFreePages <= FP;
      END;

    RTOS.Crash();
  END TriggerCollection;

(*FUNCTIONAL*)
<* UNUSED *>
 PROCEDURE Behind () : BOOLEAN =
  VAR
  BEGIN
    (* if bytesmarked *gcratio < bytes alloced *)
    RETURN BytesMarked  < Word.LeftShift(BytesAlloced, RatioPower);
  END Behind;

(* compute to mark based on  remaining memory *)
(* it's actually computed in collectsome, but we have some other stats to 
   set up*)

PROCEDURE ComputeToMark()=
  VAR
  BEGIN
    FOR i := 0 TO RP DO
      BytesLive[i] := 0;
      LiveObjects[i] := 0;
    END;
    (*ToMark := GcRatio;*)
    BytesAlloced := 0; (* so that we know how much was allocated since START of 
                          collection *)
  END ComputeToMark;


(* scan roots.  According to past experience this won't take more than 12 ms *)
PROCEDURE CollectStart() =
  VAR
  BEGIN
    (*hardclock();*)
    Spy.Enter(CollectStartTimer);

    ComputeToMark();
    BytesAlloced := 0; (* keep track of allocations during collection *)
    CountedBytes := 0;
    INC(NumCollections);
    FreeListSanityCheck("collect start");

    COLLECTING := 1;

    ThreadF.SuspendOthers();

    (* init scan ptrs *)
    FOR i := 0 TO NumFreeLists-2 DO
      Scan[i] := Top[i]; (* nothing gray *)
    END;

    (* scanning only the globals *)
    IF verbose > 0 THEN
      PutText("C:G\n");Flush();
    END;

    theMarker.depth := 0;
    IF DoTimings THEN
      Spy.Enter(GlobalTimer);
    END;
    RTHeapMap.WalkGlobals(theMarker);
    IF DoTimings THEN
      Spy.Exit(GlobalTimer);
    END;
    ThreadF.ResumeOthers();

    IF verbose > 0 THEN
      PutText("C:CS END\n");Flush();
    END;
    Spy.Exit(CollectStartTimer);
    (*FreeListSanityCheck("Collect start end");*)

    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes, TRUE,FALSE);
    END;
  END CollectStart;

<* UNUSED *>
PROCEDURE QuickSizeSanity (msg : TEXT) =
  VAR
  BEGIN
    FOR i := 0 TO NumFreeLists-2 DO
      IF Scan[i] # NIL THEN
      IF Scan[i].header.size # NodeSizes[i] THEN
        PutText ("QSS : ");
        PutText(msg);
        PutText(" list/size/nodesize ");
        PutInt(i);
        PutText("/");
        PutInt(Scan[i].header.size);
        PutText("/");
        PutInt(NodeSizes[i]);
        Flush();PutCR();
        RTOS.Crash();
      END;
      END;
    END;
  END QuickSizeSanity;


VAR
  ObjsMarked : INTEGER;
  MaxToMark  : INTEGER := 1500;

PROCEDURE CollectSome () : BOOLEAN =
  VAR
    h: RefDescriptor;
    page : Page;(* page of the markee *)
    p : Page;    (* page index *)
    listsDone:=0;(* # of lists that are completed *)
    i : INTEGER; (* # bytes marked so far *)
  BEGIN
    Spy.Enter(CollectSomeTimer);
    ObjsMarked := 0;
    i:=0;
    ToMark := BytesAlloced * GcRatio - BytesMarked;

    (* while we haven't marked ToMark bytes and 
       haven't hit object limit *)
    WHILE i < ToMark AND ObjsMarked < MaxToMark DO
      (*FOR i := 0 TO ToMark DO*)
      (* XXX stolen from markthrough..don't want additional proc call overhead *)
      needMoreRecursion := FALSE;
      listsDone := 0;
      FOR list := 0 TO NumFreeLists-2 DO
        IF Scan[list] # Top[list] THEN
          IF Scan[list].header.gcStatus # GrayStatus THEN
            PutText("GC ERROR >> CS : Not marked!!!!! ");
            PutAddr(Scan[list]);
            PutText(GcS[Scan[list].header.gcStatus]);
            PutCR();Flush();
            WhereTheHell(Scan[list]);

            (* let's check the next/prev ptrs  *)
            PutText("prev/size/gcstatus : ");
            PutAddr(PtrToDesc(Scan[list].ptrs.prev));
            PutText("/");
            PutInt(PtrToDesc(Scan[list].ptrs.prev).header.size);
            PutText("/");
            PutText(GcS[PtrToDesc(Scan[list].ptrs.prev).header.gcStatus]);

            PutCR();

            PutText("next/size/gcstatus : ");
            PutAddr(PtrToDesc(Scan[list].ptrs.next));
            PutText("/");
            PutInt(PtrToDesc(Scan[list].ptrs.next).header.size);
            PutText("/");
            PutText(GcS[PtrToDesc(Scan[list].ptrs.next).header.gcStatus]);

            PutCR();
            Flush();
            RTOS.Crash();
          END;
          theMarker.depth := 1;
          Mark(Scan[list], theMarker);

          (*----- check if the page needs to be moved ------ *)
          page := Word.RightShift(LOOPHOLE(Scan[list], INTEGER),
                                         LogBytesPerPage);
          p := page - p0;
          IF desc[p].space # Space.Previous THEN
            AddPageToBlack(p,page, list);
          END;

          IF Scan[list].header.gcStatus # BlackStatus THEN
            PutText("GC ERROR >> collectsome scanptr wasn't scanned!!!\n");
            Flush();
          END;
          INC(BytesMarked,Scan[list].header.size);
          (* implicit make-black *)
          Scan[list] := LOOPHOLE(Word.Or(Scan[list].ptrs.prev, DescMask), 
                                 RefDescriptor);
          INC(i, NodeSizes[list]);
          INC(ObjsMarked);
        ELSE
          INC (listsDone);
        END;
      END;
      (*Spy.Exit(MTFL);*)
      (*Spy.Enter(MTRP);*)
      (* --------- Reclaim reserve pool -----------*)
      ReserveCollection := TRUE;
      h := Scan[RP];

      IF h  # NIL THEN
        Scan[RP] := PtrToDesc(Scan[RP].ptrs.next);
        IF Scan[RP] # NIL THEN
          Scan[RP].ptrs.prev := 0;
        END;
        IF h.header.gcStatus # GrayStatus THEN
          PutText("GC ERROR >> CollectSome reserve pool,not marked page h/curr/stat");
          PutAddr(Scan[RP]); PutText("/");
          PutText(GcS[WhiteStatus]); PutText("/");
          PutText(GcS[Scan[RP].header.gcStatus]);
          PutCR();Flush(); RTOS.Crash();
        END;
        IF h.header.size = 0 THEN
          PutText("GC ERROR >> CS: zero size object found during marking at ");
          PutAddr(Scan[RP]); PutCR(); RTOS.Crash();
        END;
        theMarker.depth := 1; 
        Mark(h , theMarker);
        INC(i, h.header.size);
        INC(ObjsMarked);
        INC(BytesMarked,h.header.size);
      ELSE
        Scan[RP] := NIL;
        INC(listsDone);

      END;
      ReserveCollection := FALSE;
      IF listsDone = NumFreeLists THEN EXIT END;
    END;

    Spy.Exit(CollectSomeTimer);
    (*FreeListSanityCheck("Collect some end");*)

    RETURN NOT needMoreRecursion AND listsDone = NumFreeLists;
  END CollectSome;


(* if not done marking, finish that.
   recycle garbage
   sweep pages

   compute next watermark based on free memory!
   
   XXX must be careful to ensure that gray lists are empty!
*)
PROCEDURE CollectFinish () =
  VAR
  BEGIN

    RTOS.LockHeap();
    (*hardclock();*)
    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes, FALSE,TRUE);
    END;
    ThreadF.SuspendOthers();
    Spy.Enter(CollectFinishTimer);


    (* now we scan the frigging stacks *)
    IF DoTimings THEN
      Spy.Enter(StackScanTimer);
    END;

    (* make sure we do so incrementally *)
    (* we can do this better if we knew how many threads there were,
       avoiding the need to go through the entire list *)
    IF NOT ThreadF.RemoveDeadThreads(MaxThreads) THEN
      (* this does not reset Finish flag, which means we'll return
         here to finish the job on the next allocation *)
      Spy.Exit(StackScanTimer);
      Spy.Exit(CollectFinishTimer);
      RETURN;
    END;

    ThreadF.ProcessStacks(MarkFromStacks);
    IF DoTimings THEN
      Spy.Exit(StackScanTimer);
    END;
    IF TMVerbose > 1 THEN
      PutText("C:SR\n");Flush();
    END;


    IF DoTimings THEN
      Spy.Enter(StrongRefTimer);
    END;
    RTStrongRef.ProcessRefs(MarkFromStacks);
    IF DoTimings THEN
      Spy.Exit(StrongRefTimer);
    END;

    Spy.Enter(CollectRescanTimer);
    MarkThrough();
    Spy.Exit(CollectRescanTimer);

    EVAL RecycleGarbage();
    UpdateStatsEOC();
    COLLECTING := 0;
    FreeListSanityCheck("Collect Finish");

    Spy.Exit(CollectFinishTimer);

    (* uncomment to do a non-incremental collection...this 
       will make sure we didn't collect something we shouldn't have *)
    (*
    PutText("COLLECTINTERNAL FOR Sanity : ");
    CollectInternal();
    *)

    IF RTHeapStats.ReportFragmentation AND RTHeapStats.FragOn THEN
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes);
    END;

    ThreadF.ResumeOthers();
    RTOS.UnlockHeap();
  END CollectFinish;

<* UNUSED *>
PROCEDURE  EndCollectionSanity() = 
  VAR
    p : RefDescriptor;
  BEGIN

    FOR i:= 0 TO NumFreeLists-2 DO
      (* all gray lists should be empty *)
      IF Scan[i] # NIL AND Scan[i] # Top[i] THEN
        PutText("ECS : scan not empty list ");PutInt(i);
        PutCR();Flush();
      END;
      (* all black lists should be, well, black *)
      p:=PtrToDesc(Top[i].ptrs.next);
      WHILE PtrToDesc(p.ptrs.next) # Free[i] DO
        IF p.header.gcStatus # BlackStatus THEN
          PutText("ECS : not black list/p ");
          PutInt(i);
          PutText("/");
          PutAddr(p);
          PutCR();
          Flush();
        END;
        p := PtrToDesc(p.ptrs.next);
      END;
      
    END;
  END EndCollectionSanity;

(*------------------------------------------------------------- fault ---*)

PROCEDURE Fault (<*UNUSED*> addr: ADDRESS): BOOLEAN =
  BEGIN
    PutText("GC ERROR >> fault in the treadmill collector\n");
    RETURN FALSE;
  END Fault;


(*------------------------------------------------------------- debugging
   ---*)

(* runs the selected sanity checks *)
PROCEDURE FreeListSanityCheck (msg:TEXT) =
  VAR
    elems : INTEGER;
    pages : INTEGER;
  BEGIN

    IF NOT FLSC THEN RETURN END;
    (*
    IF GetPage(RTHeapVM.GetEnd()) # p1 THEN
      PutText(msg);
      PutText(": End # p1 : p1/pagefromend ");
      PutInt(p1-p0);
      PutText("/");
      PutInt(GetPage(RTHeapVM.GetEnd())-p0);
      PutCR();
      Flush();RTOS.Crash();
    END;
    *)
    IF TotalFreePages # CountFreePages()THEN
        PutText(msg);
        PutText(": WRONG TOTALFREEPAGE COUNT maintained/cuounted ");
        PutInt(TotalFreePages);
        PutText("/");
        PutInt(CountFreePages());
        PutCR();
        RTOS.Crash();
    END;

    pages := 0;
    FOR i := 0 TO RP DO
      INC(pages,FreePages[i]);
    END;
    IF pages # TotalFreePages THEN
        PutText(msg);
        PutText(": SUM of FreePages # TotalFree sum/totalfree ");
        PutInt(pages);
        PutText("/");
        PutInt(TotalFreePages);

        PutCR();
        RTOS.Crash();
    END;      



    FOR i := 0 TO RP DO
      IF BytesFree[i] # CountFreeBytes(i) THEN
        PutText(msg);
        PutText(": WRONG FREE BYTE COUNT list/maintained/cuounted ");
        PutInt(i);
        PutText("/");
        PutInt(BytesFree[i]);
        PutText("/");
        PutInt(CountFreeBytes(i));
        PutCR();
        RTOS.Crash();
      END;
      IF ListCaps[i] # CountListCap(i) THEN        PutText(msg);
        PutText(": WRONG LISTCAP COUNT list/maintained/cuounted ");
        PutInt(i);
        PutText("/");
        PutInt(ListCaps[i]);
        PutText("/");
        PutInt(CountListCap(i));
        PutCR();
        RTOS.Crash();
      END;

    END;
    FOR i := 0 TO RP-1 DO
      elems := CountFree(i);
      IF ElemsFree[i] # elems THEN
        PutText(msg);
        PutText(": WRONG ELEM COUNT list/maintained/cuounted ");
        PutInt(i);
        PutText("/");
        PutInt(ElemsFree[i]);
        PutText("/");
        PutInt(elems);
        PutCR();
        RTOS.Crash();
      END;
    END;


    IF RPSC THEN
      ResSanity(msg);
    END;
    IF TMSanity THEN
      TreadMillSanityCheck(msg);
    END;
    IF PLSC THEN
      PageListSanityCheck(msg);
    END;
    IF DescSanity THEN
      CheckDescSanity(msg);
    END;
    
    IF NOT Remapping THEN
      (* this code doesn't work if there are gaps in the virtual
         address space *)
      EVAL CheckSanity(msg);
    END;
  END FreeListSanityCheck;


PROCEDURE CheckSanity (msg: TEXT): BOOLEAN =
  VAR
    h  : RefDescriptor := MEMORY_Start;
    cnt: INTEGER   := 0;
  PROCEDURE PutError (error: TEXT) =
    BEGIN
      PutText("GC ERROR >> ");
      PutText(msg); PutText(" : ");
      PutText(error); PutText("\ncnt "); PutInt(cnt);
      PutText(", addr "); PutAddr(h);
      PutText(", tc "); PutInt(h.header.typecode);
      PutText(", status "); PutInt(ORD(h.header.gcStatus));
      PutText(", size "); PutInt(h.header.size);
      PutText("\n"); Flush();
    END PutError;
  BEGIN
    WHILE h < MEMORY_End DO
      IF h.header.size < MinMemoryNodeSize THEN
        PutError("illegal size");
        RETURN FALSE;
      END;
      VAR tc := h.header.typecode;
      BEGIN
        IF (tc <= 0 OR tc >= RT0u.nTypes) AND tc # TcFree THEN
          PutError("illegal typecode");
          RETURN FALSE;
        END;
      END;
      IF h.header.gcStatus = GcStatus.Unused
           (*OR h.header.gcStatus = GcStatus.Untraced AND h.header.typecode # TcFree*) THEN
        PutError("illegal gcStatus");
        RETURN FALSE;
      END;
      h := h + h.header.size;
      INC(cnt);
    END;
    RETURN TRUE;
  END CheckSanity;


(* ensure page descriptors make sense *)
PROCEDURE CheckDescSanity(msg: TEXT) =
  VAR
    elemsOnPage : INTEGER;
    i  : INTEGER;
    d, next : RefDescriptor;
    prev : RefDescriptor ; (* for error reporting*)
  BEGIN

    i:= p0;
    d := LOOPHOLE(Word.LeftShift(i, LogBytesPerPage), ADDRESS);
    prev := NIL;
    (* iterate over pages *)
    WHILE i <= p1 AND d < MEMORY_End DO
      IF AllocateAligned THEN
        IF LOOPHOLE(d, INTEGER) MOD BytesPerPage # 0 THEN
          PutText("*****end of an object was not aligned ");
          PutAddr(d);
          PutCR();
          Flush();
        END;
        d := LOOPHOLE(Word.LeftShift(i, LogBytesPerPage), ADDRESS);
      END;
      elemsOnPage := 0;

      (* find next allocated page *)
      WHILE desc[i-p0].space = Space.Unallocated AND i <= p1 DO
        INC (i);
      END;
      IF i > p1 THEN
        RETURN; (* whatta hack *)
      END;
      d := LOOPHOLE(Word.LeftShift(i, LogBytesPerPage), ADDRESS);

      (* iterate over descriptors on this page, count live objects
         and check alignments.  if an obj is big, advance i to 
         the end of it *)
      (* strictly less than since want to be sure i do accidentally count
         the first elem of the next page *)
      WHILE d < PageToAddress(i+1) DO
        (* make sure we don't stray into unallocated pages *)
        IF desc[i-p0].space = Space.Unallocated THEN
          INC(i, d.header.size DIV BytesPerPage);
          INC(d, d.header.size); (* urgh *)

        END;
        IF d.header.gcStatus # GcStatus.Untraced AND 
           desc[i-p0].space # Space.Unallocated THEN
          INC(elemsOnPage); (* live object *)
        END;
        IF (d.header.typecode <= 0 OR d.header.typecode >= RT0u.nTypes) AND
          desc[i-p0].space # Space.Unallocated AND 
          d.header.typecode # TcFree THEN
            PutText(msg);
            PutText(": ******CHECKDESC!  ILLEGAL TYPECODE  d/typecode : ");
            PutAddr(d); PutText("/");
            PutInt(d.header.typecode); PutCR();
        END;

        IF d.header.size = 0 AND desc[i-p0].space # Space.Unallocated THEN
          PutText("*******CheckDescSanity : ");
          PutText(msg);
          PutText("ZERO LENGTH OBJ!!!  d/size prev/prev.size");
          PutAddr(d); PutText("/");
          PutInt(d.header.size); PutText("     ");          
          PutAddr(prev); PutText("/");
          IF prev #NIL THEN
            PutInt(prev.header.size);
          END;
          PutCR();Flush();
          RTOS.Crash();
        END;
        next := d+d.header.size;
        (* if this is on the next page, make sure it is a continued page *)
        (* make sure all pages in this block are marked continued, advance
           i there, move d to next *)
         IF GetPage(next) > i AND i+1 # GetPage(next) AND 
           desc[i-p0].space # Space.Unallocated 
         THEN
          VAR 
            end : INTEGER;
          BEGIN
            (* this will check if the page that next is on is REALLY
               part of the current block.  this will set the conditions for 
               FOR loop *)
            IF LOOPHOLE(next, INTEGER) MOD BytesPerPage#0 THEN
              end := GetPage(next);
            ELSE
              end := GetPage(next)-1;
            END;

            FOR j:= i+1 TO end DO
              IF j <= p1 AND NOT desc[j-p0].continued THEN
                PutText("*******CheckDescSanity : ");
                PutText(msg);
                PutText(" continued page not marked as such!!! page/objsize ");
                PutAddr(LOOPHOLE(j,ADDRESS));
                PutText("/"); PutInt(d.header.size);
                PutCR(); Flush();
              ELSIF j > p1 THEN
                PutText("*******CheckDescSanity : ");
                PutText(msg);
                PutText(" continued page went past heap!!! page/objsize ");
                PutAddr(LOOPHOLE(i+1, ADDRESS));
                PutText("/"); PutInt(d.header.size);
                PutCR(); Flush();               
              END;  
            END;
            i := end;
          END;
        END;
        IF AllocateAligned AND d.header.size > BytesPerPage 
          AND desc[i-p0].space # Space.Unallocated
         THEN
          (* ASSERT next MOD BYTESPERPAGE = 0 *)
          (* also need to ensure that we skip any pages already scanned *)
          IF LOOPHOLE(next, INTEGER) MOD BytesPerPage # 0 THEN
            PutText("\n*******CheckDescSanity : ");
            PutText(msg);
            PutText(" Object not page aligned ! page/obj/objend/objsize ");
            PutAddr(LOOPHOLE(i, ADDRESS)); PutText("/");
            PutAddr(d); PutText("/");
            PutAddr(d+d.header.size); PutText("/");
            PutInt(d.header.size);
            PutCR(); Flush();
          END;
        END;
        prev := d;
        d := next;
      END; (* end of iterating over obj descriptors on this page *)

      INC(i);
    END;
  END CheckDescSanity;


(* make sure the page lists make sense *)
PROCEDURE PageListSanityCheck (msg: TEXT) = 
  VAR
    p : INTEGER;
    numpages : INTEGER;
    addr : RefDescriptor;
    
  PROCEDURE perr (e :TEXT; i: INTEGER; p : Page; a : ADDRESS) = 
    BEGIN
      PutText("++++++++PLSC ");
      PutText(msg);
      PutText(":");
      PutText(e);
      PutInt(i);
      PutText("/");
      PutInt(p);
      PutText("/");
      PutAddr(a);
    END perr;
  BEGIN
    IF NOT PLSC THEN RETURN; END;
    (* iterate over free page lists *)
    (* check counts, check all pages have the right sized objects on them *)
    (* check that they're free in the desc array *)
    FOR i := 0 TO NumFreeLists-1 DO

      p := FreePageList[i] - p0;
      numpages := 0;
      WHILE p+p0 # 0 DO
        INC(numpages );
        addr := PageToAddress(p+p0);

        IF desc[p].space = Space.Unallocated THEN
          perr ("unallocated page in free list! list/pagenum/addr", 
                i,p,addr);
          PutCR();Flush();
        END;

        IF desc[p].space # Space.Free THEN
          perr("Page not free! list/pagenum/addr ",i,p,addr);
          PutCR(); Flush();
        END;

        IF NOT desc[p].continued THEN
          IF i # RP AND addr.header.size # NodeSizes[i] THEN
            perr("page size inconsistent list/page/addr/addr.size/listsize ",
                 i,p,addr);
            PutInt(addr.header.size); PutText("/");
            PutInt(NodeSizes[i]); PutCR(); Flush();
          END;
        END;
        IF pagelinks[p].next # 0 AND 
           p +p0 # pagelinks[pagelinks[p].next - p0].prev THEN
          perr ("free page prev screwed list/page/next/next.prev",i,p,
                LOOPHOLE(pagelinks[p].next, ADDRESS));
          PutText("/");

          PutAddr(LOOPHOLE(pagelinks[pagelinks[p].next-p0].prev, ADDRESS));
          PutCR(); Flush();
        END;

        IF i# RP AND pagelinks[p].next =0 AND p # BottomPage[i]-p0 THEN
          perr("last elem in freelist is not bottompage! list/page/next/bottom",
               i,p,PageToAddress(pagelinks[p].next));
          PutText("/");
          PutInt(BottomPage[i]-p0);
        END;


        (* check if there's live objects on a free page *)
        (* this only works if we cleaned the gcstatus bits 
           in non-implicit mode *)
        IF NOT Implicit AND NOT desc[p].continued THEN
          VAR foundlive := FALSE;
          BEGIN
            WHILE addr < PageToAddress(p+p0+1) DO
              IF addr.header.gcStatus # GcStatus.Untraced 
               (* OR addr.header.typecode # TcFree  *)
               THEN
                foundlive := TRUE;
                EXIT;
              END;
              INC(addr,addr.header.size);
            END;
            IF foundlive THEN 
              perr("ARGH! freelist had a live object! list/page ",
                   i,p,NIL);
              RTOS.Crash();
            END;
          END;
        END;
        (* check end *)
        IF pagelinks[p].next = 0 THEN
          IF BottomPage[i]-p0 # p THEN
            perr("bottom page is not set rightlist/bottom/nil/last",i,BottomPage[i]-p0,NIL);
            PutInt(p);
            PutCR();Flush();RTOS.Crash();
                 
          END;
        END;

        p := pagelinks[p].next - p0;
      END;
      IF numpages # FreePages[i] THEN
        perr("numpages inconsisntent!  list/freepages/counted ",i,
             FreePages[i],LOOPHOLE(numpages, ADDRESS));
        PutCR(); Flush();
      END;
      (* now go over allocated pages *)
      p := AllocatedPages[i]-  p0;
      WHILE p+p0 # 0 DO
        (*PutInt(p);PutText(" ");Flush();*)
        INC (numpages);
        addr := PageToAddress(p+p0); 
        IF desc[p].space # Space.Current THEN
          perr("Page not current! list/pagenum/addr ",i,p,addr);

          IF desc[p].space = Space.Previous THEN
            PutText("previous");
          END;
          IF desc[p].space = Space.Unallocated THEN
            PutText("unallocated");
          END;
          IF desc[p].space = Space.Free THEN
            PutText("free");
          END;

          PutCR(); Flush();
        END;

        IF pagelinks[p].next # 0 AND 
            p +p0 # pagelinks[pagelinks[p].next - p0].prev THEN
          perr ("allced page prev screwed list/page/next/next.prev",i,p,
                LOOPHOLE(pagelinks[p].next, ADDRESS));
          PutText("/");

          PutAddr(LOOPHOLE(pagelinks[pagelinks[p].next-p0].prev, ADDRESS));
          PutCR(); Flush();
        END;
        (* make sure there's a live object on the live page *)
        IF NOT desc[p].continued THEN
        VAR foundlive := FALSE;
        BEGIN
          WHILE addr < PageToAddress(p+p0+1) DO
            IF addr.header.gcStatus = WhiteStatus OR
              addr.header.typecode # TcFree  THEN
              foundlive := TRUE;
              EXIT;
            END;
            INC(addr,addr.header.size);
          END;
          IF NOT foundlive THEN
            perr("FAiled to find live object list/page", i,p,NIL);
            PutCR();Flush();
          END;
        END;
        END;
        
        p := pagelinks[p].next - p0;

      END;

      (* also have to count black pages for count to be correct... *)
      p := BlackPages[i]-p0;

      WHILE p+p0 # 0 DO
        INC (numpages);
        p := pagelinks[p].next - p0;
      END;

      IF numpages # TotalPages[i] THEN 
        perr("wrong pagecount! pagecount/totpages ", numpages,
             TotalPages[i], NIL);
      END;
    END;
  END PageListSanityCheck;

PROCEDURE TreadMillSanityCheck (msg : TEXT) = 
  VAR
    p : RefDescriptor;
    count : INTEGER;
  PROCEDURE perr (i : INTEGER;e :TEXT; a1:ADDRESS; a2: ADDRESS; a3:ADDRESS) =
    BEGIN
      PutText("\n++++++++TMSC list ");
      PutInt(i); PutText(" "); PutText(msg);
      PutText(":"); PutText(e);
      PutAddr(a1); PutText("/");
      PutAddr(a2); PutText("/");
      PutAddr(a3); PutCR();
      Flush();

      PutText ("Bottom/top/scan/free ");
      PutAddr(Bottom[i]); PutText("/");
      PutAddr(Top[i]); PutText("/");
      PutAddr(Scan[i]); PutText("/");
      PutAddr(Free[i]); PutCR();
      RTOS.Crash();
    END perr;
  BEGIN
    FOR i := 0 TO NumFreeLists-2 DO
      IF COLLECTING = 0 AND Top[i] # PtrToDesc(Free[i].ptrs.prev) THEN
        perr(i,"ARGH! top # freelists.prev  ", 
             Top[i], PtrToDesc(Free[i].ptrs.prev),NIL);

      END;

      p:= Free[i];
      count := 1;

      WHILE p # Bottom[i] DO
        (* check back/fwd ptrs *)
        IF p # PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev) THEN
          perr(i,"free-bot next/nextprev screwed", p,
               PtrToDesc(p.ptrs.next), 
               PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev));
        END;

        IF p # PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next) THEN
          perr(i,"free-bot prev/prevnext screwed", p,
               PtrToDesc(p.ptrs.prev), 
               PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next));
        END;

        (* check status is what we expect *)

        IF NOT Implicit AND p.header.gcStatus # GcStatus.Untraced THEN
          perr(i,"free-bot status screwed p/p.next ", p, 
               PtrToDesc(p.ptrs.next), NIL);
          PutText(GcS[p.header.gcStatus]);
        END;

        IF p.header.size # NodeSizes[i] THEN
          perr(i,"free-bot size screwed p/size ", p, NIL, NIL);
          PutInt(p.header.size);
          PutCR();Flush();
        END;
        IF (p.header.typecode <= 0 OR p.header.typecode >= RT0u.nTypes) 
          AND p.header.typecode # TcFree THEN
          perr(i,"botfree : illegal typecode", p, NIL,NIL);
        END;
        INC(count);
        p := PtrToDesc(p.ptrs.next);
      END;
      IF count # ElemsFree[i] THEN
        perr(i, " elemcounts don't match!!!  count/elemfree ", p,NIL,NIL);
        PutInt(count);
        PutText("/");
        PutInt(ElemsFree[i]);
        PutCR();
        Flush();
      END;
      IF Bottom[i] = Free[i] THEN
        (* empty list!, so i have to move forward so i actually count elems *)
        p := PtrToDesc(p.ptrs.next);
      END;

      IF Top[i] # NIL THEN
        WHILE p # PtrToDesc(Top[i].ptrs.next) DO
          IF p = Bottom[i] AND NOT Implicit THEN
            (* skip it *)
            IF Bottom[i].header.gcStatus # GcStatus.Untraced THEN
              perr (i,"bottom not untraced! ", p, NIL,NIL);
              PutText(GcS[Bottom[i].header.gcStatus]);
              
            END;
            p := PtrToDesc(p.ptrs.next);
          ELSE

            (* check back/fwd ptrs *)
            IF p # PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev) THEN
              perr(i,"bot-top next/nextprev screwed", p,
                   PtrToDesc(p.ptrs.next), 
                   PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev));
            END;
            
            IF p # PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next) THEN
              perr(i,"bot-top prev/prevnext screwed", p,
                   PtrToDesc(p.ptrs.prev), 
                   PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next));
            END;

            (* check status is what we expect *) 
            IF p.header.gcStatus # WhiteStatus THEN
              perr(i,"bot-top status screwed p/p.next ", p, 
                   PtrToDesc(p.ptrs.next), NIL);
              PutText(GcS[p.header.gcStatus]);
            END;
            
            IF p.header.size # NodeSizes[i] THEN
              perr(i,"bot-top size screwed p/size ", p, NIL, NIL);
              PutInt(p.header.size);
              PutCR();Flush();
            END;
            
            IF (p.header.typecode <= 0 OR p.header.typecode >= RT0u.nTypes) 
               AND p.header.typecode # TcFree THEN
              perr(i,"bot-top : illegal typecode", p, NIL,NIL);
            END;
            
            INC (count);

            p := PtrToDesc(p.ptrs.next);
          END;
        END;
      END;

      IF COLLECTING = 0 THEN
        (* this checks that the black and gray lists are empty *)
        IF PtrToDesc(Top[i].ptrs.next) # Free[i] THEN
          PutText("top.next #free!");
        END;
      END;

(*      WHILE p # Free[i] DO*)
WHILE p#Scan[i] AND p#Free[i] DO
        IF p # PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev) THEN
          perr(i,"top-free next/nextprev screwed", p,
               PtrToDesc(p.ptrs.next), 
               PtrToDesc(PtrToDesc(p.ptrs.next).ptrs.prev));
        END;
        
        IF p # PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next) THEN
          perr(i,"top-free prev/prevnext screwed", p,
               PtrToDesc(p.ptrs.prev), 
               PtrToDesc(PtrToDesc(p.ptrs.prev).ptrs.next));
        END;

        (* check status is what we expect *) 
        IF  COLLECTING=0 AND p.header.gcStatus # WhiteStatus THEN
          perr(i,"top-free status screwed(not white) p/top ", p, 
               Top[i], NIL);
          PutText(GcS[p.header.gcStatus]);
          PutText(GcS[WhiteStatus]);
        END;

        IF COLLECTING#0 AND (Scan[i]# NIL AND Scan[i] # Top[i]) AND p.header.gcStatus # GrayStatus THEN
          perr(i,"top-free status screwed (gray) p/top ", p, 
               Top[i], NIL);
          PutText(GcS[p.header.gcStatus]);
          PutText(GcS[BlackStatus]);
        END;

        (*shoudl check scan-black *)
        IF (p.header.typecode <= 0 OR p.header.typecode >= RT0u.nTypes) 
           AND p.header.typecode # TcFree THEN
          perr(i,"top-free : illegal typecode", p, NIL,NIL);
        END;

        
        IF p.header.size # NodeSizes[i] THEN
          perr(i,"top-free size screwed p/size ", p, NIL, NIL);
          PutInt(p.header.size);
          PutCR();Flush();
        END;
        INC (count);        
        p := PtrToDesc(p.ptrs.next);
      END;
      (* do scan-free if any *)
      IF COLLECTING#0 AND Scan[i] # NIL THEN
        IF p # Scan[i] AND p # Free[i] THEN
          perr(i," p # scan p/nil/nil", p,NIL,NIL);
          Flush();
        END;
        (* skip scan, unless it was empty... *)
        IF p # Free[i] THEN
          IF p # Scan[i] THEN
            perr(i,"Shit..i wanted to skip scan...\n", p,NIL,NIL);
          END;
          p := PtrToDesc(p.ptrs.next);
          INC (count);        
        END;
        (* traverse black *)
        WHILE p # Free[i] DO
          IF COLLECTING=0 THEN
            PutText("WTF am i doing here???\n");
          END;
          IF COLLECTING#0 AND p.header.gcStatus # BlackStatus THEN
            perr(i,"scan-free status screwed (black) p/top ", p, 
                 Top[i], NIL);
            PutText(GcS[p.header.gcStatus]);
            PutText(GcS[BlackStatus]);
          END;
          
          INC (count);        
          p := PtrToDesc(p.ptrs.next);
        END;
      END;

      IF count # FreeListElems[i] THEN
        PutText(msg); PutText("TMSC : list/count/freelistelems ");
        PutInt(i); PutText("/"); PutInt(count);  PutText("/");
        PutInt(FreeListElems[i]); PutCR();
      END;
    END;

  END TreadMillSanityCheck;



CONST
  GcS = ARRAY GcStatus OF
  TEXT{"Unused", "Untraced", "Not Marked", "Marked",
       "Recursively Marked"};

(* prints everything in heap from start to end *)
<*UNUSED*>
PROCEDURE DumpHeap (detail: BOOLEAN := TRUE) =

  VAR h: RefHeader := MEMORY_Start;
  BEGIN
    PutText("\n-----  Heap  -----\n");
    WHILE h < MEMORY_End DO
      PutText("Address: ");
      PutAddr(h);
      PutText("   ");
      PutText("Size: ");
      PutInt(h.size);
      PutText("   ");
      IF h.typecode = TcFree THEN
        PutText("FREE\n")
      ELSE
        PutText("Typecode: ");
        PutInt(h.typecode);
        PutText("   ");
        PutText("GcStatus: ");
        PutText(GcS[h.gcStatus]);
        IF detail THEN
          <* ASSERT h.size MOD AddrPerAlign = 0 *>
          <* ASSERT h.size >= HeaderSize *>
          <* ASSERT h.size >= MinMemoryNodeSize *>
          FOR i := 0 TO ((h.size - HeaderSize) DIV ADRSIZE(Word.T)) - 1 DO
            IF i MOD 8 = 0 THEN PutText("\n") END;
            PutText("    ");
            PutInt(LOOPHOLE(h + HeaderSize + i * ADRSIZE(Word.T),
                            UNTRACED REF Word.T)^)
          END
        END;
        PutText("\n")
      END;
      IF h.size = 0 THEN PutText("size = ZERO\n"); EXIT; END;
      INC(h, h.size)
    END;
    Flush();
  END DumpHeap;

(* prints out the crud in a specific free list *)
<*UNUSED*>
PROCEDURE DumpFreeList (d: RefDescriptor; list : INTEGER) =
  VAR
  BEGIN
    PutText("\n-----  Free list  -----\n");
    WHILE d#Bottom[list] AND      d # NIL AND 
      d < PageToAddress(p1+1) DO
      IF d.header.typecode = TcFree THEN
        PutText("Address: ");
        PutAddr(d);
        PutText("-");
        PutAddr(d+d.header.size);
        PutText("   ");
        PutText("Size: ");
        PutInt(d.header.size);
        PutText("   ");
        PutText("next/prev ");
        PutAddr(PtrToDesc(d.ptrs.next));
        PutText("/");
        PutAddr(PtrToDesc(d.ptrs.prev));
        PutText("\n");
      END;
      <* ASSERT d.header.size >= MinMemoryNodeSize *>
      d := PtrToDesc(d.ptrs.next);
    END;

    PutText("\n-----  OBJECTS   -----\n");
    IF list = RP THEN 
      PutText("WTF?");Flush();
      d := Top[list];
      WHILE d # NIL DO
      IF d.header.typecode # TcFree  THEN
        PutText("Address: ");
        PutAddr(d);
        PutText("-");
        PutAddr(d+d.header.size);
        PutText("   ");
        PutText("Size: ");
        PutInt(d.header.size);
        PutText("   ");
        PutText("Typecode: ");
        PutInt(d.header.typecode);
        PutText("   ");
        PutText("next/prev ");
        PutAddr(PtrToDesc(d.ptrs.next));
        PutText("/");
        PutAddr(PtrToDesc(d.ptrs.prev));
        PutCR();
      ELSE
        PutText("DFL SCREWED\n");
        PutText("Address: ");
        PutAddr(d);
        PutText("-");
        PutAddr(d+d.header.size);
        PutText("   ");
        PutText("Size: ");
        PutInt(d.header.size);
        PutText("   ");
        PutText("Typecode: ");
        PutInt(d.header.typecode);
        PutText("   ");
        PutText("next/prev ");
        PutAddr(PtrToDesc(d.ptrs.next));
        PutText("/");
        PutAddr(PtrToDesc(d.ptrs.prev));
        PutCR();
      END;  
      d := PtrToDesc(d.ptrs.next);

        
      END;
      RETURN;
    END;
    WHILE d # Free[list] AND d < MEMORY_End DO
      IF d.header.typecode # TcFree  THEN
        PutText("Address: ");
        PutAddr(d);
        PutText("-");
        PutAddr(d+d.header.size);
        PutText("   ");
        PutText("Size: ");
        PutInt(d.header.size);
        PutText("   ");
        PutText("Typecode: ");
        PutInt(d.header.typecode);
        PutText("   ");
        PutText("next/prev ");
        PutAddr(PtrToDesc(d.ptrs.next));
        PutText("/");
        PutAddr(PtrToDesc(d.ptrs.prev));
        PutCR();
      ELSE
        PutText("DFL SCREWED\n");
        PutText("Address: ");
        PutAddr(d);
        PutText("-");
        PutAddr(d+d.header.size);
        PutText("   ");
        PutText("Size: ");
        PutInt(d.header.size);
        PutText("   ");
        PutText("Typecode: ");
        PutInt(d.header.typecode);
        PutText("   ");
        PutText("next/prev ");
        PutAddr(PtrToDesc(d.ptrs.next));
        PutText("/");
        PutAddr(PtrToDesc(d.ptrs.prev));
        PutCR();
      END;  
      d := PtrToDesc(d.ptrs.next);
    END;
  END DumpFreeList;
<*UNUSED*>
PROCEDURE DumpPageList(i : INTEGER) = 
  VAR
    p : Page;
  BEGIN
    p := FreePageList[i];
    PutText("list/page/addr/next/prev\n");
    
    WHILE p # 0 DO
      PutInt(i);
      PutText("/\t"); PutAddr(LOOPHOLE(p,ADDRESS));
      PutText("/\t"); PutAddr(PageToAddress(p));
      PutText("/\t"); PutAddr(LOOPHOLE(pagelinks[p-p0].next,ADDRESS));
      PutText("/\t"); PutAddr(LOOPHOLE(pagelinks[p-p0].prev,ADDRESS));
      PutText("/\t");
      p := pagelinks[p-p0].next;
      PutCR(); Flush();
    END;
  END DumpPageList;


(* Extra careful sanity checking on reserve pool *)
PROCEDURE ResSanity (msg : TEXT) = 
  VAR 
    p : RefDescriptor;
    count := 0; (* count total memory *)
  PROCEDURE perr(e:TEXT; p:RefDescriptor) =
    BEGIN
      PutText("GC ERROR >> ResSanity ");
      PutText(msg); PutText(": "); PutText(e); PutAddr(p);
    END perr;
  PROCEDURE eoe () =
    BEGIN
      PutCR(); Flush(); RTOS.Crash();
    END eoe;

    (* ensure page descriptors make sense *)
  PROCEDURE CheckPages(m2:TEXT;obj:RefDescriptor; s : Space) = 
    VAR
      start := GetPage(obj)-p0;
      end   := GetPage(obj+obj.header.size)-p0 - 1;
    BEGIN
      IF desc[start].space = Space.Unallocated THEN
        perr("UNALLOCATED start page for obj/page",obj);
        PutText(m2); PutText("/");
        PutAddr(obj);PutText("/");
        PutInt(start);PutCR();
        eoe();
      END;
      IF desc[start].space # s THEN
        perr("WRONG space!!!! start page for obj/page",obj);
        PutText(m2); PutText("/");
        PutAddr(obj);PutText("/");
        PutInt(start);PutCR();
        eoe();
      END;
      
      IF end = start AND obj.header.gcStatus # GcStatus.Untraced THEN
        perr("only one page? p/size ", obj);
        PutText(m2); PutText("/");
        PutInt(obj.header.size); PutText("/");
        PutText(GcS[obj.header.gcStatus]);
        PutCR();
      END;
      IF desc[start].continued THEN
        perr ("start page shouldn't be continued!!!! ", obj);
        PutText(m2); eoe();
      END;
      FOR i := start+1 TO end DO
        IF NOT desc[i].continued THEN
          perr("not continued ! obj/start/end/page ", obj);
          PutText(m2);
          PutInt(start); PutText("/"); PutInt(end); PutText("/");          
          PutInt(i); eoe();
        END;
      END;
    END CheckPages;
  BEGIN
    (* scan freelist, allocatedlist (top) , gray list (scan) and 
       black (bottom) *)
    p := Free[RP];
    WHILE p # NIL DO
      IF p.header.size = 0 THEN
        perr("freelist zero size obj",p);
        eoe();
      END;
      IF p.header.gcStatus = GrayStatus THEN
        perr("freelist gcstatus marked",p);
        eoe();
      END;
      IF p.header.typecode # TcFree THEN
        perr("freelist not TcFree",p);
        PutInt(p.header.typecode);
        eoe();
      END;
      INC(count, p.header.size);
      CheckPages("free", p, Space.Free);
      p:= PtrToDesc(p.ptrs.next);
    END;

    p := Top[RP];
    WHILE p # NIL DO
      IF p.header.size = 0 THEN
        perr("alloclist zero size obj",p);
        eoe();
      END;
      IF p.header.gcStatus # WhiteStatus THEN
        perr("alloclist gcstatus notwhite",p);
        PutText(GcS[WhiteStatus]);
        PutText(GcS[p.header.gcStatus]);
        eoe();
      END;
      IF (p.header.typecode <=0 OR p.header.typecode >= RT0u.nTypes) AND
        p.header.typecode # TcFree THEN
        perr("alloclist illegal typecode",p);
        PutInt(p.header.typecode);
        eoe();
      END;
      CheckPages("top",p, Space.Current);
      INC(count, p.header.size);
      p:= PtrToDesc(p.ptrs.next);
    END;

    p := Scan[RP];
    WHILE p # NIL DO
      IF p.header.size = 0 THEN
        perr("scanlist zero size obj",p);
        eoe();
      END;
      IF p.header.gcStatus # GrayStatus THEN
        perr("scanlist gcstatus not marked",p);
        PutText(GcS[WhiteStatus]);
        PutText(GcS[p.header.gcStatus]);
        eoe();
      END;
      IF p.header.typecode <=0 OR p.header.typecode >= RT0u.nTypes THEN
        perr("scanlist illegal typecode",p);
        PutInt(p.header.typecode);
        eoe();
      END;
      CheckPages("scan",p, Space.Previous);
      INC(count, p.header.size);
      p:= PtrToDesc(p.ptrs.next);
    END;

    p := Bottom[RP];
    WHILE p # NIL DO
      IF p.header.size = 0 THEN
        perr("blacklist zero size obj",p);
        eoe();
      END;
      IF p.header.gcStatus # BlackStatus THEN
        perr("blacklist gcstatus not black",p);
        PutText(GcS[BlackStatus]);
        PutText(GcS[p.header.gcStatus]);
        eoe();
      END;
      IF p.header.typecode <=0 OR p.header.typecode >= RT0u.nTypes THEN
        perr("blacklist illegal typecode",p);
        PutInt(p.header.typecode);
        eoe();
      END;
      CheckPages("black",p, Space.Previous);
      INC(count, p.header.size);
      p:= PtrToDesc(p.ptrs.next);
    END;
    IF count # ListCaps[RP] THEN
      PutText("GC ERROR >> Res sanity ");
      PutText(msg);
      PutText(" : reserve pool capacity screwed.  count/listcap ");
      PutInt(count); PutText("/");
      PutInt(ListCaps[RP]);
      PutCR(); Flush();
    END;
  END ResSanity;


(* set up Fragmentation and LiveBytes *)
PROCEDURE GetFrag():INTEGER=
  VAR
  BEGIN
    LiveBytes := ActiveBytes;
    AllocatedBytes := 0;
    FOR i := 0 TO RP DO
      INC(AllocatedBytes,ListCaps[i] - BytesFree[i]);
    END;
    Fragmentation := AllocatedBytes - LiveBytes;
    RETURN Fragmentation;
  END GetFrag;

(* this only cares about live memory, so iterate between FREE and BOTTOM *)
(* in addition, while we're at it, compute total live memory, 
   and total fragmentation.  
 *)


PROCEDURE VisitHeapRefs(v : RefVisitor) =
  VAR
    p : RefDescriptor;
    size : INTEGER;
  BEGIN
    Fragmentation := 0;
    LiveBytes := 0;
    AllocatedBytes := 0;
    ReallyLiveObjects := 0;
    
    FOR i := 0 TO NumFreeLists-2 DO
      p:= PtrToDesc(Free[i].ptrs.prev);

      WHILE p # Bottom[i] DO
        IF (p.header.typecode <= 0  OR p.header.typecode > RT0u.nTypes) AND 
          p.header.typecode # TcFree   THEN
          PutText("------Invalid typecode! p/tc ");
          PutAddr(p);
          PutText("/");
          PutInt(p.header.typecode);
          PutCR();
        END;
        (* Visit *)
        size := ReferentSize (LOOPHOLE(p+PtrSize, RefHeader));
        INC(Fragmentation, p.header.size - size);
        INC(LiveBytes, size);
        INC(AllocatedBytes,p.header.size);
        INC(ReallyLiveObjects);
        IF NOT v.visit(p.header.typecode, LOOPHOLE(p + Offset, REFANY), size)
         THEN RETURN; END;
        p := PtrToDesc(p.ptrs.prev);
      END;

    END;
    (* do da reserve pool *)
    p := Top[RP];
    WHILE p # NIL DO
      IF (p.header.typecode <= 0  OR p.header.typecode > RT0u.nTypes) AND 
        p.header.typecode # TcFree   THEN
        PutText("------Invalid typecode! p/tc ");
        PutAddr(p);
        PutText("/");
        PutInt(p.header.typecode);
        PutCR();
      END;
      (* Visit *)
      size := ReferentSize (LOOPHOLE(p+PtrSize, RefHeader));

      INC(Fragmentation, p.header.size - size);
      INC(LiveBytes, size);
      INC(AllocatedBytes,p.header.size);
      INC(ReallyLiveObjects);
      IF NOT v.visit(p.header.typecode, LOOPHOLE(p + Offset, REFANY), size)
       THEN RETURN; END;
      p := PtrToDesc(p.ptrs.next);
    END;
    (* FindAmbiguous(); *)
  END VisitHeapRefs;



TYPE  
  AmSearcher = RTHeapMap.Visitor OBJECT
  OVERRIDES
    apply := CountAmb;
  END;

VAR
  AmbigBytes : INTEGER;
  AmbigAlloc : INTEGER;
  AmbigObjects : INTEGER;
  mcobj : INTEGER;
  mcalloc : INTEGER;


PROCEDURE CountAmbFromStacks (start, stop: ADDRESS) =
  VAR
    fp            := start;
    p : ADDRESS;
    h : RefHeader;
  BEGIN
    WHILE fp <= stop DO
      p := LOOPHOLE(fp, UNTRACED REF ADDRESS)^;
      IF p < MEMORY_Start OR p >= MEMORY_End THEN
        INC(fp, RTMachine.PointerAlignment);
      ELSE
        (* this ref might point to something in traced heap *)
        h := LocateHeaderOf(p);
        IF h # NIL AND h.typecode # TcFree THEN
          IF h.gcStatus = WhiteStatus THEN
            PutText(":");Flush();
            h.gcStatus := GrayStatus;
          ELSE
            IF h.gcStatus # GrayStatus AND h.gcStatus # GcStatus.Untraced THEN
              (* if it's not white or gray, then it's black, but that's
                 impossible for live memory *)
              PutText("!");
              PutText("B");PutAddr(h);
              PutText("/");
              PutInt(h.size);
              PutText("/");
              PutText(GcS[h.gcStatus]);
              PutCR(); Flush();

              WhereTheHell(h-PtrSize);
            END;
            h.gcStatus := GrayStatus;
          END;

        END;
        INC(fp, RTMachine.PointerAlignment);
      END;
    END;
  END CountAmbFromStacks; 

PROCEDURE CountAmb(<*UNUSED*>self : AmSearcher; a : ADDRESS) =
  VAR
    refref            := LOOPHOLE(a, UNTRACED REF ADDRESS);
    ref               := refref^;
    h     : RefDescriptor;
  BEGIN
    IF ref = NIL THEN RETURN; END;
    h := ref - Offset;
    IF h < MEMORY_Start OR h >=MEMORY_End THEN
      IF h.header.typecode #1 THEN
        PutText("AMB : not text and outside memory!!!");
        PutAddr(h-PtrSize); PutCR();Flush();
        RTOS.Crash();
      END;
      RETURN;
    END;
    IF h.header.gcStatus = WhiteStatus THEN
      h.header.gcStatus := GrayStatus;
    END;

  END CountAmb;

(* 
   mark everything from ambig as blackstatus, since that can't happen.
   
*)

PROCEDURE CountThrough(s:AmSearcher)=
  VAR
    needmore := FALSE;
    p : RefDescriptor;
  BEGIN
    LOOP
      needmore := FALSE;
      p:= MEMORY_Start;

      WHILE p < MEMORY_End AND p.header.size # 0 DO
        IF p.header.gcStatus = GrayStatus THEN
          (* scan *)
          RTHeapMap.WalkRef(p + PtrSize, s);
          p.header.gcStatus := BlackStatus;

          (* inc counts *)
          INC (AmbigAlloc, p.header.size);
          INC (AmbigBytes, ReferentSize(LOOPHOLE(p+PtrSize, RefHeader)));
          INC (AmbigObjects);

          needmore := TRUE;
        END;
        INC(p, p.header.size);
      END;

      IF NOT needmore THEN
        RETURN;
      END;
    END;
  END CountThrough;


(* this does *not* factor out objects reachable from globals *)
<* UNUSED *>
PROCEDURE FindAmbiguous()=
  VAR
    searcher : AmSearcher;
    p : RefDescriptor;
  BEGIN
    (* pre-sweep *)
    p := MEMORY_Start;
    WHILE (p < MEMORY_End ) DO
      IF p.header.gcStatus = GrayStatus THEN
        PutText("F");PutAddr(p);
        PutText("/");
        PutInt(p.header.size);
        PutCR();
        Flush();
        p.header.gcStatus := WhiteStatus;

        (* lets see how the hell this can happen.  scan the entire
           list to figure out which sub list it is in *)
        WhereTheHell(p);
      END;
      INC(p,p.header.size);
    END;

    AmbigBytes := 0;
    AmbigAlloc := 0;
    AmbigObjects := 0;

    mcobj :=0;
    mcalloc :=0;
    searcher := NEW(AmSearcher);

    (* get roots *)
    ThreadF.ProcessStacks(CountAmbFromStacks);

    (* mark through *)
    CountThrough(searcher);
    (* cleanup *)
    p := MEMORY_Start;
    WHILE (p < MEMORY_End ) DO
      IF p.header.gcStatus = BlackStatus THEN
        p.header.gcStatus := WhiteStatus;
      END;
      INC(p,p.header.size);
    END;

    PutText("-----Ambiguous-------\n");
    PutText("ALLOCATED : ");
    PutInt(AmbigAlloc);
    PutText("\nLIVE      : ");
    PutInt(AmbigBytes);
    PutText("\nOBJECTS   : ");
    PutInt(AmbigObjects);
    PutCR();Flush();
  END FindAmbiguous;


(* locate which list an object is in *)
PROCEDURE WhereTheHell(p : RefDescriptor) = 
  VAR
    f : RefDescriptor;
    list : INTEGER;
  BEGIN
    list := GetList(p.header.size);
    f := Free[list];
    (* walk *)
    IF f = p THEN
      PutText("Free\n"); Flush();
    END;
    WHILE f # Bottom[list] DO
      IF f = p THEN 
        PutText("FREE\n"); Flush();
      END;
      f := PtrToDesc(f.ptrs.next);
    END;
    IF f = p THEN
      PutText("BOTTOM\n"); Flush();
    END;
    WHILE f # Top[list] DO
      IF f = p THEN 
        PutText("ECRU\n"); Flush();
      END;
      f := PtrToDesc(f.ptrs.next);
    END;
    IF f = p THEN
      PutText("TOP\n"); Flush();
    END;
  END WhereTheHell;



(* --------------------------------------------------------------- Stubs ----*)
(* stuff i don't need but must be implemented *)

FUNCTIONAL PROCEDURE MinAddress(): ADDRESS =
  BEGIN
    RETURN PageToAddress(p0);
  END MinAddress;

FUNCTIONAL PROCEDURE MaxAddress(): ADDRESS =
  BEGIN
    RETURN PageToAddress(p1) - 1;
  END MaxAddress;

PROCEDURE GetRealPointer(ref : ADDRESS) : ADDRESS =
  BEGIN
    RETURN ref;
  END GetRealPointer;

(*---------------------------------------------------------------------------*)

PROCEDURE ForceFragSample()=
  BEGIN
    IF RTHeapStats.ReportFragmentation THEN
      (* force a valid first sample *)
      EVAL GetFrag(); (* this sets up LiveBytes and Fragmentation *)
      RTHeapStats.SampleFrag(LiveBytes, AllocatedBytes);
    END;
  END ForceFragSample;

PROCEDURE GetStats (VAR s : RTCollectorSRC.Statistics )=
  VAR
  BEGIN
    IF MeasureMarking THEN
      DumpMG();
    END;

    (*
    PutText("TOTAL STACK BYTES = ");
    PutInt(StackBytes);
    PutCR();    PutCR();
    *)
    s.tmNumFreeLists := NumFreeLists;
    s.totalTracedBytes := RT0u.total_traced_bytes;
    s.fullCnt := NumCollections;
    FOR i:=0 TO RP DO
      s.tmNodeSize[i] := NodeSizes[i];
      s.tmAllocations[i] := Allocations[i];
      s.tmTriggers[i] := TriggerList[i];
      s.tmBalanceTriggers[i] := BalanceTriggerList[i];
      s.tmTotalPages[i] := TotalPages[i];
      s.tmFreePages[i] := FreePages[i];

      s.tmFreeObjects[i] := ElemsFree[i];

      s.tmTotalObjects[i] := FreeListElems[i];
    END;
    s.autoCnt := FreeBytesAtCollection;
    s.reqCnt := FreePagesAtCollection;
  END GetStats;

PROCEDURE CountFree(list : INTEGER) : INTEGER =
  VAR
    p := Free[list];
    count := 1;
  BEGIN
    WHILE p # NIL AND p # Bottom[list] DO
      INC(count);
      p := PtrToDesc(p.ptrs.next);
    END;
    RETURN count;
  END CountFree;

<* UNUSED *>
PROCEDURE CountLive(list : INTEGER) : INTEGER =
  VAR
    p := Free[list];
    count := 0;
  BEGIN
    WHILE p # NIL AND p # Bottom[list] DO
      IF p # Free[list] THEN
        INC(count, p.header.size);
      END;
      p := PtrToDesc(p.ptrs.prev);
    END;
    RETURN count;
  END CountLive;



PROCEDURE CountElems(list : INTEGER) : INTEGER =
  VAR
    p := Free[list];
    count := 1;
  BEGIN
    WHILE p # NIL DO

      p := PtrToDesc(p.ptrs.next);
      IF p = Free[list] THEN
        RETURN count;
      END;
      INC(count);
    END;
    RETURN count;
  END CountElems;

PROCEDURE CountListCap (list : INTEGER) : INTEGER =
  BEGIN
    IF list # RP THEN
      VAR
        p := PtrToDesc(Free[list].ptrs.next);
        count := 0;
      BEGIN
        WHILE p # NIL AND p # Free[list] DO
          INC(count, p.header.size);
          p := PtrToDesc(p.ptrs.next);
        END;
        RETURN count;
      END;
    END;

    (* res pool *)
    VAR
      p := Free[list];
      count := 0;
    BEGIN
      WHILE p # NIL DO
        INC(count, p.header.size);
        p := PtrToDesc(p.ptrs.next);
      END;
      p := Bottom[list];
      WHILE p # NIL DO
        INC(count, p.header.size);
        p := PtrToDesc(p.ptrs.next);
      END;
      p := Top[list];
      WHILE p # NIL DO
        INC(count, p.header.size);
        p := PtrToDesc(p.ptrs.next);
      END;
      p := Scan[list];
      WHILE p # NIL DO
        INC(count, p.header.size);
        p := PtrToDesc(p.ptrs.next);
      END;
      RETURN count;
    END;

  END CountListCap;

PROCEDURE CountFreeBytes(list : INTEGER) : INTEGER =
  VAR
    p := Free[list];
    count := 0;
  BEGIN
    WHILE p # NIL AND p # Bottom[list] DO
      INC(count, p.header.size);
      p := PtrToDesc(p.ptrs.next);
    END;
    RETURN count;
  END CountFreeBytes;


PROCEDURE CountFreePages() : INTEGER = 
  VAR
    free := 0;
  BEGIN
    FOR i := p0 TO p1 DO 
      IF desc[i-p0].space = Space.Free THEN
        INC(free);
      END;
    END;
    
    RETURN free;
  END CountFreePages;

(* ----------------------------------------------------- Write Barrier upcall --- *)

(* XXX change to only take one arg *)
(* actually, this upcall can be phased out - call MakeGray directly from
   RTWriteBarrier.s *)
PROCEDURE WriteBarrier(dest : ADDRESS; val : ADDRESS; ra : ADDRESS)=
  VAR
    v : RefDescriptor;
  BEGIN

    IF COLLECTING=0 OR val = NIL THEN 
      PutText("< WB : call was not pruned!>\n");Flush();
      RTOS.Crash();
      RETURN ;
    END;
    RTOS.LockHeap();    
    IF val < MEMORY_Start OR val > MEMORY_End THEN
      RTOS.UnlockHeap();
      RETURN; (* Comment this line out for sanity checking *)
      v := val-Offset;
      IF LOOPHOLE(v,INTEGER) > 16_fffffc0000000000  AND
        LOOPHOLE(v,INTEGER) < 16_fffffc00ffffffff
       THEN
        IF v.header.typecode # 1 THEN
          PutText("======= WB : referent not in heap and not a text! ref/tc/size ");
          PutAddr(val);
          PutText("/");
          PutInt(v.header.typecode);
          PutText("/");
          PutInt(v.header.size);
          PutCR();
        END;
      ELSE
        PutText("======== WB : referent not in heap, and if i derefernece it, i'll die.  dest/ref\n");
        PutAddr(dest);
        PutText("/");
        PutAddr(val);
        PutCR();
      END;
      RETURN;
    END;
    IF NOT Incremental THEN
      RETURN;
    END;

    (* let's go! *)
    Spy.Enter(WBTimer);
    (*
    IsAligned("WB ",val, ra);
    *)
    v := val-Offset;
    IF v.header.gcStatus = WhiteStatus THEN
      MakeGray(v);
      v.header.gcStatus := GrayStatus;
      Spy.Exit(WBTimer);
      RTOS.UnlockHeap();
      RETURN;
    END;


    Spy.Exit(WBTimer);
    RTOS.UnlockHeap();
    RETURN;

  END WriteBarrier;

PROCEDURE WriteBarrierCAS(dest : ADDRESS; val : ADDRESS)=
  VAR
  BEGIN
    PutText("WBC : ");
    PutAddr(dest);
    PutText("<-");
    PutAddr(val);

  END WriteBarrierCAS;
PROCEDURE WriteBarrierENQ(dest : ADDRESS; val : ADDRESS)=
  VAR
  BEGIN
    PutText("WBE : ");
    PutAddr(dest);
    PutText("<-");
    PutAddr(val);

  END WriteBarrierENQ;

<* UNUSED *>
PROCEDURE IsAligned (msg : TEXT; foo  : ADDRESS; ra : ADDRESS) =
  VAR
    h  :RefHeader := foo-HeaderSize;
  BEGIN
    IF LocateHeaderOf(foo) # foo - HeaderSize THEN
      PutText(msg);
      PutText("IsAligned : foo/header/foo-header/size/tc ");
      PutAddr(foo);
      PutText("/");
      PutAddr(LocateHeaderOf(foo));
      PutText("/");
      PutAddr(foo-HeaderSize);
      PutText("/");
      PutInt(LocateHeaderOf(foo).size);
      PutText("/");
      PutInt(LocateHeaderOf(foo).typecode);
      PutCR();
      PutText("header according to foo : size/tc ");
      PutInt(h.size);
      PutText("/");
      PutInt(h.typecode);
      PutCR();
      PutText("RA ");
      PutAddr(ra);PutCR();
      Flush();
      RTOS.Crash();
    END;
  END IsAligned;

PROCEDURE WriteBarrierDEQ(head : ADDRESS; headnext : ADDRESS) =
  VAR
    d : RefDescriptor;
  BEGIN
    IF COLLECTING=0 THEN
      PutText("<WBDEQ : not collecting!>\n");Flush();
      RTOS.Crash();
    END;

    IF (head = NIL AND headnext = NIL) THEN
      RETURN;
    END;
    IF NOT Incremental THEN
      RETURN;
    END;
    RTOS.LockHeap();

    Spy.Enter(WBTimer);

    (* should really check the alignment of the object ptr *)
    IF head # NIL AND (head >= MEMORY_Start AND head < MEMORY_End) THEN
      d := head - Offset;
      IF d.header.gcStatus = WhiteStatus THEN
        MakeGray(d);
        d.header.gcStatus := GrayStatus;
      END;
    END;

    IF headnext # NIL AND (headnext >=MEMORY_Start AND headnext < MEMORY_End) THEN
      d := headnext- Offset;
      IF d.header.gcStatus = WhiteStatus THEN
        MakeGray(d);
        d.header.gcStatus := GrayStatus;
      END;
    END;

    Spy.Exit(WBTimer);
    RTOS.UnlockHeap();
  END WriteBarrierDEQ;



(* For measuring costs to mark per byte, per reference and per object,
   turn this on
   
   PerObj can be derived from MTMG spy timer
*)



CONST
  MeasureMarking =FALSE;
  
VAR
  (* number of mark calls *)
  MarkCalls : INTEGER;

  (* measure the pauses *)
  (*
  MarkStart : INTEGER;
  MarkEnd : INTEGER;
  *)
  MarkTime : INTEGER; (* in cycles *)
  MarkTimeMax : INTEGER;
  MarkTimeMin : INTEGER;

  (* # of objects marked  = # of references *)
  MarkRef : INTEGER;
  TotalMarkRef : INTEGER; (* for average *)
  PerRefMax : INTEGER;
  PerRefMin : INTEGER;

  MarkBytes:  INTEGER;
  TotalMarkBytes : INTEGER; (* for average *)
  PerBytesMax:  INTEGER;
  PerBytesMin:  INTEGER;


PROCEDURE InitMMG()=
  VAR
  BEGIN
    TotalMarkRef := 0;
    TotalMarkBytes := 0;

    MarkCalls := 0;
    MarkTime := 0;
    

    PerRefMax := 0;
    PerRefMin := 9999999999999;

    PerBytesMax := 0;
    PerBytesMin := 999999999999;


    MarkTimeMax := 0;
    MarkTimeMin := 999999999999;
  END InitMMG;

PROCEDURE EnterMG () =
  VAR
  BEGIN
    MarkRef := 0;
    INC(MarkCalls);
    MarkBytes := 0;
(*    MarkStart := RTHeapDep.Time();*)

    
  END EnterMG;

PROCEDURE ExitMG() =
  VAR
    pause : INTEGER;
  BEGIN
(*    MarkEnd := RTHeapDep.Time();*)
(*    pause := RTOSMachine.CycleMinus(MarkStart, MarkEnd);*)

    IF pause > MarkTimeMax THEN
      MarkTimeMax := pause;
    END;
    IF pause < MarkTimeMin THEN
      MarkTimeMin := pause;
    END;
    
    INC(MarkTime,pause);
    INC(TotalMarkRef,MarkRef);
    INC(TotalMarkBytes, MarkBytes);

    IF MarkRef # 0 THEN
      MarkRef := pause DIV MarkRef;
      MarkBytes := pause DIV MarkBytes;


    IF MarkBytes > PerBytesMax THEN
      PerBytesMax := MarkBytes;
    END;
    
    IF MarkBytes < PerBytesMin THEN
      PerBytesMin := MarkBytes;
    END;


    IF MarkRef > PerRefMax THEN
      PerRefMax := MarkRef;
    END;
    
    IF MarkRef < PerRefMin THEN
      PerRefMin := MarkRef;
    END;
    END;
    IF verbose > 1 THEN
      PutText("<M pause/refs/bytes   time/perref/perbyte ");
      PutInt(pause);  PutText("/");
      PutInt(MarkRef);  PutText("/");
      PutInt(MarkBytes);  PutText("  ");

      IF MarkRef # 0 THEN
        PutInt(pause DIV MarkRef);  PutText("/");
        PutInt(pause DIV MarkBytes);
      END;
      PutText(">\n");
    END;

  END ExitMG;

PROCEDURE DumpMG () =
  VAR
    perobj, perbyte,perref : INTEGER;
  BEGIN
    (* total pause *)
    PutText("\n----Total pause:\t");
    PutInt(MarkTime);
    PutText("/");
(*    PutInt(RTOSMachine.CycleToMicrosec(MarkTime));*)
    PutText("us");

    PutText("\nREFS/BYTES = ");
    PutInt(TotalMarkRef);
    PutText("/");
    PutInt(TotalMarkBytes);
    PutCR();

    (* max/min pause - same as per obj*) 
    PutText("\nMax pause:\t");
    PutInt(MarkTimeMax);
    PutText("/");
(*    PutInt(RTOSMachine.CycleToMicrosec(MarkTimeMax));*)
    PutText("us");

    PutText("\nMin pause:\t");
    PutInt(MarkTimeMin);
    PutText("/");
(*    PutInt(RTOSMachine.CycleToMicrosec(MarkTimeMin));*)
    PutText("us");

    (* time per obj *)
    perobj := MarkTime DIV MarkCalls;
    PutText("\n----------time per obj:\t");
    PutInt(perobj);
    PutText("/");
    IF perobj # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(perobj));*)
      PutText("us");
    END;

    (* time per bytes *)
    perbyte := MarkTime DIV TotalMarkBytes;
    PutText("\n--------time per byte:\t");
    PutInt(perbyte);
    PutText("/");
    IF perbyte # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(perbyte));*)
      PutText("us");
    END;

    PutText("\nMAX time per byte:\t");
    PutInt(PerBytesMax);
    PutText("/");
    IF PerBytesMax # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(PerBytesMax));*)
      PutText("us");
    END;
    PutText("\nMIN time per byte:\t");
    PutInt(PerBytesMin);
    PutText("/");
    IF PerBytesMin # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(PerBytesMin));*)
      PutText("us");
    END;
    

    (* time per refs *)
    perref := MarkTime DIV TotalMarkRef;
    PutText("\n---------time per ref:\t");
    PutInt(perref);
    PutText("/");
    IF perref # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(perref));*)
      PutText("us");
    END;

    PutText("\nMAX time per ref:\t");
    PutInt(PerRefMax);
    PutText("/");
    IF PerRefMax # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(PerRefMax));*)
      PutText("us");
    END;

    PutText("\nMin time per ref:\t");
    PutInt(PerRefMin);
    PutText("/");
    IF PerRefMin # 0 THEN
(*      PutInt(RTOSMachine.CycleToMicrosec(PerRefMin));*)
      PutText("us");
    END;

    PutCR();    Flush();
  END DumpMG;



<* UNUSED *>
PROCEDURE ReportStart() =
  VAR
    time := Spy.GetElapsed();
  BEGIN
    TotalFreeBytes := 0;
    FOR i:=0 TO NumFreeLists-1 DO
      INC(TotalFreeBytes,BytesFree[i]);
    END;
    PutText("\n-------START---------\n");
    PutText("TIME :\t ");
    PutInt(time);
    PutText("\nMEM  :\t ");
    PutInt(TotalFreeBytes);
    PutText("\n-------^^^^^---------\n");
  END ReportStart;

<* UNUSED *>
PROCEDURE ReportEnd() =
  VAR
    time := Spy.GetElapsed();
  BEGIN
    TotalFreeBytes := 0;
    FOR i:=0 TO NumFreeLists-1 DO
      INC(TotalFreeBytes,BytesFree[i]);
    END;
    PutText("\n-------END---------\n");
    PutText("TIME :\t ");
    PutInt(time);
    PutText("\nMEM  :\t ");
    PutInt(TotalFreeBytes);
    PutText("\n-------^^^^^---------\n");


  END ReportEnd;

PROCEDURE DistributeMemory() =
  BEGIN
  END DistributeMemory;

BEGIN
END RTHeapTM.



