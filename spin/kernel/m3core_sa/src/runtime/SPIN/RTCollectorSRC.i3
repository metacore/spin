(*| Copyright (C) 1993, Digital Equipment Corporation    *)
(*| All rights reserved.                                 *)
(*| See the file COPYRIGHT for a full description.       *)
(*| Last modified on Sun Feb 21 14:29:21 PST 1993 by jdd *)

(* "RTCollectorSRC" is an extension of "RTCollector", specific to the SRC
   Modula-3 implementation. *)

(*
 * HISTORY
 * 10-Dec-97  Tian Fung Lim (tian) at the University of Washington
 *	Increased size of arrays to support Wilson's collector.
 *
 * 19-Nov-97  Tian Fung Lim (tian) at the University of Washington
 *	Added ForceFragSample for memory utilization reporting.
 *
 * 05-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added fields to Statistics for Treadmill specific stats.
 *
 * 18-Jan-97  Richard Robinson (robinson) at the University of Washington
 *	Added fields to Statistics record for counting objects,
 *      fragmentation, free chunks, unused space.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added StopBackgroundCollection().  Verbose() takes an integer level
 *	of verbocity as argument. Addd Blink().  Added spies to account
 *	for allocation time.  Added backgroundThread which keeps the
 *	current background collector thread.  Exported InstallSanityCheck()
 *	and UninstallSanityCheck().
 * 
 * 02-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	More counters to keep track of the GC operations.  Added 
 *	ResetStat().
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added SweepClosure and SweepUntracedHeapCl.
 *
 * 04-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled turning the sweeping of untraced heap on and off.
 *
 * 29-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Turned off incremental and generational collection. 
 *
 * 17-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added CheckAmbiguous() for checking pointers from untraced memory
 *	to traced heap. Added counters of ambiguous roots and GC operations
 *	to the Statistics type.  Added DoTimings flag which enables the 
 *	Spy measurements of the collector.
 *
 * 31-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Removed Stat.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Deleted StatText and went to statistics oriented interface
 *	 (removed reliance on Text, Fmt, etc; this module was not the
 *	 right place to embed knowledge of html!)
 *
 * 01-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Added StatText which returns the Status information as a TEXT
 *      with embedded HTML commands.
 *)

INTERFACE RTCollectorSRC;
IMPORT Word, Spy, Thread;

(* \paragraph*{When to collect.}

   "StartCollection" and "FinishCollection" allow the programmer direct
   control over when to collect. *)

PROCEDURE StartCollection();
(* Start a total collection, if none is in progress and if collection and
   motion are enabled. *)

PROCEDURE FinishCollection();
(* Finish the current collection, if one is on progress. *)

(* \paragraph*{Disabling VM protection.}

   The SRC collector uses VM protection to implement incremental and
   generational collection.  The SRC collector's use of VM protection is
   normally invisible to programs, but can complicate debugging Modula-3
   programs.

   The "DisableVM" and "EnableVM" procedures are analogous to
   "RTCollector"'s "DisableMotion" and "EnableMotion", and allow the
   programmer to disable the use of VM protection.  Disabling VM protection
   does not disable the collector, but collections will be neither
   incremental nor generational.

   The "@M3novm" flag performs an initial call to "DisableVM".

   The SRC collector cannot use VM protection at all on some
   architectures. *)

PROCEDURE DisableVM();
(* Disable the use of VM protection.  While VM protection is disabled, no
   objects on the heap will be protected.*)

PROCEDURE EnableVM();
(* Reenable the use of VM protection if "EnableVM" has been called as many
   times as "DisableVM".  It is a checked runtime error to call "EnableVM"
   more times than "DisableVM". *)

PROCEDURE FinishVM();
(* Equivalent to "DisableVM{}; EnableVM()".  "FinishVM" unprotects all heap
   pages, and is intended for use from the debugger. *)

(* \paragraph*{Tuning the SRC collector.}

   The following read/write parameters tune the SRC collector's
   performance.  They may be set by the client at any point, although they
   may not have an immediate effect. *)

VAR gcRatio := 50;              (* collector work / mutator work *)
(* On the average, for every page allocated by the mutator, the collector
   will copy "gcRatio" pages.  Increase the ratio to keep the heap smaller;
   decrease it to spend less time in the collector. *)

VAR incremental := TRUE;       (* incremental collection *)
(* The collector can be incremental or stop-and-copy.  Incremental
   collection has much smaller interruptions of service, but takes more
   total time and more space.

   Assume there are ``A'' pages of accessible objects.  If "incremental" is
   FALSE, the heap must contain up to A * (2 + 1 / gcRatio) pages.  If
   "incremental" is TRUE, the heap must contain up to A * (2 + 2 / gcRatio)
   pages.  In other words, to keep the same space bounds, "gcRatio" must be
   twice as large in the incremental case.

   If VM protection is disabled or not available on the current
   architecture, the collector will behave as if "incremental" = FALSE.

   Use incremental collection when the program is interactive.
   Stop-and-copy collection gives better performance. *)

VAR generational := TRUE;      (* generational collection *)
(* Generational collection causes most collections to take much less time
   than specified above, while using only a little more memory.
   Generational collection has the greatest benefit when the program has a
   large number of accessible objects, but most new objects are discarded
   shortly after they are allocated.

   If VM protection is disabled or not available on the current
   architecture, the collector will behave as if "generational" = FALSE.

   Generational collection almost always leads to performance
   improvement. *)

(* \paragraph{Background Collection.}

   There is an optional ``background'' mode, which extends incremental mode
   with a background thread that moves collection ahead in the absence of
   program activity.  The background thread is tuned to cause insignificant
   interruption of other activities, but may therefore move the collection
   forward quite slowly. *)

PROCEDURE StartBackgroundCollection();
(* Starts the background thread, if not already started *)

PROCEDURE StopBackgroundCollection();
(* Exits the background thread, after it's done with the current collection *)

(* get allocator/collector statistics *)

TYPE
  Statistics = RECORD
                 vmPages             : INTEGER;   (* allocated + unallocated *)
                 allocatedPages      : INTEGER;
                 bytesPerPage        : INTEGER;
                 minAddress          : Word.T;
                 maxAddress          : Word.T;

                 maxPause            : INTEGER;
                 avePause            : INTEGER;
                 nPause              : INTEGER;
                 
                 threshold           : INTEGER;
                 threshold0          : INTEGER;
                 gcRatio             : INTEGER;
                 collectorState      : INTEGER;
                 activePages         : INTEGER;
                 activeBytes         : INTEGER;
                 freePages           : INTEGER;
                 freeBytes           : INTEGER;
                 smallNewPages       : INTEGER;
                 largeNewPages       : INTEGER;
                 smallCopyPages      : INTEGER;
                 largeCopyPages      : INTEGER;
                 smallPromotionPages : INTEGER;
                 largePromotionPages : INTEGER;
                 totalTracedBytes    : INTEGER;
                 totalUntracedBytes  : INTEGER;
                 autoCnt             : INTEGER;
                 reqCnt              : INTEGER;
                 backCnt             : INTEGER;
                 crashCnt            : INTEGER;
                 fullCnt             : INTEGER;
                 partCnt             : INTEGER;
                 highCnt             : INTEGER;
                 longCnt             : INTEGER;
                 nextCnt             : INTEGER;
                 manuCnt             : INTEGER;
                 nogenCnt            : INTEGER;
                 totalCnt            : INTEGER;
                 incCnt              : INTEGER;
                 nincCnt             : INTEGER;
                 faultCnt            : INTEGER;
                 grayCnt             : INTEGER;
                 oldCnt              : INTEGER;

                 ambRefCnt           : INTEGER;
                 strRefCnt           : INTEGER;
                 unsafeAmbRefCnt     : INTEGER;
                 uniqAmbRefCnt       : INTEGER;
                 uniqStrRefCnt       : INTEGER;
                 unsafeUniqRefCnt    : INTEGER;

                 allocCnt            : INTEGER;
                 deallocCnt          : INTEGER;
                 promoteCnt          : INTEGER;
                 moveCnt             : INTEGER;

                 shortPauses : ARRAY [0..100] OF INTEGER;
                 longPauses  : ARRAY [0..10000] OF INTEGER;

                 (* Stats above are tracked during normal allocation/collection
                    activities, the following require heap traversal.*)
                 pinnedPages         : INTEGER;

                 objectCount         : INTEGER;
                 objBytes            : INTEGER;
                 objLargest          : INTEGER;

                 forwardedCount      : INTEGER;
                 forwardedBytes      : INTEGER;
                 forwardedLargest    : INTEGER;

                 freeChunkCnt        : INTEGER;
                 freeChunkLargest    : INTEGER;

                 fillObjCnt          : INTEGER;
                 fillObjBytes        : INTEGER;
                 fillObjLargest      : INTEGER;

                 (* the unused tails of continued pages, not marked
                    in any way, is "lost" space *)
                 lostCount           : INTEGER;
                 lostBytes           : INTEGER;
                 lostLargest         : INTEGER;

                 (* Treadmill specific fields *)
                 tmNumFreeLists      : INTEGER; (* number of freelists *)
                 tmAllocations       : ARRAY [0..30] OF INTEGER; (* allocations per list *)
                 tmTriggers          : ARRAY [0..30] OF INTEGER; (* GC triggers per list *)
                 tmBalanceTriggers   : ARRAY [0..30] OF INTEGER; (* Balance triggers per list *)
                 tmTotalPages        : ARRAY [0..30] OF INTEGER; (* total pages in lists *)
                 tmFreePages         : ARRAY [0..30] OF INTEGER; (* # free pages in lists *)
                 tmFreeObjects       : ARRAY [0..30] OF INTEGER; (* # free objects per list *)
                 tmTotalObjects      : ARRAY [0..30] OF INTEGER; 
                 tmNodeSize          : ARRAY [0..30] OF INTEGER; 
               END;

PROCEDURE GetStatistics(VAR s: Statistics; traverseHeap: BOOLEAN := TRUE);
PROCEDURE GetMoreStatistics(VAR s: Statistics);

CONST 
  DoTimings = TRUE;
  DoTimingsXtra = FALSE;

PROCEDURE CheckAmbiguous (i: INTEGER);
PROCEDURE CheckAllRefs ();

(*
 * clean regions stuff
 *)

TYPE
  RegionT = RECORD 
    start, end: ADDRESS;
    stack: BOOLEAN;
  END;

TYPE
  RegionVector = UNTRACED REF ARRAY OF RegionT;

VAR
  cleanRegions: RegionVector := NIL;
  cleanRegionsCnt: INTEGER := 0;
  traceClean: BOOLEAN := FALSE;

PROCEDURE RegisterClean (start: ADDRESS; size: INTEGER; stack: BOOLEAN);
PROCEDURE UnregisterClean (start: ADDRESS; size: INTEGER := -1);
PROCEDURE DumpClean ();
PROCEDURE PrintCloseClean (p: ADDRESS);

PROCEDURE ShowUnsafeAmbiguous(on: BOOLEAN);
PROCEDURE AnchorUnsafeAmbiguous(on: BOOLEAN);
PROCEDURE SetSweepUntraced(on: BOOLEAN);
PROCEDURE Verbose(level: INTEGER);

PROCEDURE GetReference(p: ADDRESS): REFANY;
PROCEDURE TypeName (ref: ADDRESS): TEXT;

PROCEDURE MoveFromPinned (allowMovement:BOOLEAN);

VAR
  doEagerReclamation := FALSE;
  allowPartialCleanup := FALSE;
  cleanupAlmostEmpty := FALSE;
  moveFromPinned := FALSE;
  stopMoving := FALSE;
  countStrongRef := FALSE;
  doForce := FALSE;
  doPack := FALSE;

VAR
  almostEmptyCnt: INTEGER;
  almostEmptyNeeds: INTEGER;

CONST
  fullStatsOnBehind = FALSE;
  traceAmbiguous = FALSE;
  allowGrowHeap = FALSE;

VAR
  MaxStopMoving := 25;

PROCEDURE Moved (ref: ADDRESS): BOOLEAN;

TYPE 
  SweepClosure = OBJECT
  METHODS
    apply(start, stop: ADDRESS): BOOLEAN;
  END;

PROCEDURE SweepUntracedHeap (proc: PROCEDURE (start, end: ADDRESS));
PROCEDURE SweepUntracedHeapCl (cl: SweepClosure) : BOOLEAN;

VAR
  allocTraced_timer, allocUntraced_timer: Spy.T;
  allocInit_timer  : Spy.T;    (* initialization of traced objects *)
  pause_timer: Spy.T;
  nestedAlloc: BOOLEAN;

VAR
  backgroundThread: Thread.T;

PROCEDURE InstallSanityCheck ();
PROCEDURE UninstallSanityCheck ();
PROCEDURE ResetStat ();
PROCEDURE SetGCRatio (ratio: INTEGER);
PROCEDURE IsOnStack (p: ADDRESS): BOOLEAN;
(* FIXME
PROCEDURE RTInit();
*)

(* Force memory utilization sample *)
PROCEDURE ForceFragSample();

END RTCollectorSRC.
