(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Removed "blink" and "warnLow".
 *
 * 25-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added "frag" - memory utilization reporting.
 *
 * 31-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Removed text concatenations in treadmill stats.
 *
 * 05-May-97  Tian Fung Lim (tian) at the University of Washington
 *	Added "-treadmill" to "stat".  Treadmill statistics.
 *
 * 03-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added "-delta" to "heap".
 *
 * 12-Dec-96  Richard Robinson (robinson) at the University of Washington
 *	gc stat can be -full, meaning traverse the heap counting objects etc.
 *
 * 02-Dec-96  Przemek Pardyak (pardy) at the University of Washington
 *	More statistics.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned up.
 *
 * 04-Oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 04-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled turning the sweeping of untraced heap on and off.
 *
 * 22-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added printing the status of all the pages in the traced heap.
 *
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added enable and disable.
 *
 * 10-Mar-96 Przemek Pardyak (pardy) at the University of Washington
 *	Added "gc reach -type" and "gc where".
 *
 * 07-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Corrected the usage comment to reflect that gc heap reports the 
 *      allocation history, and not the current usage of the heap.
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 31-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed "gc heap -all" not to supress printing information about
 *	types that have to objects allocated for them. Removed an unused
 *	variable in Run.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Made unsafety explicit by using UnsafeConvert.
 *
 * 23-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Vastly simplified to not try to do fancy html formatting.
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	ParseParams.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Moved StatText from the bowels of RT to here.
 *
 * 28-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *	Added subcommands to call RTutils.Heap, RTHeapStats.ReportReachable
 *      and RTHeapDebug.CheckHeap. Modified all GC commands to return TEXT
 *      objects so they can be sent to a HTML document.
 * 
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * This module is unsafe because it imports RTHeapRep and UnsafeConvert.
 *)

UNSAFE MODULE GC;

IMPORT RTCollector, RTCollectorSRC, RTIO, RTHeapRep;
IMPORT RTutils, RTHeapStats, RTHeapPages, RTStrongRef, RT0u, RTHeapDep;
IMPORT CPU, CPUPrivate, IO, Fmt, ParseParams;
IMPORT Thread, ThreadExtra, Text, Textify, UnsafeConvert, RTHeapTrace;
IMPORT TextRd, Rd, Lex, Sched, ThreadRep, RTOS;
(* IMPORT RTHeapMap;*)

IMPORT InfoFile, Wr, Error;

CONST
  PRINT_FREQ = 10;

CONST
  MAX_CRAZY = 10000;

TYPE
  RRT = REF RECORD
    a1: ARRAY [1..1000] OF INTEGER; 
    a2: INTEGER;
    a3: BOOLEAN;
    next: RRT;
 END;

TYPE
  ArgT = REF RECORD
    np: INTEGER;
    nc: INTEGER;
    nb: INTEGER;
    ml: INTEGER;
  END;

PROCEDURE SanityCheck() =
  VAR
    spl: CPU.InterruptLevel;
  BEGIN
    IO.Put("GC sanity: Checking Heap.\n");
    spl := CPUPrivate.SetInterruptMask(
               CPUPrivate.InterruptClass.High);
    RTHeapRep.InvokeMonitors(TRUE);
    CPUPrivate.RestoreInterruptMask(spl);
    IO.Put("GC sanity: Heap OK.\n");
  END SanityCheck;

PROCEDURE CrazyAllocator (keep: BOOLEAN; limit: INTEGER) =
  VAR
    trash: RRT;
    list: RRT;
  BEGIN
    FOR i := 1 TO limit DO
      IF i MOD (limit DIV PRINT_FREQ) = 0 THEN
        IO.Put( "crazy: " & Fmt.Int(i) & "\n" );
      END;
      trash := NEW (RRT);
      IF keep THEN
        trash.next := list;
        list := trash;
      END;
    END;
  END CrazyAllocator;

CONST
  NO_PRODUCERS = 5;
  NO_CONSUMERS = 5;
  BUF_SIZE = 1;
  MAX_LOOP = 10000;

TYPE
  ValT = REF RECORD
    a1: ARRAY [1..1000] OF INTEGER; 
    who: INTEGER;
    val: INTEGER;
  END;

VAR
  buf: REF ARRAY OF ValT;
  b_cnt: INTEGER;
  m_cnt: INTEGER;
  first_taken, first_free, free_cnt, taken_cnt: INTEGER;
  mu: MUTEX;
  free_cond: Thread.Condition;
  taken_cond: Thread.Condition;
  pf: INTEGER;
  cpf: INTEGER;

TYPE 
  Consumer = Thread.Closure OBJECT
    id: INTEGER;
    ml: INTEGER;
  METHODS
    initc(id: INTEGER): Consumer := InitConsumer;
  OVERRIDES
    apply := RunConsumer;
  END;

  Producer = Thread.Closure OBJECT
    id: INTEGER;
    ml: INTEGER;
  METHODS
    initp(id: INTEGER; ml: INTEGER): Producer := InitProducer;
  OVERRIDES
    apply := RunProducer;
  END;

PROCEDURE InitProducer (self: Producer; id: INTEGER; ml: INTEGER): Producer =
  BEGIN
    self.id := id;
    self.ml := ml;
    RETURN self;
  END InitProducer;

PROCEDURE InitConsumer (self: Consumer; id: INTEGER): Consumer =
  BEGIN
    self.id := id;
    RETURN self;
  END InitConsumer; 

PROCEDURE RunProducer (self: Producer): REFANY =
  VAR
    rec: ValT;
  BEGIN
    FOR i := 1 TO self.ml DO
      Thread.Acquire(mu);
        WHILE free_cnt = 0 DO
          Thread.Wait(mu, free_cond);
        END;

        IF i MOD (self.ml DIV pf) = 0 THEN
          IO.Put ("[P: " & Fmt.Int(first_free) & " " &
            Fmt.Int(self.id) & " " & Fmt.Int(i) & "] " );
        END;

        rec := NEW(ValT);
        rec.who := self.id;
        rec.val := i;
        buf[first_free] := rec;

        INC(first_free);
        IF first_free = BUF_SIZE THEN
          first_free := 0;
        END;
        DEC(free_cnt);
        INC(taken_cnt);
        IF taken_cnt = 1 THEN
          Thread.Signal(taken_cond);
        END;
      Thread.Release(mu);
    END;
    IO.Put("[[P " & Fmt.Int(self.id) & " done]\n");
    RETURN NIL;
  END RunProducer;

PROCEDURE RunConsumer (self: Consumer): REFANY =
  VAR
    who, val, i: INTEGER;
  BEGIN
    i := 0;
    LOOP
      Thread.Acquire(mu);
        IF b_cnt = 0 THEN
          Thread.Release(mu);
          EXIT;
        ELSE
          INC(i);
          DEC(b_cnt);

          WHILE taken_cnt = 0 DO
            Thread.Wait(mu, taken_cond);
          END;

          who := buf[first_taken].who;
          val := buf[first_taken].val;
          buf[first_taken] := NIL;
          
          IF b_cnt MOD (m_cnt DIV cpf) = 0 THEN
            IO.Put ("[C: " & Fmt.Int(self.id) & " " & Fmt.Int(i) & " " & 
              Fmt.Int(first_taken) & " " &
              Fmt.Int(who) & " " & Fmt.Int(val) & "] " );
          END;
          
          INC(first_taken);
          IF first_taken = BUF_SIZE THEN
            first_taken := 0;
          END;
          DEC(taken_cnt);
          INC(free_cnt);
          IF free_cnt = 1 THEN
            Thread.Signal(free_cond);
          END;
        END;
      Thread.Release(mu);
    END;
    IO.Put (" [[C: " & Fmt.Int(self.id) & " got: " & Fmt.Int(i) & "]]\n");
    RETURN NIL;
  END RunConsumer;

PROCEDURE TestThreads(a: ThreadExtra.ArgT) : ThreadExtra.ResultT =
  VAR
    arg: ArgT := NARROW(a, ArgT);
    producers: REF ARRAY OF Thread.T;
    consumers: REF ARRAY OF Thread.T;
  BEGIN
    IF arg.ml < PRINT_FREQ THEN
      pf := arg.ml;
      cpf := arg.ml;
    ELSE
      pf := PRINT_FREQ;
      cpf := PRINT_FREQ * arg.nc;
    END;

    b_cnt := arg.np * arg.ml;
    m_cnt := b_cnt;

    producers := NEW(REF ARRAY OF Thread.T, arg.np);
    consumers := NEW(REF ARRAY OF Thread.T, arg.nc);
    buf := NEW(REF ARRAY OF ValT, arg.nb);
    mu := NEW(MUTEX);

    free_cond := NEW(Thread.Condition);
    taken_cond := NEW(Thread.Condition);
    first_free := 0;
    first_taken := 0;
    free_cnt := BUF_SIZE;
    taken_cnt := 0;

    FOR i := 0 TO arg.np-1 DO
      producers[i] := Thread.Fork(NEW(Producer).initp(i, arg.ml));
    END;
    
    FOR i := 0 TO arg.nc-1 DO
      consumers[i] := Thread.Fork(NEW(Consumer).initc(i));
    END;

    FOR i := 0 TO arg.np-1 DO
      EVAL Thread.Join(producers[i]);
      IO.Put("Join P " & Fmt.Int(i) & "\n");
    END;
    
    FOR i := 0 TO arg.nc-1 DO
      EVAL Thread.Join(consumers[i]);
      IO.Put("Join C " & Fmt.Int(i) & "\n");
    END;

    IO.Put("Done\n");

    RETURN NIL;
  END TestThreads;


PROCEDURE Test() =
  VAR
    th: Thread.T;
    a: ArgT;
  BEGIN
    IO.Put (">>> GCTest:: started\n" );

    CrazyAllocator(FALSE, MAX_CRAZY);
    IO.Put ("\n>>> GCTest:: crazy allocator done\n\n" );
    
    a := NEW(ArgT,np:=NO_PRODUCERS,nc:=NO_CONSUMERS,nb:=BUF_SIZE,ml:=MAX_LOOP);
    th := ThreadExtra.PFork(TestThreads, a (*, "TestGC with threads"*));
    EVAL Thread.Join(th);
    IO.Put ("\n>>> GCTest:: thread test done\n\n" );

    IO.Put ("\n\n>>> GCTest:: done\n" );
  END Test;

PROCEDURE Kill() =
  BEGIN
    IO.Put (">>> GC kill: started\n" );
    CrazyAllocator(TRUE, MAX_CRAZY);
    IO.Put ("\n\n>>> GC kill: done (WHICH IS KINDA WEIRD!!!)\n" );
  END Kill; 

PROCEDURE Incremental() =
  BEGIN
    IO.Put (">>> GC incremental test: started\n" );

    FOR i := 2 TO 25 DO
      IO.Put ("-------- " & Fmt.Int(i) & "\n");
      CrazyAllocator(TRUE, (i * MAX_CRAZY) DIV 25);
    END;

    IO.Put ("\n\n>>> GC incremental test: done\n" );
  END Incremental; 

(* This Putter should go in a general utility file *)


PROCEDURE PutChar(<*UNUSED*>p: Putter; c: CHAR) =
  BEGIN IO.Put(Text.FromChar(c));END PutChar;

PROCEDURE PutText(<*UNUSED*>p: Putter; t: TEXT) =
  BEGIN IO.Put(t); END PutText;

PROCEDURE PutInt(<*UNUSED*>p: Putter; n: INTEGER; width := 0) =
  BEGIN IO.PutInt(n, width);END PutInt;

PROCEDURE PutString(<*UNUSED*>p: Putter; s: ADDRESS) =
  BEGIN
    IO.Put(UnsafeConvert.StoT(s, 256));
  END PutString;

PROCEDURE PutAddr(<*UNUSED*> p: Putter; a: ADDRESS; <*UNUSED*>width := 0) =
  BEGIN IO.Put(Textify.Address(a)); END PutAddr;

PROCEDURE PutHex(<*UNUSED*> p: Putter; i: INTEGER; <*UNUSED*>width := 0) =
  BEGIN IO.Put(Fmt.Unsigned(i)); END PutHex;

PROCEDURE Flush (<*UNUSED*>p: Putter) =
  BEGIN  END Flush;
    
TYPE Putter =  RTIO.SimplePutter OBJECT
  OVERRIDES
    (* We should really override all of the services *)
    putChar := PutChar;
    putText := PutText;
    putInt := PutInt;
    putString := PutString;
    putHex := PutHex;
    putAddr := PutAddr;
    flush := Flush;
  END;

PROCEDURE GCUsage () =
  BEGIN
    IO.Put("gc usage: Type \"gc\" followed by one of the following options\n");
    IO.Put("=> collector commands:\n");
    IO.Put("\tcollect - perform a collection\n");
    IO.Put("\tcheck - run sanity checks\n");
    IO.Put("=> collector settings:\n");
    IO.Put("\tcollection {enable|disable} - enable collections\n");
    IO.Put("\tmotion {enable|disable} - enable moving objects\n");
    IO.Put("\tbackground {on|off} - enable background collection\n");
    IO.Put("\tincremental {on|off} - enable incremental collection\n");
    IO.Put("\tgenerational {on|off} - enable generational collection\n");
    IO.Put("\tvm {enable|disable} - enable vm protection for collection\n");
    IO.Put("\t\tvm must be enabled to use incremental and generational\n");
    IO.Put("\tratio <n> - the ratio between allocator and collector\n");
    IO.Put("\t\tincrease for smaller heap, decrease for shorter collections\n");
    IO.Put("\tuntraced - checking untraced heap for pointers to traced heap\n");
    IO.Put("\t\tshow - show unsafe pointers during collection\n");
    IO.Put("\t\tunshow - stop showing unsafe pointers\n");
    IO.Put("\t\tanchor - immobilize objects pointed to by unsafe pointers\n");
    IO.Put("\t\tunanchor - stop anchoring\n");
    IO.Put("\t\tsweep - enable sweeping (necessary for show or anchor)\n");
    IO.Put("\t\tunsweep - to disable sweeping\n");
    IO.Put("\tsanity {on|off} - enable sanity checks on each collection\n");
    IO.Put("\tunpin {on|off} - enable moving objects from pinned pages\n");

    IO.Put("=> information about the collector:\n");
    IO.Put("\tverbose <n> - print what is going on with GC\n");
    IO.Put("\tstat [-full] - display summary of heap status\n");
    IO.Put("\tpages - show status of all pages\n");
    IO.Put("\t\t-full - show all ranges of pages\n");
    IO.Put("\t\t\totherwise, show only a summary\n");
    IO.Put("\tstrongrefs - show strongrefed objects\n");
    IO.Put("\theap [-all|-delta] {-type|-number|-bytes} [<n>] - print the allocation summary\n\n");
    IO.Put("\t\t-all - show even types that are not allocated\n");
    IO.Put("\t\t-delta - show the difference since the last scan\n");
    IO.Put("\t\t-type, -number, -bytes - choose how to sort the output\n");
    IO.Put("\t\t<n> - maximum number of entries to print\n");
    IO.Put("\treach [-detailed] [-type <tc>] - display the reachable memory\n");
    IO.Put("\t\t-type <tc> - only memory allocated for the type w/ typecode <tc>\n");
    IO.Put("\t\t-detailed - print all roots, otherwise only summaries per unit and stack\n");
    IO.Put("\t\t\tall memory otherwise\n");
    IO.Put("\twhere [-all] [-detailed] {-type <tc>|<pointer>} - show locations of pointers\n");
    IO.Put("\t\t-type <tc> - pointers to all objects of type w/ typecode <tc>\n");
    IO.Put("\t\t<pointer> - pointers to object pointed to by <pointer>\n");
    IO.Put("\t\t-all - show all locations (otherwise only one per object)\n");
    IO.Put("\t\t-detailed - print all roots, otherwise only summaries per unit and stack\n");
    IO.Put("\tpc - show allocation per call site (DISABLED)\n");

    IO.Put("=> collector tests\n");
    IO.Put("\ttest - stress test of the allocator and GC\n");
    IO.Put("\tinc  - allocate increasing amounts of memory until you run out\n");
    IO.Put("\tkill - allocate memory until you run out\n");
    IO.Put("\tdist - distribute reserve pool (applicable only to Wilson collector)\n");
    IO.Put("\tfrag - show utilization/fragmentation numbers; turns on fragmentation reporting if off\n");
    IO.Put("\thisto - show histogram of pauses\n");
  END GCUsage;

VAR 
  s: RTCollectorSRC.Statistics;

PROCEDURE TMStat () =
  VAR
    totalPages : INTEGER := 0;
    totalFreeBytes : INTEGER := 0; 
    totalFreePages : INTEGER := 0; 
  BEGIN
    RTOS.LockHeap ();
    RTCollectorSRC.GetStatistics(s);
    (* quit if certain critical numbers are not set properly *)
    IF s.tmTotalPages[0] = 0 THEN
      IO.Put("This is not the Treadmill collector.  Run without the -treadmill option\n");
      RTOS.UnlockHeap();
      RETURN;
    END;

    IO.Put(">>> Treadmill Statistics:");
    IO.Put("\n\tFreelists = ");
    IO.PutInt(s.tmNumFreeLists);
    IO.Put("\n\tFree pages / Total pages : ");

    FOR i := 0 TO s.tmNumFreeLists-1 DO
      IO.Put("\n\t\tList #");
      IO.PutInt(i);
      IO.Put(" : ");
      IO.PutInt(s.tmFreePages[i]);
      IO.Put("\t/\t");
      IO.PutInt(s.tmTotalPages[i]);
      IO.Put("  \t");
      IF s.tmTotalPages[i] # 0 THEN
        IO.PutInt((s.tmFreePages[i]*100) DIV s.tmTotalPages[i]);
      ELSE
        IO.PutInt(0);
      END;
      IO.Put("% free");
      INC(totalPages, s.tmTotalPages[i]);
      INC(totalFreePages, s.tmFreePages[i]);
    END;
    IO.Put("\n\t\ttotal : ");
    IO.PutInt(totalFreePages);
    IO.Put("\t/\t");
    IO.PutInt(totalPages);
    IO.Put("\t");
    IF totalPages # 0 THEN
      IO.PutInt((totalFreePages*100) DIV totalPages);
    END;
    IO.Put("%");

    IO.Put("\n\tFree objects by list :");

    (* skip reservepool since i don't keep stats on its objects *)
    FOR i := 0 TO s.tmNumFreeLists-2 DO
      IO.Put("\n\t\tList #");
      IO.PutInt(i);
      IO.Put(" : ");
      IO.PutInt(s.tmFreeObjects[i]);
      IO.Put("\t/\t");
      IO.PutInt(s.tmTotalObjects[i]);
      IO.Put("  \t");
      IF s.tmTotalObjects[i] # 0 THEN
        IO.PutInt((s.tmFreeObjects[i]*100) DIV s.tmTotalObjects[i]);
      ELSE
        IO.PutInt(0);
      END;
      IO.Put("% free");
      INC(totalFreeBytes, s.tmFreeObjects[i] * s.tmNodeSize[i]);
    END;
    (* count respool free bytes *)
    INC(totalFreeBytes, s.tmFreePages[s.tmNumFreeLists-1] * 8192);

    IO.Put("\n\tTotal free bytes = ");
    IO.PutInt(totalFreeBytes);
    IO.Put("\t/\t");
    IO.PutInt(totalPages*8192);
    IO.Put("\t");
    IF totalPages # 0 THEN
      IO.PutInt((totalFreeBytes*100) DIV (totalPages*8192));
    END;
    IO.Put("%");

    IO.Put("\n>>> Collection ");
    IO.Put("\n\t# Collections = ");
    IO.PutInt(s.fullCnt);
    IO.Put("\n\tGC Triggers = ");
    FOR i := 0 TO s.tmNumFreeLists-1 DO
      IO.Put("\n\t\tList #");
      IO.PutInt(i);
      IO.Put(" : ");
      IO.PutInt(s.tmTriggers[i]);
    END;
    IO.Put("\n\tBalance Triggers = ");
    FOR i := 0 TO s.tmNumFreeLists-1 DO
      IO.Put("\n\t\tList #");
      IO.PutInt(i);
      IO.Put(" : ");
      IO.PutInt(s.tmBalanceTriggers[i]);
    END;

    IO.Put("\n>>> Allocations :");
    IO.Put("\n\tTotal Traced (since last reset) = ");
    IO.PutInt(s.totalTracedBytes);
    IO.Put("\n\tAllocations by list :");
    FOR i := 0 TO s.tmNumFreeLists-1 DO
      IO.Put("\n\t\tList #");
      IO.PutInt(i);
      IO.Put(" : ");
      IO.PutInt(s.tmAllocations[i]);
      IF i # s.tmNumFreeLists-1 THEN
        (* do not know how large each obj in res pool is *)
        IO.Put("  \t(");
        IO.PutInt(s.tmAllocations[i]*s.tmNodeSize[i]);
        IO.Put(" bytes)");
      END;
    END;
    IF s.fullCnt # 0 THEN
      IO.Put("\n\nRemapping Effectiveness : \n");
      IO.Put("\tAve bytes free at collection = ");
      IO.PutInt(s.autoCnt DIV s.fullCnt);
      IO.Put("\n");
      IO.Put("\tAve pages free at collection = ");
      IO.PutInt(s.reqCnt DIV s.fullCnt);
      IO.Put("\n");
      IO.Put("\n");
    END;
    RTOS.UnlockHeap ();
  END TMStat;

PROCEDURE Stat (traverseHeap := FALSE) =
  BEGIN
    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    RTCollectorSRC.GetStatistics(s (*, traverseHeap*));
    IF s.tmTotalPages[0] # 0 THEN
      IO.Put("This is the Treadmill collector.  Running treadmill specific stats\n");
      TMStat();
      RETURN;
    END;
    IO.Put(">>> Collector:");
    IO.Put("\n\tcollection   : ");
    IF RTHeapRep.disableCount > 0 THEN 
      IO.Put("DISABLED"); 
    ELSE 
      IO.Put("ENABLED"); 
    END;
    IO.Put("\n\tmotion       : ");
    IF RTHeapRep.disableMotionCount > 0 THEN 
      IO.Put("DISABLED"); 
    ELSE 
      IO.Put("ENABLED"); 
    END;
    IO.Put("\n\tvm support   : ");
    IF RTHeapDep.VM THEN IO.Put("YES"); ELSE IO.Put("NO"); END;
    IO.Put("\n\tvm           : ");
    IF RTHeapRep.disableVMCount > 0 THEN 
      IO.Put("DISABLED"); 
    ELSE 
      IO.Put("ENABLED"); 
    END;
    IO.Put("\n\tincremental  : "); 
    IF RTCollectorSRC.incremental THEN IO.Put("ON"); ELSE IO.Put("OFF"); END;
    IO.Put("\n\tgenerational : ");
    IF RTCollectorSRC.generational THEN IO.Put("ON"); ELSE IO.Put("OFF"); END;
    IO.Put("\n\tbackground   : ");
    IF RTHeapRep.startedBackground THEN IO.Put("ON"); ELSE IO.Put("OFF"); END;
(*
    IO.Put("\n\tunpin objects: "); 
    IF RTCollectorSRC.moveFromPinned THEN IO.Put("ON"); ELSE IO.Put("OFF"); END;
*)
    IO.Put("\n>>> Heap:");
    IO.Put("\n\tpage size : ");
    IO.PutInt(s.bytesPerPage);
    IO.Put("\n\theap size : " );
    IO.PutInt(s.allocatedPages * s.bytesPerPage DIV 1000000);
    IO.Put(" MB, ");
    IO.PutInt(s.allocatedPages);
    IO.Put(" pages\n\taddress range: 0x");
    IO.Put(Fmt.Unsigned(s.minAddress));
    IO.Put(" - 0x");
    IO.Put(Fmt.Unsigned(s.maxAddress));
    IO.Put("\n>>> Current state of collection:\n\t");
    IO.PutInt(s.collectorState);
    IO.Put("\n>>> Current page assignment:\n\t");
    IO.PutInt(s.activePages);
    IF s.allocatedPages # 0 THEN
      IO.Put(" (");
      IO.PutInt(s.activePages * 100 DIV s.allocatedPages);
      IO.Put("%) used pages");
    END;
    IO.Put("\n\t");
    IO.PutInt(s.freePages);
    IF s.allocatedPages # 0 THEN
      IO.Put(" (");
      IO.PutInt(s.freePages * 100 DIV s.allocatedPages);
      IO.Put("%) free pages");
    END;
    IO.Put("\n\t");

    IO.Put(">>> Current ambiguous roots:\n");
    IO.Put("\tambiguous pointers        : "); IO.PutInt(s.ambRefCnt); 
    IO.Put("\n\tstrongref-ed pointers     : "); IO.PutInt(s.strRefCnt); 
    IO.Put("\n\tunsafe ambiguous pointers : "); IO.PutInt(s.unsafeAmbRefCnt); 
    IO.Put("\n\timmobilized objects       : "); IO.PutInt(s.uniqAmbRefCnt); 
    IO.Put("\n\tstrongref-ed objects      : "); IO.PutInt(s.uniqStrRefCnt); 
    IO.Put("\n\tunsafe objects            : "); IO.PutInt(s.unsafeUniqRefCnt); 

    IO.Put("\n>>> Garbage collections:\n");
    IO.Put("\ttotal collections        : ");
    IO.PutInt(s.totalCnt);
    IO.Put("\n\tautomatic collections    : ");
    IO.PutInt(s.autoCnt);
    IO.Put("\n\tmanual collections     : ");
    IO.PutInt(s.reqCnt);
    IO.Put("\n\tbackground collections : ");
    IO.PutInt(s.backCnt);
    IO.Put("\n\tcrash collections      : ");
    IO.PutInt(s.crashCnt);
    IO.Put("\n\tincremental collections     : ");
    IO.PutInt(s.incCnt);
    IO.Put("\n\tnon-incremental collections : ");
    IO.PutInt(s.nincCnt);
    IO.Put("\n\tpartial collections    : ");
    IO.PutInt(s.partCnt);
    IO.Put("\n\tfull collections       : ");
    IO.PutInt(s.fullCnt);
    IO.Put("\n\t  above high   : ");
    IO.PutInt(s.highCnt);
    IO.Put("\n\t  long cycle   : ");
    IO.PutInt(s.longCnt);
    IO.Put("\n\t  next         : ");
    IO.PutInt(s.nextCnt);
    IO.Put("\n\t  manual       : ");
    IO.PutInt(s.manuCnt);
    IO.Put("\n\t  because not generational : ");
    IO.PutInt(s.nogenCnt);
    IO.Put("\n\tfaults                 : ");
    IO.PutInt(s.faultCnt);
    IO.Put("\n\t  on gray              : ");
    IO.PutInt(s.grayCnt);
    IO.Put("\n\t  on old               : ");
    IO.PutInt(s.oldCnt);
    IO.Put("\n");

    IO.Put("\n>>> Pauses:\n" & "\tmax: " & Fmt.Int(s.maxPause) & 
      "usec\n\tave: " & Fmt.Int(s.avePause) & 
      "usec\n\tn: " & Fmt.Int(s.nPause) & "\n");

    (*
    FOR i := FIRST(s.shortPauses) TO LAST(s.shortPauses) DO
      IF s.shortPauses[i] # 0 THEN
        IO.Put(Fmt.Int(i*10000) & " " & Fmt.Int(s.shortPauses[i]) & "\n");
      END;
    END;
    FOR i := FIRST(s.longPauses) TO LAST(s.longPauses) DO
      IF s.longPauses[i] # 0 THEN
        IO.Put(Fmt.Int(i*25000) & " " & Fmt.Int(s.longPauses[i]) & "\n");
      END;
    END;
    *)

    IO.Put(">>> Allocation (since boot time):\n");
    IO.Put("\ttotal traced allocated   : ");
    IO.PutInt(s.totalTracedBytes DIV (1024*1024));
    IO.Put(" MB ");
    IO.PutInt(s.totalTracedBytes);
    IO.Put(" bytes\n");
    IO.Put("\ttotal untraced allocated : ");
    IO.PutInt(s.totalUntracedBytes DIV (1024*1024));
    IO.Put(" MB ");
    IO.PutInt(s.totalUntracedBytes);
    IO.Put(" bytes\n");

    IO.Put(">>> object operations:\n");
    IO.Put("\tallocate   : "); IO.PutInt(s.allocCnt); IO.Put("\n");
    IO.Put("\tdeallocate : "); IO.PutInt(s.deallocCnt); IO.Put("\n");
    IO.Put("\tpromote    : "); IO.PutInt(s.promoteCnt); IO.Put("\n");
    IO.Put("\tmove       : "); IO.PutInt(s.moveCnt); 

    IO.Put("\n>>> Current triggers:\n\t");
    IO.Put("threshold: ");
    IO.PutInt(s.threshold);
    IO.Put("\n\tthreshold0: ");
    IO.PutInt(s.threshold0);
    IO.Put("\n\tgcRatio: ");
    IO.PutInt(s.gcRatio);

    IO.Put("\n");

(*
    IF traverseHeap THEN
      IO.Put(">>> Heap Utilization:\n");

      IO.Put("\t");
      IO.PutInt(s.vmPages);
      IO.Put(" total pages from VM\n");
      IO.Put("\t");
      IO.PutInt(s.vmPages - (s.allocatedPages));
      IO.Put(" unallocated pages\n");

      IO.Put("\t");
      IO.PutInt(s.pinnedPages);
      IO.Put(" pinned pages (");
      IO.PutInt(s.pinnedPages * 100 DIV s.activePages);
      IO.Put("% of active)\n");

      IO.Put("\n\ttotal objects             : "); IO.PutInt(s.objectCount); 

      IO.Put("\n\ttotal object space        : ");
      IO.PutInt(s.objBytes DIV (1024*1024));
      IO.Put(" MB ");
      IO.PutInt(s.objBytes);
      IO.Put(" bytes\n");

      IO.Put("\tlargest object            : "); IO.PutInt(s.objLargest);
      IO.Put(" bytes\n");

      IO.Put("\n\ttotal forwarded           : "); IO.PutInt(s.forwardedCount); 

      IO.Put("\n\ttotal forwarded space     : ");
      IO.PutInt(s.forwardedBytes DIV (1024*1024));
      IO.Put(" MB ");
      IO.PutInt(s.forwardedBytes);
      IO.Put(" bytes\n");

      IO.Put("\tlargest forwarded         : "); IO.PutInt(s.forwardedLargest);
      IO.Put(" bytes\n");

      IO.Put("\n\t");
      IO.PutInt(s.freeChunkCnt);
      IO.Put(" free chunks\n");

      IO.Put("\tlargest free chunk        : "); IO.PutInt(s.freeChunkLargest);
      IO.Put(" pages\n");

      IO.Put("\n\t");
      IO.PutInt(s.fillObjCnt);
      IO.Put(" filler objects\n");

      IO.Put("\ttotal filler space        : ");
      IO.PutInt(s.fillObjBytes DIV (1024*1024));
      IO.Put(" MB ");
      IO.PutInt(s.fillObjBytes);
      IO.Put(" bytes\n");

      IO.Put("\tlargest fill object       : "); IO.PutInt(s.fillObjLargest);
      IO.Put(" bytes\n");

      IO.Put("\n\t");
      IO.PutInt(s.lostCount);
      IO.Put(" continued pages with lost space\n");

      IO.Put("\ttotal lost space          : ");
      IO.PutInt(s.lostBytes DIV (1024*1024));
      IO.Put(" MB ");
      IO.PutInt(s.lostBytes);
      IO.Put(" bytes\n");

      IO.Put("\tlargest lost chunk        : "); IO.PutInt(s.lostLargest);
      IO.Put(" bytes\n");

    END;
*)
    RTOS.UnlockHeap ();
  END Stat;

PROCEDURE AllocCnts () =
  BEGIN
    RTOS.LockHeap();
    (* FOR i := FIRST(RT0u.alloc_cnts^) TO LAST(RT0u.alloc_cnts^) DO*)
    FOR i := 0 TO RT0u.nTypes DO
      WITH cnt = RT0u.alloc_cnts[i] DO
        IF cnt # 0 THEN
          IO.Put(Fmt.Int(i) & " " & Fmt.Int(cnt) & " " & 
            Fmt.Int(RT0u.alloc_bytes[i]) & "\n");
        END;
      END;
    END;
    RTOS.UnlockHeap();
  END AllocCnts;

PROCEDURE ZeroCnts () =
  BEGIN
    RTOS.LockHeap();
    FOR i := 0 TO RT0u.nTypes DO
      RT0u.alloc_cnts[i] := 0;
      RT0u.alloc_bytes[i] := 0;
    END;
    RTOS.UnlockHeap();
  END ZeroCnts;

VAR putter := NEW(Putter);

PROCEDURE ShowHeap (pp: ParseParams.T) =
  VAR
    suppressZeros := TRUE;
    presentation  := RTutils.HeapPresentation.ByTypecode;
    window        := LAST(INTEGER);
    delta         := FALSE;
    reach         := FALSE;
  BEGIN
    IF pp.keywordPresent("-all") THEN
      suppressZeros := FALSE;
    END;
    IF pp.keywordPresent("-delta") THEN
      delta := TRUE;
    END;
    IF pp.keywordPresent("-reachable") THEN
      reach := TRUE;
    END;
    IF pp.keywordPresent("-type") THEN
      presentation := RTutils.HeapPresentation.ByTypecode;
    ELSIF pp.keywordPresent("-number") THEN
      presentation := RTutils.HeapPresentation.ByNumber;
    ELSIF pp.keywordPresent("-bytes") THEN
      presentation := RTutils.HeapPresentation.ByByteCount;
    END;
    TRY
      window := pp.getNextInt();
    EXCEPT
      ParseParams.Error =>
    END;

    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    IF reach THEN
      RTHeapStats.ReportReachable(putter, 0, 0, FALSE, TRUE, FALSE, FALSE);
    ELSE
      IF delta THEN
        RTutils.NewHeap(suppressZeros, presentation, window, putter);
      ELSE
        RTutils.Heap(suppressZeros, presentation, window, putter);
      END;
    END;
    RTOS.UnlockHeap ();
  END ShowHeap;

PROCEDURE Untraced (pp: ParseParams.T) RAISES { ParseParams.Error } =
  BEGIN
    IF pp.keywordPresent("show") THEN
      RTCollectorSRC.ShowUnsafeAmbiguous(TRUE);
    ELSIF pp.keywordPresent("unshow") THEN
      RTCollectorSRC.ShowUnsafeAmbiguous(FALSE);
    ELSIF pp.keywordPresent("anchor") THEN
      RTCollectorSRC.AnchorUnsafeAmbiguous(TRUE);
    ELSIF pp.keywordPresent("unanchor") THEN
      RTCollectorSRC.AnchorUnsafeAmbiguous(FALSE);
    ELSIF pp.keywordPresent("sweep") THEN
      RTCollectorSRC.SetSweepUntraced(TRUE);
    ELSIF pp.keywordPresent("nosweep") THEN
      RTCollectorSRC.SetSweepUntraced(FALSE);
    ELSE
      RAISE ParseParams.Error;
    END;    
  END Untraced;

PROCEDURE Enable () =
  BEGIN
    RTCollector.Enable();
  END Enable;

PROCEDURE Disable () =
  BEGIN
    RTCollector.Disable();
  END Disable;

PROCEDURE PrintReachability (pp: ParseParams.T) RAISES { ParseParams.Error } =
  VAR
    tc: INTEGER := 0;
    detailed := FALSE;
  BEGIN
    IF pp.keywordPresent("-type") THEN
      tc := pp.getNextInt();
    END;
    IF pp.keywordPresent("-detailed") THEN
      detailed := TRUE;
    END;
    IO.Put("Please hold...\n");
    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    RTHeapStats.ReportReachable(putter, tc, 0, detailed, FALSE, FALSE, FALSE);
    RTOS.UnlockHeap ();
  END PrintReachability;

PROCEDURE PrintPointers (pp: ParseParams.T) RAISES { ParseParams.Error } =
  VAR 
    idx: INTEGER := 0;
    all: BOOLEAN := FALSE;
    trd: TextRd.T;
    ref: INTEGER;
    detailed := FALSE;
  BEGIN
    IF pp.keywordPresent("-type") THEN
      idx := pp.getNextInt();
      ref := 0;
    ELSIF pp.keywordPresent("-ref") THEN
      idx := 0;
      all := TRUE;
      trd := TextRd.New(pp.getNext());
      ref := Lex.Unsigned(trd, 16);
      Rd.Close(trd);
    END;
    IF pp.keywordPresent("-all") THEN
      all := TRUE;
    END;
    IF pp.keywordPresent("-detailed") THEN
      detailed := TRUE;
    END;
    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    RTHeapStats.ReportReachable(putter, idx, ref, detailed, FALSE, TRUE, all);
    RTOS.UnlockHeap ();
  END PrintPointers;

PROCEDURE FindEnable(pp: ParseParams.T): BOOLEAN RAISES { ParseParams.Error } =
  BEGIN
    IF pp.keywordPresent("enable") THEN
      RETURN TRUE;
    ELSIF pp.keywordPresent("disable") THEN
      RETURN FALSE;
    ELSE
      RAISE ParseParams.Error;
    END;
  END FindEnable;

PROCEDURE FindOn(pp: ParseParams.T): BOOLEAN RAISES { ParseParams.Error } =
  BEGIN
    IF pp.keywordPresent("on") THEN
      RETURN TRUE;
    ELSIF pp.keywordPresent("off") THEN
      RETURN FALSE;
    ELSE
      RAISE ParseParams.Error;
    END;
  END FindOn;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR html: BOOLEAN;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();               (* command *)
      IF pp.keywordPresent("-html") THEN
        html := TRUE;
      END;

      (* 
       * collector's operations
       *)
      IF pp.keywordPresent("collect") THEN
        RTCollector.Collect();
      ELSIF pp.keywordPresent("check") THEN
        SanityCheck();

      (* 
       * control over the collector's features
       *)
      ELSIF pp.keywordPresent("collection") THEN
        IF FindEnable(pp) THEN
          RTCollector.Enable();
        ELSE
          RTCollector.Disable();
        END;
      ELSIF pp.keywordPresent("motion") THEN
        IF FindEnable(pp) THEN
          RTCollector.EnableMotion();
        ELSE
          RTCollector.DisableMotion();
        END;
      ELSIF pp.keywordPresent("background") THEN
        IF FindOn(pp) THEN
          (* create background collection thread and run it with priority
             a notch higher than the idle thread *)
          RTCollectorSRC.StartBackgroundCollection();
          Sched.SetPriority(RTCollectorSRC.backgroundThread, 1);
        ELSE
          RTCollectorSRC.StopBackgroundCollection();
        END;
      ELSIF pp.keywordPresent("incremental") THEN
        RTCollectorSRC.incremental := FindOn(pp);
        RTCollectorSRC.FinishVM();
      ELSIF pp.keywordPresent("generational") THEN
        RTCollectorSRC.generational := FindOn(pp);
        RTCollectorSRC.FinishVM();
      ELSIF pp.keywordPresent("vm") THEN
        IF FindEnable(pp) THEN
          RTCollectorSRC.EnableVM();
        ELSE
          RTCollectorSRC.DisableVM();
        END;
      ELSIF pp.keywordPresent("sanity") THEN
        IF FindOn(pp) THEN
          RTCollectorSRC.InstallSanityCheck();
        ELSE
          RTCollectorSRC.UninstallSanityCheck();
        END;
(*
      ELSIF pp.keywordPresent("unpin") THEN
        RTCollectorSRC.MoveFromPinned(FindOn(pp));
*)
      ELSIF pp.keywordPresent("ratio") THEN
        RTCollectorSRC.gcRatio := pp.getNextInt();
        RTCollectorSRC.FinishVM();

      ELSIF pp.keywordPresent("untraced") THEN
        Untraced(pp);

      (*
       * print information about the collector
       *)
      ELSIF pp.keywordPresent("verbose") THEN
        RTCollectorSRC.Verbose(pp.getNextInt());
      ELSIF pp.keywordPresent("stat") THEN
        IF pp.keywordPresent("-treadmill") THEN
          (* treadmill specific statistics *)
          TMStat();
        ELSE
          Stat(pp.keywordPresent("-full")); 
        END;
      ELSIF pp.keywordPresent("pages") THEN
        RTOS.LockHeap ();
        RTCollectorSRC.FinishCollection();
        RTHeapPages.DumpPageStatus(putter,
                                   pp.keywordPresent("-full")(*,
                                   pp.keywordPresent("-dump")*));
        RTOS.UnlockHeap ();
      ELSIF pp.keywordPresent("strongrefs") THEN
        RTOS.LockHeap ();
        RTStrongRef.Dump(putter);
        RTOS.UnlockHeap ();
      ELSIF pp.keywordPresent("heap") THEN
        ShowHeap(pp)
      ELSIF pp.keywordPresent("reach") THEN
        PrintReachability(pp);
      ELSIF pp.keywordPresent("frag") THEN
        IF NOT RTHeapStats.ReportFragmentation THEN
          RTIO.PutText("Fragmentation reporting was off!\n");
        ELSIF NOT RTHeapStats.FragOn THEN
          RTIO.PutText("Fragmentation reporting was off, now it's on \n");
          RTHeapStats.FragOn := TRUE;
          (* the caller better reset frag himself. *)
        ELSE
          RTIO.PutText(">>> Memory Utilization:\n");
          RTHeapStats.DumpSamples();
        END;
      ELSIF pp.keywordPresent("histo") THEN
        RTIO.PutText(">>> Pause distribution:\n");
        RTHeapStats.DumpHisto();
      ELSIF pp.keywordPresent("where") THEN
        PrintPointers(pp);
      ELSIF pp.keywordPresent("pc") THEN
        RTHeapTrace.Dump();
        (* SIRPA
           ELSIF pp.keywordPresent("qqq") THEN
           RTCollectorSRC.traceClean := NOT(RTCollectorSRC.traceClean);
        *)
      ELSIF pp.keywordPresent("cnt") THEN
        AllocCnts ();
      ELSIF pp.keywordPresent("rcnt") THEN
        ZeroCnts ();

      (*
       * test the collector
       *)
      ELSIF pp.keywordPresent("test") THEN
        Test();
      ELSIF pp.keywordPresent("inc") THEN
        Incremental();
      ELSIF pp.keywordPresent("kill") THEN
        Kill();
      ELSIF pp.keywordPresent("reset") THEN
        RTCollectorSRC.ResetStat();
      ELSIF pp.keywordPresent("dist") THEN
        RTIO.PutText("Distributing reserve pool..\n");
        RTHeapRep.DistributeMemory();
      ELSE
        GCUsage();
        RETURN FALSE;
      END;
    EXCEPT
      ParseParams.Error => GCUsage();
    END;
    RETURN TRUE;
  END Run;

PROCEDURE StatFile (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    Stat();
    RTHeapPages.DumpPageStatus(putter, TRUE(*, FALSE*));
    RTOS.UnlockHeap ();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END StatFile;

PROCEDURE Allocation (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    RTOS.LockHeap();
    RTCollectorSRC.FinishCollection();
    RTutils.Heap(TRUE, RTutils.HeapPresentation.ByByteCount, LAST(INTEGER),
                 putter);
    RTOS.UnlockHeap();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END Allocation;

<* UNUSED *> (* ??? *)
PROCEDURE HeapReach (wr: Wr.T) =
  VAR
    realWr := ThreadExtra.SetWrSelf(wr);
  BEGIN
    RTOS.LockHeap ();
    RTCollectorSRC.FinishCollection();
    RTHeapStats.ReportReachable(putter, 0, 0, FALSE);
    RTOS.UnlockHeap ();
    EVAL ThreadExtra.SetWrSelf(realWr);
  END HeapReach;

BEGIN
  TRY
     InfoFile.Create("/proc/gc_stat",StatFile);
     InfoFile.Create("/proc/allocation",Allocation);

     (*  This takes forever and turns off interrupts too!
     InfoFile.Create("/proc/heap_reach",HeapReach);
      *)
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END GC.
