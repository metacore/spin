(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Some changes in preparation for SMURT.
 *
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added a Spy.Reset so that Spy gets the time we started up.
 *
 * 31-Jul-97  Robert Grimm (rgrimm) at the University of Washington
 *      New security manager initialization
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL interfaces with Kernel interfaces
 *
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added a call to runtime to inform it that faults are enabled.
 *
 * 05-Mar-97  Charles Garrett (garrett) at the University of Washington
 *	Combined Gun's change of Feb 2 with Marc's changes of the same
 *	 day.
 *
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added UsersGroups and DTE Init, removed Identity
 *
 * 02-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *    Added MachineAtomicOpsPrivate.
 *
 * 02-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added DebuggerForSAL initialization.  
 *	
 *	Removed RTMem.spin_up variable.  Add it back when we actually
 *	have a GC solution.
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added Track Init.  Depends on NameServer
 *
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Clean up.
 *
 * 08-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Moved SpinControl.Init to Main.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed ordering a bit.  Moved KernelRegions.init up.  Flipped
 *	ordering of MachinTrapPrivate and Core init.  Removed
 *	SetupSALLimits and SetupCoreLimits, which are now handled by the
 *	SAL and Core modules directly.
 *
 * 27-May-96  Stefan Savage (savage) at the University of Washington
 *	Deleted Space initialization (space is now an extension)
 *
 * 13-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Calling SALPrivate to bypass more calls to C or assembly code.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled bypassing. 
 *
 * 07-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Split initialization of Domain into two, the second one initializing
 *	Modula-3 related stuff.  Had to do it to be able to garbage collect
 *	during the second half of that initialization.
 *
 * 01-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Sched --> SchedPrivate.
 *
 * 12-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Removed import of Device, Console, and decl of SALReady. All
 *	 unused.
 *
 * 20-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Added ConsoleWr.Init.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	SALUnsafe --> SALExtern.
 *
 * 23-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Moved SALInitConsole to here.  
 *
 * 11-Oct-95  Stefan Savage (savage) at the University of Washington
 *	Added SALReady so SAL is completely initialized before
 *	Advanced services start
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use the m3 thread interface.
 *
 * 21-Sep-95  Stefan Savage (savage) at the University of Washington
 *    Initialize SALServices
 *
 * 06-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 * This module is UNSAFE:
 *	import rtheaprep for heap sanity checking;
 *	SALExtern.
 *	
 *)

(* "Boot"

  See Main.m3 for details on startup path.

  RTLinker initializes the spin core services by calling
  RTStart.Init() which is exported by Boot module.
  Boot.m3 exports RTStart.
  Main's interface cannot be imported or exported.
*)

UNSAFE (* To import KernelExtern *)
MODULE Boot EXPORTS SpinStart; (* SMURT RTStart*)

IMPORT IO;
IMPORT PhysAddrPrivate, TranslationPrivate, NameServerPrivate,
       Firewall, SchedPrivate, TrapPrivate,
       DomainPrivate, KernelRegionsPrivate,
       StrandPrivate, ThreadPrivate, Core, CoreNS, ClockPrivate,
       CPUPrivate, RTCollector, Sal, SalExtern, SalSync,
       Debugger, DispatcherPrivate, AtomicOpsPrivate,
       MachineThread, Track, SecurityManagerPrivate, SecurityPolicy,
       LightMutexPrivate, SpinControl, Spy (*, RTStrongRef*);

(* RTStart.Init() is called from RTLinker.  
   See spin/kernel/start/src/Main.m3 for details *)
PROCEDURE Init() =
  VAR
    verboseInit := FALSE;
  BEGIN
    EVAL CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  
    (* Core services *)
    RTCollector.Disable(); (* don't let GC get in the way *)

    (* Boot Output
       Initially SAL prints with prom_puts().  Then after 'ps'
       (printstate) has the CONSPRINT bit turned on, output goes thru
       cnputc to the console device driver.

       Initial output from SPIN in m3core goes thru SALPrivate.Puts
       which uses SAL printf to go the console device driver.

       From here on, spincore uses the IO interface to write, usually
       with IO.Put(text).  In general IO.Put sends output to the
       currernt thread's writer.  However, until Sched.Init() is
       called a few lines down from here, Strand.GetCurrent() returns
       NIL, in which case, IO uses BootIO.Writer() for output.
    *)
  
    Debugger.Init(verboseInit);
    DispatcherPrivate.Init(verboseInit);
    
    (* Setting up support for kernel ras/nonpreemptive regions *)
    KernelRegionsPrivate.Init(verboseInit);

    (* Bypassing functions for correctness and performance. *)
    IF verboseInit THEN IO.Put("Bypassing functions in:\n"); END;
    AtomicOpsPrivate.Init(verboseInit);
    CPUPrivate.Init(verboseInit);
    MachineThread.Init(verboseInit);
    Sal.Init(verboseInit);  

    Spy.Reset();

    (* Trusted memory management *)
    PhysAddrPrivate.Init(verboseInit,
		  SalExtern.ptraced_start,
		  SalExtern.traced_size);
    TranslationPrivate.Init(verboseInit);
  
    (* Low-level exception management *)
    Firewall.Init(verboseInit);

    (* Security management - must be initialized before scheduler *)
    SecurityPolicy.Init(verboseInit);
    SecurityManagerPrivate.Init(verboseInit);
  
    (* Initialize trusted services *)
    SchedPrivate.Init(verboseInit);		(* Become somebody... *)
    LightMutexPrivate.Init(verboseInit);
    (* Naming services *)
    NameServerPrivate.Init(verboseInit);
 
    (* kernel resource accounting *)
    Track.Init();

    (* Traps and interrupts *)
    TrapPrivate.Init(verboseInit);
    Core.Init(verboseInit);

    (* Domains *)
    DomainPrivate.Init(verboseInit);

    (* Strands *)
    StrandPrivate.Init(verboseInit);
    ThreadPrivate.Init(verboseInit);
    SalSync.Init(verboseInit);

    (* SMURT
    (* Start background process for strongrefs *)
    StrongRefF.StartBackgroundCleanup();
    *)

    (* Enable faults for reference counting after traps and threads are done *)
    (* FIXME: this will be enabled when reference counting is reenabled
    RTHeapRep.EnableTraps();
    *)

    (* This is split from DomainPrivate.Init() because the call to  *)
    (* InitM3 requires garbage collection. GC doesn't work before strands    *)
    (* are initialized and strands have to be initialized after domains *)
    RTCollector.Enable();  
    DomainPrivate.InitM3(verboseInit);


    (* Simple Devices that use threads  *)
    ClockPrivate.Init(verboseInit);

    Sal.InitThreads(verboseInit);

    (* create name space for domains, events, threads, types *)
    CoreNS.Init(verboseInit);

    (* Access to core and public symbols via domains *)  
    SpinControl.Init(verboseInit);

    IF verboseInit THEN IO.Put("SpinCore initialized\n\n"); END;
  END Init;
  
BEGIN
END Boot.
