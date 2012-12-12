(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-May-96  Stefan Savage (savage) at the University of Washington
 *	NOTE: This test requires unsafe behavior from the trap handlers to 
 *	work (it wants to catch protection faults made by kernel threads and
 *	hence allows the handler to modify kernel state).  To make this code
 *	work you must _temporarily_ modify the
 *	MachineTrapPrivate.MemoryManagementFault code to read:
 *	    IF ISTYPE(activeStrand, Thread.T) AND
 *		Word.GT(addr,16_fffff40000000000) THEN
 *	instead of:
 *	    IF ISTYPE(activeStrand, Thread.T) THEN 
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Switched to new ProtectionFault signature.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 9-Jan-95 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use REF PROCEDURE instead of PROCEDURE
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Added Closure pagetrap test... will shortly move all split tests to use
 *	closures
 *
 * 12-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Added appel ASPLOS tests
 *
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Created from old ThreadShell.m3 for SOSP paper
 *
 * This module is UNSAFE because it uses LOOPHOLE.
 *
 *)
UNSAFE MODULE PageFaultTest;

IMPORT CPU, Space, VirtAddr, Dispatcher, Measure;
IMPORT Translation, Trap, Strand;
IMPORT VMError;
IMPORT Protection;

<*FATAL VMError.E*>
<*FATAL Dispatcher.Error*>

VAR
      globaltimer: Measure.T;
      globalBegin: VirtAddr.Address := 16_0800000000;
      globalSize : VirtAddr.Address := 105 * CPU.PAGESIZE;
      globalPointer: VirtAddr.Address;

CONST
  FaultIterations: CARDINAL = 10000;
  SFaultIterations: CARDINAL = 10000;
  Prot1Iterations: CARDINAL = 10000;
  Prot100Iterations: CARDINAL = 10000;
  Unprot100Iterations: CARDINAL = 10000;
  TrapIterations: CARDINAL = 10000;
  Appel1Iterations: CARDINAL = 100;
  Appel2Iterations: CARDINAL = 100;

PROCEDURE TrapHandler(<*UNUSED*> strand: Strand.T;
		      <*UNUSED*> VAR ss: CPU.SavedState; 
                                 addr: VirtAddr.Address;
				 type: INTEGER) =
  VAR
    sp: Space.T;
  BEGIN
    globaltimer.stop();
    sp := Space.GetCurrent();
    Space.Protect(sp, addr, CPU.PAGESIZE,
                  Space.ReadWriteProtection);
  END TrapHandler;

PROCEDURE TrapClosureHandler(             closure: REFANY; 
			      <*UNUSED*>  strand: Strand.T;
                              <*UNUSED*>  VAR ss: CPU.SavedState; 
                                          addr: VirtAddr.Address;
					  type: INTEGER) =
  VAR
    sp: Space.T;
    mytimer: Measure.T;
  BEGIN
    mytimer := NARROW(closure, Measure.T);
    mytimer.stop();
    sp := Space.GetCurrent();
    Space.Protect(sp, addr, CPU.PAGESIZE,
                  Space.ReadWriteProtection);
  END TrapClosureHandler;

PROCEDURE FaultHandler(<*UNUSED*> strand: Strand.T;
		       <*UNUSED*> VAR ss: CPU.SavedState; 
                                    addr: VirtAddr.Address;
				    type: INTEGER) =
  VAR
    sp: Space.T;
  BEGIN
    sp := Space.GetCurrent();
    Space.Protect(sp, addr,CPU.PAGESIZE,
                  Space.ReadWriteProtection);
  END FaultHandler;

PROCEDURE FaultHandler2(<*UNUSED*> strand: Strand.T;
		       <*UNUSED*> VAR ss: CPU.SavedState; 
                                    addr: VirtAddr.Address;
				    type: INTEGER) =
  VAR
    sp: Space.T;
  BEGIN
    sp := Space.GetCurrent();
    Space.Protect(sp, addr, CPU.PAGESIZE,
                  Space.ReadWriteProtection);
    globalPointer := globalPointer + CPU.PAGESIZE;
    Space.Protect(sp, globalPointer, CPU.PAGESIZE,
                  Space.ReadOnlyProtection);
    globaltimer.stop();
  END FaultHandler2;

PROCEDURE FaultTest() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 1 * CPU.PAGESIZE;
      prot: Protection.T;
      timer: Measure.T;
      handler: Dispatcher.Binding;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Protect(s, regionBegin, regionSize, prot);
    Space.Activate(s);

    handler := Dispatcher.InstallHandler(Trap.ProtectionFault,
                                         NIL, FaultHandler,
                                         options := Dispatcher.Options{
                                                       Dispatcher.Opt.First,
                                                       Dispatcher.Opt.Cancel});

    timer := (NEW(Measure.T).init("PageFault",FaultIterations)); 
    FOR i := 1 TO FaultIterations DO
      timer.start();
      LOOPHOLE(regionBegin, REF INTEGER)^ := 16_deadbeef;
      timer.stop();
      Space.Protect(s, regionBegin, regionSize, prot);  
    END;
    Measure.PrintStats(timer);
    Dispatcher.Uninstall(handler); 
END FaultTest;

PROCEDURE SFaultTest() =
  VAR s: Space.T;
      StackBegin: VirtAddr.Address := 16_0800000000;
      StackLen : VirtAddr.Size := 64 * 1024;
      timer: Measure.T;
  BEGIN
    s := Space.Create();
    Space.Allocate(s, StackBegin, StackLen);
    Space.Activate(s);

    timer := (NEW(Measure.T).init("PageFault Baseline",SFaultIterations)); 
    FOR i := 1 TO SFaultIterations DO
      timer.start(); 
      LOOPHOLE(StackBegin, REF INTEGER)^ := 16_deadbeef;
      timer.stop();
    END;
    Measure.PrintStats(timer);
 END SFaultTest;

PROCEDURE GetPTE() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 1 * CPU.PAGESIZE;
      trans: Translation.T;
      timer: Measure.T;
  BEGIN

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Activate(s);
    trans := Translation.GetCurrent();
    timer := (NEW(Measure.T).init("GetPTE",Prot1Iterations)); 
    FOR i := 1 TO Prot1Iterations DO
      timer.start();
      EVAL Translation.ExamineMapping(trans, regionBegin);
      timer.stop();
    END;
    Measure.PrintStats(timer);
(*    Space.Destroy(s); *)
END GetPTE;

PROCEDURE Prot1() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 1 * CPU.PAGESIZE;
      prot: Protection.T;
      timer: Measure.T;
  BEGIN
    prot.read := TRUE;
    prot.write := TRUE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Activate(s);

    timer := (NEW(Measure.T).init("Prot1",Prot1Iterations)); 
    FOR i := 1 TO Prot1Iterations DO
      prot.write := TRUE;
      Space.Protect(s, regionBegin, regionSize, prot);
      prot.write := FALSE;
      timer.start();
      Space.Protect(s, regionBegin, regionSize, prot);
      timer.stop();
    END;
    Measure.PrintStats(timer);
(*    Space.Destroy(s); *)
END Prot1;

PROCEDURE Prot100() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 100 * CPU.PAGESIZE;
      prot: Protection.T;
      timer: Measure.T;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Activate(s);

    timer := (NEW(Measure.T).init("Prot100",Prot100Iterations)); 
    FOR i := 1 TO Prot100Iterations DO
      prot.write := TRUE;
      Space.Protect(s, regionBegin, regionSize, prot);
      prot.write := FALSE;
      timer.start();
      Space.Protect(s, regionBegin, regionSize, prot);
      timer.stop();
    END;
    Measure.PrintStats(timer);
END Prot100;

PROCEDURE Unprot100() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 100 * CPU.PAGESIZE;
      prot: Protection.T;
      timer: Measure.T;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Activate(s);



    timer := (NEW(Measure.T).init("Unprot100",Unprot100Iterations)); 
    FOR i := 1 TO Unprot100Iterations DO
      prot.write := FALSE;
      Space.Protect(s, regionBegin, regionSize, prot);
      prot.write := TRUE;
      timer.start();
      Space.Protect(s, regionBegin, regionSize, prot);
      timer.stop();
    END;
    Measure.PrintStats(timer);
END Unprot100;

PROCEDURE TrapTest () = 
  VAR s: Space.T;
    regionBegin: VirtAddr.Size := 16_0800000000;
    regionSize : VirtAddr.Size := 1 * CPU.PAGESIZE;
    prot: Protection.T;
    handler: Dispatcher.Binding;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Protect(s, regionBegin, regionSize, prot);
    Space.Activate(s);

    handler := Dispatcher.InstallHandler(Trap.ProtectionFault,
                                         NIL, TrapHandler,
                                         options := Dispatcher.Options{
                                                       Dispatcher.Opt.First,
                                                       Dispatcher.Opt.Cancel});

    globaltimer := (NEW(Measure.T).init("Trap",TrapIterations)); 
    FOR i := 1 TO TrapIterations DO
      globaltimer.start();
      LOOPHOLE(regionBegin, REF INTEGER)^ := 16_deadbeef; 
      Space.Protect(s, regionBegin, regionSize, prot);  
    END;
    Measure.PrintStats(globaltimer);
    Dispatcher.Uninstall(handler);  
  END TrapTest;

PROCEDURE TrapClosure() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionSize : VirtAddr.Size := 1 * CPU.PAGESIZE;
      prot: Protection.T;
      handler: Dispatcher.Binding;
      timer: Measure.T;      
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Protect(s, regionBegin, regionSize, prot);
    Space.Activate(s);

    timer := (NEW(Measure.T).init("Trap",TrapIterations)); 
    handler := Dispatcher.InstallHandler(Trap.ProtectionFault,
                                         NIL, TrapClosureHandler, 
                                         NIL, timer,
                                         Dispatcher.Options{
                                             Dispatcher.Opt.First,
                                             Dispatcher.Opt.Cancel});


    FOR i := 1 TO TrapIterations DO
      timer.start();
      LOOPHOLE(regionBegin, REF INTEGER)^ := 16_deadbeef; 
      Space.Protect(s, regionBegin, regionSize, prot);  
    END;
    Measure.PrintStats(timer);
    Dispatcher.Uninstall(handler);  
  END TrapClosure;

PROCEDURE Appel1() = 
  VAR s: Space.T;
      prot: Protection.T;
      handler: Dispatcher.Binding;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, globalBegin, globalSize);
    Space.Activate(s);
    Space.Protect(s, globalBegin, CPU.PAGESIZE, prot);

    handler := Dispatcher.InstallHandler(Trap.ProtectionFault,
                                         NIL, FaultHandler2,
                                         options := Dispatcher.Options{
                                                       Dispatcher.Opt.First,
                                                       Dispatcher.Opt.Cancel});
    globalPointer := globalBegin;
    globaltimer := (NEW(Measure.T).init("Appel1",Appel1Iterations)); 
    FOR i := 1 TO Appel1Iterations DO
      IF globalPointer > (globalBegin + globalSize) THEN
        globalPointer := globalBegin;
      END;
      globaltimer.start();
      LOOPHOLE(globalPointer, REF INTEGER)^ := 16_deadbeef;
    END;
    Measure.PrintStats(globaltimer);
    Dispatcher.Uninstall(handler);
END Appel1;

PROCEDURE Appel2() = 
  VAR s: Space.T;
      regionBegin: VirtAddr.Size := 16_0800000000;
      regionPointer: VirtAddr.Size;
      regionSize : VirtAddr.Size := 100 * CPU.PAGESIZE;
      prot: Protection.T;
      timer: Measure.T;
      handler: Dispatcher.Binding;
  BEGIN
    prot.read := TRUE;
    prot.write := FALSE;
    prot.execute := FALSE;

    s := Space.Create();
    Space.Allocate(s, regionBegin, regionSize);
    Space.Activate(s);
    Space.Protect(s, regionBegin, regionSize, prot);

    handler := Dispatcher.InstallHandler(Trap.ProtectionFault,
                                         NIL, FaultHandler,
                                         options := Dispatcher.Options{
                                                       Dispatcher.Opt.First,
                                                       Dispatcher.Opt.Cancel});

    timer := (NEW(Measure.T).init("Appel2",Appel2Iterations)); 
    FOR i := 1 TO Appel2Iterations DO
      timer.start();
      Space.Protect(s, regionBegin, regionSize, prot);  
      regionPointer := regionBegin;
      FOR j := 1 TO 100 DO
        LOOPHOLE(regionPointer, REF INTEGER)^ := 16_deadbeef;
        regionPointer := regionPointer + CPU.PAGESIZE;
      END;
      timer.stop();
    END;
    Measure.PrintStats(timer);
    Dispatcher.Uninstall(handler);
END Appel2;

BEGIN
END PageFaultTest.
