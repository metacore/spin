(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 03-Dec-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for DebugOption.Security.
 *
 * 26-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed address comparisons to use Word instead of signed
 *      integer comparisons.
 *
 * 08-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for passing security info from user
 *      to kernel thread.
 *
 * 24-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	make debug flag a CONST
 *
 * 31-May-97  David Becker at the University of Washington
 *	Rename GetCR2 to get_cr2
 *
 * 14-Oct-96  Stefan Savage (savage) at the University of Washington
 *	Added protection fault handler for incremental/generational GC
 *
 * 18-Jul-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *) 
UNSAFE (* This module is unsafe because it uses external interfaces. *)
MODULE MachineTrapPrivate;
IMPORT Strand, StrandRep, StrandPrivate, SchedPrivate;
IMPORT MachineCPU, MachineCPUPrivate, MachineCPUExtern;
IMPORT MachineTrap;
IMPORT Thread, ThreadPrivate, ThreadRep, ClockPrivate, KernelRegions;
IMPORT Translation, DebugOption;
IMPORT Firewall, SpinException, RTOS;
IMPORT Log, Word, Spy, IO;
IMPORT RTHeapRep, RTHeapDep, RTIO; <* NOWARN *>
IMPORT SecurityContext;

IMPORT SalExtern;

VAR
  instructionFaultTime: Spy.T;
  memoryManagementFaultTime: Spy.T;
  unalignedAccessTime: Spy.T;
  machineCheckTime: Spy.T;
  syscallTime: Spy.T;
  clockIntrTime: Spy.T;
  ioInterruptTime: Spy.T;
  arithmeticFaultTime: Spy.T;
  protectionFaultTime: Spy.T;
  unalignedFaultTime: Spy.T;
  preemptTime: Spy.T;
  strayInterruptTime: Spy.T;

CONST
  debug: BOOLEAN = FALSE;

<*INLINE*> PROCEDURE EnterSalStub(timer:  Spy.T; 
                      VAR ss: MachineCPU.SavedState; 
                      VAR activeStrand: Strand.T; (* OUT strand that incurred the fault *)
                      VAR SalStrand: Strand.T; (* OUT strand that is handling the fault *)
                      VAR translation: Translation.T) =
BEGIN
    (*
     * Spy chain if necessary
     *)
    IF DebugOption.DoTimings THEN Spy.ContextSwitch(timer); END;
    activeStrand := Strand.GetCurrent();
    IF debug THEN
      Log.Log("Entered Sal at pc="); Log.Logx(ss.pc);
      Log.Log("eflags="); Log.Logx(ss.eflags); Log.Log("\n");
    END;
    IF InUserSpace(ss) THEN
      (*
       * if we came in from user space,
       * bind a Sal strand to the user strand
       *)
      SalStrand := syscallHandler;
      SalStrand.bound_to_user := activeStrand;
      activeStrand.bound_to_kernel := SalStrand;
      (* Hand off security info from user to kernel thread *)
      IF DebugOption.Security THEN
        SecurityContext.HandOffContext( activeStrand, SalStrand );
      END;
      syscallHandler := NIL;
      translation := Translation.GetCurrent();
      activeStrand.trapFrame := LOOPHOLE(ADR(ss),
				       UNTRACED REF MachineCPU.SavedState);
    ELSE
      KernelRegions.RASRegions.rasEnsure(ss.pc);
      SalStrand := activeStrand;
    END;
  END EnterSalStub;

PROCEDURE EnterSalAtClass(timer:  Spy.T; 
                      VAR ss: MachineCPU.SavedState; 
                          spl: MachineCPUPrivate.InterruptClass; (* spl during fault handling *)
                      VAR activeStrand: Strand.T; (* OUT strand that incurred the fault *)
                      VAR SalStrand: Strand.T; (* OUT strand that is handling the fault *)
                      VAR translation: Translation.T) =
  BEGIN
    EnterSalStub(timer, ss, activeStrand, SalStrand, translation);
    EVAL MachineCPUPrivate.SetInterruptMask(spl);
  END EnterSalAtClass;  

PROCEDURE EnterSalAtLevel(timer: Spy.T; 
                      VAR ss: MachineCPU.SavedState; 
                          spl: MachineCPU.InterruptLevel; (* spl during fault handling *)
                      VAR activeStrand: Strand.T; (* OUT strand that incurred the fault *)
                      VAR SalStrand: Strand.T; (* OUT strand that is handling the fault *)
                      VAR translation: Translation.T) = (* OUT translation that was active *)
  BEGIN
    EnterSalStub(timer, ss, activeStrand, SalStrand, translation);
    MachineCPUPrivate.RestoreInterruptMask(spl);
  END EnterSalAtLevel;

PROCEDURE ExitSal(VAR ss: MachineCPU.SavedState; 
                     VAR activeStrand: Strand.T;
                     VAR SalStrand: Strand.T;
                     VAR translation: Translation.T) =
  VAR
    spl: MachineCPU.InterruptLevel;
  BEGIN
    spl := MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.High);
    IF debug THEN
      Log.Log("Exiting Sal to pc="); Log.Logx(ss.pc);
    END;
    IF InUserSpace(ss) THEN
      (* unbind from user thread *)
      SalStrand.bound_to_user := NIL;
      activeStrand.bound_to_kernel := NIL;
      (*
       * if the user strand is currently blocked, we should save its
       * current state and the Sal thread should die instead of going
       * out to user space.
       *)
      IF activeStrand.count <= 0 THEN
        IF DebugOption.DoTimings THEN Spy.ContextSwitch(preemptTime) END;
        activeStrand.trapFrame := LOOPHOLE(ADR(ss),
			       UNTRACED REF MachineCPU.SavedState);
        EVAL Strand.Stop(activeStrand);
        MachineCPUPrivate.RestoreInterruptMask(spl);
        ThreadPrivate.Exit_main(SalStrand, NIL);
        IO.PutError("Should not get past Exit_main in syscall wrap\n");
      ELSE
        (*
         * Restore the state of the world before the trap and go
         * back out to user space.
         *)
        Translation.Activate(translation);
        (* 
         * If someone registered in the meantime to take traps,
         * we replace them.
         *)
        IF syscallHandler # NIL THEN
          ThreadPrivate.KillTrapHndlr(syscallHandler);
        END;
        syscallHandler := SalStrand;
      END;
    END;
    IF DebugOption.DoTimings THEN Spy.ExitInterrupt() END;
  END ExitSal;

PROCEDURE ExtractSpl(VAR ss: MachineCPU.SavedState) : MachineCPU.InterruptLevel =
  BEGIN
    RETURN ss.spl;
  END ExtractSpl;

PROCEDURE InUserSpace(VAR ss: MachineCPU.SavedState) : BOOLEAN =
  BEGIN
    RETURN Word.And(ss.cs, 3) # 0;
  END InUserSpace; 

(*
 * Trap handling routines. We get here from Core. We then perform a
 * user-Sal switch if necessary, handle the trap/interrupt/syscall
 * and do a Sal-user switch.
 *)

PROCEDURE SyscallWrap(VAR ss: MachineCPU.SavedState) =
  VAR
    SalStrand, userStrand: Strand.T;
    userTranslation: Translation.T;
  BEGIN
    EnterSalAtClass(syscallTime, ss, MachineCPUPrivate.InterruptClass.Low, 
                userStrand, SalStrand, userTranslation);
    TRY
      MachineTrap.Syscall(userStrand, ss);
    EXCEPT
    ELSE
    END;
    ExitSal(ss, userStrand, SalStrand, userTranslation);
  END SyscallWrap;

PROCEDURE PrivilegedInstructionFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(instructionFaultTime, ss, ExtractSpl(ss), 
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN (* Sal thread? *)
      IO.Put("SPIN: Privileged instruction fault at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      MachineTrap.IllegalInstruction(activeStrand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END PrivilegedInstructionFault;

PROCEDURE TraceTrap(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(instructionFaultTime, ss, ExtractSpl(ss), 
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN (* Sal thread? *)
      IO.Put("SPIN: Trace trap at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      MachineTrap.Breakpoint(activeStrand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END TraceTrap;

PROCEDURE Breakpoint(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(instructionFaultTime, ss, ExtractSpl(ss), 
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN (* Sal thread? *)
      IO.Put("SPIN: Breakpoint at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      MachineTrap.Breakpoint(activeStrand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END Breakpoint;

PROCEDURE ArithmeticTrap(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Arithmetic trap at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      IO.Put("Arithmetic trap at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END ArithmeticTrap;

PROCEDURE DivideFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Divide fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      IO.Put("Divide fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END DivideFault;

PROCEDURE OverflowFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Overflow fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      IO.Put("Overflow fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END OverflowFault;

PROCEDURE BoundsCheckFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Bounds check fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.SubscriptOutOfRange);
      (* not reached *)
    END;
    TRY
      IO.Put("Bounds check fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END BoundsCheckFault;

PROCEDURE ProtectionFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
    done := FALSE;
    addr: Word.T;
    spl: MachineCPU.InterruptLevel; 
  BEGIN
    addr := MachineCPUExtern.get_cr2();

    (*
     * ___FIRST___ check if this is a heap protection fault intended for
     * the collector.
     *)
    IF Word.GE(addr, LOOPHOLE(RTHeapRep.MinAddress(), Word.T)) AND
      Word.LE(addr, LOOPHOLE(RTHeapRep.MaxAddress(), Word.T)) AND
      InUserSpace(ss) 
     THEN
      spl := MachineCPUPrivate.SetInterruptMask(
                 MachineCPUPrivate.InterruptClass.High);
      IF RTHeapRep.verbose > 2 THEN
        RTIO.PutText("GC Fault: addr "); RTIO.PutHex(addr); 
        RTIO.PutText(" @ pc "); RTIO.PutHex(ss.pc); RTIO.PutText("\n");
      END;

      IF NOT RTHeapRep.Fault(LOOPHOLE(addr, ADDRESS)) THEN
        IO.Put("ERROR: RTHeapDep.Fault failed\n");
      END;
      MachineCPUPrivate.RestoreInterruptMask(spl);
      RETURN;
    END;

    EnterSalAtLevel(memoryManagementFaultTime, ss, ExtractSpl(ss),
                       activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IF debug THEN Log.Log("Protection Fault\n"); Log.Dumplog(); END;
      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ProtectionFault);
      (* not reached *)
    END;
    TRY
      MachineTrap.ProtectionFault(activeStrand, ss,
                                  activeTranslation, 0, 0, done);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END ProtectionFault;

PROCEDURE PageFault(VAR ss: MachineCPU.SavedState) =
  VAR
    addr: Word.T;
    reftype: Word.T;
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
    ec: SpinException.ExceptionCode;
    done := FALSE;
  BEGIN
    EnterSalAtLevel(memoryManagementFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);

    addr := MachineCPUExtern.get_cr2();

    IF Word.And(ss.err, 16_1) # 0 THEN
      reftype := MachineTrap.Write;
    ELSE
      reftype := MachineTrap.Read;
    END;

    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IF debug THEN Log.Log("Page Fault\n"); Log.Dumplog(); END;
      IF Word.LT(addr, (16_FFFF - 1)) THEN
        ec := SpinException.ExceptionCode.AttemptToDereferenceNIL;
      ELSE
        ec := SpinException.ExceptionCode.ProtectionFault;
      END;

      <*NOWARN*>Firewall.Fault(activeStrand, ss, addr, 0, reftype, ec);
      (* not reached *)
    END;
    TRY
      MachineTrap.InvalidTranslation(activeStrand, ss,
                                     activeTranslation, addr, reftype,
                                     done);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END PageFault;

PROCEDURE FPFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Floating point fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      IO.Put("Floating point fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END FPFault;

PROCEDURE FPOperandFault(VAR ss: MachineCPU.SavedState) =
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
    EnterSalAtLevel(arithmeticFaultTime, ss, ExtractSpl(ss),
                activeStrand, SalStrand, activeTranslation);
    IF ISTYPE(activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: Floating point operand fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(activeStrand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      IO.Put("Floating point operand fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END FPOperandFault;

PROCEDURE ClockTick(VAR ss: MachineCPU.SavedState) = 
  VAR
    activeStrand, SalStrand: Strand.T;
    activeTranslation: Translation.T;
  BEGIN
(* RTIO.PutChar('Z');*)
    EnterSalAtClass(clockIntrTime, ss, MachineCPUPrivate.InterruptClass.Clock,
                activeStrand, SalStrand, activeTranslation);
    (* timing statistics *)
    IF debug THEN
      Log.Log("Clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc);
      Log.Log("\n");
    END;
    TRY
      (* GprofNote(); *)
      ClockPrivate.ClockTick(ss);
      IF DebugOption.DoTimings THEN Spy.ContextSwitch(preemptTime) END;
      SchedPrivate.ClockTick(ss);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    IF debug THEN
      Log.Log("Returning from clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc);
      Log.Log("\n");
    END;
    ExitSal(ss, activeStrand, SalStrand, activeTranslation);
  END ClockTick; 

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    IF DebugOption.DoTimings THEN
      unalignedAccessTime := Spy.Create("UnalignedAccess Trap");
      instructionFaultTime := Spy.Create("InstructionFault Trap");
      machineCheckTime := Spy.Create("Machine Check");
      memoryManagementFaultTime := Spy.Create("MemoryManagement Fault");
      syscallTime := Spy.Create("syscall trap");
      clockIntrTime := Spy.Create("clock interrupt");
      ioInterruptTime := Spy.Create("io interrupt");
      arithmeticFaultTime := Spy.Create("Arithmetic Trap");
      unalignedFaultTime := Spy.Create("Unaligned Trap");
      protectionFaultTime := Spy.Create("Protection Trap");
      preemptTime := Spy.Create("Preemption path");
      strayInterruptTime := Spy.Create("Arithmetic Fault Trap");
    END;
    
    (* overriding the clocktick procedure variable that sal exports *) 
    SalExtern.ClockTick := ClockTick;

    IF verbose THEN IO.Put("MachineTrapPrivate initialized...\n"); END;
  END Init;

BEGIN 
END MachineTrapPrivate.
