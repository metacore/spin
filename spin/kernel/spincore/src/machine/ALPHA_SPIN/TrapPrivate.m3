(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *
 * 11-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	changed to use TrapRecord instead of passing args separately.
 * 24-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed most of spys. They've never been used recently.
 * 01-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made cpustate a trap frame pointer.
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for sentinel pages for reference counting buffers.
 *	Removed unnecessary spl-high in MemoryManagementFault.
 *
 * 12-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Many streamlining.
 *
 * 03-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made debug constant. Timer param is removed from EnterXXX.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added printing of fault information if GC run in verbose mode.
 *
 * 14-Oct-96  Stefan Savage (savage) at the University of Washington
 *	Added protection fault handler for incremental/generational GC
 *
 * 11-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed ArithmeticFault to call Firewall.Fault with a
 *	SpinException.ArithmeticFault rather than BadAddress.
 *
 * 27-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Trap redirection is now handled by the Core module directly.
 *	
 * 10-Jun-96  Brian Bershad (bershad) at the University of Washington
 *	Use Word compares on address compares.
 *
 * 27-May-96  Stefan Savage (savage) at the University of Washington
 *	Replaced all uses of Space to use Translation.  Space is an extension
 *	now.
 *
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *      Complete revamp to support user extensions on una/mem/inst traps.
 *	Readded some state saving code to clocktick that got dropped.
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Broke out and expanded Alpha specific memory management traps,
 *	alignment traps, instruction traps, and mcheck.
 *
 * 28-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Reroute arithmetic faults to firewall.
 *
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Take extra arguments for Protection,Unaligned, and IllegalInst faults
 *	(so we know what happened).  Allow user alignment faults.
 *
 * 21-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added the unix exit system call handler to the default syscall handler.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 19-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Respect RAS regions of interrupted code.
 *
 * 18-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Disallow exceptions in syscall handlers to leak out of this module.
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Redirecting alignment faults to Firewall.
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Measurements by Spy used only if DoTimings is on.
 *
 * 02-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added state saving of the Sal syscall handler prior to going
 *	out to a blocked user strand.
 *
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *      Added extra argument to IOinterrupt for sal IO vector.
 *	Deleted spinknio. Replaced it with indirection through the interrupt
 *	dispatch vector set up by SAL.
 *
 * 21-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Eliminated huge data copies on the syscall path.
 *	Changed Syscall interface.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *      Marked unused definition to disable warnings. 
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Made unsafe.
 *
 * 28-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to use generic regions for non-preemptive regions.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Removed use of LOOPHOLE for CPU.SavedState.strand.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added MachineCheck.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Be a bit less aggressive about printing trap state on an exception.
 *      Firewall is now responsible for that.
 *
 * 28-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to reset the quanta upon thread switch for proper accounting.
 *	Fixed the non-preemptible region tests.
 *
 * 07-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Added timing statistics to measure the cost of handling various
 *	machine traps.  Performance might be degraded.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Architectural trap events and default handlers.
 *
 *) 

UNSAFE (* This module is unsafe because it uses external interfaces. *)
MODULE TrapPrivate;
IMPORT Translation;
IMPORT Strand, StrandRep, StrandExtern, SchedPrivate;
IMPORT CPU, CPUPrivate, CPUExtern, TrapPrivate, Trap;
IMPORT Thread, ThreadPrivate, ThreadRep, ClockPrivate, KernelRegions;
IMPORT Firewall, SpinException, RTOS;
IMPORT Log, SalExtern, Word, Spy, IO, Fmt, Debugger;
IMPORT RTHeapRep, RTIO;
IMPORT PFMExtern;
IMPORT AtomicOpsExtern;

VAR
  faultHandlerTime: Spy.T;
  syscallHandlerTime: Spy.T;

CONST
  Debug = FALSE;
  DoSpy = FALSE;
  InUserSpaceBit = 8; (* If this bit of "ss" reg is set, then trap is from
			 user space. *)

TYPE TrapRecord = RECORD
  activeStrand, kernelStrand: Strand.T;
END;

<*INLINE*>
PROCEDURE EnterKernel(VAR ss: CPU.SavedState;
		      (*OUT*)VAR trap: TrapRecord) =
  BEGIN
    (*
     * Spy chain if necessary
     *)
    trap.activeStrand := StrandExtern.Cur;
    IF Debug THEN
      Log.Log("Entered Sal at pc="); Log.Logx(ss.pc);
      Log.Log("ps="); Log.Logx(ss.ps); Log.Log(" ");
      Log.Log("ra="); Log.Logx(ss.ra); Log.Log(" ");
      Log.Log("ksp="); Log.Logx(ss.ksp); Log.Log(" ");
      Log.Log("usp="); Log.Logx(ss.usp); Log.Log("\n");
    END;
    IF Word.And(ss.ps, InUserSpaceBit) # 0 THEN
      (*
       * if we came in from user space,
       * bind a Sal strand to the user strand
       *)
      (*AtomicOpsExtern.EnqueueAddr(ADR(syscallHandler),
					 NIL, 
					 ADR(trap.kernelStrand));*)
      trap.kernelStrand := syscallHandler;
      syscallHandler := NIL;
      trap.kernelStrand.translation := trap.activeStrand.translation;
      trap.kernelStrand.bound_to_user := trap.activeStrand;
      trap.activeStrand.bound_to_kernel := trap.kernelStrand;
    ELSE
      KernelRegions.RASRegions.rasEnsure(ss.pc);
      trap.kernelStrand := trap.activeStrand;
    END;
    trap.activeStrand.trapFrame :=
      LOOPHOLE(ADR(ss), UNTRACED REF CPU.SavedState);
  END EnterKernel;

PROCEDURE ExitKernel(READONLY ss: CPU.SavedState;
		     READONLY trap: TrapRecord) =
  BEGIN
    IF Debug THEN
      Log.Log("Exiting Sal to pc="); Log.Logx(ss.pc);
      Log.Log("ps="); Log.Logx(ss.ps); Log.Log("\n");
    END;
    IF Word.And(ss.ps, InUserSpaceBit) # 0 THEN
      (* unbind from user thread *)
      trap.activeStrand.bound_to_kernel := NIL;
      (*
       * if the user strand is currently blocked, we should save its
       * current state and the Sal thread should die instead of going
       * out to user space.
       *)
      IF trap.activeStrand.count <= 0 THEN
	WITH spl = CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.High) DO
	  EVAL Strand.Stop(trap.activeStrand);
	  EVAL CPUExtern.SpinSwapIpl(spl);
	  ThreadPrivate.Exit_main(trap.kernelStrand, NIL);
	  IO.PutError("Should not get past Exit_main in syscall wrap\n");
	END;
      END;
      
      (*
	 Restore the state of the world before the trap and go
	 back out to user space.
      *)
	
      (* 
	 If someone registered in the meantime to take traps,
	 we replace them.
      *)
      IF NOT AtomicOpsExtern.CompareAndSwapAddr(LOOPHOLE(syscallHandler, ADDRESS),
						NIL,
						LOOPHOLE(trap.kernelStrand,ADDRESS)) THEN
	
	EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.High);
	IF syscallHandler # NIL THEN
	  ThreadPrivate.KillTrapHndlr(syscallHandler);
	END;
	syscallHandler := trap.kernelStrand;
	(* SPL is lowered in the final rti. *)
      END;
    END;
    trap.activeStrand.trapFrame := LOOPHOLE(-1, UNTRACED REF CPU.SavedState);
    (* set an invalid value to catch an exception if someone touches
       the trap frame. *)
  END ExitKernel;

PROCEDURE ExtractSpl(ps: Word.T) : CPU.InterruptLevel =
  BEGIN
    RETURN VAL(Word.And(ps, 16_7), CPU.InterruptLevel);
  END ExtractSpl;
  
(*
 * Trap handling routines. We get here from Core. We then perform a
 * user-Sal switch if necessary, handle the trap/interrupt/syscall
 * and do a Sal-user switch.
p *)
PROCEDURE SyscallWrap (VAR ss: CPU.SavedState) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.Low);
    IF DoSpy THEN Spy.Enter(syscallHandlerTime); END;
    TRY
      Trap.Syscall(trap.activeStrand, ss);
    EXCEPT
    ELSE
    END;
    IF DoSpy THEN Spy.Exit(syscallHandlerTime); END;
    ExitKernel(ss, trap);
  END SyscallWrap;

(* 
 * Handles the entIF entry point. 
 *)
PROCEDURE InstructionFault(VAR ss: CPU.SavedState; 
                           type: Word.T) =
  VAR
    inst: INTEGER;
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    
    IF ISTYPE(trap.activeStrand, Thread.T) THEN (* Sal thread? *)
      IO.Put("SPIN: InstructionFault at pc=0x"); IO.Putx(ss.pc);
      IO.Put(" ra=0x"); IO.Putx(ss.ra);
      IO.Put(" type = 0x"); IO.Putx(type); IO.Put("\n");
      inst := LOOPHOLE(Word.And(ss.pc, 16_fffffffffffffff8), 
                              UNTRACED REF INTEGER)^;
      IO.Put("SPIN: instruction=0x"); IO.Putx(inst); IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    IO.Put("SPIN: InstructionFault at pc=0x"); IO.Putx(ss.pc); IO.Put("\n");
    TRY
      CASE type OF
      | IFTypeBpt     => Trap.Breakpoint(trap.activeStrand,ss,ss.pc);
      | IFTypeFEN     => Trap.FPDisabled(trap.activeStrand,ss,ss.pc);
      | IFTypeBugCheck, IFTypeGenTrap, IFTypeOpDec =>
	Trap.IllegalInstruction(trap.activeStrand,ss,ss.pc);
      ELSE
        IO.Put("InstructionFault of type = "); IO.Putx(type); IO.Put("\n");
      END;
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitKernel(ss, trap);
  END InstructionFault;

(* 
 * Handles the entMM entry point. 
 *)
PROCEDURE MemoryManagementFault(VAR ss: CPU.SavedState; 
				addr: CPU.VirtAddress;
				mmcsr: Word.T; reftype: Word.T) =
  VAR
    trap: TrapRecord;
    ec: SpinException.ExceptionCode;
    done := FALSE;
  BEGIN
    (*
     * ___FIRST___ check if this is a heap protection fault intended for
     * the collector.
     *)
    IF Word.And(ss.ps, InUserSpaceBit) = 0 THEN
      (* Fault inside Sal. *)
      IF mmcsr = MMCsrFOW AND 
        addr >= LOOPHOLE(SalExtern.refcount_start, Word.T) AND
        addr <= LOOPHOLE(SalExtern.refcount_end, Word.T)
       THEN
        (* fault on the sentinel page for reference counting buffer *)
        IF RTHeapRep.verbose > 2 THEN
          RTIO.PutText("RC Fault: addr "); RTIO.PutHex(addr); 
          RTIO.PutText(" @ pc "); RTIO.PutHex(ss.pc);
          RTIO.PutText(", ra "); RTIO.PutHex(ss.ra);
          RTIO.PutText(", sp "); RTIO.PutHex(ss.ksp);
          RTIO.PutText(", a0 "); RTIO.PutHex(ss.a0);
          RTIO.PutText(", a1 "); RTIO.PutHex(ss.a1);
          RTIO.PutText(", a2 "); RTIO.PutHex(ss.a1); RTIO.PutText("\n");
	END;
        RTHeapRep.MutatorFault(LOOPHOLE(addr, ADDRESS), 
                               LOOPHOLE(ss.pc, ADDRESS));
        KernelRegions.RASRegions.rasEnsure(ss.pc);
        RETURN;
      END;

      IF (mmcsr = MMCsrFOR OR mmcsr = MMCsrFOW) AND
        addr >= LOOPHOLE(RTHeapRep.MinAddress(), Word.T) AND
	addr <= LOOPHOLE(RTHeapRep.MaxAddress(), Word.T)
       THEN
        (* GC fault *)
        IF RTHeapRep.verbose > 2 THEN
          RTIO.PutText("GC Fault: addr "); RTIO.PutHex(addr); 
          RTIO.PutText(" @ pc "); RTIO.PutHex(ss.pc); RTIO.PutText(", ");
          RTIO.PutText(" & ra "); RTIO.PutHex(ss.ra); RTIO.PutText("\n");
	END;

	IF NOT RTHeapRep.Fault(LOOPHOLE(addr, ADDRESS)) THEN
	  IO.Put("ERROR: RTHeapRep.Fault failed\n");
	END;
	RETURN;
      END;

      IF NOT ((addr >= 16_100000000 AND addr <  16_200000000)
	      OR (addr >= 16_3ff00000000 AND addr < 16_40000000000)) THEN
	(* Other kind of fault is simply a programmer error. *)
	IF Debug THEN Log.Log("Protection Fault\n"); Log.Dumplog(); END;
	IF Word.LT(addr, 16_FFFF - 1) THEN
	  ec := SpinException.ExceptionCode.AttemptToDereferenceNIL;
	ELSE
	  ec := SpinException.ExceptionCode.ProtectionFault;
	END;
	<*NOWARN*>Firewall.Fault(trap.activeStrand, ss, addr, mmcsr, reftype, ec);
      END;

      (* Fault on a user address from inside Sal. This happens
	 during Translation.Read or Translation.Write. Just take an
	 ordinary page fault. *)
      IF StrandExtern.Cur.translation = NIL THEN
	RTIO.PutText("fault on user space, but translation=NIL.\n");
	Debugger.Enter();
      END;
      (* fall through. *)
    END;

    (*
     * Handle all other faults.
     *)
    
    EnterKernel(ss, trap);

    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.Low);
    TRY
      IF DoSpy THEN Spy.Enter(faultHandlerTime); END;
      CASE mmcsr OF
      | MMCsrTNV =>
        Trap.InvalidTranslation(trap.activeStrand, ss,
				       trap.kernelStrand.translation,
				       addr, reftype, done);
      | MMCsrACV =>
        Trap.AccessViolation(trap.activeStrand, ss,
				    trap.kernelStrand.translation,
				    addr, reftype, done);
      | MMCsrFOE, MMCsrFOR, MMCsrFOW =>
        Trap.ProtectionFault(trap.activeStrand, ss,
				    trap.kernelStrand.translation,
				    addr, reftype, done);
      ELSE
        IO.Put("MemoryManagementFault with MMCSR = ");
	IO.Putx(mmcsr);
        IO.Put("\n");
      END;
      IF DoSpy THEN Spy.Exit(faultHandlerTime); END;
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    
    ExitKernel(ss, trap);
  END MemoryManagementFault;

PROCEDURE UnalignedAccess(VAR ss: CPU.SavedState;
  			  addr: CPU.VirtAddress;
			  opcode: Word.T;
                          destreg: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    IF ISTYPE(trap.activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: UnalignedAccess to 0x");IO.Putx(addr);
      IO.Put("pc = 0x"); IO.Putx(ss.pc); IO.Put(" ra=0x"); IO.Putx(ss.ra);
      Debugger.Enter();
      <*NOWARN*>Firewall.Fault(trap.activeStrand, ss, addr, opcode, destreg,
                               SpinException.ExceptionCode.UnalignedFault);
      (* not reached *)
    END;
    
    TRY
      Trap.UnalignedAccess(trap.activeStrand, ss,
				  trap.kernelStrand.translation, addr);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitKernel(ss, trap);
  END UnalignedAccess;

PROCEDURE ArithmeticFault(VAR ss: CPU.SavedState;
			  exceptionReg: Word.T;
			  exceptionRegWriteMask: Word.T;
			  unused: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    
    IF TYPECODE(trap.activeStrand) = TYPECODE(Thread.T) THEN
      (* Sal thread *)
      IO.Put("SPIN: ArithmeticFault with exception register = ");
      IO.Putx(exceptionReg);
      IO.Put(" and exception register write mask = ");
      IO.Putx(exceptionRegWriteMask); IO.Put(" pc = 0x"); IO.Putx(ss.pc);
      IO.Put(" ra=0x"); IO.Putx(ss.ra); IO.Put("\n");

      <*NOWARN*>Firewall.Fault(trap.activeStrand, ss, exceptionReg,
			       exceptionRegWriteMask, unused,
                               SpinException.ExceptionCode.ArithmeticFault);
      (* not reached *)
    END;
    TRY
      (*
       * NOTE: Once there are machine independent traps defined for
       * normal arithmetic errors, they should be broken out here based on
       * the exceptionReg and called.  For now we just print.
       *)
      IO.Put("ArithmeticFault pc = 0x"); IO.Putx(ss.pc);
      IO.Put(" ra=0x"); IO.Putx(ss.ra);
      IO.Put("\nExceptionReg value is 16_");
      IO.Putx(exceptionReg);
      IO.Put(" : ");
      IF Word.And(exceptionReg, ArithSWC) = ArithSWC THEN
        IO.Put("Software completion ");
      END;
      IF Word.And(exceptionReg, ArithINV) = ArithINV THEN
        IO.Put("Invalid operation ");
      END;
      IF Word.And(exceptionReg, ArithDZE) = ArithDZE THEN
        IO.Put("Division by zero ");
      END;
      IF Word.And(exceptionReg, ArithOVF) = ArithOVF THEN
        IO.Put("Overflow ");
      END;
      IF Word.And(exceptionReg, ArithUNF) = ArithUNF THEN
        IO.Put("Underflow ");
      END;
      IF Word.And(exceptionReg, ArithINE) = ArithINE THEN
        IO.Put("Inexecact result ");
      END;
      IF Word.And(exceptionReg, ArithIOV) = ArithIOV THEN
        IO.Put("Integer overflow ");
      END;
      IO.Put("\nException Summary Reg Write Mask = ");
      IO.Putx(exceptionRegWriteMask); IO.Put("\n");
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitKernel(ss, trap);
  END ArithmeticFault;

PROCEDURE ClockTick(VAR ss: CPU.SavedState) = 
  VAR
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.Clock);
    (* timing statistics *)
    IF Debug THEN
      Log.Log("Clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
    ClockPrivate.ClockTick(ss);
    SchedPrivate.ClockTick(ss);
    IF Debug THEN
      Log.Log("Returning from clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
    ExitKernel(ss, trap);
  END ClockTick; 

PROCEDURE IOInterrupt(VAR ss: CPU.SavedState; scboffset: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    IF Debug THEN
      Log.Log("Interrupt at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.IO);
    TRY
      (* 
       * find the interrupt handler vector set up by intr_setvec.
       *)
      WITH vec = SalExtern.scb[scboffset DIV ADRSIZE(SalExtern.SCBT)] DO
        (* 
         * handle the interrupt.
         *)
        vec.proc(vec.arg);
      END;
      SchedPrivate.PreemptIfNeeded(ss);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;

    IF Debug THEN
      Log.Log("Returning from interrupt at ss=");
      Log.Log("Interrupt at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
    ExitKernel(ss, trap);
  END IOInterrupt; 

(* 
 * Handles machine checks
 *)
PROCEDURE MachineCheck(<*UNUSED*>VAR ss: CPU.SavedState;
                       vector: Word.T;
                       logout: CPU.VirtAddress) =
  BEGIN
(* XXX the following causes hangs on TCs.
    EnterKernel(machineCheckTime, ss, CPU.InterruptLevel.High,
                activeStrand, kernelStrand, activeTranslation);
*)
    TRY
      (* Call SAL mach_error() routine.  This is necessary to do bus probe *)
      SalExtern.HandleMachineCheck(vector, logout, 0);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
(*     ExitKernel(ss, activeStrand, kernelStrand, activeTranslation); *)
  END MachineCheck;

PROCEDURE StrayInterrupt(VAR ss: CPU.SavedState; value: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterKernel(ss, trap);
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    IO.Put("SPIN stray interrupt no 0x"&Fmt.Unsigned(value) &
	   " at pc=0x" & Fmt.Unsigned(ss.pc) &
	   " ra=0x" & Fmt.Unsigned(ss.ra) & "\n");
    ExitKernel(ss, trap);
  END StrayInterrupt;

PROCEDURE PFMInterrupt (VAR ss: CPU.SavedState; value: Word.T) =
  VAR
    rpcc := SalExtern.rpcc();
  BEGIN
    PFMExtern.Interrupt(ss.pc, ss.ps, value, rpcc, 0);
  END PFMInterrupt;
  
PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    IF DoSpy THEN
      faultHandlerTime := Spy.Create("faulthandler", FALSE, 100);
      syscallHandlerTime := Spy.Create("syscallhandler", FALSE, 100);
    END;
    faultHandlerTime := Spy.Create("page fault", TRUE);
    syscallHandlerTime := Spy.Create("syscall wrap", TRUE);
    
    CPUPrivate.InstallTrapHandler(CPU.TrapType.INSNFAULT,
                                         TrapPrivate.InstructionFault);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.UNALIGNED,
                                         TrapPrivate.UnalignedAccess);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.MACHINECHECK,
                                         TrapPrivate.MachineCheck);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.MEMORYMANAGEMENT,
					 TrapPrivate.MemoryManagementFault);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.KNIO, 
                                         TrapPrivate.IOInterrupt);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.CLOCK,
                                         TrapPrivate.ClockTick);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.ARITHMETIC,
                                         TrapPrivate.ArithmeticFault);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.STRAY,
                                         TrapPrivate.StrayInterrupt);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.SYSCALL,
                                         TrapPrivate.SyscallWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.PFM,
					 TrapPrivate.PFMInterrupt);
    IF verbose THEN 
      IO.Put("Traps redirected...\n"); 
    END;
  END Init;

BEGIN
END TrapPrivate.
