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
 *      Changed address comparisons to use Word.* instead of signed
 *      integer comparisons.
 *
 * 08-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Added support for passing security info from user
 *      to kernel thread.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and MachineCPU interfaces
 *
 * 11-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	changed to use TrapRecord instead of passing args separately.
 *
 * 24-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed most of spys. They've never been used recently.
 *
 * 01-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made cpustate a trap frame pointer.
 *
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
 *	Removed use of LOOPHOLE for MachineCPU.SavedState.strand.
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
MODULE MachineTrapPrivate;

IMPORT Strand, StrandRep, SchedPrivate;
IMPORT MachineCPU, MachineCPUPrivate, MachineTrapPrivate, MachineTrap;
IMPORT Thread, ThreadPrivate, ThreadRep, ClockPrivate, KernelRegions;
IMPORT Firewall, SpinException, RTOS;
IMPORT Log, SalExtern, Word, Spy, IO, Fmt, Debugger;
IMPORT RTHeapRep, RTIO;
IMPORT PFMExtern;
IMPORT DebugOption, SecurityContext;

VAR
  faultHandlerTime: Spy.T;
  syscallHandlerTime: Spy.T;

CONST
  USER_VIRT_ADDR_MIN = 16_100000000;
  USER_VIRT_ADDR_MAX = 16_200000000;
  (* the possible min and max address of user virtual address. These
     values are OS dependent, but we don't care since we
     support DEC UNIX only. *)

CONST
  Debug = FALSE;
  DoSpy = FALSE;
  InUserSpaceBit = 8; (* If this bit of "ss" reg is set, then trap is from
			 user space. *)

TYPE TrapRecord = RECORD
  activeStrand, SalStrand: Strand.T;
END;
  
<*INLINE*>
PROCEDURE EnterSal(VAR ss: MachineCPU.SavedState;
		      (*OUT*)VAR trap: TrapRecord) =
  BEGIN
    (*
     * Spy chain if necessary
     *)
    trap.activeStrand := Strand.GetCurrent();
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
      trap.SalStrand := syscallHandler;
      trap.SalStrand.translation := trap.activeStrand.translation;
      trap.SalStrand.bound_to_user := trap.activeStrand;
      trap.activeStrand.bound_to_kernel := trap.SalStrand;
      (* Hand off security info from user thread to kernel thread *)
      IF DebugOption.Security THEN
        SecurityContext.HandOffContext( trap.activeStrand, trap.SalStrand );
      END;
      syscallHandler := NIL;
    ELSE
      KernelRegions.RASRegions.rasEnsure(ss.pc);
      trap.SalStrand := trap.activeStrand;
    END;
    trap.activeStrand.trapFrame :=
      LOOPHOLE(ADR(ss), UNTRACED REF MachineCPU.SavedState);
  END EnterSal;

PROCEDURE ExitSal(READONLY ss: MachineCPU.SavedState;
		     READONLY trap: TrapRecord) =
  VAR
    spl := MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.High);
  BEGIN
    IF Debug THEN
      Log.Log("Exiting Sal to pc="); Log.Logx(ss.pc);
      Log.Log("ps="); Log.Logx(ss.ps); Log.Log("\n");
    END;
    IF Word.And(ss.ps, InUserSpaceBit) # 0 THEN
      (* unbind from user thread *)
      trap.SalStrand.translation := NIL;
      trap.SalStrand.bound_to_user := NIL;
      trap.activeStrand.bound_to_kernel := NIL;
      (*
       * if the user strand is currently blocked, we should save its
       * current state and the Sal thread should die instead of going
       * out to user space.
       *)
      IF trap.activeStrand.count <= 0 THEN
	trap.activeStrand.trapFrame :=
	  LOOPHOLE(ADR(ss), UNTRACED REF MachineCPU.SavedState);
        EVAL Strand.Stop(trap.activeStrand);
        MachineCPUPrivate.RestoreInterruptMask(spl);
        ThreadPrivate.Exit_main(trap.SalStrand, NIL);
        IO.PutError("Should not get past Exit_main in syscall wrap\n");
      ELSE
        (*
         * Restore the state of the world before the trap and go
         * back out to user space.
         *)
        (* 
         * If someone registered in the meantime to take traps,
         * we replace them.
         *)
        IF syscallHandler # NIL THEN
          ThreadPrivate.KillTrapHndlr(syscallHandler);
        END;
        syscallHandler := trap.SalStrand;
      END;
    END;
    trap.activeStrand.trapFrame :=
      LOOPHOLE(-1,UNTRACED REF MachineCPU.SavedState);
    (* set an invalid value to catch an exception if someone touches
       the trap frame. *)
  END ExitSal;

PROCEDURE ExtractSpl(ps: Word.T) : MachineCPU.InterruptLevel =
  BEGIN
    RETURN VAL(Word.And(ps, 16_7), MachineCPU.InterruptLevel);
  END ExtractSpl;

(*
 * Trap handling routines. We get here from Core. We then perform a
 * user-Sal switch if necessary, handle the trap/interrupt/syscall
 * and do a Sal-user switch.
 *)
PROCEDURE SyscallWrap (VAR ss: MachineCPU.SavedState) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.Low);
    IF DoSpy THEN Spy.Enter(syscallHandlerTime); END;
    TRY
      MachineTrap.Syscall(trap.activeStrand, ss);
    EXCEPT
    ELSE
    END;
    IF DoSpy THEN Spy.Exit(syscallHandlerTime); END;
    ExitSal(ss, trap);
  END SyscallWrap;

(* 
 * Handles the entIF entry point. 
 *)
PROCEDURE InstructionFault(VAR ss: MachineCPU.SavedState; 
                           type: Word.T) =
  VAR
    inst: INTEGER;
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(ExtractSpl(ss.ps));
    
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
      | IFTypeBpt     => MachineTrap.Breakpoint(trap.activeStrand,ss,ss.pc);
      | IFTypeFEN     => MachineTrap.FPDisabled(trap.activeStrand,ss,ss.pc);
      | IFTypeBugCheck, IFTypeGenTrap, IFTypeOpDec =>
	MachineTrap.IllegalInstruction(trap.activeStrand,ss,ss.pc);
      ELSE
        IO.Put("InstructionFault of type = "); IO.Putx(type); IO.Put("\n");
      END;
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitSal(ss, trap);
  END InstructionFault;

(* 
 * Handles the entMM entry point. 
 *)
PROCEDURE MemoryManagementFault(VAR ss: MachineCPU.SavedState; 
				addr: MachineCPU.VirtAddress;
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
        Word.GE(addr, LOOPHOLE(SalExtern.refcount_start, Word.T)) AND
        Word.LE(addr, LOOPHOLE(SalExtern.refcount_end, Word.T))
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
        Word.GE(addr, LOOPHOLE(RTHeapRep.MinAddress(), Word.T)) AND
	Word.LE(addr, LOOPHOLE(RTHeapRep.MaxAddress(), Word.T))
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

      IF NOT (Word.GE(addr, USER_VIRT_ADDR_MIN) AND
	      Word.LT(addr, USER_VIRT_ADDR_MAX)) THEN
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
      IF Strand.GetCurrent().translation = NIL THEN
	RTIO.PutText("fault on user space, but translation=NIL.\n");
	Debugger.Enter();
      END;
      (* fall through. *)
    END;

    (*
     * Handle all other faults.
     *)
    
    EnterSal(ss, trap);

    EVAL MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.Low);
    TRY
      IF DoSpy THEN Spy.Enter(faultHandlerTime); END;
      CASE mmcsr OF
      | MMCsrTNV =>
        MachineTrap.InvalidTranslation(trap.activeStrand, ss,
				       trap.SalStrand.translation,
				       addr, reftype, done);
      | MMCsrACV =>
        MachineTrap.AccessViolation(trap.activeStrand, ss,
				    trap.SalStrand.translation,
				    addr, reftype, done);
      | MMCsrFOE, MMCsrFOR, MMCsrFOW =>
        MachineTrap.ProtectionFault(trap.activeStrand, ss,
				    trap.SalStrand.translation,
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
    
    ExitSal(ss, trap);
  END MemoryManagementFault;

PROCEDURE UnalignedAccess(VAR ss: MachineCPU.SavedState;
  			  addr: MachineCPU.VirtAddress;
			  opcode: Word.T;
                          destreg: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(ExtractSpl(ss.ps));
    IF ISTYPE(trap.activeStrand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: UnalignedAccess to 0x");IO.Putx(addr);
      IO.Put("pc = 0x"); IO.Putx(ss.pc); IO.Put(" ra=0x"); IO.Putx(ss.ra);
      Debugger.Enter();
      <*NOWARN*>Firewall.Fault(trap.activeStrand, ss, addr, opcode, destreg,
                               SpinException.ExceptionCode.UnalignedFault);
      (* not reached *)
    END;
    
    TRY
      MachineTrap.UnalignedAccess(trap.activeStrand, ss,
				  trap.SalStrand.translation, addr);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitSal(ss, trap);
  END UnalignedAccess;

PROCEDURE ArithmeticFault(VAR ss: MachineCPU.SavedState;
			  exceptionReg: Word.T;
			  exceptionRegWriteMask: Word.T;
			  unused: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(ExtractSpl(ss.ps));
    
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
    ExitSal(ss, trap);
  END ArithmeticFault;

PROCEDURE ClockTick(VAR ss: MachineCPU.SavedState) = 
  VAR
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.Clock);
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
    ExitSal(ss, trap);
  END ClockTick; 

PROCEDURE IOInterrupt(VAR ss: MachineCPU.SavedState; scboffset: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    IF Debug THEN
      Log.Log("Interrupt at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.IO);
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
    ExitSal(ss, trap);
  END IOInterrupt; 

(* 
 * Handles machine checks
 *)
PROCEDURE MachineCheck(<*UNUSED*>VAR ss: MachineCPU.SavedState;
                       vector: Word.T;
                       logout: MachineCPU.VirtAddress) =
  BEGIN
(* XXX the following causes hangs on TCs.
    EnterSal(machineCheckTime, ss, MachineCPU.InterruptLevel.High,
                activeStrand, SalStrand, activeTranslation);
*)
    TRY
      (* Call SAL mach_error() routine.  This is necessary to do bus probe *)
      SalExtern.HandleMachineCheck(vector, logout, 0);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
(*     ExitSal(ss, activeStrand, SalStrand, activeTranslation); *)
  END MachineCheck;

PROCEDURE StrayInterrupt(VAR ss: MachineCPU.SavedState; value: Word.T) =
  VAR
    trap: TrapRecord;
  BEGIN
    EnterSal(ss, trap);
    EVAL MachineCPUPrivate.SetInterruptMask(ExtractSpl(ss.ps));
    IO.Put("SPIN stray interrupt no 0x"&Fmt.Unsigned(value) &
	   " at pc=0x" & Fmt.Unsigned(ss.pc) &
	   " ra=0x" & Fmt.Unsigned(ss.ra) & "\n");
    ExitSal(ss, trap);
  END StrayInterrupt;

PROCEDURE PFMInterrupt (VAR ss: MachineCPU.SavedState; value: Word.T) =
  VAR
    rpcc := SalExtern.rpcc();
  BEGIN
    PFMExtern.Interrupt(ss.pc, ss.ps, value, rpcc, 0);
  END PFMInterrupt;
  
PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    faultHandlerTime := Spy.Create("page fault", TRUE);
    syscallHandlerTime := Spy.Create("syscall wrap", TRUE);
    
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.INSNFAULT,
                                         MachineTrapPrivate.InstructionFault);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.UNALIGNED,
                                         MachineTrapPrivate.UnalignedAccess);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.MACHINECHECK,
                                         MachineTrapPrivate.MachineCheck);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.MEMORYMANAGEMENT,
					 MachineTrapPrivate.MemoryManagementFault);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.KNIO, 
                                         MachineTrapPrivate.IOInterrupt);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.CLOCK,
                                         MachineTrapPrivate.ClockTick);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.ARITHMETIC,
                                         MachineTrapPrivate.ArithmeticFault);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.STRAY,
                                         MachineTrapPrivate.StrayInterrupt);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.SYSCALL,
                                         MachineTrapPrivate.SyscallWrap);
    MachineCPUPrivate.InstallTrapHandler(MachineCPU.TrapType.PFM,
					 MachineTrapPrivate.PFMInterrupt);
    IF verbose THEN 
      IO.Put("Traps redirected...\n"); 
    END;
  END Init;

BEGIN
  IF DoSpy THEN
    faultHandlerTime := Spy.Create("faulthandler", FALSE, 100);
    syscallHandlerTime := Spy.Create("syscallhandler", FALSE, 100);
  END;
END MachineTrapPrivate.
