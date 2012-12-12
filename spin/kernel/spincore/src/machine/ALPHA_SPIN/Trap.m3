(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Changed an error message.
 *
 * 09-Sep-97  Yasushi Saito (yasushi) at the University of Washington
 *	Merged Trap and TrapPrivate.
 *	
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 21-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added fault handler authorizer etc
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 27-May-96  Stefan Savage (savage) at the University of Washington
 *	Moved DumpState to CPU
 *
 * 17-May-96  Brian Bershad (bershad) at the University of Washington
 *	Added some semblance of auth on override of syscall.
 *
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Moved default system call handlers here.
 *
 * 28-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added code for default trap handlers for memory management traps,
 *	instruction traps and alignment traps.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 09-Jan-95 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the Syscall trap signature.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Added dispatcher exceptions to the list of exceptions raised
 *	by procedures that interface clients with handler installation.
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Nuked InstallProtectionFaultHandler
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Moved the registration with interrupt-level code to Trap, so
 *	that functions that are installed upon (e.g. ClockTick) are
 *	visible. Someday the dispatcher will be fixed and we won't have
 *	to bend backwards.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Wrappers to impose when clauses on internal trap events.
 *) 


UNSAFE MODULE Trap EXPORTS Trap, TrapPrivate;
(* unsafe because of loopholes and externs *)

IMPORT StrandExtern;
IMPORT Trap, CPU, CPUPrivate, Strand, StrandRep;
IMPORT Log, SafeConvert, Word, IO, RTIO, Fmt;
IMPORT Dispatcher, Auth;
IMPORT Translation;
IMPORT Spy;
IMPORT KernelRegions;
IMPORT CPUExtern;
IMPORT Thread, ThreadRep, ThreadPrivate;
IMPORT RTOS;
IMPORT SalExtern;
IMPORT RTHeapRep;
IMPORT SpinException;
IMPORT Firewall;
IMPORT Debugger;
IMPORT ClockPrivate;
IMPORT SchedPrivate;
IMPORT PFMExtern;

CONST
  Debug = FALSE;
  DoSpy = FALSE;

(*
   Default event handlers
*)  
PROCEDURE Syscall(<*UNUSED*>strand: Strand.T;
		  <*UNUSED*>VAR ss: CPU.SavedState) =
  BEGIN
  END Syscall;

PROCEDURE UnhandledUserSpaceException(<*UNUSED*>strand: Strand.T;
			      <*UNUSED*>VAR ss: CPU.SavedState) =
  BEGIN
    IO.Put("Trap.UserSpaceThreadDied called.\n");
    Strand.Block(Strand.GetCurrent()); 
  END UnhandledUserSpaceException;
  
PROCEDURE DefaultAccessFaultHandler(msg : TEXT;
				    strand: Strand.T; 
				    VAR ss: CPU.SavedState;
				    <*UNUSED*>map : Translation.T;
				    addr: CPU.VirtAddress;
				    type: Word.T) =
  BEGIN
    IO.Put("\n\n" & msg & " while ");
    CASE type OF
    | Execute =>
      IO.Put("executing an instruction at ");
    | Read =>
      IO.Put("reading from ");
    | Write =>
      IO.Put("writing to ");
    ELSE
      IO.Put("doing unknown operation ("); IO.Putx(type);IO.Put(" ) to "); 
    END;
    IO.Putx(addr); IO.Put("\n");

    CPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Trap.UnhandledUserSpaceException(strand, ss);
  END DefaultAccessFaultHandler;

PROCEDURE InvalidTranslation(strand: Strand.T;
			     VAR ss: CPU.SavedState;
			     map : Translation.T;
  			     addr: CPU.VirtAddress;
			     type: INTEGER;
			     VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("translation", strand, ss, map, addr, type);
  END InvalidTranslation;

PROCEDURE AccessViolation(strand: Strand.T; 
			  VAR ss: CPU.SavedState;	
			  map : Translation.T;
  			  addr: CPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("access privilege", strand, ss, map, addr, type);
  END AccessViolation; 

PROCEDURE ProtectionFault(strand: Strand.T; 
			  VAR ss: CPU.SavedState;
			  map : Translation.T;
  			  addr: CPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("protection", strand, ss, map, addr, type);
  END ProtectionFault;

PROCEDURE UnalignedAccess(strand: Strand.T; 
			  VAR ss: CPU.SavedState;
			  <*UNUSED*>map : Translation.T;
  			  addr: CPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nUnalignedAccess at 0x"); IO.Putx(addr);
    IO.Put(". No fixup support.\n");
    CPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Trap.UnhandledUserSpaceException(strand, ss);
  END UnalignedAccess;

(*
 * Instruction traps
 *)
PROCEDURE Breakpoint(strand: Strand.T; 
                     VAR ss: CPU.SavedState; 
  		     addr: CPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nBreakpoint at 0x"); IO.Putx(addr);IO.Put(".\n");
    CPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Strand.Block(Strand.GetCurrent());
  END Breakpoint;    

PROCEDURE IllegalInstruction(strand: Strand.T; 
                             VAR ss: CPU.SavedState;
  			     addr: CPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nIllegalInstruction at 0x"); IO.Putx(addr); IO.Put(".\n");
    IO.Put("Instruction = 0x");
    IO.Putx(LOOPHOLE(Word.And(addr, 16_fffffffffffffff8),
                     UNTRACED REF INTEGER)^); IO.Put("\n");

    CPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread.");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Trap.UnhandledUserSpaceException(strand, ss);
  END IllegalInstruction; 

PROCEDURE FPDisabled(strand: Strand.T; 
                     VAR ss: CPU.SavedState; 
  		     addr: CPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nFPDisabled at 0x"); IO.Putx(addr); IO.Put(".\n");
    CPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Trap.UnhandledUserSpaceException(strand, ss);
  END FPDisabled; 

  
(* Systemcall authorizer stuff *)

FUNCTIONAL PROCEDURE
UntrustedImposedSyscallGuard (key: AuthKey; strand: Strand.T;
				VAR ms: CPU.SavedState): BOOLEAN = 
  BEGIN
    RETURN ms.v0 >= key.minProcID
      AND ms.v0 <= key.maxProcID
      AND strand = key.strand;
  END UntrustedImposedSyscallGuard;
  
FUNCTIONAL PROCEDURE
TrustedImposedSyscallGuard (key: AuthKey;
			    <*UNUSED*>strand: Strand.T;
			    VAR ms: CPU.SavedState): BOOLEAN = 
  BEGIN
    RETURN ms.v0 >= key.minProcID AND ms.v0 <= key.maxProcID;
  END TrustedImposedSyscallGuard;

		 
PROCEDURE AuthorizeSyscall (<*UNUSED*>a : Auth.T;
			    k : Auth.Key; spindle : REFANY) : BOOLEAN =
  VAR
    newKey := NEW(AuthKey);
  BEGIN
    IF NOT ISTYPE(k, AuthKey) THEN
      IO.Put("syscall auth : key is not of type AuthKey\n");
      RETURN FALSE;
    END;
    newKey^ := NARROW(k, AuthKey)^;

    TRY
      IO.Put("Trap.syscall auth :");
      IF newKey.strand = NIL THEN
	EVAL Dispatcher.ImposeGuard(spindle, TrustedImposedSyscallGuard,
				    newKey);
      ELSE
	EVAL Dispatcher.ImposeGuard(spindle, UntrustedImposedSyscallGuard,
				    newKey);
      END;
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("Trap.Authorize : can't impose guard("
	     & Fmt.Int(ORD(ec)) & ").\n");
      RETURN FALSE;
    END;

    IO.Put("syscall extension for [" 
	   & Fmt.Int(newKey.minProcID) & " .. " 
	   & Fmt.Int(newKey.maxProcID) & "] is installed.\n");
    RETURN TRUE;
  END AuthorizeSyscall;
  
(* Fault handler authorizer stuff *)
FUNCTIONAL
  PROCEDURE ImposedFaultGuard (key : FaultHandlerAuthKey;
			       <*UNUSED*>strand: Strand.T;
			       <*UNUSED*>VAR ss: CPU.SavedState;
			       map : Translation.T;
			       <*UNUSED*>addr: CPU.VirtAddress;
			       <*UNUSED*>type: INTEGER;
			       <*UNUSED*>VAR done : BOOLEAN) : BOOLEAN =
    BEGIN
      RETURN key = map;
    END ImposedFaultGuard;
  
PROCEDURE AuthorizeFaultHandler (<*UNUSED*>a : Auth.T;
				 k : Auth.Key; binding : REFANY) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE(k, FaultHandlerAuthKey) THEN
      IO.Put("fault auth : key is of wrong type\n");
      RETURN FALSE;
    END;
    TRY
      EVAL Dispatcher.ImposeGuard(binding, ImposedFaultGuard, k);
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("Trap.Authorize : can't impose guard("
	     & Fmt.Int(ORD(ec)) & ").\n");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END AuthorizeFaultHandler;

(*
   Low level trap handlers
*)
  
VAR
  faultHandlerTime: Spy.T;
  syscallHandlerTime: Spy.T;
  
CONST
  InUserSpaceBit = 8; (* If this bit of "ss" reg is set, then trap is from
			 user space. *)


(*
   Called first by all low-level trap handlers.
   This proc sets up the kernel execution environment (if the intr is from
   user space).
*)
PROCEDURE EnterKernel (VAR ss: CPU.SavedState): Strand.T =
  VAR strand := StrandExtern.Cur;
  BEGIN
    IF Debug THEN
      Log.Log("Entered kernel at pc="); Log.Logx(ss.pc);
      Log.Log("ps="); Log.Logx(ss.ps); Log.Log(" ");
      Log.Log("ra="); Log.Logx(ss.ra); Log.Log(" ");
      Log.Log("ksp="); Log.Logx(ss.ksp); Log.Log(" ");
      Log.Log("usp="); Log.Logx(ss.usp); Log.Log("\n");
    END;
    (* Note: user space thread(ust) and kernel thread are bound when the ust
       is created. Therefore, we don't do anything here.
       
       Yasushi is responsible for this change, and if you plan to
       resurrect the fast preemption, consult him or Gun. *)
    IF Word.And(ss.ps, InUserSpaceBit) # 0 THEN
      (*
       * if we came in from user space,
       * bind a Sal strand to the user strand
       *)
      strand.trapFrame := LOOPHOLE(ADR(ss), UNTRACED REF CPU.SavedState);
    ELSE
      KernelRegions.RASRegions.rasEnsure(ss.pc);
    END;
    RETURN strand;
  END EnterKernel;

(* Called just before low level handlers switch back to the interrupted
   context. This proc unbinds the kernel thread from the user thread. *)
  
PROCEDURE ExitKernel(strand: Strand.T; READONLY ss: CPU.SavedState) =
  BEGIN
    IF Debug THEN
      Log.Log("Exiting Sal to pc="); Log.Logx(ss.pc);
      Log.Log("ps="); Log.Logx(ss.ps); Log.Log("\n");
    END;
    IF Word.And(ss.ps, InUserSpaceBit) # 0 THEN
      (* set an invalid value to the trap frame to catch an
	 exception if someone touches it. *)
      strand.trapFrame := LOOPHOLE(-1, UNTRACED REF CPU.SavedState);
      
      (*
       * if the user strand is currently blocked, we should save its
       * current state and the Sal thread should die instead of going
       * out to user space.
       *)
      IF strand.count <= 0 THEN
	VAR
	  kernelStrand := strand.bound_to_kernel;
	BEGIN
	  EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.High);
	  strand.bound_to_kernel := NIL;
	  kernelStrand.bound_to_user := NIL;
	  ThreadPrivate.KillTrapHndlr(kernelStrand);
	  IO.PutError("Should not get past Exit_main in syscall wrap\n");
	END;
      END;
    END;
  END ExitKernel;

PROCEDURE ExtractSpl(ps: Word.T) : CPU.InterruptLevel =
  BEGIN
    RETURN VAL(Word.And(ps, 16_7), CPU.InterruptLevel);
  END ExtractSpl;

PROCEDURE SyscallWrap (VAR ss: CPU.SavedState) =
  VAR strand := StrandExtern.Cur;
  BEGIN
    strand.trapFrame := LOOPHOLE(ADR(ss), UNTRACED REF CPU.SavedState);
    IF DoSpy THEN Spy.Enter(syscallHandlerTime); END;
    TRY
      
      (*CASE ss.v0 OF 
      | 4 =>
	Spy.Enter(writeSpy);
	Trap.Syscall(strand, ss);
	Spy.Exit(writeSpy);
      | 67 =>
	Spy.Enter(statSpy);
	Trap.Syscall(strand, ss);
	Spy.Exit(statSpy);
      | 20 =>
	Spy.Enter(getpidSpy);
	Trap.Syscall(strand, ss);
	Spy.Exit(getpidSpy);
      ELSE
	Trap.Syscall(strand, ss);
	 END;*)
      Trap.Syscall(strand, ss);
    EXCEPT
    ELSE
    END;
    IF DoSpy THEN Spy.Exit(syscallHandlerTime); END;
    IF strand.count <= 0 THEN
      VAR
	kernelStrand := strand.bound_to_kernel;
      BEGIN
	EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.High);
	strand.bound_to_kernel := NIL;
	kernelStrand.bound_to_user := NIL;
	ThreadPrivate.KillTrapHndlr(kernelStrand);
	IO.PutError("Should not get past Exit_main in syscall wrap\n");
      END;
    END;
    strand.trapFrame := NIL;
  END SyscallWrap;

(* 
 * Handles the entIF entry point. 
 *)
PROCEDURE InstructionFaultWrap (VAR ss: CPU.SavedState; type: Word.T) =
  VAR
    inst: INTEGER;
    strand := EnterKernel(ss);
  BEGIN
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    
    IF ISTYPE(strand, Thread.T) THEN (* Sal thread? *)
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
      | IFTypeBpt     => Trap.Breakpoint(strand,ss,ss.pc);
      | IFTypeFEN     => Trap.FPDisabled(strand,ss,ss.pc);
      | IFTypeBugCheck, IFTypeGenTrap, IFTypeOpDec =>
	Trap.IllegalInstruction(strand,ss,ss.pc);
      ELSE
        IO.Put("InstructionFault of type = "); IO.Putx(type); IO.Put("\n");
      END;
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitKernel(strand, ss);
  END InstructionFaultWrap;

(* 
 * Handles the entMM entry point. 
 *)
PROCEDURE MemoryFaultWrap (VAR ss: CPU.SavedState; 
			   addr: CPU.VirtAddress;
			   mmcsr: Word.T; reftype: Word.T) =
  VAR
    strand: Strand.T;
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

      IF NOT (addr >= 16_1000000 AND addr < 16_40000000000) THEN 
	(* Other kind of fault is simply a programmer error. *)
	IF Debug THEN Log.Log("Protection Fault\n"); Log.Dumplog(); END;
	IF Word.LT(addr, 16_FFFF - 1) THEN
	  ec := SpinException.ExceptionCode.AttemptToDereferenceNIL;
	ELSE
	  ec := SpinException.ExceptionCode.ProtectionFault;
	END;
	<*NOWARN*>Firewall.Fault(strand, ss, addr, mmcsr, reftype, ec);
      END;

      (* Fault on a user address from inside Sal. This happens
	 during Translation.Read or Translation.Write. Just take an
	 ordinary page fault. *)
      IF Strand.GetCurrent().translation = NIL THEN
        RTIO.PutText("ERROR >> unknown fault in kernel mode and translation=NIL.\n");
        RTIO.PutText("Fault addr "); RTIO.PutHex(addr); 
        RTIO.PutText(", pc "); RTIO.PutHex(ss.pc);
        RTIO.PutText(", ra "); RTIO.PutHex(ss.ra);
        RTIO.PutText(", sp "); RTIO.PutHex(ss.ksp);
        RTIO.PutText(", a0 "); RTIO.PutHex(ss.a0);
        RTIO.PutText(", a1 "); RTIO.PutHex(ss.a1);
        RTIO.PutText(", a2 "); RTIO.PutHex(ss.a1);
        RTIO.PutText("\n");
        Debugger.Enter();
      END;
      (* fall through. *)
    END;

    (*
     * Handle all other faults.
     *)
    
    strand := EnterKernel(ss);

    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.Low);
    TRY
      IF DoSpy THEN Spy.Enter(faultHandlerTime); END;
      CASE mmcsr OF
      | MMCsrTNV =>
        Trap.InvalidTranslation(strand, ss,
				strand.translation,
				addr, reftype, done);
      | MMCsrACV =>
        Trap.AccessViolation(strand, ss,
			     strand.translation,
			     addr, reftype, done);
      | MMCsrFOE, MMCsrFOR, MMCsrFOW =>
        Trap.ProtectionFault(strand, ss,
			     strand.translation,
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
    
    ExitKernel(strand, ss);
  END MemoryFaultWrap;

PROCEDURE UnalignedAccessWrap (VAR ss: CPU.SavedState;
			       addr: CPU.VirtAddress;
			       opcode: Word.T;
			       destreg: Word.T) =
  VAR
    strand := EnterKernel(ss);
  BEGIN
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    IF ISTYPE(strand, Thread.T) THEN  (* Sal thread? *)
      IO.Put("SPIN: UnalignedAccess to 0x");IO.Putx(addr);
      IO.Put("pc = 0x"); IO.Putx(ss.pc); IO.Put(" ra=0x"); IO.Putx(ss.ra);
      Debugger.Enter();
      <*NOWARN*>Firewall.Fault(strand, ss, addr, opcode, destreg,
                               SpinException.ExceptionCode.UnalignedFault);
      (* not reached *)
    END;
    
    TRY
      Trap.UnalignedAccess(strand, ss,
			   strand.translation, addr);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitKernel(strand, ss);
  END UnalignedAccessWrap;

PROCEDURE ArithmeticFaultWrap (VAR ss: CPU.SavedState;
			       exceptionReg: Word.T;
			       exceptionRegWriteMask: Word.T;
			       unused: Word.T) =
  VAR
    strand := EnterKernel(ss);
  BEGIN
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN
      (* Sal thread *)
      IO.Put("SPIN: ArithmeticFault with exception register = ");
      IO.Putx(exceptionReg);
      IO.Put(" and exception register write mask = ");
      IO.Putx(exceptionRegWriteMask); IO.Put(" pc = 0x"); IO.Putx(ss.pc);
      IO.Put(" ra=0x"); IO.Putx(ss.ra); IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, exceptionReg,
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
    ExitKernel(strand, ss);
  END ArithmeticFaultWrap;

PROCEDURE ClockTickWrap (VAR ss: CPU.SavedState) = 
  VAR
    strand := EnterKernel(ss);
  BEGIN
    EVAL CPUExtern.SpinSwapIpl(CPUPrivate.InterruptClass.Clock);
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
    ExitKernel(strand, ss);
  END ClockTickWrap; 

PROCEDURE IOInterruptWrap (VAR ss: CPU.SavedState; scboffset: Word.T) =
  VAR
    strand := EnterKernel(ss);
  BEGIN
    IF Debug THEN
      Log.Log("Interrupt at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc); Log.Log(" ps="); Log.Logx(ss.ps);
      Log.Log("\n");
    END;
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
    ExitKernel(strand, ss);
  END IOInterruptWrap; 

(* 
 * Handles machine checks
 *)
PROCEDURE MachineCheckWrap (<*UNUSED*>VAR ss: CPU.SavedState;
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
  END MachineCheckWrap;

PROCEDURE StrayInterruptWrap (VAR ss: CPU.SavedState; value: Word.T) =
  VAR
    strand := EnterKernel(ss);
  BEGIN
    EVAL CPUExtern.SpinSwapIpl(ExtractSpl(ss.ps));
    IO.Put("SPIN stray interrupt no 0x"&Fmt.Unsigned(value) &
	   " at pc=0x" & Fmt.Unsigned(ss.pc) &
	   " ra=0x" & Fmt.Unsigned(ss.ra) & "\n");
    ExitKernel(strand, ss);
  END StrayInterruptWrap;

(* Performance monitor interrupt *)
PROCEDURE PFMInterruptWrap (VAR ss: CPU.SavedState; value: Word.T) =
  VAR
    rpcc := SalExtern.rpcc();
  BEGIN
    PFMExtern.Interrupt(ss.pc, ss.ps, value, rpcc, 0);
  END PFMInterruptWrap;

VAR
  writeSpy, statSpy, getpidSpy: Spy.T;
  
PROCEDURE Init (verbose: BOOLEAN) =
  PROCEDURE InitFaultHandler(event: PROCANY) RAISES {Dispatcher.Error} =
    BEGIN
      (* Call the default handler last. *)
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(event));
      EVAL Dispatcher.InstallHandler(event, NIL, event,
		options := Dispatcher.Options{Dispatcher.Opt.Last});
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
				       authorize := AuthorizeFaultHandler),
				       event, THIS_MODULE());
    END InitFaultHandler;
  BEGIN
    writeSpy := Spy.Create("trap-write", FALSE, 400);
    statSpy := Spy.Create("trap-stat", FALSE, 400);
    getpidSpy := Spy.Create("trap-getpid", FALSE, 400);
    IF DoSpy THEN
      faultHandlerTime := Spy.Create("faulthandler", FALSE, 100);
      syscallHandlerTime := Spy.Create("syscallhandler", FALSE, 100);
    END;

    (* Redirect low level traps to the M3 world. *)
    CPUPrivate.InstallTrapHandler(CPU.TrapType.INSNFAULT, InstructionFaultWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.UNALIGNED, UnalignedAccessWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.MACHINECHECK, MachineCheckWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.MEMORYMANAGEMENT, MemoryFaultWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.KNIO, IOInterruptWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.CLOCK, ClockTickWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.ARITHMETIC, ArithmeticFaultWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.STRAY, StrayInterruptWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.SYSCALL, SyscallWrap);
    CPUPrivate.InstallTrapHandler(CPU.TrapType.PFM, PFMInterruptWrap);

    (* Tweak the default memory management fault handlers so that they are
       placed at the last of the call chain. *)
    TRY
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Syscall));
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
					       authorize := AuthorizeSyscall),
					   Syscall, THIS_MODULE());
      InitFaultHandler(InvalidTranslation);
      InitFaultHandler(AccessViolation);
      InitFaultHandler(ProtectionFault);
    EXCEPT
    | Dispatcher.Error => 
      IO.PutError("Trap.Init: dispatcher failure.\n");
    END;
    
    IF verbose THEN
      IO.Put("Trap handlers installed.\n"); 
    END;
  END Init;

BEGIN 
END Trap.
