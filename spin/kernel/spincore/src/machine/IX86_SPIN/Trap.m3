(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Oct-97  Yasushi Saito (yasushi) at the University of Washington
 *	Merged trapprivate and trap. Static binding introduced.
 * 10-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *
 *) 


UNSAFE MODULE Trap EXPORTS Trap, TrapPrivate; (* unsafe because of loophole *)
IMPORT Trap, CPU, CPUPrivate, CPUExtern;
IMPORT Strand, StrandRep;
IMPORT Log, SafeConvert, Word, IO, Fmt;
IMPORT Dispatcher, Auth;
IMPORT Translation;
IMPORT StrandExtern;
IMPORT KernelRegions;
IMPORT Thread, ThreadRep, ThreadPrivate;
IMPORT RTOS;
IMPORT RTHeapRep;
IMPORT RTIO;
IMPORT Firewall, SpinException;
IMPORT ClockPrivate, SchedPrivate;
IMPORT SalExtern;
IMPORT MachineThreadExtern; (* For clk_int flag *)

CONST
  Debug = FALSE;
  InUserSpaceBit = 3;
  
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
    | Trap.Execute =>
      IO.Put("executing an instruction at ");
    | Trap.Read =>
      IO.Put("reading from ");
    | Trap.Write =>
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
    DefaultAccessFaultHandler("translation", strand, ss, map,
			      addr, type);
  END InvalidTranslation;

PROCEDURE AccessViolation(strand: Strand.T; 
			  VAR ss: CPU.SavedState;	
			  map : Translation.T;
  			  addr: CPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("access", strand, ss, map, addr, type);
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
    IO.Putx(LOOPHOLE(addr, UNTRACED REF INTEGER)^); IO.Put("\n");

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

FUNCTIONAL
PROCEDURE UntrustedImposedSyscallGuard (key : AuthKey;
					strand: Strand.T;
					VAR ms: CPU.SavedState) : BOOLEAN = 
  BEGIN
    IF strand = key.strand
      AND (ms.eax >= key.minProcID AND ms.eax <= key.maxProcID) THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END UntrustedImposedSyscallGuard;
  
FUNCTIONAL
PROCEDURE TrustedImposedSyscallGuard (key : AuthKey;
				      <*UNUSED*>strand: Strand.T;
				      VAR ms: CPU.SavedState) : BOOLEAN = 
  BEGIN
    IF ms.eax >= key.minProcID AND ms.eax <= key.maxProcID THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END TrustedImposedSyscallGuard;

		 
PROCEDURE AuthorizeSyscall (<*UNUSED*>a : Auth.T;
			    k : Auth.Key; spindle : REFANY) : BOOLEAN =
  VAR
    newKey := NEW(Trap.AuthKey);
  BEGIN
    IF NOT ISTYPE(k, Trap.AuthKey) THEN
      IO.Put("syscall auth : key is not of type AuthKey\n");
      RETURN FALSE;
    END;
    newKey^ := NARROW(k, Trap.AuthKey)^;

    TRY
      IO.Put("Trap.syscall auth :");
      IF newKey.strand = NIL THEN
        (*
	IF Identity.IsPrivileged() THEN
	  IO.Put("PRIVILEGED SYSCALL\n");
	ELSE
	  IO.Put("Trap.syscall auth insufficient privs\n");
	  RETURN FALSE;
	END;

              Security Change Me : Add DTE extend rights test
        *) 
	
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
FUNCTIONAL PROCEDURE ImposedFaultGuard (key : FaultHandlerAuthKey;
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
    IF NOT ISTYPE(k, Trap.FaultHandlerAuthKey) THEN
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

PROCEDURE EnterKernel (VAR ss: CPU.SavedState): Strand.T =
  VAR strand := StrandExtern.Cur;
  BEGIN
    IF Debug THEN
      Log.Log("Entered Sal at pc="); Log.Logx(ss.pc);
      Log.Log("eflags="); Log.Logx(ss.eflags); Log.Log("\n");
    END;
    
    IF Word.And(ss.cs, InUserSpaceBit) # 0 THEN
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

PROCEDURE ExitKernel(strand: Strand.T; VAR ss: CPU.SavedState) =
  BEGIN
    IF Debug THEN
      Log.Log("Exiting Sal to pc="); Log.Logx(ss.pc);
    END;
    IF Word.And(ss.cs, InUserSpaceBit) # 0 THEN
      (* unbind from user thread *)
      (*
       * if the user strand is currently blocked, we should save its
       * current state and the Sal thread should die instead of going
       * out to user space.
       *)
      strand.trapFrame := LOOPHOLE(-1, UNTRACED REF CPU.SavedState);
      IF strand.count <= 0 THEN
	VAR
	  kernelStrand := strand.bound_to_kernel;
	BEGIN
	  EVAL CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
	  strand.bound_to_kernel := NIL;
	  kernelStrand.bound_to_user := NIL;
	  ThreadPrivate.KillTrapHndlr(kernelStrand);
	  IO.PutError("Should not get past Exit_main in syscall wrap\n");
	END;
      END;
    END;
  END ExitKernel;

(*
 * Trap handling routines. We get here from Core. We then perform a
 * user-Sal switch if necessary, handle the trap/interrupt/syscall
 * and do a Sal-user switch.
 *)

PROCEDURE SyscallWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    EVAL CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.Low);
    strand := EnterKernel(ss);
    TRY
      Trap.Syscall(strand, ss);
    EXCEPT
    ELSE
    END;
    ExitKernel(strand, ss);
  END SyscallWrap;

PROCEDURE PrivilegedInstructionFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN (* Sal thread? *)
      IO.Put("SPIN: Privileged instruction fault at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      Trap.IllegalInstruction(strand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitKernel(strand, ss);
  END PrivilegedInstructionFaultWrap;

PROCEDURE TraceTrapWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN 
      IO.Put("SPIN: Trace trap at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      Trap.Breakpoint(strand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitKernel(strand, ss);
  END TraceTrapWrap;

PROCEDURE BreakpointWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN 
      IO.Put("SPIN: Breakpoint at pc=0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      RTOS.Crash();
      IO.PutError("RTOS.Crash returned to caller!\n");
      (* not reached *)
    END;
    TRY
      Trap.Breakpoint(strand,ss,ss.pc);
    EXCEPT
      (* do nothing, just go back. *)
    ELSE
    END;
    ExitKernel(strand, ss);
  END BreakpointWrap;

PROCEDURE ArithmeticTrapWrap(VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IO.Put("SPIN: Arithmetic trap at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END ArithmeticTrapWrap;

PROCEDURE DivideFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IO.Put("SPIN: Divide fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END DivideFaultWrap;

PROCEDURE OverflowFaultWrap(VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IO.Put("SPIN: Overflow fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END OverflowFaultWrap;

PROCEDURE BoundsCheckFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IO.Put("SPIN: Bounds check fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END BoundsCheckFaultWrap;

PROCEDURE ProtectionFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
    done := FALSE;
    addr: Word.T;
    spl: CPU.InterruptLevel; 
    reftype: Word.T;
  BEGIN
    addr := CPUExtern.get_cr2();

    (*
     * ___FIRST___ check if this is a heap protection fault intended for
     * the collector.
     *)
    IF Word.GE(addr, LOOPHOLE(RTHeapRep.MinAddress(), Word.T))
      AND Word.LE(addr, LOOPHOLE(RTHeapRep.MaxAddress(), Word.T)) 
      AND Word.And(ss.cs, InUserSpaceBit) # 0 THEN
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      IF RTHeapRep.verbose > 2 THEN
        RTIO.PutText("GC Fault: addr "); RTIO.PutHex(addr); 
        RTIO.PutText(" @ pc "); RTIO.PutHex(ss.pc); RTIO.PutText("\n");
      END;

      IF NOT RTHeapRep.Fault(LOOPHOLE(addr, ADDRESS)) THEN
        IO.Put("ERROR: RTHeapDep.Fault failed\n");
      END;
      CPUPrivate.RestoreInterruptMask(spl);
      RETURN;
    END;

    IF Word.And(ss.err, 16_1) # 0 THEN
      reftype := Trap.Write;
    ELSE
      reftype := Trap.Read;
    END;
    
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IF Debug THEN Log.Log("Protection Fault\n"); Log.Dumplog(); END;
      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
                               SpinException.ExceptionCode.ProtectionFault);
      (* not reached *)
    END;
    TRY
      Trap.ProtectionFault(strand, ss,
				  strand.translation,
				  addr, 0 (* REFTYPE *), done);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitKernel(strand, ss);
  END ProtectionFaultWrap;

PROCEDURE PageFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
    addr: Word.T;
    reftype: Word.T;
    ec: SpinException.ExceptionCode;
    done := FALSE;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);

    addr := CPUExtern.get_cr2();

    IF Word.And(ss.err, 16_1) # 0 THEN
      reftype := Trap.Write;
    ELSE
      reftype := Trap.Read;
    END;

    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IF Debug THEN Log.Log("Page Fault\n"); Log.Dumplog(); END;
      IF Word.LT(addr, (16_FFFF - 1)) THEN
        ec := SpinException.ExceptionCode.AttemptToDereferenceNIL;
      ELSE
        ec := SpinException.ExceptionCode.ProtectionFault;
      END;

      <*NOWARN*>Firewall.Fault(strand, ss, addr, 0, reftype, ec);
      (* not reached *)
    END;
    TRY
      Trap.InvalidTranslation(strand, ss,
				     strand.translation,
				     addr, reftype, done);
    EXCEPT
    ELSE
      (* do nothing, just go back. *)
    END;
    ExitKernel(strand, ss);
  END PageFaultWrap;

PROCEDURE FPFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN  
      IO.Put("SPIN: Floating point fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");
      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END FPFaultWrap;

PROCEDURE FPOperandFaultWrap (VAR ss: CPU.SavedState) =
  VAR
    strand: Strand.T;
  BEGIN
    strand := EnterKernel(ss);
    CPUPrivate.RestoreInterruptMask(ss.spl);
    IF TYPECODE(strand) = TYPECODE(Thread.T) THEN 
      IO.Put("SPIN: Floating point operand fault at pc = 0x"); IO.Putx(ss.pc);
      IO.Put("\n");

      <*NOWARN*>Firewall.Fault(strand, ss, 0, 0, 0,
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
    ExitKernel(strand, ss);
  END FPOperandFaultWrap;

PROCEDURE ClockTickWrap(VAR ss: CPU.SavedState) = 
  VAR
    strand: Strand.T;
    spl: CPU.InterruptLevel; 
  BEGIN
    strand := EnterKernel(ss);
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.Clock);
    (* timing statistics *)
    IF Debug THEN
      Log.Log("Clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc);
      Log.Log("\n");
    END;
    ClockPrivate.ClockTick(ss);
    (* 
       Set the clock int flag so that if preemption takes place
       we will know to re-enable clock ints.
    *)
    MachineThreadExtern.clk_int := TRUE;
    SchedPrivate.ClockTick(ss);
    (* Restore the flag because most likely no preemption occurred *)
    MachineThreadExtern.clk_int := FALSE;
    IF Debug THEN
      Log.Log("Returning from clocktick at ss="); Log.Logx(LOOPHOLE(ADR(ss), Word.T));
      Log.Log(" pc="); Log.Logx(ss.pc);
      Log.Log("\n");
    END;
    ExitKernel(strand, ss);
  END ClockTickWrap; 

PROCEDURE Init(verbose: BOOLEAN) =
  VAR
    ok : BOOLEAN := TRUE;
  BEGIN
    (* overriding the clocktick procedure variable that sal exports *) 
    SalExtern.ClockTick := ClockTickWrap;

    TRY
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeSyscall),
                               Trap.Syscall,
                               THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       Trap.InvalidTranslation,
			       THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       Trap.AccessViolation,
			       THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       Trap.ProtectionFault,
			       THIS_MODULE());
    EXCEPT
    | Dispatcher.Error => 
      IO.Put("Systemcall authorizer installation failed\n");
      ok := FALSE;
    END;
    
    IF verbose AND ok THEN
      IO.Put("Systemcall authorizer installed\n");
    END;
  END Init;

BEGIN 
END Trap.
