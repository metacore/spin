(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
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


UNSAFE MODULE MachineTrap; (* unsafe because of loophole *)
IMPORT MachineTrap, MachineCPU, MachineCPUPrivate, Strand;
IMPORT Log, SafeConvert, Word, IO, Fmt;
IMPORT Dispatcher, Auth;
IMPORT Translation;

PROCEDURE Syscall(<*UNUSED*>strand: Strand.T;
		  <*UNUSED*>VAR ss: MachineCPU.SavedState) =
  BEGIN
  END Syscall;

PROCEDURE UnhandledUserSpaceException(<*UNUSED*>strand: Strand.T;
			      <*UNUSED*>VAR ss: MachineCPU.SavedState) =
  BEGIN
    IO.Put("MachineTrap.UserSpaceThreadDied called.\n");
    Strand.Block(Strand.GetCurrent()); 
    
  END UnhandledUserSpaceException;

PROCEDURE DefaultAccessFaultHandler(msg : TEXT;
				    strand: Strand.T; 
				    VAR ss: MachineCPU.SavedState;
				    <*UNUSED*>map : Translation.T;
				    addr: MachineCPU.VirtAddress;
				    type: Word.T) =
  BEGIN
    IO.Put("\n\n" & msg & " while ");
    CASE type OF
    | MachineTrap.Execute =>
      IO.Put("executing an instruction at ");
    | MachineTrap.Read =>
      IO.Put("reading from ");
    | MachineTrap.Write =>
      IO.Put("writing to ");
    ELSE
      IO.Put("doing unknown operation ("); IO.Putx(type);IO.Put(" ) to "); 
    END;
    IO.Putx(addr); IO.Put("\n");

    MachineCPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    MachineTrap.UnhandledUserSpaceException(strand, ss);
  END DefaultAccessFaultHandler;

PROCEDURE InvalidTranslation(strand: Strand.T;
			     VAR ss: MachineCPU.SavedState;
			     map : Translation.T;
  			     addr: MachineCPU.VirtAddress;
			     type: INTEGER;
			     VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("translation", strand, ss, map,
			      addr, type);
  END InvalidTranslation;

PROCEDURE AccessViolation(strand: Strand.T; 
			  VAR ss: MachineCPU.SavedState;	
			  map : Translation.T;
  			  addr: MachineCPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("access", strand, ss, map, addr, type);
  END AccessViolation; 

PROCEDURE ProtectionFault(strand: Strand.T; 
			  VAR ss: MachineCPU.SavedState;
			  map : Translation.T;
  			  addr: MachineCPU.VirtAddress;
			  type: INTEGER;
			  VAR done : BOOLEAN) =
  BEGIN
    IF done THEN RETURN; END;
    DefaultAccessFaultHandler("protection", strand, ss, map, addr, type);
  END ProtectionFault;

PROCEDURE UnalignedAccess(strand: Strand.T; 
			  VAR ss: MachineCPU.SavedState;
			  <*UNUSED*>map : Translation.T;
  			  addr: MachineCPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nUnalignedAccess at 0x"); IO.Putx(addr);
    IO.Put(". No fixup support.\n");
    MachineCPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    MachineTrap.UnhandledUserSpaceException(strand, ss);
  END UnalignedAccess;

(*
 * Instruction traps
 *)
PROCEDURE Breakpoint(strand: Strand.T; 
                     VAR ss: MachineCPU.SavedState; 
  		     addr: MachineCPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nBreakpoint at 0x"); IO.Putx(addr);IO.Put(".\n");
    MachineCPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    Strand.Block(Strand.GetCurrent());
  END Breakpoint;    

PROCEDURE IllegalInstruction(strand: Strand.T; 
                             VAR ss: MachineCPU.SavedState;
  			     addr: MachineCPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nIllegalInstruction at 0x"); IO.Putx(addr); IO.Put(".\n");
    IO.Put("Instruction = 0x");
    IO.Putx(LOOPHOLE(addr, UNTRACED REF INTEGER)^); IO.Put("\n");

    MachineCPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread.");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    MachineTrap.UnhandledUserSpaceException(strand, ss);
  END IllegalInstruction; 

PROCEDURE FPDisabled(strand: Strand.T; 
                     VAR ss: MachineCPU.SavedState; 
  		     addr: MachineCPU.VirtAddress) =
  BEGIN
    IO.Put("\n\nFPDisabled at 0x"); IO.Putx(addr); IO.Put(".\n");
    MachineCPUPrivate.DumpState(ss);
    IO.Put("\n\tBlocking Thread ");
    IO.Putx(SafeConvert.RefAnyToWord(strand));
    IO.Put("\n");
    Log.Dumplog();
    MachineTrap.UnhandledUserSpaceException(strand, ss);
  END FPDisabled; 



  
(* Systemcall authorizer stuff *)

FUNCTIONAL PROCEDURE UntrustedImposedSyscallGuard (key : AuthKey;
				   strand: Strand.T;
				   VAR ms: MachineCPU.SavedState)
  	: BOOLEAN = 
  BEGIN
    IF strand = key.strand
      AND (ms.eax >= key.minProcID AND ms.eax <= key.maxProcID) THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END UntrustedImposedSyscallGuard;
  
FUNCTIONAL PROCEDURE TrustedImposedSyscallGuard (key : AuthKey;
				      <*UNUSED*>strand: Strand.T;
				      VAR ms: MachineCPU.SavedState)
  	: BOOLEAN = 
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
    newKey := NEW(MachineTrap.AuthKey);
  BEGIN
    IF NOT ISTYPE(k, MachineTrap.AuthKey) THEN
      IO.Put("syscall auth : key is not of type AuthKey\n");
      RETURN FALSE;
    END;
    newKey^ := NARROW(k, MachineTrap.AuthKey)^;

    TRY
      IO.Put("MachineTrap.syscall auth :");
      IF newKey.strand = NIL THEN
        (*
	IF Identity.IsPrivileged() THEN
	  IO.Put("PRIVILEGED SYSCALL\n");
	ELSE
	  IO.Put("MachineTrap.syscall auth insufficient privs\n");
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
      IO.Put("MachineTrap.Authorize : can't impose guard("
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
			     <*UNUSED*>VAR ss: MachineCPU.SavedState;
			     map : Translation.T;
  			     <*UNUSED*>addr: MachineCPU.VirtAddress;
			     <*UNUSED*>type: INTEGER;
			     <*UNUSED*>VAR done : BOOLEAN) : BOOLEAN =
  BEGIN
    RETURN key = map;
  END ImposedFaultGuard;
  
PROCEDURE AuthorizeFaultHandler (<*UNUSED*>a : Auth.T;
				 k : Auth.Key; binding : REFANY) : BOOLEAN =
  BEGIN
    IF NOT ISTYPE(k, MachineTrap.FaultHandlerAuthKey) THEN
      IO.Put("fault auth : key is of wrong type\n");
      RETURN FALSE;
    END;
    TRY
      EVAL Dispatcher.ImposeGuard(binding, ImposedFaultGuard, k);
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put("MachineTrap.Authorize : can't impose guard("
	     & Fmt.Int(ORD(ec)) & ").\n");
      RETURN FALSE;
    END;
    RETURN TRUE;
  END AuthorizeFaultHandler;

PROCEDURE Init(verbose: BOOLEAN) =
  VAR
    ok : BOOLEAN := TRUE;
  BEGIN
    TRY
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeSyscall),
                               MachineTrap.Syscall,
                               THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       MachineTrap.InvalidTranslation,
			       THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       MachineTrap.AccessViolation,
			       THIS_MODULE());
      Dispatcher.InstallAuthorizerForEvent(NEW(Auth.T,
			       authorize := AuthorizeFaultHandler),
			       MachineTrap.ProtectionFault,
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
END MachineTrap.
