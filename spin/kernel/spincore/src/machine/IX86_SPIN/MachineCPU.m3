(*
 * Copyright 1994, 1995, 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Added cycle counter procs
 *
 * 27-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added ExtractReturnAddress.
 *
 * 27-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Got rid of identity.
 *
 * 27-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fixed TextifyRegs to return value
 *
 * 16-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Created from Alpha version.
 *)
UNSAFE (* This module is unsafe because it imports an unsafe interface. *)
MODULE MachineCPU EXPORTS MachineCPU, MachineCPUPrivate;
IMPORT MachineCPUExtern, MachineCPU, MachineCPUPrivate; 
IMPORT MachineTrapPrivate, MachineThread;
IMPORT ThreadPrivate, SalExtern;
IMPORT DebugOption, Log;
IMPORT Textify, Fmt;
IMPORT Dispatcher, DispatcherPrivate, IO, Word;

PROCEDURE FlushInstructionCache() =
  BEGIN	
    MachineCPUExtern.FlushInstructionCache();
  END FlushInstructionCache;

PROCEDURE FlushDataCache() =
  BEGIN	
    MachineCPUExtern.FlushDataCache();
  END FlushDataCache;

PROCEDURE FlushTLB () =
  BEGIN
    IO.Put("x86 flushtlb is nop now.\n");
  END FlushTLB;
  
PROCEDURE SetUserGeneralRegs (VAR ss: GeneralRegs) =
  BEGIN
    WITH hndlr = MachineTrapPrivate.syscallHandler DO
      IF hndlr = NIL THEN
        hndlr := ThreadPrivate.CreateTrapHndlr();
      END;
      ss.ksp := MachineThread.ReturnSP(hndlr);
      IF TRUE OR DebugOption.Cswtch THEN
        Log.Log("Jumping to user level at pc=");
        Log.Logx(ss.pc);
        Log.Log(" usp=");
        Log.Logx(ss.usp);
        Log.Log(" ksp=");
        Log.Logx(ss.ksp);
        Log.Log(" hndlr=");
        Log.LogRef(hndlr);
        Log.Log("\n");
      END;
      MachineCPUExtern.RestoreUserGeneralRegs(ss);
    END;
  END SetUserGeneralRegs ;

PROCEDURE DumpGeneralRegs(READONLY ss: GeneralRegs) =
  BEGIN
    IO.Put( Textify.Regs(ss));
  END DumpGeneralRegs;

PROCEDURE SetUserFloatRegs(READONLY s: FloatRegs) =
  BEGIN
    MachineCPUExtern.SetUserFloatRegs(s);
  END SetUserFloatRegs;

PROCEDURE GetUserFloatRegs(VAR s: FloatRegs) =
  BEGIN
    MachineCPUExtern.GetUserFloatRegs(s);
  END GetUserFloatRegs;

PROCEDURE EnableUserFloatOps(on: BOOLEAN) =
  BEGIN
    MachineCPUExtern.EnableUserFloatOps(on);
  END EnableUserFloatOps;

PROCEDURE SaveAllGeneralRegs(VAR gp: GeneralRegs): BOOLEAN =
  BEGIN
    RETURN MachineCPUExtern.SaveAllGeneralRegs(gp);
  END SaveAllGeneralRegs;

PROCEDURE SaveCalleeSaveGeneralRegs(gp: UNTRACED REF GeneralRegs): BOOLEAN =
  BEGIN
    (*
     * XXX if you are going through this code, you are dead.
     * this routine should be bypassed by the dispatcher and
     * take the caller directly to the assembler implementation.
     * Having a wrapper around setjmp/longjmp does not work.
     * This code is here only to indicate what ought to happen
     * under the covers.
     *)
    RETURN MachineCPUExtern.SaveCalleeSaveGeneralRegs(gp);
  END SaveCalleeSaveGeneralRegs;

PROCEDURE RestoreCalleeSaveGeneralRegs(gp: UNTRACED REF GeneralRegs) =
  BEGIN
    MachineCPUExtern.RestoreCalleeSaveGeneralRegs(gp);
  END RestoreCalleeSaveGeneralRegs;

PROCEDURE RestoreUserGeneralRegs(READONLY gp: GeneralRegs) =
  BEGIN
    MachineCPUExtern.RestoreUserGeneralRegs(gp);
  END RestoreUserGeneralRegs;

(* PROCEDURE SetInterruptLevel(il: InterruptLevel): InterruptLevel =
  BEGIN
    RETURN MachineCPUExtern.SpinSwapIpl(il);
  END SetInterruptLevel;
*)

(* This procedure will be useful once we identify the PC value at
   which delayed clock ticks get delivered. *)
PROCEDURE InterruptDropPC(): INTEGER =
  BEGIN
    RETURN 0;
  END InterruptDropPC;

PROCEDURE InstallTrapHandler(<*UNUSED*>s: TrapType; <*UNUSED*>proc: PROCANY) =
  BEGIN
    (*XXX do something here*)
  END InstallTrapHandler;

PROCEDURE Breakpoint() =
  BEGIN
    SalExtern.Breakpoint();
  END Breakpoint;

PROCEDURE GetDebuggerRegs(VAR state: MachineState) =
  BEGIN
    MachineCPUExtern.GetDebuggerRegs(state);
  END GetDebuggerRegs;

PROCEDURE SetDebuggerRegs(READONLY state: MachineState) =
  BEGIN
    MachineCPUExtern.SetDebuggerRegs(state);
  END SetDebuggerRegs;

PROCEDURE ExtractInterruptLevel(READONLY ss: SavedState) : InterruptLevel =
  BEGIN
    RETURN ss.spl;
  END ExtractInterruptLevel;

PROCEDURE ExtractReturnAddress(READONLY ss: SavedState) : GeneralRegister =
  BEGIN
    (* this is called by Firewall so we do not raise an exception seeing
       how we are already trying to explain one *)
    IF Word.And(ss.ebp, 16_f0000000) # 16_f0000000 THEN
      IO.Put("\nNo RA Pointer: ebp = "); IO.Putx(ss.ebp);
      IO.Put("\n");
      RETURN 0;
    END;
    RETURN LOOPHOLE(ss.ebp+4, UNTRACED REF GeneralRegister)^;
  END ExtractReturnAddress; 

PROCEDURE DumpState(READONLY ss: SavedState) =
  BEGIN
    IO.Put("\nvec = "); IO.Putx(ss.vec); 
    IO.Put("\nspl = "); IO.Putx(ss.spl);
    IO.Put("\nes = "); IO.Putx(ss.es);
    IO.Put("\nds = "); IO.Putx(ss.ds);
    IO.Put("\nedi = "); IO.Putx(ss.edi);
    IO.Put("\nesi = "); IO.Putx(ss.esi);
    IO.Put("\nebp = "); IO.Putx(ss.ebp);
    IO.Put("\nksp = "); IO.Putx(ss.ksp);
    IO.Put("\nebx = "); IO.Putx(ss.ebx);
    IO.Put("\nedx = "); IO.Putx(ss.edx);
    IO.Put("\necx = "); IO.Putx(ss.ecx);
    IO.Put("\neax = "); IO.Putx(ss.eax);
    IO.Put("\ntrapno = "); IO.Putx(ss.trapno);
    IO.Put("\nerr = "); IO.Putx(ss.err);
    IO.Put("\npc = "); IO.Putx(ss.pc);
    IO.Put("\ncs = "); IO.Putx(ss.cs);
    IO.Put("\neflags = "); IO.Putx(ss.eflags);
    IO.Put("\nusp = "); IO.Putx(ss.usp);
    IO.Put("\nuss = "); IO.Putx(ss.uss);
    IO.Put("\n");
  END DumpState;


PROCEDURE TextifyRegs(READONLY ss: SavedState) : TEXT =
  VAR t: TEXT;
  BEGIN
    t :=  "vec=" & Fmt.Unsigned(ss.vec) & "\n" &
          "spl=" & Fmt.Unsigned(ss.spl) & "\n" & 
	  "es=" & Fmt.Unsigned(ss.es) & "\n" & 
	  "ds=" & Fmt.Unsigned(ss.ds) & "\n" & 
	  "edi=" & Fmt.Unsigned(ss.edi) & "\n" & 
	  "esi=" & Fmt.Unsigned(ss.esi) & "\n" & 
	  "ebp=" & Fmt.Unsigned(ss.ebp) & "\n" & 
	  "ksp=" & Fmt.Unsigned(ss.ksp) & "\n" & 
	  "ebx=" & Fmt.Unsigned(ss.ebx) & "\n" & 
	  "edx=" & Fmt.Unsigned(ss.edx) & "\n" & 
	  "ecx=" & Fmt.Unsigned(ss.ecx) & "\n" & 
	  "eax=" & Fmt.Unsigned(ss.eax) & "\n" & 
	  "trapno=" & Fmt.Unsigned(ss.trapno) & "\n" & 
	  "err=" & Fmt.Unsigned(ss.err) & "\n" & 
	  "cs=" & Fmt.Unsigned(ss.cs) & "\n" & 
	  "eflags=" & Fmt.Unsigned(ss.eflags) & "\n" & 
	  "usp=" & Fmt.Unsigned(ss.usp) & "\n" & 
	  "uss=" & Fmt.Unsigned(ss.uss) & "\n";
    RETURN t;
  END TextifyRegs;

PROCEDURE CopyCalleeSavedRegs(from: UNTRACED REF CalleeSavedRegs; 
                                to: UNTRACED REF GeneralRegs) =
  BEGIN
    to.ebx := from.ebx;
    to.ebp := from.ebp;
    to.edi := from.edi;
    to.esi := from.esi;
    to.ksp := from.sp;
    to.pc  := from.ra;
  END CopyCalleeSavedRegs;

(*
 * XXX Welcome to crudland. This stuff has to be reduced/eliminated. - egs.
 *) 
PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    IF verbose THEN IO.Put("\tMachineCPU:\n"); END;

    TRY
      (* FlushInstructionCache *)
      DispatcherPrivate.Bypass(MachineCPU.FlushInstructionCache, 
			       MachineCPUExtern.FlushInstructionCache);
      IF verbose THEN IO.Put("\t\tFlushInstructionCache\n"); END;

      (* FlushDataCache *)
      DispatcherPrivate.Bypass(MachineCPU.FlushDataCache,
			       MachineCPUExtern.FlushDataCache);
      IF verbose THEN IO.Put("\t\tFlushDataCache\n"); END;

      (* SetUserFloatRegs *)
      DispatcherPrivate.Bypass(MachineCPU.SetUserFloatRegs,
			       MachineCPUExtern.SetUserFloatRegs);
      IF verbose THEN IO.Put("\t\tSetUserFloatRegs\n"); END;

      (* GetUserFloatRegs *)
      DispatcherPrivate.Bypass(MachineCPU.GetUserFloatRegs,
			       MachineCPUExtern.GetUserFloatRegs);
      IF verbose THEN IO.Put("\t\tGetUserFloatRegs\n"); END;

      (* EnableUserFloatOps *)
      DispatcherPrivate.Bypass(MachineCPU.EnableUserFloatOps,
			       MachineCPUExtern.EnableUserFloatOps);
      IF verbose THEN IO.Put("\t\tEnableUserFloatOps\n"); END;

      (* SaveAllGeneralRegs *)
      DispatcherPrivate.Bypass(MachineCPUPrivate.SaveAllGeneralRegs,
			       MachineCPUExtern.SaveAllGeneralRegs);
      IF verbose THEN IO.Put("\t\tSaveAllGeneralRegs\n"); END;

      (* SaveGeneralRegs *)
      DispatcherPrivate.Bypass(MachineCPUPrivate.SaveCalleeSaveGeneralRegs, 
			       MachineCPUExtern.SaveCalleeSaveGeneralRegs);
      IF verbose THEN IO.Put("\t\tSaveGeneralRegs\n"); END;

      (* RestoreGeneralRegs *)
      DispatcherPrivate.Bypass(MachineCPUPrivate.RestoreCalleeSaveGeneralRegs, 
			       MachineCPUExtern.RestoreCalleeSaveGeneralRegs);
      IF verbose THEN IO.Put("\t\tRestoreGeneralRegs\n"); END;

      (* RestoreUserGeneralRegs *)
      DispatcherPrivate.Bypass(MachineCPUPrivate.RestoreUserGeneralRegs, 
			       MachineCPUExtern.RestoreUserGeneralRegs);
      IF verbose THEN IO.Put("\t\tRestoreUserGeneralRegs\n"); END;
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("Dispatcher error during initialization of MachineCPU\n");
    END;
  END Init;

PROCEDURE CycleCounter(): Word.T =
  BEGIN
    RETURN MachineCPUExtern.cyclecounter();
  END CycleCounter;

PROCEDURE Hertz(): Word.T =
  BEGIN
    RETURN MachineCPUExtern.hertz();
  END Hertz;

PROCEDURE CycleMinus(stop,start: Word.T): Word.T =
  BEGIN
    RETURN MachineCPUExtern.cycleminus(stop,start);
  END CycleMinus;

PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T =
  BEGIN
    RETURN MachineCPUExtern.cycle_to_microsec(cycles);
  END CycleToMicrosec;


PROCEDURE CurrentStackPointer(): GeneralRegister =
  BEGIN
    RETURN MachineCPUExtern.mvesp();
  END CurrentStackPointer;

BEGIN
END MachineCPU.
