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
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Enabled bypassing. 
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	SetInterruptLevel has been made an EXTERNAL procedure to give
 *      better profiling information.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 29-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Pass through functions are now bypassed using the dispatcher.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread and ThreadExtra.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Use Textify.
 *
 * 11-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. FPU state manipulation support.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Removed LOOPHOLE for Strand.T.
 *
 * 28-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Replaced OS.
 *
 * 12-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Replaces a bunch of LOOPHOLEs with calls to Clib and OS
 *
 * 30-Jul-95  Stefan Savage (savage) at the University of Washington
 *	Replaced Kthread.CurrentIdentity with Identity.GetCurrent
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Changed to use ConsolePrivate rather than console.  We need to 
 *	work even when nothing else does.
 *
 * 10-Dec-94  Emin Gun Sirer (egs) at the University of Washington
 *	Created. CPU state manipulation for untrusted clients.
 *
 *)
UNSAFE (* This module is unsafe because it imports an unsafe interface. *)
MODULE MachineCPU EXPORTS MachineCPU, MachineCPUPrivate;
IMPORT MachineCPUExtern, MachineCPU, MachineCPUPrivate, MachineTrapPrivate;
IMPORT ThreadPrivate;
IMPORT SalExtern, DebugOption, Log, MachineThread;
IMPORT Textify, Word, Fmt;
IMPORT DispatcherPrivate, Dispatcher, IO;
IMPORT ProfileSupport, ThreadExtern, ThreadRep;
IMPORT RTCollectorSRC;  (* FIXME *)

PROCEDURE FlushInstructionCache() =
  BEGIN	
    MachineCPUExtern.FlushInstructionCache();
  END FlushInstructionCache;

PROCEDURE FlushDataCache() =
  BEGIN	
    MachineCPUExtern.FlushDataCache();
  END FlushDataCache;

PROCEDURE FlushTLB() =
  BEGIN
    MachineCPUExtern.FlushTLB(-2);
  END FlushTLB;
  
PROCEDURE SetUserGeneralRegs (VAR ss: GeneralRegs) =
  BEGIN
    WITH hndlr = MachineTrapPrivate.syscallHandler DO
      IF hndlr = NIL THEN
        hndlr := ThreadPrivate.CreateTrapHndlr();
      END;
      ss.ksp := MachineThread.ReturnSP(hndlr);
      IF DebugOption.Cswtch THEN
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

      IF ProfileSupport.CompiledIn THEN 
        (* Raise SPL, so that we can set the profile stack variable
           without interruption. The RestoreUserGeneralRegs procedure
           should lower the SPL *)
        EVAL MachineCPUPrivate.SetInterruptMask(MachineCPUPrivate.InterruptClass.High);
        ThreadExtern.curProfileStack := hndlr.profileStack;
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

PROCEDURE InterruptDropPC(): INTEGER =
  BEGIN
    RETURN LOOPHOLE(MachineCPUExtern.SpinSwapIpl, INTEGER) + 4;
  END InterruptDropPC;

PROCEDURE InstallTrapHandler(s: TrapType; proc: PROCANY) =
  BEGIN
    MachineCPUExtern.ChangeSoftSCB(s, proc);
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
    RETURN VAL(Word.Extract(ss.ps, 0, 3), MachineCPU.InterruptLevel);
  END ExtractInterruptLevel;

PROCEDURE ExtractReturnAddress(READONLY ss: SavedState) : GeneralRegister =
  BEGIN
    RETURN ss.ra;
  END ExtractReturnAddress; 

PROCEDURE DumpState(READONLY ss: SavedState) =
  BEGIN
    IO.Put("\t(regs) = ");
    IO.Put("\n\tv0 = "); IO.Putx(ss.v0); 
    IO.Put("\n\tt0 = "); IO.Putx(ss.t0);
    IO.Put("\n\tt1 = "); IO.Putx(ss.t1);
    IO.Put("\n\tt2 = "); IO.Putx(ss.t2);
    IO.Put("\n\tt3 = "); IO.Putx(ss.t3);
    IO.Put("\n\tt4 = "); IO.Putx(ss.t4);
    IO.Put("\n\tt5 = "); IO.Putx(ss.t5);
    IO.Put("\n\tt6 = "); IO.Putx(ss.t6);
    IO.Put("\n\tt7 = "); IO.Putx(ss.t7);
    IO.Put("\n\ts0 = "); IO.Putx(ss.s0);
    IO.Put("\n\ts1 = "); IO.Putx(ss.s1);
    IO.Put("\n\ts2 = "); IO.Putx(ss.s2);
    IO.Put("\n\ts3 = "); IO.Putx(ss.s3);
    IO.Put("\n\ts4 = "); IO.Putx(ss.s4);
    IO.Put("\n\ts5 = "); IO.Putx(ss.s5);
    IO.Put("\n\ts6 = "); IO.Putx(ss.s6);
    IO.Put("\n\ta3 = "); IO.Putx(ss.a3);
    IO.Put("\n\ta4 = "); IO.Putx(ss.a4);
    IO.Put("\n\ta5 = "); IO.Putx(ss.a5);
    IO.Put("\n\tt8 = "); IO.Putx(ss.t8);
    IO.Put("\n\tt9 = "); IO.Putx(ss.t9);
    IO.Put("\n\tt10 = "); IO.Putx(ss.t10);
    IO.Put("\n\tt11 = "); IO.Putx(ss.t11);
    IO.Put("\n\tra = "); IO.Putx(ss.ra);
    IO.Put("\n\tpv = "); IO.Putx(ss.pv);
    IO.Put("\n\tat = "); IO.Putx(ss.at);
    IO.Put("\n\tksp = "); IO.Putx(ss.ksp);
    IO.Put("\n\tusp = "); IO.Putx(ss.usp);
    IO.Put("\n\tgp = "); IO.Putx(ss.gp);
    IO.Put("\n\tps = "); IO.Putx(ss.ps); 
    IO.Put("\n\tpc = "); IO.Putx(ss.pc);
    IO.Put("\n\ta0 = "); IO.Putx(ss.a0);
    IO.Put("\n\ta1 = "); IO.Putx(ss.a1);
    IO.Put("\n\ta2 = "); IO.Putx(ss.a2);
    IO.Put("\n");
  END DumpState;

PROCEDURE TextifyRegs(READONLY ss: SavedState) : TEXT =
  VAR t: TEXT;
  BEGIN
    t :=  "v0=" & Fmt.Unsigned(ss.v0) & "\n" &
          "t0=" & Fmt.Unsigned(ss.t0) & "\n" & 
	  "t1=" & Fmt.Unsigned(ss.t1) & "\n" & 
	  "t2=" & Fmt.Unsigned(ss.t2) & "\n" & 
	  "t3=" & Fmt.Unsigned(ss.t3) & "\n" & 
	  "t4=" & Fmt.Unsigned(ss.t4) & "\n" & 
	  "t5=" & Fmt.Unsigned(ss.pv) & "\n" & 
	  "t6=" & Fmt.Unsigned(ss.t6) & "\n" & 
	  "t7=" & Fmt.Unsigned(ss.t7) & "\n" & 
	  "s0=" & Fmt.Unsigned(ss.s0) & "\n" & 
	  "s1=" & Fmt.Unsigned(ss.s1) & "\n" & 
	  "s2=" & Fmt.Unsigned(ss.s2) & "\n" & 
	  "s3=" & Fmt.Unsigned(ss.s3) & "\n" & 
	  "s4=" & Fmt.Unsigned(ss.s4) & "\n" & 
	  "s5=" & Fmt.Unsigned(ss.s5) & "\n" & 
	  "s6=" & Fmt.Unsigned(ss.s6) & "\n" & 
	  "a3=" & Fmt.Unsigned(ss.a3) & "\n" & 
	  "a4=" & Fmt.Unsigned(ss.a4) & "\n" & 
	  "a5=" & Fmt.Unsigned(ss.a5) & "\n" & 
	  "t8=" & Fmt.Unsigned(ss.t8) & "\n" & 
	  "t9=" & Fmt.Unsigned(ss.t9) & "\n" & 
	  "t10=" & Fmt.Unsigned(ss.t10) & "\n" & 
	  "t11=" & Fmt.Unsigned(ss.t11) & "\n" & 
	  "ra=" & Fmt.Unsigned(ss.ra) & "\n" & 
	  "pv=" & Fmt.Unsigned(ss.pv) & "\n" & 
	  "at=" & Fmt.Unsigned(ss.at) & "\n" & 
	  "ksp=" & Fmt.Unsigned(ss.ksp) & "\n" & 
          "usp=" & Fmt.Unsigned(ss.usp) & "\n" & 
	  "gp=" & Fmt.Unsigned(ss.gp) & "\n" & 
	  "ps =" & Fmt.Unsigned(ss.ps ) & "\n" & 
	  "pc=" & Fmt.Unsigned(ss.pc) & "\n" & 
	  "a0=" & Fmt.Unsigned(ss.a0) & "\n" & 
	  "a1=" & Fmt.Unsigned(ss.a1) & "\n" & 
	  "a2=" & Fmt.Unsigned(ss.a2) & "\n";   
    RETURN t;
  END TextifyRegs;

PROCEDURE CopyCalleeSavedRegs(from: UNTRACED REF CalleeSavedRegs; 
                                to: UNTRACED REF GeneralRegs) =
  BEGIN
    to.pc := from.ra;
    to.s0 := from.s0;
    to.s1 := from.s1;
    to.s2 := from.s2;
    to.s3 := from.s3;
    to.s4 := from.s4;
    to.s5 := from.s5;
    to.s6 := from.s6;
    to.ksp := from.sp;
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
    RETURN MachineCPUExtern.getsp();
  END CurrentStackPointer;

PROCEDURE RegisterClean (start: ADDRESS; size: INTEGER) =
  BEGIN
    RTCollectorSRC.RegisterClean(start, size, FALSE);
  END RegisterClean;

PROCEDURE UnregisterClean (start: ADDRESS) =
  BEGIN
    RTCollectorSRC.UnregisterClean(start);
  END UnregisterClean;

BEGIN
END MachineCPU.
