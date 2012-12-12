(*
 * Copyright 1994, 1995, 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 05-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *      Added Save/RestoreESP
 *
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
MODULE CPU EXPORTS CPU, CPUPrivate;
IMPORT CPUExtern, CPU, CPUPrivate; 
IMPORT MachineThread;
IMPORT SalExtern;
IMPORT DebugOption, Log;
IMPORT Textify, Fmt;
IMPORT Dispatcher, DispatcherPrivate, IO, Word;
IMPORT Thread, ThreadRep, Strand, StrandRep;
IMPORT Debugger;

PROCEDURE FlushInstructionCache() =
  BEGIN	
    CPUExtern.FlushInstructionCache();
  END FlushInstructionCache;

PROCEDURE FlushDataCache() =
  BEGIN	
    CPUExtern.FlushDataCache();
  END FlushDataCache;

PROCEDURE FlushTLB () =
  BEGIN
    IO.Put("x86 flushtlb is nop now.\n");
  END FlushTLB;
  
PROCEDURE SetUserGeneralRegs (VAR ss: GeneralRegs) =
  VAR hndlr: Thread.T;
  BEGIN
    WITH cur = Strand.GetCurrent() DO
      IF NOT ISTYPE(cur, Thread.T)
	OR cur.bound_to_user = NIL THEN
	IO.Put("CPU.SetUserGeneralRegs: foofoo\n");
	Debugger.Enter();
      END;
      hndlr := cur;
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
    CPUExtern.RestoreUserGeneralRegs(ss);
  END SetUserGeneralRegs ;

PROCEDURE DumpGeneralRegs(READONLY ss: GeneralRegs) =
  BEGIN
    IO.Put( Textify.Regs(ss));
  END DumpGeneralRegs;

PROCEDURE SetUserFloatRegs(READONLY s: FloatRegs) =
  BEGIN
    CPUExtern.SetUserFloatRegs(s);
  END SetUserFloatRegs;

PROCEDURE GetUserFloatRegs(VAR s: FloatRegs) =
  BEGIN
    CPUExtern.GetUserFloatRegs(s);
  END GetUserFloatRegs;

PROCEDURE EnableUserFloatOps(on: BOOLEAN) =
  BEGIN
    CPUExtern.EnableUserFloatOps(on);
  END EnableUserFloatOps;

PROCEDURE SaveAllGeneralRegs(VAR gp: GeneralRegs): BOOLEAN =
  BEGIN
    RETURN CPUExtern.SaveAllGeneralRegs(gp);
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
    RETURN CPUExtern.SaveCalleeSaveGeneralRegs(gp);
  END SaveCalleeSaveGeneralRegs;

PROCEDURE RestoreCalleeSaveGeneralRegs(gp: UNTRACED REF GeneralRegs) =
  BEGIN
    CPUExtern.RestoreCalleeSaveGeneralRegs(gp);
  END RestoreCalleeSaveGeneralRegs;

PROCEDURE RestoreUserGeneralRegs(READONLY gp: GeneralRegs) =
  BEGIN
    CPUExtern.RestoreUserGeneralRegs(gp);
  END RestoreUserGeneralRegs;

(* PROCEDURE SetInterruptLevel(il: InterruptLevel): InterruptLevel =
  BEGIN
    RETURN CPUExtern.SpinSwapIpl(il);
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
    CPUExtern.GetDebuggerRegs(state);
  END GetDebuggerRegs;

PROCEDURE SetDebuggerRegs(READONLY state: MachineState) =
  BEGIN
    CPUExtern.SetDebuggerRegs(state);
  END SetDebuggerRegs;

PROCEDURE ExtractInterruptLevel(READONLY ss: SavedState) : InterruptLevel =
  BEGIN
    RETURN ss.spl;
  END ExtractInterruptLevel;

PROCEDURE ExtractReturnAddress(READONLY ss: SavedState) : GeneralRegister =
  BEGIN
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
(*
 * XXX Welcome to crudland. This stuff has to be reduced/eliminated. - egs.
 *) 
PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    IF verbose THEN IO.Put("\tCPU:\n"); END;

    TRY
      DispatcherPrivate.Bypass(CPU.FlushInstructionCache, 
			       CPUExtern.FlushInstructionCache);
      IF verbose THEN IO.Put("\t\tFlushInstructionCache\n"); END;

      DispatcherPrivate.Bypass(CPU.FlushDataCache,
			       CPUExtern.FlushDataCache);
      IF verbose THEN IO.Put("\t\tFlushDataCache\n"); END;

      DispatcherPrivate.Bypass(CPU.SetUserFloatRegs,
			       CPUExtern.SetUserFloatRegs);
      IF verbose THEN IO.Put("\t\tSetUserFloatRegs\n"); END;

      DispatcherPrivate.Bypass(CPU.GetUserFloatRegs,
			       CPUExtern.GetUserFloatRegs);
      IF verbose THEN IO.Put("\t\tGetUserFloatRegs\n"); END;

      DispatcherPrivate.Bypass(CPU.EnableUserFloatOps,
			       CPUExtern.EnableUserFloatOps);
      IF verbose THEN IO.Put("\t\tEnableUserFloatOps\n"); END;

      DispatcherPrivate.Bypass(CPUPrivate.SaveAllGeneralRegs,
			       CPUExtern.SaveAllGeneralRegs);
      IF verbose THEN IO.Put("\t\tSaveAllGeneralRegs\n"); END;

      DispatcherPrivate.Bypass(CPUPrivate.SaveCalleeSaveGeneralRegs, 
			       CPUExtern.SaveCalleeSaveGeneralRegs);
      IF verbose THEN IO.Put("\t\tSaveGeneralRegs\n"); END;

      DispatcherPrivate.Bypass(CPUPrivate.RestoreCalleeSaveGeneralRegs, 
			       CPUExtern.RestoreCalleeSaveGeneralRegs);
      IF verbose THEN IO.Put("\t\tRestoreGeneralRegs\n"); END;

      DispatcherPrivate.Bypass(CPUPrivate.RestoreUserGeneralRegs, 
			       CPUExtern.RestoreUserGeneralRegs);
      IF verbose THEN IO.Put("\t\tRestoreUserGeneralRegs\n"); END;
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("CPU.Init: dispatcher error.\n");
    END;
  END Init;

PROCEDURE CycleCounter(): Word.T =
  BEGIN
    RETURN CPUExtern.cyclecounter();
  END CycleCounter;

PROCEDURE Hertz(): Word.T =
  BEGIN
    RETURN CPUExtern.hertz();
  END Hertz;

PROCEDURE CycleMinus(stop,start: Word.T): Word.T =
  BEGIN
    RETURN CPUExtern.cycleminus(stop,start);
  END CycleMinus;

PROCEDURE CycleToMicrosec(cycles: Word.T) : Word.T =
  BEGIN
    RETURN CPUExtern.cycle_to_microsec(cycles);
  END CycleToMicrosec;


PROCEDURE CurrentStackPointer(): GeneralRegister =
  BEGIN
    RETURN CPUExtern.mvesp();
  END CurrentStackPointer;

PROCEDURE SaveESP(VAR s: GeneralRegister) =
  BEGIN
    CPUExtern.SaveESP(s);
  END SaveESP;

PROCEDURE RestoreESP(READONLY s: GeneralRegister) =
  BEGIN
    CPUExtern.RestoreESP(s);
  END RestoreESP;

BEGIN
END CPU.
