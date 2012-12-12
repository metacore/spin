(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE SphinxMachineDep;
IMPORT Proc, ProcRep, CPU, Translation;
IMPORT Word, VirtAddr;
IMPORT VMError;
IMPORT MachineSigContext;
IMPORT Strand;
IMPORT IO;

(* Turn off bits below log2(wordsize) *)
PROCEDURE StackAlign(addr: Word.T): Word.T =
  BEGIN
    RETURN Word.And(addr, Word.Not(Word.Minus(BITSIZE(VirtAddr.Address),1)));
  END StackAlign;
  
(* sigtramp(signal, code, sigcontext_ptr, sighandler) *)
PROCEDURE SetupSignalContext (proc: Proc.T;
			      signo: INTEGER; args: Proc.SigArgs)
	  RAISES { VMError.E } =
  PROCEDURE Sub (VAR ss: CPU.SavedState) =
    PROCEDURE Sub2(VAR x: ARRAY OF CHAR) =
      BEGIN
	WITH c = VIEW(x, MachineSigContext.T) DO
	  c.mask := proc.sigMask;
	  c.pc := ss.pc;
	  
	  (* Save only caller saved regs. Other regs are saved in the
	     trampolin code *)
	  c.regs[0] := ss.v0;
	  c.regs[16] := ss.a0;
	  c.regs[17] := ss.a1;
	  c.regs[18] := ss.a2;
	  c.regs[19] := ss.a3;
	  c.regs[20] := ss.a4;
	  c.regs[21] := ss.a5;
	  c.regs[30] := ss.usp;
	  (* Save FP regs.
	     I assume two things. offsets 37 .. 70 hold the FP regs, and
	     the layout is the same as CPU.FloatRegs.
	     XXX this should be combined with used_fp traps, or otherwise
	     we would get terrible number. *)
	  IF FALSE THEN
	    CPU.GetUserFloatRegs(VIEW(SUBARRAY(VIEW(c, ARRAY OF INTEGER), 37, 33),
					     CPU.FloatRegs));
	  END;
	END;
      END Sub2;
      
    VAR
      sp: VirtAddr.Address;
    BEGIN
      (* Allocate space for saved state *)
      sp := Word.Minus(ss.usp, BYTESIZE(MachineSigContext.T));
      sp := StackAlign(sp);
      (* Align to stack boundaries *)

      Translation.Access(proc, sp, BYTESIZE(MachineSigContext.T), Sub2);
      
      ss.pc := args.tramp;
      ss.a0 := signo;
      ss.a1 := 0; (* We don't pass code *)
      ss.a2 := sp;
      ss.a3 := args.action.sa_handler;
      ss.usp := sp;
    END Sub;
  BEGIN
    Strand.AccessTrapFrame(proc.thread, Sub);
  END SetupSignalContext;
  

PROCEDURE GetpidReturn (VAR ss: CPU.SavedState; pid, ppid: INTEGER) =
  BEGIN
    ss.a3 := 0;
    ss.v0 := pid;
    ss.a4 := ppid;
  END GetpidReturn;
  
PROCEDURE ForkReturn (<*UNUSED*> parentPid: INTEGER;
		      childPid: INTEGER; 
		      VAR parentState: CPU.SavedState;
		      VAR childState: CPU.GeneralRegs) =
  BEGIN
    parentState.a4 := 0;
    parentState.a3 := 0;
    parentState.v0 := childPid;
    
    childState.a4 := 1;
    childState.a3 := 0;
    childState.v0 := 0;
  END ForkReturn;

PROCEDURE Syscall_Return(VAR ss: CPU.SavedState; ret: INTEGER) =
  BEGIN
    IO.PutError("Syscall_Return called on ALPHA. ???\n");
  END Syscall_Return;
		      
BEGIN
END SphinxMachineDep.
