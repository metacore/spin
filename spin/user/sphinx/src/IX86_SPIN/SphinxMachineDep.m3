(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Dec-97  Tsutomu Owa (owa) at the University of Washington.
 *	Added SigFrame structure.
 *)
MODULE SphinxMachineDep EXPORTS SphinxMachineDep, Sphinx;
IMPORT Proc, ProcRep, CPU, Translation;
IMPORT Word, VirtAddr;
IMPORT VMError;
IMPORT MachineSigContext;
IMPORT ExecX86;
IMPORT Strand;
IMPORT Ctypes, IO, Fmt;
IMPORT Log, Debugger;

CONST DEBUG = FALSE;

PROCEDURE GetpidReturn (VAR ss : CPU.SavedState; pid, ppid : INTEGER) =
  BEGIN
    ss.eax := pid;
    ss.edx := ppid;
    ss.eflags := Word.And(ss.eflags, 16_fffffffe);
  END GetpidReturn;

(* Turn off bits below log2(wordsize) *)
PROCEDURE StackAlign(addr: Word.T): Word.T =
  BEGIN
    RETURN Word.And(addr, Word.Not(Word.Minus(BITSIZE(VirtAddr.Address),1)));
  END StackAlign;
  
(* sigframe structure defined in sys/i386/include/frame.h *)
TYPE SigFrame = RECORD
  signum : Ctypes.int;
  code : Ctypes.int;
  scp : Ctypes.int;     (* address of sc. *)
  addr : Ctypes.int;
  handler : Ctypes.int; (* actually a pointer to the handler function *)
  sc : MachineSigContext.T;
END;

(* sigtramp(signal, code, sigcontext_ptr, sighandler) *)
PROCEDURE SetupSignalContext(proc : Proc.T;
			   signo : INTEGER; args : Proc.SigArgs)
	  RAISES { VMError.E } =
  VAR
    c : SigFrame;
    sp: VirtAddr.Address;
  PROCEDURE Sub (VAR ss: CPU.SavedState) =
    BEGIN
      (* Allocate space for saved state *)
      sp := Word.Minus(ss.usp, BYTESIZE(SigFrame));
      sp := StackAlign(sp);
      (* Align to stack boundaries *)

      IF sp > 16_90000000 THEN
	Log.Dumplog();
	Debugger.Enter();
      END;

      c.sc.mask := proc.sigMask;
      c.sc.pc := ss.pc;
      
      c.signum := signo;
      c.code := 0;	(* we don't pass code *)
      c.scp := sp + 17; (* XXX this is the presumed address of c.onstack. *)
      c.addr := ss.err;
      c.handler := args.action.sa_handler;
      c.sc.onstack := 0;

      c.sc.sp := ss.usp;
      c.sc.ebp := ss.ebp;
      c.sc.isp := ss.ksp;
      c.sc.efl := ss.eflags;
      c.sc.es := ss.es;
      c.sc.ds := ss.ds;
      c.sc.cs := ss.cs;
      c.sc.ss := ss.uss;
      c.sc.edi := ss.edi;
      c.sc.esi := ss.esi;
      c.sc.ebx := ss.ebx;
      c.sc.edx := ss.edx;
      c.sc.ecx := ss.ecx;
      c.sc.eax := ss.eax;

      Translation.Write(proc, VIEW(c, ARRAY [1..BYTESIZE(c)] OF CHAR), sp);
      
      ss.pc := ExecX86.UPCB;
      ss.cs := ExecX86.UserCodeSel;
      ss.usp := sp;
      ss.uss := ExecX86.UserDataSel;
      ss.ds := ExecX86.UserDataSel;
      ss.es := ExecX86.UserDataSel;
    END Sub;
  BEGIN
    Strand.AccessTrapFrame(proc.thread, Sub);
  END SetupSignalContext;

PROCEDURE ForkReturn(
     parentPid       : INTEGER;
     childPid        : INTEGER; 
     VAR parentState : CPU.SavedState;
     VAR childState  : CPU.GeneralRegs) =
  BEGIN
    parentState.eax := childPid;
    parentState.edx := 0;
    parentState.eflags := Word.And(childState.eflags, 16_fffffffe);
    
    childState.eax := parentPid;
    childState.edx := 1;
    childState.eflags := Word.And(childState.eflags, 16_fffffffe);
  END ForkReturn;

PROCEDURE Syscall_Return(VAR ss: CPU.SavedState; ret: INTEGER) =
  BEGIN
    ss.eax := ret;
    ss.eflags := Word.And(ss.eflags, 16_fffffffe);
  END Syscall_Return;

BEGIN
END SphinxMachineDep.
