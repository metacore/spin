(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)

MODULE MachineSig EXPORTS MachineSig, Sphinx;
IMPORT Proc, ProcRep, CPU, Translation;
IMPORT Word, VirtAddr;
IMPORT VMError;
IMPORT MachineSigContext;
IMPORT ExecX86;
IMPORT Strand;

(* Turn off bits below log2(wordsize) *)
PROCEDURE StackAlign(addr: Word.T): Word.T =
  BEGIN
    RETURN Word.And(addr, Word.Not(Word.Minus(BITSIZE(VirtAddr.Address),1)));
  END StackAlign;
  
(* sigtramp(signal, code, sigcontext_ptr, sighandler) *)
PROCEDURE SetupUserContext(proc : Proc.T;
			   signo : INTEGER; args : Proc.SigArgs)
	  RAISES { VMError.E } =
  VAR
    c : MachineSigContext.T;
    sp: VirtAddr.Address;
  PROCEDURE Sub (VAR ss: CPU.SavedState) =
    BEGIN
      (* Allocate space for saved state *)
      sp := Word.Minus(ss.usp, BYTESIZE(MachineSigContext.T));
      sp := StackAlign(sp);
      (* Align to stack boundaries *)

      c.mask := proc.sigMask;
      c.pc := ss.pc;
      
      c.signum := signo;
      c.code := 0; (* we don't pass code *)
      c.scp := sp + 20; (* XXX this is the presumed address of c.onstack. *)
      c.addr := ss.err;
      c.handler := args.action.sa_handler;

      c.sp := ss.usp;
      c.ebp := ss.ebp;
      c.isp := ss.ksp;
      c.efl := ss.eflags;
      c.es := ss.es;
      c.ds := ss.ds;
      c.cs := ss.cs;
      c.ss := ss.uss;
      c.edi := ss.edi;
      c.esi := ss.esi;
      c.ebx := ss.ebx;
      c.edx := ss.edx;
      c.ecx := ss.ecx;
      c.eax := ss.eax;
      
      Translation.Write(proc, VIEW(c, ARRAY OF CHAR), sp);
      
      ss.pc := ExecX86.UPCB;
      ss.cs := ExecX86.UserCodeSel;
      ss.usp := sp;
      ss.uss := ExecX86.UserDataSel;
      ss.ds := ExecX86.UserDataSel;
      ss.es := ExecX86.UserDataSel;
    END Sub;
  BEGIN
    Strand.AccessTrapFrame(proc.thread, Sub);
  END SetupUserContext;
  

PROCEDURE Sigreturn (VAR ss : CPU.SavedState;
		     READONLY c: MachineSigContext.T) =
  VAR
    proc := Proc.Self();
  BEGIN
    proc.sigMask := c.mask;
    ss.pc := c.pc;
    ss.usp := c.sp;
    ss.ebp := c.ebp;
    ss.eflags := c.efl;
    ss.es := c.es;
    ss.ds := c.ds;
    ss.cs := c.cs;
    ss.uss := c.ss;
    ss.edi := c.edi;
    ss.esi := c.esi;
    ss.ebx := c.ebx;
    ss.edx := c.edx;
    ss.ecx := c.ecx;
    ss.eax := c.eax;
  END Sigreturn;

BEGIN
END MachineSig.
