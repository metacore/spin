(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 3-Mar-98  David Becker at the University of Washington
 *	Added Equal and Hash for StrandSema tables
 *
 * 08-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Made cpustate an untraced ref to stack.
 * 11-Dec-96  Charles Garrett (garrett) at the University of Washington
 *	Added a special profiling run handler.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Made architecture-independent by passing a reference to a structure of
 *	the callee-save registers to Strand.Stop rather than passing each of 
 *      the registers as an argument.  It's probably a de-optimization, but 
 *      it's necessary for portability.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread and ThreadExtra.
 *
 * 02-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Removed passing information about number of arguments 
 *	to the dispatcher.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Added exception handling for dispatcher calls.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use REF PROCEDURE instead of PROCEDURE
 *	added message at end of initialization
 *
 * 05-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Folded fast kernel-user thread binding into this file.
 *	Split Strand interface into Strand and StrandImpl.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added default implementation of RaiseException.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Default strand stop/run handlers.
 *)

UNSAFE MODULE Strand EXPORTS Strand, StrandRep, StrandPrivate;
(* UNSAFE because it manipulates trapFrame which is an untraced ref to
   CPU.SavedState. *)
IMPORT Strand, SpinException, ThreadPrivate, Dispatcher, DispatcherPrivate;
IMPORT CPU, ProfileSupport;
IMPORT RTIO;

PROCEDURE Stop(<*UNUSED*>s: T) : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END Stop;

PROCEDURE Run(<*UNUSED*>s: T) =
  BEGIN
  END Run;

PROCEDURE AccessTrapFrame(s: T;
		    callback: PROCEDURE(VAR ss: CPU.SavedState))=
  BEGIN
    IF s.trapFrame = NIL THEN
      RTIO.PutText("Strand.AccessTrapFrame called for non-ust.\n");
      RETURN;
    END;
    callback(s.trapFrame^);
  END AccessTrapFrame;

PROCEDURE RaiseException(<*UNUSED*>s: T; <*UNUSED*> reason: SpinException.ExceptionInfo) =
  BEGIN
    RTIO.PutText("Strand.RaiseException unimplemented\n");
  END RaiseException;

(* ---------------------- support for fast user to kernel thread binding --- *)
FUNCTIONAL PROCEDURE KernelHandlerThreadGuard(st: T) : BOOLEAN =
  BEGIN
    RETURN st.bound_to_kernel # NIL;
  END KernelHandlerThreadGuard;

PROCEDURE KernelHandlerThreadStop(s: T; 
                     regs: UNTRACED REF CPU.CalleeSavedRegs) : BOOLEAN =
  BEGIN
    RETURN ThreadPrivate.StopHandler(s.bound_to_kernel, regs);
  END KernelHandlerThreadStop; 

PROCEDURE KernelHandlerThreadRun(s: T) =
  BEGIN
    ThreadPrivate.RunHandler(s.bound_to_kernel);
  END KernelHandlerThreadRun; 

PROCEDURE KernelHandlerProfilingThreadRun(s: T) =
  BEGIN
    ThreadPrivate.ProfilingRunHandler(s.bound_to_kernel);
  END KernelHandlerProfilingThreadRun; 

PROCEDURE Equal(a, b: T) : BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE Hash(a: T) : INTEGER =
  BEGIN
    (* Kernel strands never move, so strongreffing them is unnecessary.
       We rely on kernel strands not being moved by the garbage collector.
     *)
    RETURN LOOPHOLE(a, INTEGER);
  END Hash;


PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    (* tell the dispatcher to pass callee-saved registers *)
    (* to handlers of Strand.Stop *)
    TRY
      DispatcherPrivate.SetSave(Strand.Stop);
    EXCEPT
    | Dispatcher.Error => 
      RTIO.PutText("Strand.Stop cannot be passed callee-saved registers\n");
      <* ASSERT FALSE *>
    END;

    (* install handlers for kernel thread binding *)
    TRY
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Strand.Run));
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(Strand.Stop));
      
      IF ProfileSupport.CompiledIn THEN
        EVAL Dispatcher.InstallHandler(Strand.Run, 
                                       KernelHandlerThreadGuard, 
                                       KernelHandlerProfilingThreadRun);
      ELSE
        EVAL Dispatcher.InstallHandler(Strand.Run, 
                                       KernelHandlerThreadGuard, 
                                       KernelHandlerThreadRun);
      END;
      EVAL Dispatcher.InstallHandler(Strand.Stop, 
                                     KernelHandlerThreadGuard, 
                                     KernelHandlerThreadStop);
    EXCEPT
    ELSE
      RTIO.PutText("Strand: handler installation failed\n");
    END;

    IF verbose THEN 
      RTIO.PutText("Strand initialized.\n");
    END;
  END Init;

BEGIN
END Strand.
