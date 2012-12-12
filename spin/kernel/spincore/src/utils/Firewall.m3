(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *	Remove OKToAllocAtSPL
 *
 * 27-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Use ExtractReturnAddress.
 *
 * 03-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *      Do not use concatenation if we died inside of the collector.
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed all references to priviledged execution. Replaced
 *      Identity with SecurityContext, though kept it commented out
 *
 * 02-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Print more info for a fault.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Print pc of the faults.
 *
 * 05-Sep-96  Przemek Pardyak (pardy) at the University of Washington
 *      Call the clean-up of the exception handling stack when a thread
 *      is being killed becuase of unhandled exception.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cleanup for machine independence.
 *
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 30-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added debugging printout to firewall to make sure checked
 *	runtime errors are properly sent to the right place.
 *	Made Failstop call debugger after count exceeds 200.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Check if strand isn't a thread (from becker)
 *
 * 27-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added extra args to Firewall.Fault so we can better debug 
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added UnalignedFault handler.  Generalized Fault handling support.
 *
 * 21-Feb-96 Brian Bershad (bershad) at the University of Washington
 *      Fixed Where to be more forgiving in the event that we don't know
 *	the source line for the fault.
 *
 * 17-Feb-96 Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of warnings.
 *
 * 26-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for size fault for VIEW
 *
 * 26-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Clean up to simplify. Grab RTHooks.ReportFault and
 *	 RTException.NoHandler and nothing more. FatalErrors still go
 *	 through Firewall.Crash indirectly via RTMisc.  Added file and
 *	 proc information for Protection faults.
 *	
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed the trashing that occurred when we passed an ExceptionInfo
 *	structure that was allocated on stack to RTException.Raise.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Use Textify interface.
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Added exception handling for dispatcher calls.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use REF PROCEDURE instead of PROCEDURE
 *
 * 24-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	We were improperly catching ALL exceptions inside
 *	CheckedRuntimeError, even on the call to RTException.Raise. This
 *	would terminate the flow of the exception upwards, returning
 *	control from CheckedRuntimeError.  Bad news.  Changed signatures
 *	to RAISE ANY and do no handling internally.  Everything passes
 *	thru.
 *
 * 01-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Map runtime into implicit exceptions.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Catch runtime exceptions and narrow faults.
 *
 * 27-Sep-95  Stefan Savage (savage) at the University of Washington
 *	Firewall is unsafe, changed to SAL interfaces
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

(* "FireWall" All runtime crashes come here.  

   This module is trusted and safely uses system unsafe modules:
     RTException -- because we need to know about exceptions and
                    their representation.

   We rely on the dispatcher to hijack events having to do with
   process and system failure. We route these events into cleaner
   handlers.

   We take over the RTMisc.Fatal* facilities from the runtime.  This
   module does several things. First, we map all checked runtime
   exceptions out of process failures and back into exceptions.
   CheckedRuntimeError raises a SpinException.CheckedRuntimeError
   which can be caught by the offending thread.

   Consequently, any and all procedures can RAISE a
   SpinException.CheckedRuntimeError.  To ensure this, we inform the
   m3 runtime of a set of implicit exceptions that, when raised,
   should not cause an runtime error if they are not included in a
   procedure's RAISES clause.

   If an exception is raised but not caught by the raising thread,
   then the runtime will notify us of an "Unhandled Exception."
   Clearly, we can't reraise in the context of the offending thread,
   so we stop the offending thread in an 'exceptional' state that
   reflects the unhandled exception.  NOTE: If the offending thread is
   a privileged Sal thread (TAZ), then we go through
   RTProcess.Crash, which will halt the system. *)


UNSAFE (* because we import unsafe interface *)
MODULE Firewall EXPORTS Firewall, SpinException;

IMPORT Dispatcher, RTOS, Thread, ThreadRep, IO, Strand, StrandRep,
       CPU, M3toC, RTProcess, Word, Fmt, Debugger,
       RTException, Ctypes, Sal, RT0, RTHooks,
       RTProcedureSRC, RTProcedure, RTIO, RT0u;

VAR nestedFaults: CARDINAL;	(* Hard crash if too many *)

VAR implicitExceptions : ARRAY [0..7] OF RTException.ExceptionName; 
		(* all implicit exceptions *)
    implicitSpinException: RTException.ExceptionName;
		(* the primary spin implicit exception *)
    discoveringException: CARDINAL;	
        	(* used during exception discovery *)

CONST Dbg = TRUE;


VAR
  crashMsg := ARRAY [0..14] OF
  CHAR {'F', 'i', 'r', 'e', 'w', 'a', 'l', 'l', ' ',
        'C', 'r', 'a', 's', 'h', '\n'};

(* Crash overrides *)
PROCEDURE Crash () =
  BEGIN
    FOR i := FIRST(crashMsg) TO LAST(crashMsg) DO
      Sal.PutChar(crashMsg[i]);     (* it doesn't get any 
       					     simpler than this *)
    END;
    Debugger.Enter();
  END Crash;


(* Override NoHandler service *)
PROCEDURE NoHandler (en: RTException.ExceptionName; 
                     arg: RTException.ExceptionArg; raises: BOOLEAN := TRUE) 
                     RAISES ANY =
  VAR ec: ExceptionCode;
  BEGIN
    IF raises THEN
      ec := ExceptionCode.NotInRaisesList;
    ELSE
      ec := ExceptionCode.UnhandledException;
    END;
    CheckedRuntimeError(ec, EName(en), en, arg);
  END NoHandler;


(* An unraisable exception is one that, if we raise it, will get us
  into an infinite loop. Instead, we should try to halt the offending
  thread.  *)


PROCEDURE PutError (t: TEXT) =
  BEGIN
    RTIO.PutText(t);
  END PutError;

PROCEDURE FailStop() =
  BEGIN
    nestedFaults := nestedFaults + 1;
    IF nestedFaults > 200 THEN
      (* Temporary double hulled firewall *)
      PutError("Checked runtime error tired of faulting...\n");
      Debugger.Enter();
    END;
  END FailStop;

PROCEDURE RaisableException (ec: ExceptionCode): BOOLEAN =
  BEGIN
    CASE ec OF
    | ExceptionCode.NotInRaisesList,
      ExceptionCode.UnhandledException,
      ExceptionCode.UnknownException =>
      RETURN FALSE;      
    ELSE
      RETURN TRUE;
    END;      
  END RaisableException;


(* Deal with a checked runtime error. Map into an exception and raise,
   Unless we've got ourselves an unhandled exception which is our own,
   in which case we punt to Strand or we crash (if privileged) *)

PROCEDURE CheckedRuntimeError (ec: ExceptionCode; msg: TEXT;
                               en: RTException.ExceptionName := NIL; 
                               arg: RTException.ExceptionArg := NIL) 
                               RAISES ANY =
  VAR
    s : Thread.T;
    ex: ExceptionInfo;
  BEGIN
    IF Dbg THEN
      IF RT0u.inCritical # 0 THEN
        RTIO.PutText("Raising ");
        RTIO.PutText(msg);
        RTIO.PutText("\n"); 
      ELSE
        IO.Put("Raising " & msg & "\n");
      END;
    END;

    (* Thread.Self() NARROW the the current strand to a Thread.T but
       the current strand may not be a Sal thread during interrupts *)
    IF NOT ISTYPE(Strand.GetCurrent(),Thread.T) THEN
      PutError("Firewall: Current strand not a kernel thread.\n");
      RTProcess.Crash(msg);
    END;

    s := Thread.Self();
    IF s = NIL THEN
      PutError("Firewall: Current strand NIL.\n");
      RTProcess.Crash(msg);
    END;

    ex.code := ec;
    IF RaisableException(ec) THEN
      ex.msg := msg;
      s.exception := ex;         (* save what's wrong in thread state *)
      RTException.Raise(implicitSpinException, ADR(s.exception));
    ELSE
      (* if we got here from NoHandler then make sure that the finally
         clauses are executed *)
      IF en # NIL THEN
        RTException.ResumeRaise(en, arg, FALSE);
      END;

      (* Runtime exception machinery unable to deal with the problem.
         Pass on to spin. *)
      IF s.exception.msg # NIL THEN
        (* we cannot concatene if we died inside of RT critical section
           pass on the new message *)
        IF RT0u.inCritical # 0 THEN
          ex.msg := msg; 
        ELSE
          ex.msg := msg & " for " & s.exception.msg;
        END;
      ELSE
        ex.msg := msg;
      END;
      Strand.RaiseException(s, ex);
    END;
  END CheckedRuntimeError;



PROCEDURE Where (pc: Word.T): TEXT =
  VAR
    p         : RTProcedure.Proc;
    file, name: RTProcedureSRC.Name;
    f, n      : TEXT;
    proc: RTProcedureSRC.ProcInfo; 
    unit: RTProcedureSRC.UnitInfo;
    ignore: ADDRESS;
  BEGIN
    RTProcedureSRC.FromPC(LOOPHOLE(pc, ADDRESS), p, ignore, proc, unit, 
                          unit, file, name);
    f := "SAL";
    n := "??";
    IF file # NIL THEN f := M3toC.CopyStoT(file); END;
    IF name # NIL THEN
      n := M3toC.CopyStoT(name);
    END;
    RETURN " in " & f & ":" & n;
  END Where;
    
    
PROCEDURE Fault (<*UNUSED *>s: Strand.T;
                 VAR ss : CPU.SavedState;
                 badaddr: Word.T;
                 arg1: Word.T;
                 arg2: Word.T;
                 ec: ExceptionCode) RAISES ANY =
  BEGIN
    (* we might have faulted inside of the collector, use only
       RTIO because it does not do any allocation *)
    IF debug THEN
      RTIO.PutText("SPIN [");
      RTIO.PutInt(nestedFaults);
      RTIO.PutText("] fault @ address -- "); RTIO.PutHex(badaddr);
      RTIO.PutText(", pc = "); RTIO.PutHex(ss.pc);
      RTIO.PutText(", ra = "); 
      RTIO.PutHex(CPU.ExtractReturnAddress(ss));
      RTIO.PutText(", arg1 = "); RTIO.PutHex(arg1);
      RTIO.PutText(", arg2 = "); RTIO.PutHex(arg2);
      RTIO.PutText("\n");
      FailStop();
    END;

    VAR 
      msg := ExceptionNames[ec];
    BEGIN
      IF RT0u.inCritical = 0 THEN
        msg := msg & Where(ss.pc);
      END;
      CheckedRuntimeError(ec, msg);
    END;

    (* TextifyRegs(...) *)
    RTIO.PutText(
        "ERROR >> CheckedRuntimeError should not return to Firewall!\n");
    RTOS.Crash();
  END Fault; 


(* report the runtime fault in the specified module.  "info" encodes
   the source line number and fault code [info = line*16 + code].
   Where the fault codes are:
     0 - assertion failure
     1 - value out of range
     2 - subscript out of range
     3 - incompatible array shape
     4 - attempt to dereference NIL
     5 - NARROW failure
     6 - missing RETURN in function
     7 - missing CASE arm
     8 - missing TYPECASE arm
     9 - stack overflow
    10 - VIEW size fault
    11 - VIEW alignment fault
*)

PROCEDURE ReportFault (module: ADDRESS (*RT0.ModulePtr*); info: INTEGER) 
                       RAISES ANY =
  VAR
    line: INTEGER       := Word.RightShift(info, 4);
    code: INTEGER       := Word.And(info, 16_f);
    mi  : RT0.ModulePtr := module;
    msg : TEXT          := "ReportFault -- bad error code!";
  BEGIN
    IF (0 <= code) AND (code <= ORD(LastFault) - ORD(FirstFault)) THEN
      (* first Few codes reserved for VM related faults *)
      msg := ExceptionNames[
               VAL(code + ORD(FirstFault), ExceptionCode)];
    END;
    IF RT0u.inCritical = 0 THEN
      IF (mi # NIL) THEN msg := msg & " in " & RefCharToT(mi.file); END;
      msg := msg & " at line " & Fmt.Int(line);
    ELSE
      IF Dbg THEN
        RTIO.PutText("Fault ");
        IF mi # NIL THEN
          RTIO.PutText("in "); RTIO.PutString(mi.file);
        END;
        RTIO.PutText(" at line "); RTIO.PutInt(line);
        RTIO.PutText("\n"); 
      END;
    END;
    CheckedRuntimeError(VAL(code + ORD(FirstFault), ExceptionCode), msg);
  END ReportFault;

PROCEDURE RefCharToT(rc: UNTRACED REF CHAR): TEXT =
  BEGIN
    RETURN M3toC.CopyStoT(LOOPHOLE(rc, UNTRACED REF [-128..127]));
  END RefCharToT; 

(* Install firewall facilities into runtime *)
     
PROCEDURE Override (p: PROCANY; q: PROCANY; t: TEXT): Dispatcher.Binding =
  VAR o: Dispatcher.Binding;
  VAR n: Dispatcher.Binding;
  BEGIN
    TRY
      o := Dispatcher.GetOriginalHandler(p);
      IF o = NIL THEN RETURN NIL; END;
      Dispatcher.Uninstall(o);
      n := Dispatcher.InstallHandler(p, NIL, q);
      IF n = NIL THEN
        IO.Put("Override: (" & t & ") Can't override existing procedure\n");
        IF Dispatcher.InstallHandler(p, NIL, p) = NIL THEN
          PutError("Whoa, can't even rewrite default\nFirewall crash.\n");
        END;
      END;
    EXCEPT
    ELSE
      IO.Put("ERROR >> Firewall: override failed\n");
      RETURN NIL;
    END;

    RETURN n;
  END Override;


(* Temporarily override the runtime exception service. Install our own
   handler, and then raise an exception. This allows us to discover
   the runtime exception name for a particular exception.  We then
   install this as an an implicit exception which we communicate to
   the exception raise services at runtime.  *)

PROCEDURE MyRaise (en : RTException.ExceptionName;
                   <*UNUSED*>arg: RTException.ExceptionArg) (*RAISES ANY*) =
  BEGIN
    implicitExceptions[discoveringException] := en;
    (*
    IO.Put("Discovered exception " & EName(en) & "\n");
    *)

    INC(discoveringException);
  END MyRaise;



PROCEDURE DiscoverImplicitExceptions () =

  VAR
    oRaise: Dispatcher.Binding;
    ex    : ExceptionInfo;
  BEGIN
    discoveringException := 0;
    oRaise := Override(RTException.Raise, MyRaise, "Raise");

    TRY
      RAISE Exception(ex);
    EXCEPT
      Exception =>               (* Fool the compiler *)
    END;
    implicitSpinException := implicitExceptions[0];

    TRY
      Dispatcher.Uninstall(oRaise);
      Dispatcher.Install(Dispatcher.GetOriginalHandler(RTException.Raise));
    EXCEPT
    ELSE
      IO.Put("ERROR >> Firewall: dispatcher call failed\n");
    END;

    FOR i := 0 TO discoveringException - 1 DO
      RTException.NoteImplicitException(implicitExceptions[i], TRUE);
    END;
  END DiscoverImplicitExceptions;

(*
 * Some prettifiers
 *)

PROCEDURE EName (en: RTException.ExceptionName): TEXT =
  BEGIN
    (* should this be "copy" *)
    RETURN M3toC.StoT (LOOPHOLE (en^, Ctypes.char_star));
  END EName;

PROCEDURE Init (verbose: BOOLEAN) =
  BEGIN
    debug := TRUE;

    DiscoverImplicitExceptions();

    EVAL Override(RTHooks.ReportFault, ReportFault, "ReportFault");
    EVAL Override(RTOS.Crash, Crash, "Crash");
    EVAL Override(RTException.NoHandler, NoHandler, "NoHandler");

    IF verbose THEN IO.Put("Firewall initialized\n"); END;
  END Init;

BEGIN
END Firewall.
