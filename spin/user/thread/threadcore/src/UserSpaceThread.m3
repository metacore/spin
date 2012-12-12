(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *	added code to save&restore ESP0 for IX86.
 *
 * 27-Oct-97  Yasushi Saito (yasushi) at the University of Washington
 *	changed RunHandler implementation. Each thread has a boolean flag indicating
 *	whether it has been scheduled before or not. The thread is thrown
 *	into user space only in the first call to Strand.Run.
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Updated security management to reflect new manager
 *
 * 28-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed externref procs.
 *
 * 10-Mar-97  Robert Grimm (rgrimm) at the University of Washington
 *      Updated SecurityContext initialization to reflect
 *      new version which allocates out of a pool
 *      (made some fixes on 02-Apr-97)
 *
 * 06-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Set securitycontext on newly created threads.
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 22-Nov-96  becker at the University of Washington
 *	Added TrackStrand resource tracking
 *
 * 20-Oct-96  Emin Gun Sirer (egs) at the University of Washington
 *	SAL still does not send FPDisabled faults, so disabled
 *      on demand FPU support until SAL is fixed.
 *
 * 20-Oct-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added on-demand allocation and save/restore of floating point state.
 *
 * 19-Oct-96  Przemek Pardyak at the University of Washington
 *	Split the RunHandler into one that runs when the thread
 *	is not bound to a kernel thread (RunNotBoundHandler) and one that
 *	is run otherwise (RunBoundHandler).  RunBoundHandler must execute
 *	before the run handler is executed for the kernel thread to which
 *	the given user thread is bound.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Factored out FPU setup.
 *
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Changed Externalize and Internalize to use Spaces
 *
 * 02-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Alternate implementation of suspend and resume for ddion.
 *
 * 1-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *      Added exception handling for dispatcher calls. Corrected
 *      type errors in handler installation. 
 *
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Made strands partially opaque.
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Updated to use fastlists for internal queues.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Use console device interface.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. This file implements user-level threads. The interface
 *      to these threads is that of the Mach Thread interface that our 
 *      Unix server wants to see.
 *) 
UNSAFE MODULE UserSpaceThread;
(* UNSAFE because we are juggling with general cpu trap frame. *)
IMPORT Space, Strand, CPU, Dispatcher;
IMPORT DebugOption, Log, Debugger, Fmt;
IMPORT TrackStrand; <*NOWARN*>
IMPORT FastList;
IMPORT IO;
IMPORT Textify;
IMPORT StrandRep; (* XXX debugging *)
IMPORT Trap;
IMPORT UserSpaceThreadInterface, Auth;
IMPORT USTFPU;
IMPORT SecurityContext, SecurityManagerProtected;
IMPORT Translation;
IMPORT ThreadRep, ThreadPrivate;
IMPORT USTESP;

TYPE USThreadState = { Runnable, Suspended, Dead};

REVEAL T = Strand.T BRANDED OBJECT
  lock: MUTEX;
  utnext: T;
  fpuused : BOOLEAN := FALSE;
  fpustate: REF CPU.FloatRegs;
  tmpCPUState: UNTRACED REF CPU.GeneralRegs;
  thstate: USThreadState := USThreadState.Suspended;
  started: BOOLEAN := FALSE;
  blockedinrendezvous: BOOLEAN := FALSE; (* XXX debugging for ddion *)
END;

CONST
  MaxTCBs = 250;
  FPU = FALSE;
  
VAR
  freethreads: REF FastList.T;
PROCEDURE Message (th: T; msg: TEXT) =
  PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
    BEGIN
      Log.Log(msg & " for user strand " & Textify.Ref(th) &
	      " at pc=" & Fmt.Unsigned(cpustate.pc) &
	      "	 pri=" & Fmt.Unsigned(th.pri) & 
	      " sp=" & Fmt.Unsigned(cpustate.usp) & "\n");
    END Sub;
  BEGIN
    Strand.AccessTrapFrame(th, Sub);
  END Message;

FUNCTIONAL
PROCEDURE MyStrand(s: Strand.T): BOOLEAN =
  BEGIN
    RETURN TYPECODE(s) = TYPECODE(T);
  END MyStrand;    

FUNCTIONAL
PROCEDURE MyStrandTrap(strand: Strand.T;
		       <*UNUSED*>VAR ss: CPU.SavedState; 
		       <*UNUSED*>addr: CPU.VirtAddress): BOOLEAN =
  BEGIN
    RETURN TYPECODE(strand) = TYPECODE(T);
  END MyStrandTrap; 

PROCEDURE GetT() : T =
  VAR th: T;
  BEGIN
    LOOP
      th := FastList.Dequeue(freethreads);
      IF th = NIL THEN
        FOR i := 1 TO MaxTCBs DO
          th := NEW(T);
          th.lock := NEW(MUTEX);
          th.pri := Strand.defPriority;
          (* XXX enable FPU state by default, since SAL does not *)
          (* XXX reflect instruction faults. *)
          th.fpustate := NEW(REF CPU.FloatRegs);
	  th.fpuused := TRUE;
          FastList.Enqueue(th, freethreads);
        END;
      ELSE
        IF th.fpustate # NIL THEN
          USTFPU.Setup(th.fpustate);
        END;
        RETURN th;
      END;
    END;
  END GetT;

  (* Create a new UserSpacethread, with space as its address map *)
  (* The returned thread is not runnable. *)
PROCEDURE Create (space: Space.T): T =
  VAR
    newt: T;
  BEGIN
    newt := GetT();
    newt.translation := space;
    
    newt.bound_to_kernel := ThreadPrivate.CreateTrapHndlr();
    newt.bound_to_kernel.bound_to_user := newt;
    newt.bound_to_kernel.translation := space;
    
    IF DebugOption.DoTrackStrand THEN
      TrackStrand.Create(newt);
      TrackStrand.SetName(newt.track,"ust_"&Fmt.Int(newt.tid));
    ELSE
      newt.track := TrackStrand.untracked;
    END;

    (* XXX unsafe construct. need to move the thing to spincore. *)
    <*ASSERT newt.tmpCPUState = NIL*>
    newt.tmpCPUState := NEW(UNTRACED REF CPU.GeneralRegs);
    newt.trapFrame := newt.tmpCPUState;

    (* Inherit parent's security info *)
    SecurityManagerProtected.InheritContext(newt);

    RETURN newt;
  END Create;
  
PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
  BEGIN
    CPU.SetUserGeneralRegs(cpustate);
  END Sub;

PROCEDURE Run (s: Strand.T) =
  PROCEDURE GetESP(VAR cpustate: CPU.SavedState) =
    BEGIN
      ksp := cpustate.ksp;
    END GetESP;
  VAR th : T;
      ksp : CPU.GeneralRegister := 0;
  BEGIN
    th := NARROW(s, T);

    IF DebugOption.Cswtch THEN
      Message(s, "Restoring registers");
      IF th.fpuused THEN
        Log.Log("run strand: " & Textify.Ref(s) & "fpuused\n");
      ELSE
        Log.Log("run strand: " & Textify.Ref(s) & "fpu not used\n");
      END;
    END;
    Translation.Activate(th.translation);
    
    IF NOT th.started THEN
      th.started := TRUE;
      (* Hand off security info from user to kernel thread *)
      SecurityContext.HandOffContext(th, th.bound_to_kernel);
      Strand.AccessTrapFrame(th, Sub);
      (* notreached *)
      IO.Put("UserSpaceThread.????\n");
    END;

    IF FPU AND th.fpuused THEN
      CPU.SetUserFloatRegs(th.fpustate^);
    END;

    IF USTESP.UseESP THEN			(* restore ksp *)
      Strand.AccessTrapFrame(th, GetESP);
      CPU.RestoreESP(ksp);
    END;

  END Run; 

PROCEDURE StopHandler (s: Strand.T): BOOLEAN =
  PROCEDURE SetESP(VAR cpustate: CPU.SavedState) =
    BEGIN
      cpustate.ksp := ksp;
    END SetESP;

  VAR th: T;
      ksp: CPU.GeneralRegister := 0;
  BEGIN
    th := NARROW(s, T);

    IF DebugOption.Cswtch THEN
      Message(s, "Stopping ust");
      IF th.fpuused THEN
        Log.Log("stop strand: " & Textify.Ref(s) & "fpuused\n");
      ELSE
        Log.Log("stop strand: " & Textify.Ref(s) & "fpu not used\n");
      END;
    END;
    IF FPU AND th.fpuused THEN
      CPU.GetUserFloatRegs(th.fpustate^);
    ELSE
      CPU.EnableUserFloatOps(FALSE);
    END;

    IF USTESP.UseESP THEN			(* save ksp *)
      CPU.SaveESP(ksp);
      Strand.AccessTrapFrame(th, SetESP);
    END;

    RETURN TRUE;
  END StopHandler;

  (* Destroy the named thread after stopping it safely *)
PROCEDURE Destroy (th: T) =
  BEGIN
    IF DebugOption.DoStrandDebug THEN Debugger.DeregisterStrand(th); END;
    IF DebugOption.DoTrackStrand THEN TrackStrand.Delete(th.track);  END;
    <*ASSERT th.tmpCPUState # NIL*>

    DISPOSE(th.tmpCPUState);
    th.tmpCPUState := NIL;
  END Destroy;

  (* Stop the thread. [GS]etState will only be coherent *)
  (* with respect to the process after a call to Suspend returns *)
PROCEDURE Suspend(Uthread: T) =
  BEGIN
    (* XXX old version of code is just Strand.Block(Uthread); *)
    (* XXX begin debugging version for ddion *)
    IF Uthread.bound_to_kernel = Strand.GetCurrent() THEN
      Uthread.blockedinrendezvous := TRUE;
      Strand.Block(Strand.GetCurrent());
    ELSE
      Strand.Block(Uthread);
    END;
    (* XXX end debugging version for ddion *)
  END Suspend; 

  (* Make the process runnable. *)
PROCEDURE Resume(Uthread: T) =
  BEGIN
    IF DebugOption.DoStrandDebug THEN 
       Debugger.RegisterStrand(Uthread, Uthread.translation); 
    END;
    (* XXX begin debugging version for ddion *)
    IF Uthread.blockedinrendezvous THEN
      Uthread.blockedinrendezvous := FALSE;
      Strand.Unblock(Uthread.bound_to_kernel);
    ELSE
      Strand.Unblock(Uthread);
    END;
    (* XXX end debugging version for ddion *)
  END Resume;

  (* Set execution state *)
PROCEDURE SetState(Uthread: T; state: State) =
  PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
    BEGIN
      cpustate := state.cpustate^;
    END Sub;
  BEGIN
    LOCK Uthread.lock DO
      IF state.fpustate # NIL THEN
        IF Uthread.fpustate = NIL THEN
          Uthread.fpustate := NEW(REF CPU.FloatRegs);
          Uthread.fpuused := TRUE;
        END;
        Uthread.fpustate^ := state.fpustate^;
      END;
      Strand.AccessTrapFrame(Uthread, Sub);
    END;
  END SetState;

  (* Get execution state *)
PROCEDURE GetState(Uthread: T; VAR state: State) =
  PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
    BEGIN
      state.cpustate^ := cpustate;
    END Sub;
  BEGIN
    LOCK Uthread.lock DO
      IF state.fpustate # NIL THEN
        state.fpustate^ := Uthread.fpustate^;
      END;
      Strand.AccessTrapFrame(Uthread, Sub);
    END;
  END GetState;

PROCEDURE DisableFloatingPointState(Uthread: T) =
  BEGIN
    LOCK Uthread.lock DO
      Uthread.fpuused := FALSE;
    END;
  END DisableFloatingPointState;

PROCEDURE EnableFloatingPointState(Uthread: T) =
  BEGIN
    LOCK Uthread.lock DO
      IF Uthread.fpustate = NIL THEN
        Uthread.fpustate := NEW(REF CPU.FloatRegs);
      END;
      Uthread.fpuused := TRUE;
    END;
  END EnableFloatingPointState;

PROCEDURE FPDisabledHandler(strand: Strand.T;
                  <*UNUSED*>VAR ss: CPU.SavedState; 
                  <*UNUSED*>  addr: CPU.VirtAddress) =
  BEGIN
    EnableFloatingPointState(NARROW(strand, T));
    CPU.EnableUserFloatOps(TRUE);
  END FPDisabledHandler;

PROCEDURE GetSpace(Uthread: T) : Space.T =
  BEGIN
    RETURN Uthread.translation;
  END GetSpace;

PROCEDURE Self (): T =
  BEGIN
    RETURN NARROW(Strand.GetCurrentUserStrand(), T);
  END Self;

BEGIN
  EVAL UserSpaceThreadInterface.Export(NEW(Auth.AuthAlways));
  freethreads := NEW(REF FastList.T);
  (*XXX Take out when SAL reflects instruction faults to spin *)
  CPU.EnableUserFloatOps(TRUE);

  TRY
    EVAL Dispatcher.InstallHandler(Strand.Run, MyStrand, Run,
		   options := Dispatcher.Options {Dispatcher.Opt.First});
    EVAL Dispatcher.InstallHandler(Strand.Stop, MyStrand, StopHandler);
    EVAL Dispatcher.InstallHandler(Trap.FPDisabled,
				   MyStrandTrap, FPDisabledHandler)
  EXCEPT
  ELSE
    IO.Put("ERROR >> UserSpaceThread: handler installation failed\n");
  END;
  IO.Put("FPU is disabled for user space threads.\n");
END UserSpaceThread. 
