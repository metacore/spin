(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
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
 * 10-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Converted SMutex.Alloc to NEW(MUTEX).
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Cthreads for users, implemented on top of strand events.
 *) 
UNSAFE MODULE CThreadNative;
(* because it uses untraced ref to machinecpu.generalregs. blame yasushi *)
IMPORT CThreadNativeRep;
IMPORT Thread, UserSpaceThread, Space, VMError, Strand, StrandRep, CPU;
IMPORT FastList, Dispatcher;
IMPORT CodeRegions, Region;
IMPORT Log, DebugOption, Fmt, Word;
IMPORT IO, Textify;
IMPORT Translation;

CONST
  MaxTCBs = 10;
  DoMem = TRUE;
  DoRas = FALSE;
  DoCas = FALSE;

VAR
  freethreads: REF FastList.T;

FUNCTIONAL PROCEDURE MyStrand(s: Strand.T) : BOOLEAN =
  BEGIN
    RETURN ISTYPE(s, T);
  END MyStrand; 

PROCEDURE Message (th: T; msg: TEXT) =
  PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
    BEGIN
      Log.Log(msg & " for cthread " & Textify.Ref(th) &
	      " at pc=" & Fmt.Unsigned(cpustate.pc) &
	      "	 pri=" & Fmt.Unsigned(th.pri) & 
	      " sp=" & Fmt.Unsigned(cpustate.usp) & "\n");
    END Sub;
  BEGIN
    Strand.AccessTrapFrame(th, Sub);
  END Message;
  
PROCEDURE RunHandler(s: Strand.T) =
  VAR th : T;
  PROCEDURE Sub (VAR cpustate: CPU.SavedState) =
    BEGIN
      CPU.SetUserGeneralRegs(cpustate);
    END Sub;
  BEGIN
    th := NARROW(s, T);
    IF DoMem THEN
      TRY
        FOR i := FIRST(th.protectedregions) TO LAST(th.protectedregions) DO
	  Space.Protect(th.space, 
			th.protectedregions[i].begin,
			th.protectedregions[i].length,
			Space.ReadWriteProtection);
        END;
      EXCEPT
      | VMError.E(ec) => IO.Put("Space Protect failed (" & Fmt.Int(ec) & ".\n");
      END;
    END;
    Space.Activate(th.space);
    IF DebugOption.Cswtch THEN Message(s, "Resuming"); END;
    Strand.AccessTrapFrame(th, Sub);
  END RunHandler;
 
PROCEDURE StopHandler(s: Strand.T) : BOOLEAN =
  VAR th : T;
  BEGIN
    th := NARROW(s, T);
    IF DoRas THEN th.ras.rasEnsure(th.trapFrame.pc); END;
    IF DoCas THEN
      IF th.trapFrame.pc >= th.casRestartBegin AND 
         th.trapFrame.pc < th.casRestartEnd THEN
        th.trapFrame.pc := th.casRestartBegin;
      ELSIF th.trapFrame.pc >= th.casRestartEnd AND
            th.trapFrame.pc < th.casRestartEnd + th.casRollForwardLength THEN
        WITH val = VIEW(th.trapFrame.a2, ARRAY[0..7] OF CHAR) DO
	  Translation.Write(th.space, val, th.trapFrame.a0);
        END;
        WITH val = VIEW(th.trapFrame.a5, ARRAY[0..7] OF CHAR) DO
	  Translation.Write(th.space, val, th.trapFrame.a3);
        END;
      END;
    END;
    IF DoMem THEN
      TRY
        FOR i := FIRST(th.protectedregions) TO LAST(th.protectedregions) DO
          Space.Protect(th.space, 
                        th.protectedregions[i].begin,
                        th.protectedregions[i].length,
			(* XXX space doesnt have this prot anymore -DB
			Space.NoReadNoWriteNoExecProtection
			*)
                        Space.ReadWriteProtection
			);
        END;
      EXCEPT
      | VMError.E(ec) => IO.Put("Space Protect failed (" & Fmt.Int(ec) & ".\n");
      END;
    END;
    IF DebugOption.Cswtch THEN
      Message(s, "Stopping");
    END;
    (*
      IF th.fpuused THEN
      FPUState.Get(th.fpustate);
      END;
     *)                                     
    RETURN TRUE;
  END StopHandler;

(**********************************************************************) 

PROCEDURE GetT() : T =
  VAR th: T;
  BEGIN
    LOOP
      th := FastList.Dequeue(freethreads);
      IF th = NIL THEN
        FOR i := 1 TO MaxTCBs DO
          th := NEW(T);
          th.lock := NEW(MUTEX);
          th.ras := CodeRegions.New();
          th.done := NEW(Thread.Condition);
          th.pri := Strand.defPriority;
          FastList.Enqueue(th, freethreads);
        END;
      ELSE
        RETURN th;
      END;
    END;
  END GetT;

VAR 
  ProtBase : Word.T := 16_180000000;
CONST
  PageSize = 8192;

PROCEDURE Fork(th: UserSpaceThread.T; startpc,arg,gp,ra,startsp: Word.T) : T =
  VAR
    newt: T;
  BEGIN
    (*
     * We assume that the stack has already been allocated
     *)
    newt := GetT();
    newt.space := UserSpaceThread.GetSpace(th);
    IF DoMem THEN
      TRY
        FOR i := FIRST(newt.protectedregions) TO LAST(newt.protectedregions) DO
          newt.protectedregions[i].begin := ProtBase;
          newt.protectedregions[i].length := PageSize;
          INC(ProtBase, PageSize);
        END;
      EXCEPT
      | VMError.E(ec) => IO.Put("Cannot allocate space(" & Fmt.Int(ec) & ".\n");
        RETURN NIL;
      END;
    END;
    newt.uth := th;
    newt.tmpTrapFrame := NEW(UNTRACED REF CPU.GeneralRegs);
    newt.trapFrame := newt.tmpTrapFrame;
    newt.trapFrame.usp := startsp;
    newt.trapFrame.pc := startpc;
    newt.trapFrame.a0 := arg;
    newt.trapFrame.gp := gp;
    newt.trapFrame.ra := ra;
    newt.trapFrame.pv := newt.trapFrame.pc;
    Strand.Unblock(newt);
    RETURN newt;
  END Fork;

PROCEDURE Exit(cthread: T; exitcode: Word.T) =
  BEGIN
    LOCK cthread.lock DO
      cthread.returned := TRUE;
      cthread.result := exitcode;
      Thread.Broadcast(cthread.done);
      DISPOSE(cthread.tmpTrapFrame);
      cthread.trapFrame := NIL;
    END;
    Strand.Block(cthread);
  END Exit;

PROCEDURE Join(cthread: T) : Word.T =
  BEGIN
    LOCK cthread.lock DO
      IF cthread.returned = FALSE THEN
        Thread.Wait(cthread.lock, cthread.done);
      END;
      RETURN cthread.result;
    END;
  END Join;

PROCEDURE RegisterRas(cthread: T; beginpc, len: Word.T) =
  VAR region: Region.T;
  BEGIN
    (* 
       IO.Put("Cthread_strands registering ras region begin: 0x" &
              Fmt.Unsigned(beginpc) & 
              "len: 0x" & Fmt.Unsigned(len) & "\n");
     *)
    IF DoRas THEN
      region.begin := beginpc;
      region.length := len;
      cthread.ras.add(region);
    END;
  END RegisterRas;

PROCEDURE RegisterCas(cthread: T; beginpc, endpc, rollforwardlength: Word.T) =
  BEGIN
    (* 
       IO.Put("Cthread_strands registering cas region begin: 0x" &
              Fmt.Unsigned(beginpc) & 
              "len: 0x" & Fmt.Unsigned(len) & "\n");
     *)
    IF DoCas THEN
      cthread.casRestartBegin := beginpc;
      cthread.casRestartEnd := endpc;
      cthread.casRollForwardLength := rollforwardlength;
    END;
  END RegisterCas;

BEGIN
  freethreads := NEW(REF FastList.T);
  TRY
    EVAL Dispatcher.InstallHandler(Strand.Run, MyStrand, RunHandler);
    EVAL Dispatcher.InstallHandler(Strand.Stop, MyStrand, StopHandler);
    IO.Put("Native cthreads added to the kernel...\n");
  EXCEPT
  ELSE
    IO.Put("ERROR >> CThreadNative: handler installation failed\n");
  END;
END CThreadNative.
