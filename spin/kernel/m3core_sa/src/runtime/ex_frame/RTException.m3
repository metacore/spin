(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 26 13:47:05 PST 1995 by kalsow     *)
(*      modified on Wed Jun  2 15:21:34 PDT 1993 by muller     *)
(*							       *)
(*							       *)

(* HISTORY						       
 * 5-Sep-96  Przemek Pardyak (pardy) at the University of Washington
 *	Change ResumeRaise to be able to clean-up the exception stack
 *	(execute the TRY-FINALLY handlers) even if no exception handler
 *	will be executed.  Original code crashes the runtime if no handler
 *	is present.  Our code only kills the thread, but the accumulated
 *	TRY-FINALLY handlers should be executed.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Ensure we don't try to dereference ex if it is NIL.
 *
 * 26-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Explicitly import RTHooks so that our methods can be overridden.
 *
 * 01-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Added implicit exception support.
 *)


UNSAFE MODULE RTException EXPORTS RTException, RTExRep;

IMPORT RT0, RTMisc, RTIO, RTParams, RTOS;
IMPORT Thread, ThreadF, M3toC, Ctypes, Csetjmp;
IMPORT RTException;

VAR
  DEBUG := FALSE;
  dump_enabled := FALSE;
  implicitExceptions : ARRAY [0..7] OF ExceptionName;

TYPE
  FinallyProc = PROCEDURE () RAISES ANY;

EXCEPTION
  OUCH; (* to keep the compiler from complaining *)


PROCEDURE IsImplicitException(ex: ExceptionName) : BOOLEAN =
  BEGIN
    FOR i := FIRST(implicitExceptions) TO LAST(implicitExceptions) DO
      IF implicitExceptions[i] = ex THEN RETURN TRUE; END;
    END;
    RETURN FALSE;
  END IsImplicitException;

        
PROCEDURE Raise (en: ExceptionName; arg: ExceptionArg) RAISES ANY =
  VAR
    f                 := LOOPHOLE(ThreadF.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
    isImplicitException: BOOLEAN;
  BEGIN

    IF DEBUG THEN
      RTIO.PutText("---> RAISE:");
      RTIO.PutText("  en=");
      RTIO.PutAddr(en);
      RTIO.PutText(" ");
      RTIO.PutString(en^);
      RTIO.PutText("  arg=");
      RTIO.PutAddr(arg);
      RTIO.PutText("\n");
      DumpStack();
    END;

    isImplicitException := IsImplicitException(en);

    LOOP
      IF (f = NIL) THEN RTException.NoHandler(en, arg, raises := FALSE); END;

      CASE f.class OF
      | ORD(ScopeKind.Except) =>
          ex := LOOPHOLE(f, PF1).handles;
          IF (ex # NIL) THEN
            WHILE (ex^ # NIL) DO
              IF (ex^ = en) THEN ResumeRaise(en, arg) END;
              INC(ex, ADRSIZE(ex^));
            END;
          END;
      | ORD(ScopeKind.ExceptElse) =>
          (* 's' is a TRY-EXCEPT-ELSE frame => go for it *)
          ResumeRaise(en, arg);
      | ORD(ScopeKind.Finally), ORD(ScopeKind.FinallyProc),
          ORD(ScopeKind.Lock) =>
        (* ignore for this pass *)
      | ORD(ScopeKind.Raises) =>
          (* check that this procedure does indeed raise 'en' *)
          IF NOT isImplicitException THEN
            ex := LOOPHOLE(f, PF3).raises;
            IF ex = NIL THEN RTException.NoHandler(en, arg); END;
            LOOP
              IF (ex^ = NIL) THEN RTException.NoHandler(en, arg) END;
              IF (ex^ = en) THEN (* ok, it passes *) EXIT END;
              INC(ex, ADRSIZE(ex^));
            END;
          END;
      | ORD(ScopeKind.RaisesNone) =>
          IF NOT isImplicitException THEN RTException.NoHandler(en, arg); END;
      ELSE
        BadStack();
      END;

      f := f.next;               (* try the previous frame *)
    END;
  END Raise;

PROCEDURE ResumeRaise (en: ExceptionName;  arg: ExceptionArg;
                       crash: BOOLEAN := TRUE) RAISES ANY =
  VAR
    f := LOOPHOLE(ThreadF.GetCurrentHandlers(), Frame);
    ex: ExceptionList;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("---> RERAISE:");
      RTIO.PutText ("  en=");   RTIO.PutAddr (en);
      RTIO.PutText (" ");       RTIO.PutString (en^);
      RTIO.PutText ("  arg=");  RTIO.PutAddr (arg);
      RTIO.PutText ("\n");
      DumpStack ();
    END;

    LOOP
      IF (f = NIL) THEN 
        IF crash THEN 
          BadStack ();
        ELSE
          RETURN; 
        END;
      END;

      CASE f.class OF
      | ORD (ScopeKind.ExceptElse),
        ORD (ScopeKind.Finally) =>
          InvokeHandler (f, en, arg);
      | ORD (ScopeKind.Except) =>
          ex := LOOPHOLE (f, PF1).handles;
          WHILE (ex^ # NIL) DO
            IF (ex^ = en) THEN InvokeHandler (f, en, arg) END;
            INC (ex, ADRSIZE (ex^));
          END;
      | ORD (ScopeKind.FinallyProc) =>
          InvokeFinallyHandler (f, en, arg);
      | ORD (ScopeKind.Lock) =>
          ReleaseLock (f);
      | ORD (ScopeKind.Raises) =>
          (* already checked during the first pass *)
      | ORD(ScopeKind.RaisesNone) =>	
          IF NOT IsImplicitException(en) AND crash THEN BadStack() END;
          (* Else, already checked during the first pass *)
      ELSE
          BadStack ();
      END;

      ThreadF.SetCurrentHandlers (f.next); (* cut to the new handler *)
      f := f.next;                         (* try the previous frame *)
    END;
  END ResumeRaise;

PROCEDURE InvokeHandler (f: Frame; en: ExceptionName;
                         arg: ExceptionArg) RAISES ANY =
  VAR p := LOOPHOLE (f, PF1);
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> INVOKE HANDLER:");
      RTIO.PutText ("  en=");     RTIO.PutAddr (en);
      RTIO.PutText (" ");         RTIO.PutString (en^);
      RTIO.PutText ("  arg=");    RTIO.PutAddr (arg);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    ThreadF.SetCurrentHandlers (f.next); (* cut to the new handler *)
    p.exception := en;                   (* record the exception *)
    p.arg := arg;                        (* and it argument *)
    Csetjmp.ulongjmp (p.jmpbuf, 1);      (* and jump... *)
    RAISE OUCH;
  END InvokeHandler;

PROCEDURE InvokeFinallyHandler (f: Frame; en: ExceptionName;
                                arg: ExceptionArg) RAISES ANY =
  VAR
    p := LOOPHOLE (f, PF2);
    cl: RT0.ProcedureClosure;
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> INVOKE FINALLY HANDLER:");
      RTIO.PutText ("  en=");     RTIO.PutAddr (en);
      RTIO.PutText (" ");         RTIO.PutString (en^);
      RTIO.PutText ("  arg=");    RTIO.PutAddr (arg);
      RTIO.PutText ("  frame=");  RTIO.PutAddr (f);
      RTIO.PutText ("  class=");  RTIO.PutInt (f.class);
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;

    (* build a nested procedure closure  *)
    cl.marker := RT0.ClosureMarker;
    cl.proc   := p.handler;
    cl.frame  := p.frame;
    
    ThreadF.SetCurrentHandlers (f.next); (* cut to the new handler *)
    CallProc (LOOPHOLE (ADR (cl), FinallyProc));
  END InvokeFinallyHandler;

PROCEDURE CallProc (p: FinallyProc) RAISES ANY =
  (* we need to fool the compiler into generating a call
     to a nested procedure... *)
  BEGIN
    p ();
  END CallProc;

PROCEDURE ReleaseLock (f: Frame) =
  VAR p := LOOPHOLE (f, PF4);
  BEGIN
    IF DEBUG THEN
      RTIO.PutText ("--> UNLOCK:");
      RTIO.PutText ("  frame=");  RTIO.PutAddr (p);
      RTIO.PutText ("  mutex=");  RTIO.PutAddr (LOOPHOLE (p.mutex, ADDRESS));
      RTIO.PutText ("\n");
      RTIO.Flush ();
    END;
    ThreadF.SetCurrentHandlers (f.next); (* cut to the new handler *)
    Thread.Release (p.mutex);            (* and release the lock *)
  END ReleaseLock;

PROCEDURE NoHandler (en: ExceptionName;
                     <* UNUSED *> arg: ExceptionArg; raises := TRUE) =
  VAR nm := EName (en);
  BEGIN
    IF (raises) THEN
      RTMisc.FatalError (NIL, 0, "Exception \"", nm, "\" not in RAISES list");
    ELSE
      RTMisc.FatalError (NIL, 0, "Unhandled exception \"", nm, "\"");
    END;
  END NoHandler;

PROCEDURE BadStack () =
  BEGIN
    RTMisc.FatalError (NIL, 0, "corrupt exception stack");
  END BadStack;

(*----------------------------------------------------------- diagnostics ---*)

PROCEDURE SanityCheck () =
  CONST Min_SK = ORD (FIRST (ScopeKind));
  CONST Max_SK = ORD (LAST (ScopeKind));
  VAR f := LOOPHOLE(ThreadF.GetCurrentHandlers(), Frame);
  VAR i: INTEGER;
  BEGIN
    WHILE (f # NIL) DO
      i := f.class;
      IF (i < Min_SK) OR (Max_SK < i) THEN BadStack () END;
      f := f.next;
    END;
  END SanityCheck;

PROCEDURE DumpStack () =
  VAR f := LOOPHOLE(ThreadF.GetCurrentHandlers(), Frame);
  BEGIN
    IF NOT DEBUG AND NOT dump_enabled THEN RETURN; END;

    RTOS.LockHeap (); (* disable thread switching... (you wish!) *)

    RTIO.PutText ("------------------ EXCEPTION HANDLER STACK ---------------------\n");
    WHILE (f # NIL) DO
      RTIO.PutAddr (f);

      CASE f.class OF
      | ORD (ScopeKind.Except) =>
          RTIO.PutText (" TRY-EXCEPT ");
          DumpHandles (LOOPHOLE (f, PF1).handles);
      | ORD (ScopeKind.ExceptElse) =>
          RTIO.PutText (" TRY-EXCEPT-ELSE ");
      | ORD (ScopeKind.Finally) =>
          RTIO.PutText (" TRY-FINALLY ");
      | ORD (ScopeKind.FinallyProc) =>
          VAR x := LOOPHOLE (f, PF2); BEGIN
            RTIO.PutText (" TRY-FINALLY  proc = ");
            RTIO.PutAddr (x.handler);
            RTIO.PutText ("   frame = ");
            RTIO.PutAddr (x.frame);
          END;
      | ORD (ScopeKind.Raises) =>
          RTIO.PutText (" RAISES ");
          DumpHandles (LOOPHOLE (f, PF3).raises);
      | ORD (ScopeKind.RaisesNone) =>
          RTIO.PutText (" RAISES {}");
          RTIO.PutText (" IMPLICIT Exceptions allowed include:");
          FOR i := FIRST(implicitExceptions) TO LAST(implicitExceptions) DO
            IF implicitExceptions[i] # NIL THEN
              RTIO.PutText(EName(implicitExceptions[i]) & ",");
            END;
          END;
      | ORD (ScopeKind.Lock) =>
          VAR x := LOOPHOLE (f, PF4); BEGIN
            RTIO.PutText (" LOCK  mutex = ");
            RTIO.PutAddr (LOOPHOLE (x.mutex, ADDRESS));
          END;
      ELSE
         RTIO.PutText (" *** BAD EXCEPTION RECORD, class = ");
         RTIO.PutInt (f.class);
         RTIO.PutText (" ***\n");
         EXIT;
      END;
      RTIO.PutText ("\n");
      f := f.next;
    END;
    RTIO.PutText ("----------------------------------------------------------------\n");
    RTIO.Flush ();

    RTOS.UnlockHeap ();
  END DumpStack;

PROCEDURE NoteImplicitException (ex: ExceptionName; enable: BOOLEAN) =
  BEGIN
    FOR i := FIRST(implicitExceptions) TO LAST(implicitExceptions) DO
      IF implicitExceptions[i] = ex THEN
        IF NOT enable THEN implicitExceptions[i] := NIL; END;
        RETURN;
      ELSIF implicitExceptions[i] = NIL AND enable THEN
        implicitExceptions[i] := ex;
        RETURN;
      END;
    END;
    IF enable THEN
      RTMisc.FatalError(NIL, 0, "Too many implicit exceptions");
    END;
  END NoteImplicitException;

    

PROCEDURE DumpHandles (x: ExceptionList) =
  VAR first := TRUE;  en: ExceptionName;
  BEGIN
    RTIO.PutText (" {");
    IF (x # NIL) THEN
      WHILE (x^ # NIL) DO
        IF (NOT first) THEN RTIO.PutText (", ");  END;
        first := FALSE;
        en := x^;
        RTIO.PutString (en^);
        INC (x, ADRSIZE (x^));
      END;
    END;
    RTIO.PutText ("}");
  END DumpHandles;


PROCEDURE EName (en: ExceptionName): TEXT =
  BEGIN
    RETURN M3toC.StoT (LOOPHOLE (en^, Ctypes.char_star));
  END EName;

PROCEDURE Init() =
BEGIN
  (* SPIN - mainbody init moved here and is run directly by RTLinker *)
  dump_enabled := RTParams.IsPresent ("stackdump");
  EVAL SanityCheck; (* avoid the unused warning *)
END Init;

BEGIN
  (* SPIN - mainbody moved to Init() and is run directly by RTLinker *)
END RTException.

