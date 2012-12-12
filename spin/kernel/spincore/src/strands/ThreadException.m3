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
 * 07-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)
MODULE ThreadException;

IMPORT ThreadRep, ThreadPrivate, ThreadExtra;
IMPORT Auth, Dispatcher, IO, Debugger, Thread;
IMPORT SpinException;

(*
 * A client is allowed to override the RaiseException event only for 
 * its own threads
 *)

TYPE ThreadAuth = Auth.T OBJECT OVERRIDES authorize := Authorize; END;


(*
 * The original handler for ThreadExtra.RaiseException. Should never run.
 *)

PROCEDURE RaiseException(<*UNUSED*>th: Thread.T) =
  BEGIN
    IO.Put("Thread: UNEXPECTED ThreadException.RaiseException!!\n");
    Debugger.Enter()
  END RaiseException;


PROCEDURE InException(th: Thread.T; VAR ei: SpinException.ExceptionInfo) : BOOLEAN =
  BEGIN
    LOCK th.lock DO
      IF th.state = ThreadPrivate.State.ExceptionPosted THEN
        ei := th.exception;
      RETURN TRUE;
      END;
    END;
    RETURN FALSE;
  END InException;
    

PROCEDURE TerminateInException(th: Thread.T) : BOOLEAN =
  BEGIN
    LOCK th.lock DO
      IF th.state # ThreadPrivate.State.ExceptionPosted THEN RETURN FALSE; END;
      Thread.Signal(th.done);
    END;
    IF th = Thread.Self() THEN
	ThreadPrivate.Exit_main(th, NIL);
        (* NOT REACHED *)
        Debugger.Enter();
    END;
    RETURN TRUE;
  END TerminateInException;




(*GUARD*) 
FUNCTIONAL PROCEDURE MyThread (cl: REFANY; t: Thread.T): BOOLEAN =
  BEGIN
    RETURN ThreadExtra.GetId(t) = NARROW(cl, REF INTEGER)^;
  END MyThread;

PROCEDURE Authorize (<*UNUSED*> a: ThreadAuth; key: Auth.Key; arg: REFANY):
  BOOLEAN =
  VAR idRef : REF INTEGER;
  BEGIN
    IF TYPECODE(key) # TYPECODE(Thread.T) THEN RETURN FALSE; END;
    (* All is well, impose a guard so that caller's method is only called
     * when RaiseException is called with the specified T's ID *)
    <* ASSERT TYPECODE(arg) = TYPECODE(Dispatcher.Binding) *>

    idRef := NEW(REF INTEGER);
    idRef^ := ThreadExtra.GetId(NARROW(key, Thread.T));
    TRY
      EVAL Dispatcher.ImposeGuard(
             NARROW(arg, Dispatcher.Binding), MyThread, idRef);
    EXCEPT
      Dispatcher.Error =>
        IO.Put("Thread.Authorizer failed to impose guard\n");
        RETURN FALSE;
    END;
    RETURN TRUE;
  END Authorize;





(*
 * INITIALIZATION 
 *)

PROCEDURE InstallAuthorizers () =
  BEGIN
    TRY
      Dispatcher.InstallAuthorizerForEvent(
        NEW(ThreadAuth), RaiseException, THIS_MODULE());
    EXCEPT
      Dispatcher.Error => IO.Put("Thread can not install authorizers\n");
    END;
  END InstallAuthorizers;


PROCEDURE InstallHandlers () =
  BEGIN
    (* Ddeinstall original ThreadExtra.RaiseException handler *)
    TRY
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(RaiseException));
    EXCEPT
      Dispatcher.Error =>
        IO.Put("Could not uninstall ThreadExtra.RaiseException\n");
    END;
  END InstallHandlers;




BEGIN
  InstallHandlers(); 
  InstallAuthorizers(); 
END ThreadException.
