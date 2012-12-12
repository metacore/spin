(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Explicit NARROW on NameServer results for Domain. New Auth
 *	 interface.
 *
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Changed syscall signature to take strand argument explicitly.
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Use UNSAFEWaitUser to be explicit about how bad we're being.
 *	Made this a SAFE module.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Changed LOOPHOLE(s, Strand.T) to NARROW(s, Strand.T)
 *
 * 03-Aug-95  Stefan Savage (savage) at the University of Washington
 *	From brian: replaced PrimaryName() with per-interface Brand
 *
 * 03-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Don't use space for Identity
 *
 * 25-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Rely on nameserver to determine safecore.
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made domain creation a safe operation.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created.
 *) 
MODULE CTSyscall;
IMPORT Strand, StrandRep, Thread, CPU;
IMPORT Fmt, Word, Domain, Condition;
IMPORT UserSpaceThread, CThread, CThreadNative, Space;
IMPORT NameServer, SpinPublic;
IMPORT Dispatcher, Trap;
IMPORT SecurityError, SecurityManager;
IMPORT Translation;
IMPORT IO;
IMPORT ExternalRef;
VAR UHz: INTEGER := 266;

  
PROCEDURE Syscall (strand: Strand.T; VAR ss: CPU.SavedState) =
  VAR
    caller           : UserSpaceThread.T;
    d1, d2                : Domain.T;
    safeCore              : Domain.T;
    cthread: CThreadNative.T;
    cond                  : Thread.Condition;
    codebuf               : REF ARRAY OF CHAR;
    buf                   : ARRAY [0 .. 256] OF CHAR;
    xref: ExternalRef.T;
  BEGIN
    (* XXX a lot of this code assumes that spaces are locked, i.e.  they *)
    (* do not change underneath us after a call to getcurrent.  this is *)
    (* no longer true.  This needs to be addressed. *)
    CASE ss.v0 OF
    | 0 =>
    | 1 => Terminate(strand);    (* Unix exit *)
    | 2 =>
        TRY
	  Translation.Write(Translation.GetCurrent(), SUBARRAY(buf, 0, 128), ss.a0);
        EXCEPT
        ELSE
          IO.Put("Space failure.\n");
        END;
    | 5 => IO.Put(Fmt.Char(VAL(ss.a0, CHAR)));
    | 6 => IO.Put("User Print: 0x" & Fmt.Unsigned(ss.a0) & "\n");
    | 7 =>
        TRY
          Translation.Read(Translation.GetCurrent(),
			   ss.a0, SUBARRAY(buf, 0, ss.a1));
        EXCEPT
        ELSE
          IO.Put("Space failure.\n");
        END;
        FOR i := 0 TO ss.a1 - 1 DO IO.Put(Fmt.Char(buf[i])); END;
        IO.Put(Fmt.Int(ss.a2) & " cycles "
                 & Fmt.Int(Word.Divide(ss.a2, UHz)) & " usecs.\n");
    | 10 =>
      IO.Put("Creating Domains\n");
      xref := ExternalRef.GetCurrent();
      codebuf := NEW(REF ARRAY OF CHAR, 15000);
      TRY
	Translation.Read(Translation.GetCurrent(),
			 ss.a0, SUBARRAY(codebuf^, 0, ss.a1));
      EXCEPT
      ELSE
	IO.Put("Space failure.\n");
      END;
      d1 := Domain.Create("TrapPrivate2", codebuf);
      ss.v0 := xref.externalize(d1);
    | 11 =>
      IO.Put("Dynamic linking\n");
      xref := ExternalRef.GetCurrent();      
      caller := NARROW(strand, UserSpaceThread.T);
      d1 := xref.internalize(ss.a0);
      d2 := xref.internalize(ss.a1);
      TRY
	WITH entry = NameServer.Lookup(NIL, "/../svc/domains/"&SpinPublic.Brand) DO
	  safeCore := NARROW(entry, Domain.T);
	  Domain.Resolve(d1, safeCore);
	  Domain.Resolve(d1, d2);
	  Domain.Resolve(d2, safeCore);
	  Domain.Resolve(d2, d1);
	END;
      EXCEPT
     | NameServer.Error (ec) =>
	IF ec = NameServer.EC.NameNotFound THEN
	  IO.Put("Can't find " & SpinPublic.Brand & "\n");
	ELSIF ec = NameServer.EC.Unauthorized THEN
	  IO.Put("Not authorized to access " & SpinPublic.Brand & "\n");
	END;
      END;
      Domain.Add(d1, d2);
      EVAL Domain.Initialize(d1);
    | 40 => ss.v0 := CThread.Fork(strand, ss);
    | 41 => CThread.Exit(strand, ss);
    | 42 => ss.v0 := CThread.Join(strand, ss);
    | 43 => Strand.Yield();
    | 69 =>                      (* sbrk *)
        IO.Put("CTsyscall: sbrk not supported.\n");
        caller := NARROW(strand, UserSpaceThread.T);
        ss.v0 := (*UserSpaceThread.GetSpaceTop(caller)*) 0;
        TRY
	  Space.Allocate(UserSpaceThread.GetSpace(caller), ss.v0, ss.a0);
        EXCEPT
        ELSE
          IO.Put("Space failure.\n");
        END;
        (*UserSpaceThread.SetSpaceTop(caller, Word.Plus(ss.v0, ss.a0));*)
    | 140 =>                     (* cond_alloc *)
      xref := ExternalRef.GetCurrent();
      caller := NARROW(strand, UserSpaceThread.T);
      ss.v0 := xref.externalize(NEW(Thread.Condition));
    | 141 =>                     (* cond_signal *)
      xref := ExternalRef.GetCurrent();
      cond := xref.internalize(ss.a0);
      Thread.Signal(cond);
    | 142 =>                     (* cond_wait *)
(*
        VAR s: REFANY := strand;
        BEGIN
          IF ISTYPE(s, CThreadNative.T) THEN
            cthreadcaller := NARROW(s, CThreadNative.T);
            cond := NARROW(UserSpaceThread.Internalize(
                             cthreadcaller.uth, ss.a0), Thread.Condition);
            Condition.UnsafeWaitUser(ss.a1, cond); (* XXX *)
          ELSIF ISTYPE(s, UserSpaceThread.T) THEN
            uth := NARROW(s, UserSpaceThread.T);
            cond := NARROW(UserSpaceThread.Internalize(uth, ss.a0),
                           Thread.Condition);
            Condition.UnsafeWaitUser(ss.a1, cond); (*XXX *)
          END;
        END;
*)
    | 143 =>                     (* cond_broadcast *)
      xref := ExternalRef.GetCurrent();
      cond := xref.internalize(ss.a0);
      Thread.Broadcast(cond);
    | 145 =>
        cthread := NARROW(strand, CThreadNative.T);
        CThreadNative.RegisterRas(cthread, ss.a0, ss.a1);
    | 146 =>
        cthread := NARROW(strand, CThreadNative.T);
        CThreadNative.RegisterCas(cthread, ss.a0, ss.a1, ss.a2);
    | 150 =>
      xref := ExternalRef.GetCurrent();
      caller := NARROW(strand, UserSpaceThread.T);
      ss.v0 := xref.externalize(CThreadNative.Fork(
                             caller, ss.a0, ss.a1, ss.a2, ss.a3, ss.a4));
    | 151 => CThreadNative.Exit(NARROW(strand, CThreadNative.T), ss.a0);
    | 152 =>
      xref := ExternalRef.GetCurrent();  
      cthread := xref.internalize(ss.a0);
      ss.v0 := CThreadNative.Join(cthread);
    ELSE
    END;
  END Syscall;

PROCEDURE Terminate (s: Strand.T) =
  VAR
    ust  : UserSpaceThread.T;
    space: Space.T;
  BEGIN
    IF TYPECODE(s) = TYPECODE(UserSpaceThread.T) THEN
      ust := NARROW(s, UserSpaceThread.T);
    ELSIF TYPECODE(s) = TYPECODE(Strand.T) THEN
      ust := NARROW(s.bound_to_user, UserSpaceThread.T);
    END;
    IF ust = NIL THEN
      IO.PutError(
        "Terminating a user space which has no user space threads\n");
    END;

    space := UserSpaceThread.GetSpace(ust);

    TRY
      IO.Put("Terminating space for exiting thread with id "
             & SecurityManager.GetUserName(SecurityManager.GetCurrentUid())
             & "\n");
    EXCEPT
    | SecurityError.T =>
      IO.Put("Terminating space for exiting thread with id "
             & Fmt.Int(SecurityManager.GetCurrentUid())
             & "\n");
    END;
    (*
     * Outstanding threads will die when they try to run in a destroyed
     * space.
     *)
(*   Space.Destroy(space); *)
    (* UserSpaceThread.DestroyThreadsBySpace(space) *)
    Strand.Block(s);
  END Terminate;
 
VAR
  authkey : Trap.AuthKey;
BEGIN
  TRY
    authkey := NEW(Trap.AuthKey);
    authkey.minProcID := 0;
    authkey.maxProcID := 10000;
    EVAL Dispatcher.InstallHandler(Trap.Syscall, NIL, Syscall, 
                            options:=Dispatcher.Options{Dispatcher.Opt.First,
                                               Dispatcher.Opt.Cancel},
                            key:=authkey);
    IO.Put("Cthread syscall handlers installed...\n"); 
  EXCEPT
    Dispatcher.Error => IO.Put("Installation Failure\n");
  END;

END CTSyscall.

