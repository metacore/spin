(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 22-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the Syscall trap signature.
 *
 * 21-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Strengthened LOOPHOLE(s, Strand.T) to NARROW(s, Strand.T)
 *	Made module safe.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Use console object.
 *
 * 10-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed SMutex.Alloc() to NEW (MUTEX).
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Cthreads for users, implemented on top of userspace threads.
 *) 
MODULE CThread;
IMPORT Thread, UserSpaceThread, Strand, CPU;
IMPORT Word, RefRefTbl;
IMPORT Space, VMError;
IMPORT IO;
IMPORT Fmt, Log; <* NOWARN *> (* for debugging *)
IMPORT ExternalRef;

TYPE T = REF RECORD
  uth: UserSpaceThread.T;
  lock: MUTEX;
  done: Thread.Condition;
  returned: BOOLEAN := FALSE;
  result: Word.T;
  ctnext: T;
END;

CONST
  MaxTCBs = 32;

VAR
  freemutex: MUTEX;
  freethreads: T := NIL;

  (* mappings supplies the necessary info to go from a userthread to its *)
  (* cthread control block. *)
  mappings: RefRefTbl.Default;
  mappingslock: MUTEX;

PROCEDURE GetT() : T =
  VAR th: T;
  BEGIN
    LOCK freemutex DO
      IF freethreads = NIL THEN
        FOR i := 1 TO MaxTCBs DO
          th := NEW(T);
          th.lock := NEW (MUTEX);
          th.done := NEW(Thread.Condition);
          th.ctnext := freethreads;
          freethreads := th;
        END;
      END;
      th := freethreads;
      freethreads := freethreads.ctnext;
    END;
    RETURN th;
  END GetT;

CONST
  StackSize = 2 * 8192;
  Slack = 64;

PROCEDURE Fork(strand: Strand.T; VAR ss: CPU.SavedState) : Word.T =
  VAR
      parent_userthread, child_userthread: Strand.T;
      child: T;
      x: UserSpaceThread.State;
      stackbase: Word.T;
  BEGIN
    (* IO.Put("Cthread layered create\n"); *)
    child := GetT();
    parent_userthread := NARROW(strand, UserSpaceThread.T);
    x := NEW(UserSpaceThread.State);
    x.cpustate := NEW(REF CPU.SavedState);
    x.cpustate.pc := ss.a0;
    x.cpustate.a0 := ss.a1;
    x.cpustate.gp := ss.a2;
    x.cpustate.ra := ss.a3;
    (*stackbase := UserSpaceThread.GetSpaceTop(parent_userthread);*)
    TRY
      Space.Allocate(UserSpaceThread.GetSpace(parent_userthread),
		     stackbase, StackSize);
    EXCEPT
    | VMError.E(ec) => IO.Put("Cannot allocate stack(" & Fmt.Int(ec) & ".\n");
      RETURN -1;
    END;
    (*UserSpaceThread.SetSpaceTop(parent_userthread, Word.Plus(stackbase, 
                                                             StackSize));*)
    x.cpustate.usp := Word.Plus(stackbase, StackSize - Slack);
    x.cpustate.pv := x.cpustate.pc;

    (*
     * Create a new user thread in this space
     *)
    child_userthread := UserSpaceThread.Create(
                            UserSpaceThread.GetSpace(parent_userthread));
    (*
     * Set its state to the initial conditions
     *)
    UserSpaceThread.SetState(child_userthread, x);
(*
    IO.Put("Going to pc: " & Fmt.Unsigned(x.cpustate.pc) &
                       " sp: " & Fmt.Unsigned(x.cpustate.usp) & 
                       " a0: " & Fmt.Unsigned(x.cpustate.a0) &
                       " a1: " & Fmt.Unsigned(x.cpustate.a1) &
                       " a2: " & Fmt.Unsigned(x.cpustate.a2) & 
                       " gp: " & Fmt.Unsigned(x.cpustate.gp) & 
                       " ra: " & Fmt.Unsigned(x.cpustate.ra) & "\n");
    Log.Log("Going to pc: " & Fmt.Unsigned(x.cpustate.pc) &
                       " sp: " & Fmt.Unsigned(x.cpustate.usp) & 
                       " a0: " & Fmt.Unsigned(x.cpustate.a0) &
                       " a1: " & Fmt.Unsigned(x.cpustate.a1) &
                       " a2: " & Fmt.Unsigned(x.cpustate.a2) & 
                       " gp: " & Fmt.Unsigned(x.cpustate.gp) & 
                       " ra: " & Fmt.Unsigned(x.cpustate.ra) & "\n");
*)
    LOCK mappingslock DO
      EVAL mappings.put(child_userthread, child);
      child.uth := child_userthread;
    END;
    (*
     * Let it rip.
     *)
    UserSpaceThread.Resume(child_userthread);
    RETURN ExternalRef.GetCurrent().externalize(child);
  END Fork;

PROCEDURE Exit(strand: Strand.T; VAR ss: CPU.SavedState) =
  VAR 
    th: REFANY;
    cthread: T;
  BEGIN
(*
    IO.Put("Cthread exit with pc: " & Fmt.Unsigned(ss.pc) &
                       " sp: " & Fmt.Unsigned(ss.usp) & 
                       " a0: " & Fmt.Unsigned(ss.a0) &
                       " a1: " & Fmt.Unsigned(ss.a1) &
                       " a2: " & Fmt.Unsigned(ss.a2) & 
                       " gp: " & Fmt.Unsigned(ss.gp) & 
                       " ra: " & Fmt.Unsigned(ss.ra) & "\n");
    Log.Log("Cthread exit with pc: " & Fmt.Unsigned(ss.pc) &
                       " sp: " & Fmt.Unsigned(ss.usp) & 
                       " a0: " & Fmt.Unsigned(ss.a0) &
                       " a1: " & Fmt.Unsigned(ss.a1) &
                       " a2: " & Fmt.Unsigned(ss.a2) & 
                       " gp: " & Fmt.Unsigned(ss.gp) & 
                       " ra: " & Fmt.Unsigned(ss.ra) & "\n");
*)
    LOCK mappingslock DO
      EVAL mappings.get(strand, th);
    END;
    cthread := NARROW(th, T);
    IF cthread # NIL THEN
      LOCK cthread.lock DO
        <*ASSERT strand = cthread.uth*>
        UserSpaceThread.Suspend(strand);
        UserSpaceThread.Destroy(strand);
        cthread.returned := TRUE;
        cthread.result := ss.a0;
        Thread.Broadcast(cthread.done);
      END;
    ELSE
      IO.Put("No mapping for cthread\n");
    END;
  END Exit;

PROCEDURE Join(<*UNUSED*>strand: Strand.T;
	       VAR ss: CPU.SavedState) : Word.T =
  VAR 
    cthread: T;
  BEGIN
(*
    IO.Put("Cthread layered join with pc: " & Fmt.Unsigned(ss.pc) &
                       " sp: " & Fmt.Unsigned(ss.usp) & 
                       " a0: " & Fmt.Unsigned(ss.a0) &
                       " a1: " & Fmt.Unsigned(ss.a1) &
                       " a2: " & Fmt.Unsigned(ss.a2) & 
                       " gp: " & Fmt.Unsigned(ss.gp) & 
                       " ra: " & Fmt.Unsigned(ss.ra) & "\n");
    Log.Log("Cthread layered join with pc: " & Fmt.Unsigned(ss.pc) &
                       " sp: " & Fmt.Unsigned(ss.usp) & 
                       " a0: " & Fmt.Unsigned(ss.a0) &
                       " a1: " & Fmt.Unsigned(ss.a1) &
                       " a2: " & Fmt.Unsigned(ss.a2) & 
                       " gp: " & Fmt.Unsigned(ss.gp) & 
                       " ra: " & Fmt.Unsigned(ss.ra) & "\n");
*)
    cthread := ExternalRef.GetCurrent().internalize(ss.a0);
    IF cthread # NIL THEN
      LOCK cthread.lock DO
        IF cthread.returned = FALSE THEN
          Thread.Wait(cthread.lock, cthread.done); 
        END;
      END;
    END;
    RETURN cthread.result;
  END Join;

BEGIN
  freemutex := NEW (MUTEX);
  mappingslock := NEW (MUTEX);
  mappings := NEW(RefRefTbl.Default).init();
  IO.Put("Cthreads layered on UserSpaceThreads added to the kernel...\n");
END CThread.
