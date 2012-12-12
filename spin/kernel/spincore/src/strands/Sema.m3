(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt management.
 *
 * 07-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added the reset operation to support reuse of semaphores.
 *
 * 10-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added semaphore broadcast. Wakes up all threads
 *	waiting on the semaphore.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added VThread to support Mach synch primitives.
 *      Added equal, hash and brand to allow tables of semaphores.
 *      This module is unsafe because we hash on the pointer value of the
 *      semaphore.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Counting semaphores.
 *)
UNSAFE MODULE Sema;
IMPORT SemaRep, Strand, StrandRep, CPU, CPUPrivate;
IMPORT StrongRef;
IMPORT Debugger;<*NOWARN*>
PROCEDURE Alloc(initval: INTEGER := 0) : T =
  VAR s: T;
  BEGIN
    s := NEW(T);
    s.val := initval;
    s.list := NIL;
    RETURN s;
  END Alloc;

PROCEDURE Dealloc(sema: T) RAISES {InUse} = 
  BEGIN
    (* should we unstrongref here ? *)
     IF sema.list # NIL THEN
       RAISE InUse;
     END;
  END Dealloc;

PROCEDURE Reset(sema: T; val: INTEGER := 0) RAISES {InUse} =
  VAR
    spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    TRY
      IF sema.list # NIL THEN
        RAISE InUse;
      END;
      sema.val := val;
    FINALLY
      CPUPrivate.RestoreInterruptMask(spl);
    END;    
  END Reset;

PROCEDURE P(sema: T) =
  VAR s: Strand.T;
      spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    DEC(sema.val);
    IF sema.val < 0 THEN 
      s := Strand.GetCurrent();
      s.syncnext := sema.list;
      sema.list := s;
      Strand.Block(s);
    END;
    CPUPrivate.RestoreInterruptMask(spl); (* XXX (mef) *)
  END P;

PROCEDURE V(sema: T) =
  VAR s: Strand.T;
      spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    INC(sema.val);
    IF sema.val <= 0 THEN
      s := sema.list;
      IF s # NIL THEN
        sema.list := s.syncnext;
      END;
      CPUPrivate.RestoreInterruptMask(spl);
      IF s # NIL THEN
        Strand.Unblock(s);
      END;
    ELSE 
      CPUPrivate.RestoreInterruptMask(spl);
    END;
  END V;

(*
 * Wake up all the threads waiting on this semaphore.
 * No-op if there are no blocked threads.
 *)
PROCEDURE Broadcast(sema: T) =
  VAR spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    IF sema.val < 0 THEN
      sema.val := 0;
      WHILE sema.list # NIL DO
        Strand.Unblock(sema.list);
        sema.list := sema.list.syncnext;
      END;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END Broadcast;

(*
 * Signal a particular thread waiting on a condition variable.
 * Required for mach support for assert_wait and timeouts - not
 * performance critical.
 *)
PROCEDURE VThread(sema: T; s: Strand.T) =
  VAR spl: CPU.InterruptLevel;
      prev: Strand.T;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    INC(sema.val);
    IF sema.val <= 0 THEN
      IF sema.list = s THEN
	sema.list := s.syncnext;
	Strand.Unblock(s);
      ELSE
	prev := sema.list;
	WHILE prev # NIL DO
	  IF prev.syncnext = s THEN
	    prev.syncnext := s.syncnext;
	    Strand.Unblock(s);
	    EXIT;
	  END;
	  prev := prev.syncnext;
	END;
      END;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END VThread; 

PROCEDURE Equal(a, b: T) : BOOLEAN =
  BEGIN
    RETURN a = b;
  END Equal;

PROCEDURE Hash(a: T) : INTEGER =
  BEGIN
    StrongRef.Add(a);
    RETURN LOOPHOLE(a, INTEGER);
  END Hash;

BEGIN
END Sema.
