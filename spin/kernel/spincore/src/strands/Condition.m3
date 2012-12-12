(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Implemented Alert***
 *	
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Got rid of unsafewaituser.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt management.
 *
 * 14-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Made callable from interrupt level.
 *
 * 10-Apr-95  Marc Fiuczynski (mef) at the University of Washington
 *	Changed from SMutex to MUTEX.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Condition variables, callable from interrupt level.
 *)
UNSAFE MODULE Condition EXPORTS Thread, Condition;
IMPORT Strand, StrandRep, ThreadRep, CPU, CPUPrivate;
IMPORT AtomicOpsExtern;

REVEAL
  Condition = BRANDED "Condition" OBJECT
    list: Strand.T;
  END;

PROCEDURE Wait(m: MUTEX; c: Condition) =
  VAR
    s := Strand.GetCurrent();
  BEGIN
    AtomicOpsExtern.EnqueueAddr(ADR(c.list), LOOPHOLE(s, ADDRESS),
				       ADR(s.syncnext));
    Release(m);
    Strand.Block(s);
    Acquire(m);
  END Wait;

PROCEDURE AlertWait (m: Mutex; c: Condition) RAISES {Alerted} =
  VAR
    s := NARROW(Strand.GetCurrent(), T);
  BEGIN
    s.condition := c;
    s.waitResult := ThreadRep.WaitResult.Awakened;
    (* Set the default wakeup result. Overwritten only by Alert. *)
    AtomicOpsExtern.EnqueueAddr(ADR(c.list),
				       LOOPHOLE(s, ADDRESS),
				       ADR(s.syncnext));
    Release(m);
    Strand.Block(s);
    Acquire(m);
    IF s.waitResult # ThreadRep.WaitResult.Awakened THEN RAISE Alerted; END;
  END AlertWait;
  
PROCEDURE Alert (t: T) =
  VAR
    c := t.condition;
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  BEGIN
    c := t.condition;
    IF c # NIL THEN
      DequeueStrandFromCondition(c, t);
      t.condition := NIL;
      t.waitResult := ThreadRep.WaitResult.Interrupted;
      Strand.Unblock(t);
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END Alert;

PROCEDURE TestAlert(): BOOLEAN =
  BEGIN
    RETURN FALSE;
  END TestAlert;


PROCEDURE Signal (c: Condition) =
  VAR
    s: Strand.T;
    spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    s := c.list;
    IF s # NIL THEN 
      c.list := s.syncnext;
      Strand.Unblock(s);
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END Signal;

PROCEDURE Broadcast(c: Condition) =
  VAR
    s: Strand.T;
    spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    s := c.list;
    WHILE s # NIL DO
      Strand.Unblock(s);
      s := s.syncnext;
    END;
    c.list := NIL;
    CPUPrivate.RestoreInterruptMask(spl);
  END Broadcast; 

PROCEDURE DequeueStrandFromCondition (c: Condition; s: Strand.T) =
  VAR
    prev: Strand.T;
  BEGIN
    IF c.list = s THEN
      c.list := s.syncnext;
    ELSE
      prev := c.list;
      WHILE prev # NIL DO
	IF prev.syncnext = s THEN
	  prev.syncnext := s.syncnext;
	  EXIT;
	END;
	prev := prev.syncnext;
      END;
    END;
  END DequeueStrandFromCondition;

(*
 * Signal a particular thread waiting on a condition variable.
 * Required for mach support for assert_wait and timeouts - not
 * performance critical.
 *)
PROCEDURE SignalThread(c: Condition; s: Strand.T) =
  VAR spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    DequeueStrandFromCondition(c, s);
    Strand.Unblock(s);
    CPUPrivate.RestoreInterruptMask(spl);
  END SignalThread;

BEGIN
END Condition.
