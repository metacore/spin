(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Dec-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added to mutex sanity checking.
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Changed from SPIN locks to blocking locks.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Mutexes.
 *
 * This module is unsafe because it exports two safe interfaces in the
 * unsafe module RTHooks. 
*)
UNSAFE MODULE Mutex EXPORTS Thread, Mutex, RTHooks;
IMPORT AtomicOpsExtern;
IMPORT Strand, StrongRef;
IMPORT DebugOption, RTIO, CPU, CPUPrivate, Debugger;
IMPORT StrandRep;

(*
 * The mulock field is
 *      0 if the lock is not held
 *	1 if the lock is held exclusively by a strand.
 *      pointer to a queued strand if the lock has waiters.
 *              in this case, the nextmuwaiter field of the strand
 *              points to the next thread that is waiting for the lock.
 *
 * A TCB has to be pinned down in memory against movement while a mutex
 * keeps a reference to it.
 *)
  
REVEAL
  MUTEX = BRANDED "Mutex" OBJECT
    mulock: INTEGER := 0;
  END;

(* extension to support better interfacing with interrupts *)
PROCEDURE TryLock(l: MUTEX) : BOOLEAN =
  BEGIN
    IF DebugOption.MutexSanity THEN
      CheckMutexSanity(l);
    END;
    RETURN AtomicOpsExtern.TryLock(l.mulock);
  END TryLock;
  
PROCEDURE IsLocked (l: MUTEX): BOOLEAN =
  BEGIN
    IF DebugOption.MutexSanity THEN
      CheckMutexSanity(l);
    END;
    RETURN l.mulock # 0;
  END IsLocked;

(* compiler emits calls directly to LockMutex and UnlockMutex *)
PROCEDURE LockMutex (l: MUTEX) =
  BEGIN
    IF DebugOption.MutexSanity THEN
      CheckMutexSanity(l);
      CheckStrandSanity();
    END;
    
    IF AtomicOpsExtern.TryLock(l.mulock) = TRUE THEN
      (*
       * fast acquire path
       *)
      RETURN;
    END;
    (*
     * failure to acquire. this case is slower
     *)
    VAR
      me: Strand.T;
    BEGIN
      me := Strand.GetCurrent();
      StrongRef.Add(me);
      IF AtomicOpsExtern.LockOrEnqueue(l.mulock, me) = FALSE THEN
        Strand.Block(me);
      END;
      StrongRef.Remove(me);
    END;
  END LockMutex;
 
PROCEDURE UnlockMutex(l: MUTEX) =
  VAR
    ret: INTEGER;
    spl      : CPU.InterruptLevel;
  BEGIN
    IF DebugOption.MutexSanity THEN
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      CheckMutexSanity(l);
      IF l.mulock = 0 THEN
        RTIO.PutText("ERROR >> unlocking an unlocked mutex\n");
        Debugger.Enter();
      END;
    END;

    ret := AtomicOpsExtern.UnlockAndDequeue(l.mulock);
    
    IF DebugOption.MutexSanity THEN
      CPUPrivate.RestoreInterruptMask(spl);
    END;

    IF ret = 0 THEN
      RETURN;
    ELSE
      Strand.Unblock(LOOPHOLE(ret, Strand.T));
    END;
  END UnlockMutex;
 
(* These are user callable from the thread interface *)
PROCEDURE Acquire(l: MUTEX) =
  BEGIN
    IF DebugOption.MutexSanity THEN
      CheckMutexSanity(l);
      CheckStrandSanity();
    END;
    IF AtomicOpsExtern.TryLock(l.mulock) = TRUE THEN
      (*
       * fast acquire path
       *)
      RETURN;
    END;
    IF DebugOption.MutexSanity THEN
      CheckMutexSanity(l);
      CheckStrandSanity();
    END;
    (*
     * failure to acquire. this case is slower
     *)
    VAR
      me: Strand.T;
    BEGIN
      me := Strand.GetCurrent();
      StrongRef.Add(me);
      IF AtomicOpsExtern.LockOrEnqueue(l.mulock, me) = FALSE THEN
        Strand.Block(me);
      END;
      StrongRef.Remove(me);
    END;
  END Acquire;
 
PROCEDURE Release(l: MUTEX) =
  VAR
    ret: INTEGER;
    spl      : CPU.InterruptLevel;
  BEGIN
    IF DebugOption.MutexSanity THEN
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      CheckMutexSanity(l);
      IF l.mulock = 0 THEN
        RTIO.PutText("ERROR >> unlocking an unlocked mutex\n");
        Debugger.Enter();
      END;
    END;

    ret := AtomicOpsExtern.UnlockAndDequeue(l.mulock);

    IF DebugOption.MutexSanity THEN
      CPUPrivate.RestoreInterruptMask(spl);
    END;

    IF ret = 0 THEN
      RETURN;
    ELSE
      Strand.Unblock(LOOPHOLE(ret, Strand.T));
    END;
  END Release;

PROCEDURE CheckMutexSanity (l: MUTEX) =
  BEGIN
    IF l = NIL THEN
      RTIO.PutText("ERROR >> mutex is NIL.\n");
      Debugger.Enter();
    END;
  END CheckMutexSanity;

VAR
  reentered := 0;

PROCEDURE CheckStrandSanity (st: Strand.T := NIL) =
  BEGIN
    IF reentered # 0 THEN RETURN; END;
    IF st = NIL THEN
      st := Strand.GetCurrent();
    END;
    IF st = NIL THEN RETURN; END;
    
    IF st.bound_to_user # NIL THEN
      IF st.bound_to_user.count <= 0 THEN
	reentered := 1;
	RTIO.PutText("bound to user count <= 0.\n");
	Debugger.Enter();
	reentered := 0;
      END;
    END;
  END CheckStrandSanity;

BEGIN
END Mutex.
