(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Took out all TRYs and redundant spls.
 *
 * 22-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to use inlined version of Sema.P and Sema.Broadcast.
 *	Don't need to StrongRef semaphores anymore, since we are acting
 *	on the passed in values of the ULock directly.  This improves
 *	both performance and avoids the problems associated with
 *	StrongRef.
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Removed paranoid strongref ops in ulock_setup.
 *
 * 08-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed ulock_write not to strongref the thread if it is a
 *	recursive write lock.  Both ulock_write and ulock_read compensate
 *	for recursive locks by decrementing the write/read counts,
 *	respectively.
 *
 * 12-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed ulock_done to only unlock either a reader or a writer.
 *      Fixed ulock_write to synchronize properly with readers.
 *
 * 04-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Support for DEC OSF synchronization. A Ulock is DEC's
 *      version of reader/writer locks. Ulocks are recursive.
 *)
UNSAFE (* because converting addresses to traced types. *)
MODULE ULockForSAL;
IMPORT ULockForSALExtern;
IMPORT Sema, AtomicOps; <* NOWARN *>
IMPORT CPU, CPUPrivate, Ctypes; <* NOWARN *>
IMPORT Thread, ThreadRep, Strand, StrandRep;
IMPORT IO, StrongRef;

CONST oldUlock = FALSE;
(* set this if paranoid that thread.t's are being moved. *)
CONST paranoid = FALSE;

(* interface for C clients that need reader/writer locks *)

PROCEDURE ulock_setup(
    VAR rwl: T; 
    <*UNUSED*>
    lip: ADDRESS; 
    canwait: BOOLEAN) = 
  BEGIN
    rwl.readerCnt := 0;
    rwl.writerCnt := 0;
    IF NOT canwait THEN 
      IO.Put("Warning: ulock_setup canwait = FALSE.\n"); 
    END;

    IF oldUlock THEN
      (* XXX The following code may block, which is bad if canwait = FALSE *)
      LOOPHOLE(rwl.reader,Sema.T) := Sema.Alloc(0);
      LOOPHOLE(rwl.writer,Sema.T) := Sema.Alloc(0);
      LOOPHOLE(rwl.lastlocker,Thread.T) := NIL;

      StrongRef.Add(NARROW(LOOPHOLE(rwl.reader,REFANY), Sema.T));
      StrongRef.Add(NARROW(LOOPHOLE(rwl.writer,REFANY), Sema.T));
    ELSE
      rwl.readerVal := 0;
      rwl.writerVal := 0;
      rwl.reader    := NIL;
      rwl.writer    := NIL;
      rwl.lastlocker := NIL;
    END;
  END ulock_setup;

PROCEDURE ulock_terminate(VAR rwl: T) = 
  BEGIN
    IF oldUlock THEN 
      StrongRef.Remove(NARROW(LOOPHOLE(rwl.reader,REFANY), Sema.T));
      StrongRef.Remove(NARROW(LOOPHOLE(rwl.writer,REFANY), Sema.T));
      IF paranoid THEN
        StrongRef.Remove(NARROW(LOOPHOLE(rwl.lastlocker,REFANY), Thread.T));
      END;

      LOOPHOLE(rwl.reader,Sema.T) := NIL;
      LOOPHOLE(rwl.writer,Sema.T) := NIL;
      LOOPHOLE(rwl.lastlocker,Thread.T) := NIL;
    ELSE
      rwl.reader := NIL;
      rwl.writer := NIL;
      rwl.lastlocker := NIL;
    END;
  END ulock_terminate;

(* semaphore emulation.
   Precondition: SPL is high. *)
PROCEDURE P(VAR sema: ADDRESS; VAR val: BITS 32 FOR Ctypes.int) =
  BEGIN
    IF oldUlock THEN 
      Sema.P(NARROW(LOOPHOLE(sema,REFANY), Sema.T));
    ELSE
      DEC(val);
      IF val < 0 THEN 
	WITH s = Strand.GetCurrent() DO
	  s.syncnext := LOOPHOLE(sema,Strand.T);
	  LOOPHOLE(sema,Strand.T) := s;
	  Strand.Block(s);
	END;
      END;
    END;
  END P;

(* Precondition: SPL is high. *)  
PROCEDURE Broadcast(VAR sema: ADDRESS; VAR val: BITS 32 FOR Ctypes.int) = 
  BEGIN
    IF oldUlock THEN
      Sema.Broadcast(NARROW(LOOPHOLE(sema,REFANY),Sema.T));
    ELSE
      IF val < 0 THEN
	val := 0;
	WHILE LOOPHOLE(sema,Strand.T) # NIL DO
	  Strand.Unblock(LOOPHOLE(sema,Strand.T));
	  LOOPHOLE(sema,Strand.T) := LOOPHOLE(sema,Strand.T).syncnext;
	END;
      END;
    END;
  END Broadcast;


PROCEDURE ulock_write(VAR rwl: T) = 
  VAR myno: INTEGER;
      myThread: Thread.T;
      ret := FALSE;
      spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    
    myThread := NARROW(Strand.GetCurrent(),Thread.T);
    INC(rwl.writerCnt);
    myno := rwl.writerCnt; (* AtomicOps.AtomicInc32(rwl.writerCnt); *)
    WHILE myno # 1 OR rwl.readerCnt > 0 DO
      (*
       * We allow recursive write locks, but currently prohibit
       * upgrading from a read lock to a write lock.
       *)
      IF LOOPHOLE(rwl.lastlocker, Thread.T) = myThread THEN
	(* compensate for recursive lock.
	   XXX WRONG! EVAL AtomicOps.AtomicDec32(rwl.writerCnt); *)
	(* RETURN;*)
	ret := TRUE;
	EXIT;
      END;
      
      (*
       * Wait as long as there is a reader or a writer in progress.
       *)
      DEC(rwl.writerCnt); (* EVAL AtomicOps.AtomicDec32(rwl.writerCnt); *)
      P(rwl.writer,rwl.writerVal);
      INC(rwl.writerCnt);
      myno := rwl.writerCnt; (* AtomicOps.AtomicInc32(rwl.writerCnt); *)
    END;
    IF NOT ret THEN
      IF paranoid THEN
	StrongRef.Add(myThread);
      END;
      LOOPHOLE(rwl.lastlocker,Thread.T) := myThread;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ulock_write;
  
PROCEDURE ulock_read(VAR rwl: T) = 
  VAR myThread: Thread.T;
      spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    INC(rwl.readerCnt); (* EVAL AtomicOps.AtomicInc32(rwl.readerCnt); *)
    WHILE rwl.writerCnt > 0 DO
      myThread := NARROW(Strand.GetCurrent(),Thread.T);
      (* Allow recursive locks *)
      IF LOOPHOLE(rwl.lastlocker, Thread.T) = myThread THEN 
	(* compensate for recursive lock
	   XXX WRONG! EVAL AtomicOps.AtomicDec32(rwl.readerCnt);
	*)
	(* RETURN; *)
	EXIT;
      END;

      (*
       * wait as long as there is a writer in progress.
       *)
      DEC(rwl.readerCnt); (* EVAL AtomicOps.AtomicDec32(rwl.readerCnt); *)
      P(rwl.reader, rwl.readerVal);
      INC(rwl.readerCnt); (* EVAL AtomicOps.AtomicInc32(rwl.readerCnt); *)
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ulock_read;

PROCEDURE ulock_done(VAR rwl: T) = 
  VAR spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    IF rwl.readerCnt > 0 THEN
      DEC(rwl.readerCnt);
      IF rwl.readerCnt > 0 THEN 
        CPUPrivate.RestoreInterruptMask(spl);
        RETURN;
      END;

      (* last read is out, is there a writer at the line *)
      IF rwl.writer # NIL THEN
        Broadcast(rwl.writer,rwl.writerVal);          
        CPUPrivate.RestoreInterruptMask(spl);
        RETURN;
      END;
    ELSIF rwl.writerCnt > 0 THEN
      DEC(rwl.writerCnt);
      IF rwl.writerCnt = 0 THEN
        IF LOOPHOLE(rwl.lastlocker,Thread.T) # NIL THEN
          IF paranoid THEN
	    StrongRef.Remove(LOOPHOLE(rwl.lastlocker, Thread.T));
          END;
          LOOPHOLE(rwl.lastlocker,Thread.T) := NIL;
        END;
      END;
    END;

    (* if last gate opened was a reader and a writer is waiting,
       then open the writer gate. *)
    IF rwl.readerCnt = 0 AND rwl.reader = NIL AND rwl.writer # NIL THEN
      Broadcast(rwl.writer,rwl.writerVal);
    ELSIF rwl.reader # NIL THEN
      (* wake up thread sleeping on read lock *)
      Broadcast(rwl.reader,rwl.readerVal);
    ELSE
      (* there is noone waiting. *)
    END;
    CPUPrivate.RestoreInterruptMask(spl);
  END ulock_done;

PROCEDURE ulock_try_write(VAR rwl: T): BOOLEAN = 
  VAR myno: INTEGER;
      myThread: Thread.T;
      spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    INC(rwl.writerCnt);
    myno := rwl.writerCnt; (* AtomicOps.AtomicInc32(rwl.writerCnt); *)
    IF myno # 1 OR rwl.readerCnt > 0 THEN
      DEC(rwl.writerCnt); 
      (* EVAL AtomicOps.AtomicDec32(rwl.writerCnt); *)
      CPUPrivate.RestoreInterruptMask(spl);
      RETURN FALSE;
    END;
    myThread := NARROW(Strand.GetCurrent(),Thread.T);
    IF paranoid THEN StrongRef.Add(myThread); END;
    LOOPHOLE(rwl.lastlocker,Thread.T) := myThread;
    CPUPrivate.RestoreInterruptMask(spl);
    RETURN TRUE;
  END ulock_try_write;

PROCEDURE ulock_try_read(VAR rwl: T): BOOLEAN = 
  VAR
    spl: CPU.InterruptLevel;
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
    INC(rwl.readerCnt);
    (* EVAL AtomicOps.AtomicInc32(rwl.readerCnt); *)
    IF rwl.writerCnt > 0 THEN
      DEC(rwl.readerCnt);
      CPUPrivate.RestoreInterruptMask(spl);
      (* EVAL AtomicOps.AtomicDec32(rwl.readerCnt); *)
      RETURN FALSE;
    END;
    CPUPrivate.RestoreInterruptMask(spl);
    RETURN TRUE;
  END ulock_try_read;

PROCEDURE dummy(<*UNUSED*>VAR rwl: T) = 
  BEGIN
  END dummy;

BEGIN
  ULockForSALExtern.ULockInterface := ULockForSALExtern.T {
    ulock_setup,
    ulock_terminate,
    ulock_write,
    ulock_read,
    ulock_done,
    ulock_try_write,
    ulock_try_read,
    dummy,
    dummy,
    dummy
  };
END ULockForSAL.
