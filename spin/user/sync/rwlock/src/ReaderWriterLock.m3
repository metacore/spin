(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 10-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Reader/writer locks. Multiple readers, single writer.
 *
 *)
MODULE ReaderWriterLock;
IMPORT AtomicOps, Sema;

REVEAL T = BRANDED "ReaderWriterLock" OBJECT
  readerCnt: INTEGER := 0;
  writerCnt: INTEGER := 0;
  writerQ: Sema.T;
  readerQ: Sema.T;
END;

PROCEDURE Allocate() : T =
  VAR rwl: T;
  BEGIN
    rwl := NEW(T);
    rwl.readerQ := Sema.Alloc(0);
    rwl.writerQ := Sema.Alloc(0);
    RETURN rwl;
  END Allocate;

PROCEDURE ReaderLock(rwl: T) =
  BEGIN
    EVAL AtomicOps.AtomicInc(rwl.readerCnt);
    WHILE rwl.writerCnt > 0 DO
      (*
       * wait as long as there is a writer in progress.
       *)
      EVAL AtomicOps.AtomicDec(rwl.readerCnt);
      Sema.P(rwl.readerQ);
      EVAL AtomicOps.AtomicInc(rwl.readerCnt);
    END;
  END ReaderLock;

PROCEDURE ReaderUnlock(rwl: T) =
  BEGIN
    IF AtomicOps.AtomicDec(rwl.readerCnt) = 0 THEN
      (*
       * wake up a waiting writer and make it recheck.
       *)
      Sema.Broadcast(rwl.writerQ);
    END;
  END ReaderUnlock; 

PROCEDURE WriterLock(rwl: T) =
  VAR myno: INTEGER;
  BEGIN
    myno := AtomicOps.AtomicInc(rwl.writerCnt);
    WHILE myno # 1 OR rwl.readerCnt > 0 DO
      (*
       * Wait as long as there is a reader or a writer in progress.
       *)
      EVAL AtomicOps.AtomicDec(rwl.writerCnt);
      Sema.P(rwl.writerQ);
      myno := AtomicOps.AtomicInc(rwl.writerCnt);
    END;
  END WriterLock; 

PROCEDURE WriterUnlock(rwl: T) =
  BEGIN
    IF AtomicOps.AtomicDec(rwl.writerCnt) = 0 THEN
      (*
       * wake up the waiting readers and writers.
       *)
      Sema.Broadcast(rwl.readerQ);
      Sema.Broadcast(rwl.writerQ);
    END;
  END WriterUnlock; 

PROCEDURE ReaderTryLock(rwl: T) : BOOLEAN =
  BEGIN
    EVAL AtomicOps.AtomicInc(rwl.readerCnt);
    IF rwl.writerCnt > 0 THEN
      EVAL AtomicOps.AtomicDec(rwl.readerCnt);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END ReaderTryLock; 

PROCEDURE WriterTryLock(rwl: T) : BOOLEAN =
  VAR myno: INTEGER;
  BEGIN
    myno := AtomicOps.AtomicInc(rwl.writerCnt);
    WHILE myno # 1 OR rwl.readerCnt > 0 DO
      (*
       * Wait as long as there is a reader or a writer in progress.
       *)
      EVAL AtomicOps.AtomicDec(rwl.writerCnt);
      RETURN FALSE;
    END;
    RETURN TRUE;
  END WriterTryLock; 

BEGIN
END ReaderWriterLock.
