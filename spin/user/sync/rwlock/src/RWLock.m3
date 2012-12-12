(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *
 * HISTORY
 * 20-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Test for reader/writer locks.
 *)
MODULE RWLock;

IMPORT ParseParams, IO, ThreadExtra, ReaderWriterLock, Nice;
IMPORT RWLockInterface, Auth;

VAR
  rwl: ReaderWriterLock.T := ReaderWriterLock.Allocate();

TYPE Control = REF RECORD
  func: PROCEDURE(rwl: ReaderWriterLock.T);
END;
  
PROCEDURE RWLThread (arg: REFANY): REFANY =
  VAR c: Control;
  BEGIN
    c := NARROW(arg, Control);
    c.func(rwl);
    RETURN NIL;
  END RWLThread;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR c := NEW(Control);
  BEGIN
    TRY
      pp.reset();
      pp.skipNext();
      IF pp.keywordPresent("rlock") THEN
        c.func := ReaderWriterLock.ReaderLock;
      ELSIF pp.keywordPresent("wlock") THEN
        c.func := ReaderWriterLock.WriterLock;
      ELSIF pp.keywordPresent("runlock") THEN
        c.func := ReaderWriterLock.ReaderUnlock;
      ELSIF pp.keywordPresent("wunlock") THEN
        c.func := ReaderWriterLock.WriterUnlock;
      ELSE
        RAISE ParseParams.Error;
      END;
      EVAL ThreadExtra.PFork(RWLThread, c, Nice.Priority());
    EXCEPT
      ParseParams.Error =>
        IO.Put("Usage: rwl rlock runlock wlock wunlock\n");
    END;
    RETURN TRUE;
  END Run;



BEGIN
  EVAL RWLockInterface.Export(NEW (Auth.AuthAlways));
END RWLock.

