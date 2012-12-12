(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransSyscall;
IMPORT ProcRep;
IMPORT Word;
IMPORT Error;
IMPORT VMError;
IMPORT CPU;
IMPORT Transaction;
IMPORT TransProc;
IMPORT Storage, StorageRep;
IMPORT WAL;
IMPORT TransMode;
IMPORT Spy;
IMPORT Thread;

IMPORT TransUtils;

VAR
  nullTimer, beginTimer, setRangeTimer, commitTimer: Spy.T;

PROCEDURE Null () =
  BEGIN
    IF TransUtils.SpyTime THEN Spy.Enter(nullTimer); END;
    IF TransUtils.SpyTime THEN Spy.Exit(nullTimer); END;
  END Null;
  
PROCEDURE Begin (flags: INTEGER): T = 
  VAR
    mode: TransMode.Set;
    proc: TransProc.T;
    t: T;
  BEGIN
    IF TransUtils.SpyTime THEN Spy.Enter(beginTimer); END;
    mode := TransMode.WordToT(flags);
    proc := TransProc.Self();
    proc.curTrans := Transaction.Begin(proc, mode);
    t := TransProc.AddObject(proc, proc.curTrans);
    IF TransUtils.SpyTime THEN Spy.Exit(beginTimer); END;
    RETURN t;
  END Begin;

PROCEDURE Commit (et: T): BOOLEAN RAISES {Error.E} =
  VAR
    proc: TransProc.T;
    t: Transaction.T;
    status: BOOLEAN;
  BEGIN
    IF TransUtils.SpyTime THEN Spy.Enter(commitTimer); END;
    proc := TransProc.Self();    
    t := proc.obj[et];
    proc.curTrans := NIL;
    status := Transaction.Commit(t);
    TransProc.UnaddObject(proc, et);
    IF TransUtils.SpyTime THEN Spy.Exit(commitTimer); END;
    RETURN status;
  END Commit;
  
PROCEDURE Abort (et: T) RAISES {Error.E} =
  VAR
    proc := TransProc.Self();
    t: Transaction.T := proc.obj[et];
  BEGIN
    proc.curTrans := NIL;
    Transaction.Abort(t);
    TransProc.UnaddObject(proc, et);
  END Abort;
  
PROCEDURE Open (path: TEXT): FD RAISES {Error.E} =
  VAR
    proc := TransProc.Self();
    st := Storage.Open(path, proc);
  BEGIN
    <*ASSERT st # NIL*> (* in case of error, exception is raised. *)
    RETURN TransProc.AddObject(proc, st);
  END Open;

PROCEDURE GetStat (fd: FD; VAR stat: Stat) =
  VAR
    proc := TransProc.Self();
    st: Storage.T := proc.obj[fd];
  BEGIN
    stat.size := st.memObj.stat().virtualSize;
  END GetStat;
  
(* XXX this mmap has to be unified with sphinx mmap in the future. *)  
PROCEDURE Mmap (addr, len: Word.T;
		<*UNUSED*>prot, flags: Word.T;
		fd: FD; off: Word.T): INTEGER
  RAISES {VMError.E} =
  VAR
    page := addr DIV CPU.PAGESIZE;
    nPages := (addr+len-1) DIV CPU.PAGESIZE - page + 1;
    proc := TransProc.Self();
    st: Storage.T := proc.obj[fd];
  BEGIN
    off := off DIV CPU.PAGESIZE;
    proc.proc.allocate(page, nPages, FALSE);
    proc.proc.map(page, nPages, st.memObj, off, TRUE);
    RETURN 0;
  END Mmap;

PROCEDURE Munmap (addr, len: Word.T): INTEGER RAISES {VMError.E} =
  VAR
    page := addr DIV CPU.PAGESIZE;
    nPages := (addr+len-1) DIV CPU.PAGESIZE - page + 1;
    proc := TransProc.Self();
  BEGIN
    proc.proc.unmap(page, nPages);
    proc.proc.deallocate(page, nPages);
    RETURN 0;
  END Munmap;
  
PROCEDURE Close (est: FD) =
  VAR
    proc := TransProc.Self();
    st: Storage.T := proc.obj[est];
  BEGIN
    st.close(proc);
    TransProc.UnaddObject(proc, est);
  END Close;

PROCEDURE FlushLogs () =
  BEGIN
    WAL.FlushLog();
  END FlushLogs;

VAR
  barrierMu := NEW(MUTEX);
  barrierCond := NEW(Thread.Condition);
  rest: CARDINAL := 0;
  
PROCEDURE Barrier (n: INTEGER) =
  BEGIN
    LOCK barrierMu DO
      IF rest = 0 THEN
	rest := n-1;
	Thread.Wait(barrierMu, barrierCond);
      ELSE
	DEC(rest);
	IF rest = 0 THEN
	  Thread.Broadcast(barrierCond);
	ELSE
	  Thread.Wait(barrierMu, barrierCond);
	END;
      END;
    END;
  END Barrier;
  
BEGIN
  IF TransUtils.SpyTime THEN
    nullTimer := Spy.Create("trans:null", FALSE, TransUtils.NItr);
    beginTimer := Spy.Create("trans:begin", FALSE, TransUtils.NItr);
    setRangeTimer := Spy.Create("trans:setrange", FALSE, TransUtils.NItr);
    commitTimer := Spy.Create("trans:commit", FALSE, TransUtils.NItr);
  END;
END TransSyscall.
