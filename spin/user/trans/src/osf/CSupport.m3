(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted. Pass mode bits to Transaction.Begin.
 *)
UNSAFE MODULE CSupport;
IMPORT Spy;
IMPORT Transaction;
IMPORT Storage, StorageProtected, StorageRep;
IMPORT IO;
IMPORT M3toC;
IMPORT Ctypes;
IMPORT TransMode;
IMPORT TransGroup;
IMPORT WAL;
IMPORT TransCommands;
IMPORT LockMode;
IMPORT Buffer;
IMPORT CPU;
IMPORT Umman, Utypes;
IMPORT Trap;
IMPORT Word;
IMPORT Round;
IMPORT UNIXUtils;
IMPORT TransOS;
IMPORT Text;
VAR table: ARRAY [0..100] OF REFANY;
  (* mapping table between ref and integer ID. C clients specify
     refs by ID. *)
  
PROCEDURE Intern (t: REFANY): INTEGER =
  BEGIN
    FOR i := FIRST(table) TO LAST(table) DO
      IF table[i] = NIL THEN
	table[i] := t;
	RETURN i;
      END;
    END;
    <*ASSERT FALSE*>
  END Intern;

PROCEDURE Unintern (i: INTEGER) =
  BEGIN
    <*ASSERT table[i] # NIL*>
    table[i] := NIL;
  END Unintern;
  
PROCEDURE Find (i: INTEGER): REFANY =
  BEGIN
    <*ASSERT table[i] # NIL*>
    RETURN table[i];
  END Find;
  
PROCEDURE trans_begin (flags: INTEGER): INTEGER =
  BEGIN
    RETURN Intern(Transaction.Begin(TransGroup.default,
				    TransMode.WordToT(flags)));
  END trans_begin;
  
PROCEDURE trans_commit(tid: INTEGER): INTEGER =
  VAR status: BOOLEAN;
  BEGIN
    status := Transaction.Commit(Find(tid));
    Unintern(tid);
    IF status THEN 
      RETURN 1;
    END;
    RETURN 0;
  END trans_commit;

PROCEDURE trans_abort(tid: INTEGER) =
  BEGIN
    Transaction.Abort(Find(tid));
    Unintern(tid);
  END trans_abort;

PROCEDURE trans_open(name: Ctypes.char_star): INTEGER =
  BEGIN 
   RETURN Intern(Storage.Open(M3toC.CopyStoT(name), TransGroup.default));
  END trans_open;

PROCEDURE trans_storage_base(sid: INTEGER): INTEGER =
  VAR st: Storage.T := Find(sid);
  BEGIN
    RETURN st.memObj.base;
  END trans_storage_base;
		     
VAR mapBegin, mapLen: INTEGER;
VAR mapSt: Storage.T;
  
PROCEDURE trans_mmap(addr,len,prot,flags,fd,off: INTEGER): INTEGER =
  BEGIN
    mapSt := Find(fd);    
    mapBegin := addr;
    mapLen := len;
    mapSt.memObj.map(addr);
    RETURN 0;
  END trans_mmap;

PROCEDURE trans_munmap(addr, len: INTEGER): INTEGER =
  BEGIN
    mapSt.memObj.unmap(addr);
    mapSt := NIL;
    mapBegin := 0;
    mapLen := 0;
    RETURN 0;
  END trans_munmap;

PROCEDURE trans_getstat (sid: INTEGER; VAR stat: Stat): INTEGER =
  VAR
    st: Storage.T := Find(sid);    
  BEGIN
    stat.size := st.memObj.virtualSize();
    RETURN 0;
  END trans_getstat;
  
PROCEDURE trans_flush_logs() =
  BEGIN
    WAL.FlushLog();
  END trans_flush_logs;

PROCEDURE trans_close(sid: INTEGER): INTEGER =
  VAR
    st: Storage.T := Find(sid);    
  BEGIN
    st.close(TransGroup.default);
    RETURN 0;
  END trans_close;
  
PROCEDURE trans_pagefault(sid, tid, addr, type: INTEGER): INTEGER =
  VAR
    st: Storage.T := Find(sid);    
    tr: Transaction.T := Find(tid);
    at := StorageProtected.InternTrans(st, tr);
    allocated := TRUE;
    buf: Buffer.T;
    mode: Word.T;
  BEGIN
    Spy.Enter(pfSpy);
    LOCK st.mu DO
      IF type = Trap.Write THEN
	mode := Word.Or(Umman.PROT_READ, Umman.PROT_WRITE);
	buf := st.pinFrame(at, LockMode.T.Write, addr DIV CPU.PAGESIZE,
			   allocated, NIL);
      ELSE
	mode := Umman.PROT_READ;
	buf := st.pinFrame(at, LockMode.T.Read, addr DIV CPU.PAGESIZE,
			   allocated, NIL);
      END;
      Buffer.Unlock(buf);
      IF Umman.mprotect(LOOPHOLE(st.memObj.base+addr, Utypes.caddr_t),
			CPU.PAGESIZE, mode) < 0 THEN
	UNIXUtils.Perror("pagefault:mprotect");
      END;
    END;
    Spy.Exit(pfSpy);
    RETURN 0;
  END trans_pagefault;

PROCEDURE usyscall_system (text: INTEGER): INTEGER =
  VAR
    line: TEXT;
    argv: REF ARRAY OF TEXT;
  BEGIN
    line := M3toC.CopyStoT(LOOPHOLE(text, Ctypes.char_star));
    argv := UNIXUtils.Tokenize(line);
    
    IF Text.Equal(argv[0], "trans") THEN
      TransCommands.Execute(SUBARRAY(argv^, 1, NUMBER(argv^)-1));
    ELSIF Text.Equal(argv[0], "spy") THEN
      IF Text.Equal(argv[1], "reset") THEN
	Spy.Reset();
      ELSIF Text.Equal(argv[1], "dump") THEN
	Spy.Dump();
      END;
    ELSE
      IO.Put("Usyscall.System: only trans commands are supported.\n");
      TransOS.Exit(1);
    END;
    RETURN 0;
  END usyscall_system;
  
VAR
  pfSpy: Spy.T;
BEGIN
  pfSpy := Spy.Create("page-fault", TRUE, 400);
END CSupport.






