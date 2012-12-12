(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(* This file defines the system call interface to the transaction manager.
*)

INTERFACE TransSyscall;
IMPORT VMError;
IMPORT Word;
IMPORT Error;

<*EPILOG_BRANCH_ON_REGISTER _Seterrno*>
<*INTERFACE_PROC_BASE 3600*>

TYPE
  T = INTEGER;
  FD = INTEGER;
  Stat = RECORD
    size: INTEGER;
  END;
  
<*SYNONYMS trans_null,_trans_null*>
PROCEDURE Null();
  
<*SYNONYMS _trans_begin*>
PROCEDURE Begin(flags: INTEGER): T RAISES {Error.E};
  
<*SYNONYMS _trans_commit*>
PROCEDURE Commit(t: T): BOOLEAN RAISES {Error.E};

<*SYNONYMS trans_abort, _trans_abort*>
PROCEDURE Abort(t: T) RAISES {Error.E};

<*SYNONYMS _trans_open*>
PROCEDURE Open(<*AS CTEXT*>path: TEXT): FD RAISES {Error.E};

<*SYNONYMS trans_getstat*>
PROCEDURE GetStat(fd: FD; VAR stat: Stat) RAISES {Error.E};
    
<*SYNONYMS _trans_mmap*>
PROCEDURE Mmap(addr, len, prot, flags: Word.T; fd: FD; off: Word.T): INTEGER
  RAISES {Error.E, VMError.E};
(* This is mmap clone. This has to be merged into sphinx mmap as soon as
   caching file system design is completed.
   Only "addr", "len", "fd" are meaningful now. All other arguments are
   ignored.
*)

<*SYNONYMS _trans_munmap*>
PROCEDURE Munmap(addr, len: Word.T): INTEGER RAISES {Error.E, VMError.E};
  
<*SYNONYMS trans_close,_trans_close*>
PROCEDURE Close(st: FD) RAISES {Error.E};
(* Close down the storage "st". *)
  
<*SYNONYMS trans_flush_logs,_trans_flush_logs*>
PROCEDURE FlushLogs();

<*SYNONYMS trans_barrier, _trans_barrier*>  
PROCEDURE Barrier(n: INTEGER);
(* Just used for a debugging. Establish barrier synch. among "n" procs. *)

END TransSyscall.


