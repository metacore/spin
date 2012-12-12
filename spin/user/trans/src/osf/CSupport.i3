(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *)

(* This defines the psuedo systemcall interface for OSF clients written in
   C. *)
INTERFACE CSupport;
IMPORT Ctypes;
TYPE Stat = RECORD
  size: INTEGER;
END;
  
PROCEDURE trans_begin(flags: INTEGER): INTEGER;
PROCEDURE trans_commit(tid: INTEGER):INTEGER;
PROCEDURE trans_abort(tid: INTEGER);
PROCEDURE trans_open(fileName: Ctypes.char_star): INTEGER;
PROCEDURE trans_storage_base(sid: INTEGER): INTEGER;
PROCEDURE trans_mmap(addr,len,prot,flags,fd,off: INTEGER): INTEGER;
PROCEDURE trans_flush_logs();
PROCEDURE trans_close(sid: INTEGER): INTEGER;
PROCEDURE trans_munmap(addr, len: INTEGER): INTEGER;
PROCEDURE trans_getstat(sid: INTEGER; VAR stat: Stat): INTEGER;
PROCEDURE trans_pagefault(sid, tid, addr, type: INTEGER): INTEGER;

PROCEDURE usyscall_system(text: INTEGER): INTEGER;

  
END CSupport.
