(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Dec-98  Tsutomu Owa (owa) at the University of Washington
 *	Fixed Sigreturn.
 *
 * 14-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This module implements system calls specific to FreeBSD. *)

MODULE SphinxBSD EXPORTS Sphinx;
IMPORT Word;
IMPORT Translation;
IMPORT Sieg;
IMPORT Proc, ProcRep;
IMPORT VMError, Error;
IMPORT Errno, ErrnoDep;
IMPORT CPU;
IMPORT UserSpaceThread;
IMPORT Exec;
IMPORT SphinxUtils;
IMPORT Strand;
IMPORT Sphinx;
IMPORT MachineMem;
IMPORT MachineSigContext;
IMPORT IO, Fmt;
IMPORT NameServer, FileSystem;
IMPORT FileStat, Spy;

CONST DEBUG = FALSE;

PROCEDURE ReadArgv (space: Translation.T;
		    argvAddr: Word.T;
		    (*OUT*)VAR argc: INTEGER): REF ARRAY OF TEXT
    RAISES {VMError.E} =
  VAR 
    argv := NEW(REF ARRAY OF TEXT, 16_8000);
    addr: Word.T;
  BEGIN
    argc := 0;
    LOOP
      (* read the address in argv[i].  A NIL means end of list. *)
      Translation.Read(space, argvAddr+argc*BYTESIZE(Word.T),
		       VIEW(addr, ARRAY OF CHAR));
      IF addr = 0 THEN EXIT END;

      (* get the C string pointed to by argv[i] *)
      argv[argc] := Sieg.UnpackCTEXT(space, addr);
      INC(argc);
    END;
    RETURN  argv;
  END ReadArgv;

PROCEDURE Execve (<*UNUSED*>us : Strand.T; VAR state : CPU.SavedState)
  RAISES {Error.E, VMError.E, Errno.E} =
  VAR
    path : TEXT;
    argc : INTEGER;
    envc : INTEGER;
    argv : REF ARRAY OF TEXT;
    environ : REF ARRAY OF TEXT;
    proc : Proc.T := Translation.GetCurrent();
    args_ : ARRAY [0..2] OF Word.T;
    threadState : UserSpaceThread.State;
    info : Exec.Info;
  BEGIN
    (* get our args *)
    Translation.Read(proc, state.usp+4, VIEW(args_, ARRAY OF CHAR));

    path := Sieg.UnpackCTEXT(proc, args_[0]);
    IF SphinxUtils.Debug THEN SphinxUtils.Msg("execve", path); END;
    
    (* read argv and environ from caller *)
    argv := ReadArgv(proc, args_[1], argc);
    environ := ReadArgv(proc, args_[2], envc);

    (* XXX ought to see if path is an executable or not *)
    TRY
      EVAL FileSystem.Lookup(proc.cwd, path, TRUE);
    EXCEPT
    | NameServer.Error =>
      RAISE Errno.E(ErrnoDep.ENOENT);
    END;

    (* XXX we should kill all other threads are operating in this space *)
    proc.clear(); (* out with the old, before in with the new *)
    
    (* Close fds that are marked CLOEXEC. *)
    FOR i := FIRST(proc.fdTable) TO LAST(proc.fdTable) DO
      IF proc.fdTable[i] # NIL AND proc.closeOnExec[i] THEN
	EVAL Sphinx.Close(i);
      END;
    END;
	
    Exec.LoadFile(path,
		  SUBARRAY(argv^,0,argc), SUBARRAY(environ^,0,envc),
		  proc, threadState, info);
    proc.break := MachineMem.RoundToPage(info.break);
    (* XXX state := threadState.cpustate^ DOES NOT WORK 
    	You get pal faults and machine checks.
	I believe fields in the strand surrounding 'state' are corrupted
	by the assignment. db
     *)
    state.pc := threadState.cpustate.pc;
    state.usp := threadState.cpustate.usp;
    state.uss := threadState.cpustate.uss;
    state.eax := threadState.cpustate.eax;
    state.ds := threadState.cpustate.ds;
    state.es := threadState.cpustate.es;
    state.cs := threadState.cpustate.cs;
    state.eflags := threadState.cpustate.eflags;

    CPU.SetUserFloatRegs(threadState.fpustate^);
  END Execve;

PROCEDURE Sigreturn (VAR ss : CPU.SavedState;
		     READONLY c: MachineSigContext.T) =
  VAR
    proc : Proc.T := Translation.GetCurrent();
  BEGIN
    proc.sigMask := c.mask;
    ss.pc := c.pc;
    ss.usp := c.sp;
    ss.ebp := c.ebp;
    ss.eflags := c.efl;
    ss.es := c.es;
    ss.ds := c.ds;
    ss.cs := c.cs;
    ss.uss := c.ss;
    ss.edi := c.edi;
    ss.esi := c.esi;
    ss.ebx := c.ebx;
    ss.edx := c.edx;
    ss.ecx := c.ecx;
    ss.eax := c.eax;
  END Sigreturn;

BEGIN
END SphinxBSD.

