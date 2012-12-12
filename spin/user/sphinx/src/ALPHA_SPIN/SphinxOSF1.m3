(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This file implements the system calls that appear only in DEC OSF/1.
   Note that in FreeBSD, getfoobar system calls are mostly implemented
   using sysctl(2).*)

MODULE SphinxOSF1 EXPORTS Sphinx;
IMPORT Word;
IMPORT Text;
IMPORT MachineMem;
IMPORT Salnet;
IMPORT Errno;
IMPORT Ctypes;
IMPORT VMError;
IMPORT Sieg;
IMPORT CPU;
IMPORT Translation;
IMPORT Error;
IMPORT Strand;
IMPORT Proc, ProcRep;
IMPORT Exec;
IMPORT UserSpaceThread;
IMPORT MachineSigContext;

FROM SphinxUtils IMPORT Debug, Msg;
PROCEDURE Getpagesize () : INTEGER =
  BEGIN
    RETURN MachineMem.PAGESIZE;
  END Getpagesize;
  
PROCEDURE Gethostid () : INTEGER =
  VAR hID : Word.T;
  BEGIN
    hID := Salnet.GetLocalIp();
    hID := Word.Shift(hID,-32);
    RETURN hID;
  END Gethostid;

PROCEDURE Uname (VAR buf: Utsname) =
  BEGIN
    Text.SetChars(buf.sysname, "OSF1");
    Text.SetChars(buf.nodename, "unknown"); (*XXX no way to know the
					     hostname *)
    Text.SetChars(buf.release, "V3.2");
    Text.SetChars(buf.version, "214");
    Text.SetChars(buf.machine, "alpha"); 
  END Uname;
  
PROCEDURE Sigsetmask (mask : Ctypes.unsigned_long)
  	: Ctypes.unsigned_long RAISES {Errno.E} =
  BEGIN
    RETURN Sigprocmask(3, mask);
  END Sigsetmask;
  
PROCEDURE Sigblock (mask : Ctypes.unsigned_long)
  	: Ctypes.unsigned_long RAISES {Errno.E} =
  BEGIN
    RETURN Sigprocmask(1, mask);
  END Sigblock;

PROCEDURE ReadArgv (space_: Translation.T;
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
      Translation.Read(space_, argvAddr+argc*BYTESIZE(Word.T),
		       VIEW(addr, ARRAY OF CHAR));
      IF addr = 0 THEN EXIT END;

      (* get the C string pointed to by argv[i] *)
      argv[argc] := Sieg.UnpackCTEXT(space_, addr);
      INC(argc);
    END;
    RETURN  argv;
  END ReadArgv;
  
PROCEDURE Execve (<*UNUSED*>us : Strand.T; VAR state : CPU.SavedState)
  RAISES {Errno.E, VMError.E, Error.E} =
  VAR
    path : TEXT;
    argc := 0;
    argv : REF ARRAY OF TEXT;
    
    envc := 0;
    environ: REF ARRAY OF TEXT;
    info : Exec.Info;
    proc := Proc.Self();
    threadState: UserSpaceThread.State;
  BEGIN
    (* get our args *)
    path := Sieg.UnpackCTEXT(proc, state.a0);
    IF Debug THEN Msg("execve: ", path); END;
    
    (* read argv and environ from caller *)
    argv := ReadArgv(proc, state.a1, argc);
    environ := ReadArgv(proc, state.a2, envc);

    (* XXX we should kill all other threads are operating in this space *)
    proc.clear(); (* out with the old, before in with the new *)

    (* Close fds that are marked CLOEXEC. *)
    FOR i := FIRST(proc.fdTable) TO LAST(proc.fdTable) DO
      IF proc.fdTable[i] # NIL AND proc.closeOnExec[i] THEN
	EVAL Close(i);
      END;
    END;
    
    Exec.LoadFile(path,
		  SUBARRAY(argv^,0,argc),
		  SUBARRAY(environ^,0,envc),
		  proc, threadState, info);

    (* XXX state := threadState.cpustate^ DOES NOT WORK 
    	You get pal faults and machine checks.
	I believe fields in the strand surrounding 'state' are corrupted
	by the assignment. db

       IS THIS STILL TRUE? SOMEONE NEEDS TO CHECK -- yas
     *)
    state.pc := threadState.cpustate.pc;
    state.gp := threadState.cpustate.gp;
    state.usp := threadState.cpustate.usp;
    state.a0 := threadState.cpustate.a0;
    state.ra := threadState.cpustate.ra;
    state.pv := threadState.cpustate.pv;

    (* XXX should change the name (addrspace.print()) here to aid
       debugging. *)
    proc.break := MachineMem.RoundToPage(info.break);
    proc.stackTop := info.stackTop;
  END Execve;
PROCEDURE Sigreturn (VAR ss : CPU.SavedState;
		     context: INTEGER) RAISES {VMError.E} =
  VAR
    proc := Proc.Self();
    
  PROCEDURE Sub (VAR x: ARRAY OF CHAR) =
    BEGIN
      WITH c = VIEW(x, MachineSigContext.T) DO
	proc.sigMask := c.mask;
	ss.pc := c.pc;
	ss.v0 := c.regs[0];
	ss.t0 := c.regs[1];
	ss.t1 := c.regs[2];
	ss.t2 := c.regs[3];
	ss.t3 := c.regs[4];
	ss.t4 := c.regs[5];
	ss.t5 := c.regs[6];
	ss.t6 := c.regs[7];
	ss.t7 := c.regs[8];
	ss.s0 := c.regs[9];
	ss.s1 := c.regs[10];
	ss.s2 := c.regs[11];
	ss.s3 := c.regs[12];
	ss.s4 := c.regs[13];
	ss.s5 := c.regs[14];
	ss.s6 := c.regs[15];
	ss.a0 := c.regs[16];
	ss.a1 := c.regs[17];
	ss.a2 := c.regs[18];
	ss.a3 := c.regs[19];
	ss.a4 := c.regs[20];
	ss.a5 := c.regs[21];
	ss.t8 := c.regs[22];
	ss.t9 := c.regs[23];
	ss.t10 := c.regs[24];
	ss.t11 := c.regs[25];
	ss.ra := c.regs[26];
	ss.pv := c.regs[27];
	ss.at := c.regs[28];
	ss.gp := c.regs[29];
	ss.usp := c.regs[30];
	IF FALSE THEN
	  CPU.SetUserFloatRegs(VIEW(SUBARRAY(VIEW(c, ARRAY OF INTEGER), 37, 33),
					   CPU.FloatRegs));
	END;
      END;
    END Sub;
  BEGIN
    Translation.Access(proc, context, BYTESIZE(MachineSigContext.T), Sub);
  END Sigreturn;

  
BEGIN
END SphinxOSF1.
