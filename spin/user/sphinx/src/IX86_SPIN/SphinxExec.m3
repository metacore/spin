MODULE SphinxExec EXPORTS SphinxExec, SphinxPrivate;

IMPORT UserSpaceThread;
IMPORT CPU;
IMPORT Space;
IMPORT VMError;
IMPORT Sieg;
IMPORT Word;
IMPORT MachineMem;
IMPORT Error, Errno;
IMPORT Proc, ProcRep;
IMPORT Strand;
IMPORT Exec;
IMPORT SphinxUtils;
IMPORT Sphinx;

PROCEDURE Execve (<*UNUSED*>us : Strand.T; VAR state : CPU.SavedState)
  RAISES {Error.E, VMError.E, Errno.E} =
  VAR
    path : TEXT;
    argc : INTEGER;
    argvAddr : Word.T;
    envc : INTEGER;
    environAddr : Word.T;
    argv : REF ARRAY OF TEXT;
    environ : REF ARRAY OF TEXT;
    addr : Word.T;
    proc := Proc.Self();
    c_ := Sieg.Context{caller:=UserSpaceThread.Self(), space :=proc};
    args_ : ARRAY [0..2] OF Word.T;
    threadState : UserSpaceThread.State;
    info : Exec.Info;
  BEGIN
    (* get our args *)
    Space.Read(c_.space, state.usp+4, VIEW(args_, ARRAY [0..11] OF CHAR));

    Sieg.UnpackCTEXT(c_, args_[0], path);
    IF SphinxUtils.Debug THEN
      SphinxUtils.Msg("execve", path);
    END;
    
    argvAddr:=VAL(args_[1], Word.T);
    environAddr:=VAL(args_[2], Word.T);
    
    (* read argv and environ from caller *)
    (* The OSF limit on args is a number of bytes in argv and env
       called ARG_MAX.
       /usr/include/sys/syslimits.h:#define    ARG_MAX         38912 /* max bytes for an exec function */

       our limit will be 32k args.
     *)
    argc := 0;
    argv := NEW(REF ARRAY OF TEXT, 16_8000);
    LOOP
      (* read the address in argv[i].  A NIL means end of list. *)
      Space.Read(c_.space, argvAddr+argc*BYTESIZE(Word.T),
		 VIEW(addr, ARRAY[1..BYTESIZE(Word.T)] OF CHAR),
		 BYTESIZE(Word.T));
      IF addr = 0 THEN EXIT END;

      (* get the C string pointed to by argv[i] *)
      Sieg.UnpackCTEXT(c_, addr, argv[argc]);
      INC(argc);
    END;

    envc := 0;
    environ := NEW(REF ARRAY OF TEXT, 16_8000);
    LOOP
      (* read the address in env[i].  A NIL means end of list. *)
      Space.Read(c_.space, environAddr+envc*BYTESIZE(Word.T),
		 VIEW(addr, ARRAY[1..BYTESIZE(Word.T)] OF CHAR),
		 BYTESIZE(Word.T));
      IF addr = 0 THEN EXIT END;

      (* get the C string pointed to by argv[i] *)
      Sieg.UnpackCTEXT(c_, addr, environ[envc]);
      INC(envc);
    END;

    (* XXX we should kill all other threads are operating in this space *)
    c_.space.clear(); (* out with the old, before in with the new *)
    
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

    proc.path := path;
  END Execve;

PROCEDURE ForkReturn(
     parentPid       : INTEGER;
     childPid        : INTEGER; 
     VAR parentState : CPU.SavedState;
     VAR childState  : CPU.GeneralRegs) =
  BEGIN
    parentState.eax := childPid;
    parentState.edx := 0;
    
    childState.eax := parentPid;
    childState.edx := 1;
  END ForkReturn;



BEGIN
END SphinxExec.
