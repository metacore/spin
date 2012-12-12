(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE SyscallBoot;
IMPORT IO;
IMPORT USyscall;
IMPORT Proc, ProcRep;
IMPORT Text;
IMPORT Dispatcher;
IMPORT Trap;
FROM SphinxUtils IMPORT Msg;

TYPE RdvClosure = REF RECORD
  syscallHandler: PROCANY;
  maxID, minID: INTEGER;
END;
  
FUNCTIONAL
PROCEDURE RdvGuard (name1, name2: TEXT;
		    <*UNUSED*>key: REFANY;
		    <*UNUSED*>VAR repl: REFANY): BOOLEAN =
  BEGIN
    RETURN Text.Equal(name1, name2);
  END RdvGuard;

PROCEDURE Rdv (r: RdvClosure;
	       <*UNUSED*>name: TEXT;
	       <*UNUSED*>key: REFANY;
	       <*UNUSED*>VAR repl: REFANY): USyscall.ErrorCode =
  VAR
    proc := Proc.Self();
    binding: Dispatcher.Binding;
  BEGIN
    (* XXX remember to hack the default handler for the rdv event. *)
    TRY
      binding := Dispatcher.InstallHandler(Trap.Syscall,
					   NIL,
					   r.syscallHandler,
					   key := NEW(Trap.AuthKey,
						      strand := proc.thread, 
						      minProcID := r.minID,
						      maxProcID := r.maxID));
      Proc.AddBinding(proc, binding);
    EXCEPT
    | Dispatcher.Error =>
      Msg("SyscallBoot.Rdv: dispatcher error");
    END;
    RETURN USyscall.Success;
  END Rdv;
  
PROCEDURE Setup (name: TEXT;
		 syscallHandler: PROCANY;
		 minID, maxID: INTEGER) =
  VAR cl: RdvClosure;
  BEGIN
    cl := NEW(RdvClosure,
	      syscallHandler := syscallHandler,
	      minID := minID,
	      maxID := maxID);
    TRY
      EVAL Dispatcher.InstallHandler(USyscall.Rendezvous,
				     guard := RdvGuard,
				     guardClosure := name,
				     handler := Rdv,
				     handlerClosure := cl);
    EXCEPT
    | Dispatcher.Error =>
      IO.Put("SyscallBoot.Rdv: dispatcher error.\n");
    END;
  END Setup;
		
BEGIN
END SyscallBoot.
