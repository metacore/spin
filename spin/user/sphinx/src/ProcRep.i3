(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 08-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added FindAtom and InternAtom to make transaction life easier.
 * 13-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Facelift.
 * 6-21-96  becker at the University of Washington
 *	Added un/installRendezvous
 #	Changed T to track breakPage
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 11-Jun-96 oystr at the University of Washington
 *	Add file descriptor manipulation.
 * 11-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(*
 
 This module defines the process and the process group data structures.
 
*)

INTERFACE ProcRep;
IMPORT Proc;
IMPORT BsdSignal;
IMPORT Dispatcher, Space, Ctypes, Thread;
IMPORT UserSpaceThread;
IMPORT Word;
IMPORT Types;
IMPORT ProcQ;
IMPORT OpenFile;
IMPORT RefRefTbl;
IMPORT NameServer;
IMPORT Sphinx;

CONST MAX_NOFILE = 64;

TYPE
  T = Proc.T;
  
REVEAL
  Proc.T = Space.T BRANDED OBJECT
    prev, next: Proc.T;
    (* Linked to parent activeChildren or otherChildren *)
    
    mu: MUTEX;
    pid: Types.Pid;
    state: Proc.State;
    tmpbuf: REF ARRAY OF CHAR;
    exit: Ctypes.unsigned_int; (* Exit status, or signal status,
				 passed back to the parent proc
				 through waitpid(2). *)

    cwd: NameServer.T;
    parent: Proc.T; (* my parent proc *)
    grp: Proc.Group; (* The proc group I belong to.
		  Note that there is no separate group table.
		  The group "gid" is found by first looking up a 
		  process whose pid is "gid", and then following the
		  "grp" pointer.
		  Thus, a process whose is now a group leader can't
		  move itself to another group, because if you do so,
		  the original group will be orphaned. *)
    
    break: Word.T; (* current brk, i.e. current end of data seg. *)
    stackTop: Word.T; (* current stack top *)
    
    sigMask: Proc.SigSet; (* list of masked signals. '1' means that
		       the signal is blocked. *)
    sig: Proc.SigSet; (* list of delivered signals. '1' means that
		   the signal is pending *)
    
    thread: UserSpaceThread.T; (* representative thread of this space.
				Currently, the user space app
				can't be multi-threaded, so there is
				one to one correspondence between
				"space" and "thread". *)
    
    cond: Thread.Condition; (* This condition is used for multiple purposes.
			      
			      First, it is used in sigsuspend. When a child
			      of the process changes it's status, it
			      moves itself to the otherChildren chain, and
			      signals this condition variable.

			      Next, it is used to implement SIGSTOP
			      (and SIGTTIN, etc). When the "state" of the
			      process is Stopped, the process loops
			      waiting for signal, until the state changes
			      to Active.
			      
			      *)
    
    activeChildren: ProcQ.T; (* list of child procs that are
			      currently running *)
    otherChildren: ProcQ.T; (* list of child procs who has changed
			  it's status, and waiting for the parent
			  to wait. XXX change the name *)
    binding: ARRAY [0 .. 16] OF Dispatcher.Binding;
    sigHandler: ARRAY [0 .. BsdSignal.Max] OF Proc.SigArgs;
    
    fdTable: ARRAY [0..MAX_NOFILE-1] OF OpenFile.T;
    closeOnExec: ARRAY [0..MAX_NOFILE-1] OF BITS 1 FOR BOOLEAN;
      (* Infamous CLOEXEC flag. see fcntl(2) *)
    timer: Sphinx.ItimerVal;	(* to support set/getitimer *)
      
    atomTable: RefRefTbl.T;

    (* Below are just used for debugging & ps output *)
    startTime: INTEGER; (* UTC when the proc started. *)
    nthSyscall: INTEGER; (* used by syscall tracer. *)
    END;
    
CONST Brand = "ProcRep";
END ProcRep.
