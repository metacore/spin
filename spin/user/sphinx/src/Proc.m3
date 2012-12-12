(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 * 11-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed the Iterator so we can list processes.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 6-21-96  becker at the University of Washington
 *	Added un/installRendezvous
 *	Changed WaitInternal to use osf semantics.
 *
 * 11-Jun-96 oystr at the University of Washington
 *	Add file descriptor manipulation.
 *
 * 19-May-96  Brian Bershad (bershad) at the University of Washington
 *	Added Dlib Rendezvous to allow apps to initialize their syscall
 *	 handling.
 *
 * 17-May-96  Brian Bershad (bershad) at the University of Washington
 *	Added InitializeThread.
 *
 * 11-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Proc;
IMPORT ProcRep;
IMPORT ProcQ;
IMPORT Translation;
IMPORT IntRefTbl;
IMPORT RefRefTbl;
IMPORT Thread;
IMPORT CharArray;
IMPORT Dispatcher;
IMPORT Sig;
IMPORT Errno;
IMPORT ErrnoDep;
IMPORT Types;
IMPORT RefQ;
IMPORT IO, Fmt;
IMPORT Mutex;
IMPORT OpenFile;
IMPORT SphinxExtension;
IMPORT SphinxPrivate;
IMPORT Trap;

FROM SphinxUtils IMPORT Msg;

CONST
  MaxPid = 65535;
  MinPid = 2; (* 1 is excluded because we must avoid using the "init" pid *)
  
VAR
  mu: MUTEX; (* guards "pidTable" *)
  pidTable: IntRefTbl.T; (* pid -> Proc.T *)
  
  lastPid: [MinPid .. MaxPid] := MinPid;
  (* The last+1 proc id created. The next process id
     will be searched from here *)

PROCEDURE Self (): T =
  BEGIN
    RETURN Translation.GetCurrent();
  END Self;

PROCEDURE Create (name: TEXT; parent: T): T =
  VAR
    pid: Types.Pid := 0;
    grp: Group;
    t: T;
    r: REFANY;
  BEGIN
    LOCK mu DO
      (* look for the first free pid *)
      pid := lastPid;
      LOOP 
	IF NOT pidTable.get(pid, r) THEN 
	  EXIT;
	END;
	
	IF pid = MaxPid THEN
	  pid := MinPid;
	ELSE
	  INC(pid);
	END;

	(* XXX assuming proc table never becomes full *)
      END;

      lastPid := pid;
      IF lastPid = MaxPid THEN
	lastPid := MinPid;
      ELSE
	INC(lastPid);
      END;

      name := name & ":" & Fmt.Int(pid);
      
      t := NEW(T,
	       mu := NEW(MUTEX),
	       pid := pid, parent := parent,
	       exit := 1, 
	       cond := NEW(Thread.Condition),
	       activeChildren := ProcQ.NewHeader(), 
	       otherChildren := ProcQ.NewHeader()).init(name);
      
      IF parent = NIL THEN
	grp := NEW(Group,
		   mu := NEW(MUTEX),
		   gid := pid, members := RefQ.NewHeader());
      ELSE
	grp := parent.grp;
      END;
      
      Sig.SetDefaultHandlers(t.sigHandler);
      
      AddToGroup(t, grp);

      IF parent # NIL THEN 
	ProcQ.InsertTail(parent.activeChildren, t);
      END;
    
      EVAL pidTable.put(pid, t);
    END;
    
    RETURN t;
  END Create;

PROCEDURE Destroy (proc: T) =
  VAR r: REFANY;
  BEGIN
    <*ASSERT Mutex.IsLocked(proc.mu)*>
    (* Uninstall the extensions *)
    FOR i := FIRST(proc.binding) TO LAST(proc.binding) DO
      IF proc.binding[i] # NIL THEN
	TRY 
	  Dispatcher.Uninstall(proc.binding[i]);
	EXCEPT
	| Dispatcher.Error =>
	  IO.Put("exit : Can't uninstall the binding.\n");
	END;
      END;
    END;
    RemoveFromCurrentGroup(proc);
    
    IF proc.parent # NIL THEN
      LOCK proc.parent.mu DO 
	ProcQ.Remove(proc);
      END;
    END;
    
    LOCK mu DO 
      EVAL pidTable.delete(proc.pid, r);
    END;
  END Destroy;

PROCEDURE FindFromID (pid : Types.Pid) : T RAISES {Errno.E} =
  VAR
    r : REFANY;
  BEGIN
    LOCK mu DO 
      IF pidTable.get(pid, r) THEN
	RETURN r;
      END;
    END;
    RAISE Errno.E(ErrnoDep.ESRCH);
  END FindFromID;

PROCEDURE InstallStandardHandlers (proc: T) =
  VAR
    binding: Dispatcher.Binding;
  BEGIN
    TRY
      binding := Dispatcher.InstallHandler(Trap.Syscall, NIL, 
					   SphinxExtension.Syscall, 
                       key := NEW(Trap.AuthKey,
				  strand := proc.thread, 
				  minProcID := SphinxExtension.MinProcID,
				  maxProcID := SphinxExtension.MaxProcID));
      AddBinding(proc, binding);
      binding := Dispatcher.InstallHandler(Trap.InvalidTranslation,
					   NIL, SphinxPrivate.PageFault,
					   key := proc);
      AddBinding(proc, binding);
      binding := Dispatcher.InstallHandler(Trap.AccessViolation,
					   NIL, SphinxPrivate.PageFault,
					   key := proc);
      AddBinding(proc, binding);
    EXCEPT
    | Dispatcher.Error(ec) =>
      Msg("InstallStandardHandlers: error", Fmt.Int(ORD(ec)));
    END;
  END InstallStandardHandlers;
			  
PROCEDURE AddBinding (t: T; binding: Dispatcher.Binding) =
  BEGIN
    FOR i := FIRST(t.binding) TO LAST(t.binding) DO
      IF t.binding[i] = NIL THEN
	t.binding[i] := binding;
	RETURN;
      END;
    END;
    Msg("AddBinding : adding more than ", Fmt.Int(NUMBER(t.binding)));
  END AddBinding;

PROCEDURE AddToWaitList (proc: T) =
  VAR
    parent := proc.parent;
  BEGIN
    <*ASSERT Mutex.IsLocked(proc.mu)*>
    IF parent # NIL THEN
      LOCK parent.mu DO 
	<*ASSERT ProcQ.Member(parent.activeChildren, proc)
	     OR  ProcQ.Member(parent.otherChildren, proc)*>
	ProcQ.Remove(proc);
	ProcQ.InsertTail(parent.otherChildren, proc);
      END;
    END;
  END AddToWaitList;
  


(*
 
 Group management

 *)

PROCEDURE FindGroupFromID (gid: Types.Pid): Group RAISES {Errno.E} =
  VAR proc := FindFromID(gid);
  BEGIN
    IF proc.pid = proc.grp.gid THEN RETURN proc.grp; END;
    RAISE Errno.E(ErrnoDep.ESRCH);
  END FindGroupFromID;

(* See if the process "proc" is a member of the process "group". *)
PROCEDURE IsMemberOfGroup (proc: T; grp: Group): BOOLEAN =
  VAR
    itr : RefQ.Iterator;
    rq : RefQ.T;
  BEGIN
    BEGIN
      LOCK grp.mu DO 
	itr := RefQ.Iterate(grp.members);
	WHILE RefQ.NextItr(itr, rq) DO
	  IF rq.data = proc THEN
	    RETURN TRUE;
	  END;
	END;
	RETURN FALSE;
      END;
    END;
  END IsMemberOfGroup;
  
PROCEDURE RemoveFromCurrentGroup (proc: T) =
  VAR
    itr : RefQ.Iterator;
    rq : RefQ.T;
  BEGIN
    <*ASSERT IsMemberOfGroup(proc, proc.grp)*>
    LOCK proc.grp.mu DO 
      itr := RefQ.Iterate(proc.grp.members);
      WHILE RefQ.NextItr(itr, rq) DO
	IF rq.data = proc THEN
	  RefQ.Remove(rq);
	  proc.grp := NIL;
	  RETURN;
	END;
      END;
    END;
    <*ASSERT FALSE*>
  END RemoveFromCurrentGroup;
  
(* Add the process "proc" to a process "group". Process should not be
   a member of any group prior to call of this proc.

 *)
PROCEDURE AddToGroup (proc: T; grp: Group) =
  VAR rq := NEW(RefQ.T, data := proc);
  BEGIN
    <*ASSERT NOT IsMemberOfGroup(proc, grp)*>
    <*ASSERT proc.grp = NIL*>
    LOCK grp.mu DO 
      RefQ.InsertTail(grp.members, rq);
      proc.grp := grp;
    END;
  END AddToGroup;

(*
 
   Iterator

 *)

REVEAL Iterator = IteratorPublic BRANDED OBJECT
  (* XXX (idx : INTEGER;) taken out by mef *)
  iter : IntRefTbl.Iterator;
OVERRIDES
  next := NextItr;
END;

PROCEDURE Iterate () : Iterator =
  VAR itr : Iterator;
  BEGIN
    itr := NEW(Iterator);
    itr.iter := pidTable.iterate();
    RETURN itr;
  END Iterate;

PROCEDURE NextItr (itr : Iterator; VAR proc : T) : BOOLEAN =
  VAR  
    key   : INTEGER;
    entry : REFANY;
    more  : BOOLEAN;
  BEGIN
    more := itr.iter.next(key,entry);
    IF more THEN
      proc := NARROW(entry,T);
    ELSE
      proc := NIL;
    END;
    RETURN more;
  END NextItr;

  
(*
 * File descriptor management.
 *)

PROCEDURE AllocateFD (t: T; fh: OpenFile.T) : INTEGER RAISES {Errno.E} =
  BEGIN
    <*ASSERT NOT Mutex.TryLock(t.mu)*>
    FOR i := FIRST(t.fdTable) TO LAST(t.fdTable) DO
      IF t.fdTable[i] = NIL THEN 
	t.fdTable[i] := fh;
	t.closeOnExec[i] := FALSE;
	INC(fh.refCount);
	RETURN i;
      END;
    END;
    RAISE Errno.E(ErrnoDep.EMFILE);
  END AllocateFD;
  
PROCEDURE AllocateFDatSlot (t: T; fh: OpenFile.T; fd: INTEGER) =
  BEGIN
    <*ASSERT NOT Mutex.TryLock(t.mu)*>
    <*ASSERT t.fdTable[fd] = NIL*>
    t.fdTable[fd] := fh;
    t.closeOnExec[fd] := FALSE;
    INC(fh.refCount);
  END AllocateFDatSlot;

PROCEDURE FindFH (proc: T; fd: INTEGER): OpenFile.T RAISES {Errno.E} =
  VAR fh: OpenFile.T;
  BEGIN
    fh := proc.fdTable[fd]; (* Note: out-of-range FD will raise
			       a spin exception, and it will be converted
			       into EINVAL. *)
    IF fh = NIL THEN
      RAISE Errno.E(ErrnoDep.EBADF);
    END;
    RETURN fh;
  END FindFH;

PROCEDURE InternAtom (proc: T; key, val: REFANY): BOOLEAN =
  BEGIN
    <*ASSERT NOT Mutex.TryLock(proc.mu)*>
    IF proc.atomTable = NIL THEN
      proc.atomTable := NEW(RefRefTbl.Default).init();
    END;
    IF proc.atomTable.get(key, val) THEN
      RETURN TRUE;
    END;
    EVAL proc.atomTable.put(key, val);
    RETURN FALSE;
  END InternAtom;
  
PROCEDURE FindAtom (proc: T; key: REFANY; VAR val: REFANY): BOOLEAN =
  BEGIN
    <*ASSERT NOT Mutex.TryLock(proc.mu)*>
    IF proc.atomTable = NIL THEN
      proc.atomTable := NEW(RefRefTbl.Default).init();
    END;
    RETURN proc.atomTable.get(key, val);
  END FindAtom;

PROCEDURE AllocateMemory (proc: T; size: CARDINAL): REF ARRAY OF CHAR =
  BEGIN
    (* XXX this assumes "proc" is not multi-threaded. *)
    IF proc.tmpbuf # NIL THEN
      IF NUMBER(proc.tmpbuf^) >= size THEN
	RETURN proc.tmpbuf;
      ELSE
	(* buffer too small. Free the current one and obtain a bigger one. *)
	CharArray.Free(proc.tmpbuf);
      END;
    END;
    proc.tmpbuf := CharArray.Allocate(size);
    RETURN proc.tmpbuf;
  END AllocateMemory;
  
BEGIN
  mu := NEW(MUTEX);
  pidTable := NEW(IntRefTbl.Default).init();
END Proc.
