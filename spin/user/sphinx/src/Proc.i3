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

INTERFACE Proc;
IMPORT Dispatcher, Space;
IMPORT Word;
IMPORT Types;
IMPORT Errno;
IMPORT RefQ;
IMPORT OpenFile;
IMPORT Sphinx;

TYPE
  T <: Space.T;
  
  SigArgs = RECORD
    action: Sphinx.sigaction;
    tramp: Word.T; (* user space trampolin code *)
  END;

  SigSet = Word.T; (* When used as a mask, then if bit is set,
		    then the signal is blocked.
		    When used as a sig delivery set,
		    then 1 indicates the arrival of the signal. *)
  State = {Active, Stopped, Zombie};
  

  (* "Group" is a process group control block *)
  Group = REF RECORD 
    mu: MUTEX;
    gid: Types.Pid;
    members: RefQ.T;
  END;

<*OBSOLETE*>
PROCEDURE Self(): T;
  (* Get the process that is now active.
     Use Translation.GetCurrent() instead. *)

PROCEDURE AllocateMemory(proc: T; size: CARDINAL): REF ARRAY OF CHAR;
  
PROCEDURE Create(name: TEXT; parent: T): T;
  (* Register a new process. "parent" may be NIL. *)

PROCEDURE FindFromID(id: Types.Pid): T RAISES {Errno.E};
(* Find the process with ID "ID". If there's no such process, then
 "Error.E" is raised. *)

PROCEDURE Destroy(proc: T);
(* Remove all the resource held by "proc" *)
  
PROCEDURE FindGroupFromID(gid: Types.Pid): Group RAISES
{Errno.E};
(* Find group with ID "gid". If not found, "Error.E" is raised. *)

PROCEDURE AddToWaitList(proc: T);
(* Let the parent of "proc" be notified
 of "proc"'s status change. As a side effect, the parent
 will be waken up if it's waiting on "cond".
 Pre: "proc" is locked. *)
  
PROCEDURE AddBinding(proc: T; binding: Dispatcher.Binding);
(* Notify the proc manager that the handler "binding" is added
   to the process "proc". The binding is automatically uninstalled when
   process exits. *)

PROCEDURE InstallStandardHandlers(proc: T);
(* Install the syscall and stack page fault handlers. *)
  
PROCEDURE RemoveFromCurrentGroup(proc: T);
(* Remove the process "proc" from the group it is not a member of *)

PROCEDURE AddToGroup(proc: T; grp: Group);
(* Add the process "proc" to a process "group". Process should not be
   a member of any group prior to call of this proc. *)

TYPE
  Iterator <: IteratorPublic;
  IteratorPublic = OBJECT
  METHODS 
    next(VAR proc: T): BOOLEAN;
  END;
  
PROCEDURE Iterate(): Iterator;

PROCEDURE AllocateFD(proc: T; of: OpenFile.T): INTEGER RAISES
{Errno.E};
(* Return the first available Unix file descriptor.
   -1 if all are in use. refcount of "of" is incremented.
 Pre: proc is locked, "of" is not used. *)
PROCEDURE AllocateFDatSlot(proc: T; of: OpenFile.T; slot: INTEGER);
(* Allocate the file descriptor "fd". 
 refcount of "of" is incremented.
 Pre: "proc" is locked, "fd" is not used. *)

PROCEDURE InternAtom(proc: T; key, val: REFANY): BOOLEAN;
PROCEDURE FindAtom(proc: T; key: REFANY; VAR val: REFANY): BOOLEAN;
  
PROCEDURE FindFH(proc: T; fd: INTEGER): OpenFile.T RAISES {Errno.E};
(* Pre: proc is locked *)

CONST Brand = "Proc";
END Proc.
