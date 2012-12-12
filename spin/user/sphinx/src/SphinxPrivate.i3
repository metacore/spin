(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      Added Error.E to raises list of all procs that use File
 * 28-May-96  becker at the University of Washington
 *	Moved exclusive open from device to dlib
 #	Changed WaitInternal to use osf args.
 *
 * 22-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Moved socket out of the StreamFH definition.
 * 20-May-96 oystr at the University of Washington
 *	Centralized definitions *FH types.
 *
 * 09-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* This interface provides backdoor entrance to commonly used
  UNIX functions. *)
INTERFACE SphinxPrivate;
IMPORT OpenFile;
IMPORT Proc;
IMPORT Errno, Error;
IMPORT NameServer;
IMPORT Strand;
IMPORT CPU;
IMPORT Translation;

(* Pre : t is NOT locked . t = Proc.Self()*)
PROCEDURE ExitInternal(t : Proc.T; status : INTEGER);

(* Pre : t is NOT locked *)
PROCEDURE KillInternal(proc : Proc.T; signo : INTEGER);

PROCEDURE OpenInternal(proc: Proc.T;
		       READONLY path: NameServer.Name;
		       mode, createMode: INTEGER)
	  : OpenFile.T RAISES {Errno.E, Error.E, NameServer.Error};
(* Open a file. This proc does not allocate a file destriptor.
 "proc" is used merely to locate the cwd when the path is relative. *)

PROCEDURE PageFault (strand: Strand.T; 
		     VAR ss: CPU.SavedState;
		     map: Translation.T;		    
		     addr: CPU.VirtAddress;
		     type: INTEGER;
		     VAR done: BOOLEAN);
(* Page fault handler that's responsible only for stack faults. *)
  
END SphinxPrivate.
