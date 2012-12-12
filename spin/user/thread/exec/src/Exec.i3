(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Attempt to merge sphinx exec and generic exec.
 *	OSF dymamic linking support.
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated exec cmd and Exec itself.
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(* "LoadExecFile loads the content of an executable file into
   a vm space and set the register contents of a thread.
 This is used by the shell "exec" command and also by the
 sphinx exec systemcall.*)

INTERFACE Exec;
IMPORT Error AS GenError;
IMPORT VMError;
IMPORT Space;
IMPORT UserSpaceThread;
IMPORT AoutParser;

TYPE Info = AoutParser.Info;

PROCEDURE LoadFile (path: TEXT;
		    READONLY argv: ARRAY OF TEXT;
		    READONLY environ: ARRAY OF TEXT;
		    space : Space.T;
		    (*OUT*)VAR state: UserSpaceThread.State;
		    (*OUT*)VAR info : Info) RAISES {GenError.E, VMError.E};
(* Load the executable into the space "space" from the file "fp".
  "state" is set for the executable. *)
  
END Exec.

