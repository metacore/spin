(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Program exec
 *
 * HISTORY
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated exec cmd and Exec itself.
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

INTERFACE Exec;
IMPORT Error, Space, UserSpaceThread;
IMPORT AoutParser;

TYPE Info = AoutParser.Info;
  
PROCEDURE Execute (filename : TEXT;
		   READONLY argv : ARRAY OF TEXT;
		   READONLY environ : ARRAY OF TEXT;
		   space : Space.T; thread : UserSpaceThread.T;
		   VAR info : Info) RAISES {Error.E};
(* If "space" = NIL, it is allocated. Otherwise,
 caller provided space is used. *)

CONST
  UserCodeSel = 16_1f;
  UserDataSel = 16_27;

END Exec.
