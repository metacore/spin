(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Auxv is a secret information used to pass various information to the
   dynamic linker. This is used only on Alpha OSF/1.
 See <sys/auxv.h> *)

INTERFACE Auxv;
IMPORT Ctypes;

CONST
  AT_NULL = 0; (* mark end of the list *)
  AT_IGNORE = 1; (* notused *)
  AT_EXECFD = 2; (* pass the fd of the executable. *)
  AT_OSF_BASE =	1000;
  AT_EXEC_FILENAME = (AT_OSF_BASE+1); (* target file to dynlink *)
  AT_EXEC_LOADER_FILENAME = (AT_OSF_BASE+2); (* loader name itself *)
  AT_EXEC_LOADER_FLAGS	= (AT_OSF_BASE+3); (* ??? *)
  
TYPE
  Type = [AT_NULL .. AT_EXEC_LOADER_FLAGS];
  T = RECORD
    type: Type;
    data: Ctypes.long; (* Actually, only one of "data" or "string" is valid
			depending on the value of "type". For example,
			if "type=AT_EXETFD", then "data" is used,
			If "type=AT_EXEC_FILENAME", then "string" is used. *)
    string: TEXT;
  END;


PROCEDURE Init(VAR auxv: ARRAY OF T);
PROCEDURE AddInt(VAR auxv: ARRAY OF T; type: Type; data: INTEGER);
PROCEDURE AddText(VAR auxv: ARRAY OF T; type: Type; string: TEXT);
  
END Auxv.
