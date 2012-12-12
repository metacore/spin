(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 29-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE SalDep;
IMPORT Ctypes;

TYPE SelectProc = PROCEDURE(dev: INTEGER; VAR events: Ctypes.short_int;
			    VAR revents: Ctypes.short_int;
			    scanning: INTEGER) : INTEGER;

END SalDep.
