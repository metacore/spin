(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE SyscallBoot;
PROCEDURE Setup(name: TEXT;
		syscallHandler: PROCANY;
		maxID, minID: INTEGER);

END SyscallBoot.
