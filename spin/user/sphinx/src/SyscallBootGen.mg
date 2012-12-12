(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
GENERIC MODULE SyscallBootGen(Intf);
IMPORT SyscallBoot;

BEGIN
  SyscallBoot.Setup(Intf.Domain_, Intf.Syscall,
		    Intf.MinProcID, Intf.MaxProcID);
END SyscallBootGen.
