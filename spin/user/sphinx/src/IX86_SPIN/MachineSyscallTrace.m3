(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE MachineSyscallTrace;
IMPORT Strand;
IMPORT CPU;
IMPORT Translation;
IMPORT UserSpaceThread;

PROCEDURE GetArgs (strand: Strand.T;
		   READONLY ss: CPU.SavedState;
		   VAR arg: Args) =
  VAR
    space: Translation.T;
  BEGIN
    arg.syscallNum := ss.eax;
    arg.retval := ss.eax;
    space := UserSpaceThread.GetSpace(strand);
    Translation.Read(space, ss.usp+4, VIEW(arg.args, ARRAY OF CHAR));
  END GetArgs;

BEGIN
END MachineSyscallTrace.
