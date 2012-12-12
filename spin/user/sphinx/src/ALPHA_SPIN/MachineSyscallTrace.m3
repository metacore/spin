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

PROCEDURE GetArgs (<*UNUSED*>strand: Strand.T;
		   READONLY ss: CPU.SavedState;
		   VAR arg: Args) =
  BEGIN
    arg.syscallNum := ss.v0;
    arg.retval := ss.v0;
    arg.args[0] := ss.a0;
    arg.args[1] := ss.a1;
    arg.args[2] := ss.a2;
    arg.args[3] := ss.a3;
    arg.args[4] := ss.a4;
    arg.args[5] := ss.a5;
  END GetArgs;

BEGIN
END MachineSyscallTrace.
	