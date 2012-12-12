(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
	BootIO.i3

	The BootIO interface is used by IO and Sched to find the initial
	reader and writer for the boot thread.  All subsequent threads
	inherit their initial rd/wr from the their parent.

	This interface is not exported to extensions.  They must use the
	rd/wr exported from the Console device extension.
 *)

INTERFACE BootIO;

IMPORT Wr AS WrBase;
IMPORT NullRd;

TYPE
  Wr <: WrBase.T;
  Rr = NullRd.T;

PROCEDURE Writer(): Wr;
PROCEDURE Reader(): Rr;

PROCEDURE Redirect(wr:WrBase.T);

END BootIO. 
