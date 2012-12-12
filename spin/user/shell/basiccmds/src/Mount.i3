(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description 
 *)
(* HISTORY
 * 08-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added -u flag to unmount.
 *
 * 26-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *)

INTERFACE Mount;
CONST CommandName = "mount";
CONST CommandHelp = " fs-type device path| -u path";
END Mount.
