(*
 * Copyright 1994-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(* HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Borrowed from urt/urtcore/src.
 *)

INTERFACE StcpTime;
IMPORT Ctypes;

(*
 * The following are the BSD labels for the timer types.
 *)
CONST
  ITIMER_REAL =           0;       (* Real time *)
  ITIMER_VIRTUAL =        1;       (* Per-process time *)
  ITIMER_PROF =           2;       (* Per-process user time *)

TYPE Seconds = Ctypes.int;
TYPE Microseconds = BITS 32 FOR [0..999999];

TYPE timeval = RECORD
  tv_sec : Seconds;
  tv_usec: Microseconds;
END;

TYPE T = timeval;

END StcpTime.
