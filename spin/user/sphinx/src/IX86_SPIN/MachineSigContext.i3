(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)

(* FreeBSD struct sigcontext *)

INTERFACE MachineSigContext;
IMPORT Ctypes;

(*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to restore state properly if
 * a non-standard exit is performed.
 *)

(* Caution: sigcode.s and MachineSig.SetupUserContext
  (where c.scp is set) depends on this structure!! *)

TYPE T = RECORD
  onstack : Ctypes.int;
  mask : Ctypes.int;
  sp : Ctypes.int;
  ebp : Ctypes.int;
  isp : Ctypes.int;
  pc : Ctypes.int;
  efl : Ctypes.int;
  es : Ctypes.int;
  ds : Ctypes.int;
  cs : Ctypes.int;
  ss : Ctypes.int;
  edi : Ctypes.int;
  esi : Ctypes.int;
  ebx : Ctypes.int;
  edx : Ctypes.int;
  ecx : Ctypes.int;
  eax : Ctypes.int;
END;
 
END MachineSigContext.
