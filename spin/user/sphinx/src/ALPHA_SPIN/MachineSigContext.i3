(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created
 *)

(* OSF/1 struct sigcontext *)

INTERFACE MachineSigContext;
IMPORT Ctypes;

TYPE T = RECORD
  onstack, mask : Ctypes.long;
  pc, ps : Ctypes.long;
  regs : ARRAY [0 .. 31] OF Ctypes.long;
  ownedfp : Ctypes.long;
  fpregs : ARRAY [0 .. 31] OF Ctypes.long;
  fpcr, fp_control : Ctypes.unsigned_long;
  reserved1 : Ctypes.long;
  kreserved1, kreserved2 : Ctypes.int; 
  ssize, sbase : Ctypes.long;
  traparg_a0, traparg_a2, traparg_a1, fp_trap_pc : Ctypes.long;
  fp_trigger_sum, fp_trigger_inst : Ctypes.long;
END;

END MachineSigContext.
