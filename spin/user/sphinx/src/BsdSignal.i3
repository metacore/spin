(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added "Number" type.
 * 11-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added sigset_t stuff.
 * 23-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* UNIX signal.h definitions. *)
INTERFACE BsdSignal;
IMPORT Ctypes;

CONST
  Min = 1;
  Max = 31;
  
  HUP = 1;
  INT = 2;
  QUIT = 3;
  ILL = 4;
  TRAP = 5;
  ABRT = 6;
  EMT = 7;
  FPE = 8;
  KILL = 9;
  BUS = 10;
  SEGV = 11;
  SYS = 12;
  PIPE = 13;
  ALRM = 14;
  TERM = 15;
  URG = 16;
  STOP = 17;
  TSTP = 18;
  CONT = 19;
  CHLD = 20;
  TTIN = 21;
  TTOU = 22;
  IO = 23;
  XCPU = 24;
  XFSZ = 25;
  VTALRM = 26;
  PROF = 27;
  WINCH = 28;
  INFO = 29;
  USR1 = 30;
  USR2 = 31;

  SIG_IGN = 1;
  SIG_DFL = 0;

TYPE
  Set = Ctypes.unsigned_long; (* sigset_t *)
  Number = [Min .. Max];

(* Note: The implementation of the below procs may depend on platforms,
   but I assume the DEC UNIX version anyway -- yas. *)
  
PROCEDURE Mask(sig: INTEGER): Set;
(* Return the mask that has 1 for the bit corresponding to "sig".
   Other bits are 0. *)
  
PROCEDURE AddSet(VAR set:Set; sig: Number);
  (* Add the signal "sig" to the mask "set" *)
  
PROCEDURE DelSet(VAR set:Set; sig: Number);
  (* Remove the signal "sig" to the mask "set" *)
  
PROCEDURE IsMember(set:Set; sig: [Min .. Max]): BOOLEAN;
  (* "IsMember(AddSet(foo, sig), sig) = TRUE" for all "foo" and "sig".
     "IsMember(DelSet(foo, sig), sig) = FALSE" for all "foo" and "sig". *)

PROCEDURE Name(signo: Number): TEXT;
  (* Return the human readable name for the signal "signo". *)
END BsdSignal.
