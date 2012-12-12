(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	Basic ioctl support to create the cmd integer used to make ioctl
 *	calls with devices.  This interface is based on sys/ioctl.h out
 *	of the OSF/1 3.0 source tree.
 *
 *)


INTERFACE Ioctl;
IMPORT Ctypes;
IMPORT Word;

(*
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 3 bits of the upper word are used
 * to encode the in/out status of the parameter.
 *)

TYPE T = Ctypes.unsigned_int; (* XXX: should eb the size of an ioctly command. *)

(* sys/ioctly support *)
CONST
  IOCPARM_MASK = 16_1fff;          (* parameter length, at most 13 bits *)
  IOC_VOID     = 16_20000000;      (* no parameters                     *)
  IOC_OUT      = 16_40000000;      (* copy out parameters               *)
  IOC_IN       = 16_80000000;      (* copy in parameters                *)
  IOC_DIRMASK  = 16_e0000000;      (* mask for IN/OUT/VOID              *)

VAR
  IOCPARM_MAX: T; (* := NBPG *)    (* max size of ioctl, mult. of NBPG *)
  IOC_INOUT  : T;

PROCEDURE IOCPARM_LEN (x:T):T;
PROCEDURE IOCBASECMD  (x:T):T;
PROCEDURE IOCGROUP    (x:T):T;
PROCEDURE IOC         (inout: Word.T; group: Word.T; num: Word.T; len : Word.T):T;
PROCEDURE IO          (group: CHAR; num: T):T;
PROCEDURE IOR         (group: CHAR; num: T; sizeoftype: T):T;
PROCEDURE IOW         (group: CHAR; num: T; sizeoftype: T):T;
PROCEDURE IOWR        (group: CHAR; num: T; sizeoftype: T):T;
PROCEDURE IORW        (group: CHAR; num: T; sizeoftype: T):T;

PROCEDURE Init();
END Ioctl.
