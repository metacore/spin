(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	Basic ioctl support to create the cmd integer used to make ioctl
 *	calls with devices.  This module implements various macro from
 *	sys/ioctl.h.
 *	
 *
 *)

MODULE Ioctl;
IMPORT Word;

<* INLINE *> PROCEDURE IOCPARM_LEN(x:T):T =
  BEGIN
    (* #define IOCPARM_LEN(x)  (((x) >> 16) & IOCPARM_MASK) *)
    RETURN Word.And(Word.RightShift(x,16),IOCPARM_MASK);
  END IOCPARM_LEN;

<* INLINE *> PROCEDURE IOCBASECMD(x:T):T =
  BEGIN
    (* #define IOCBASECMD(x)   ((x) & ~IOCPARM_MASK) *)
    RETURN Word.And(Word.Not(IOCPARM_MASK), x);
  END IOCBASECMD;

<* INLINE *> PROCEDURE IOCGROUP(x:T):T =
  BEGIN
    (* #define IOCGROUP(x)     (((x) >> 8) & 0xff) *)
    RETURN Word.And(16_ff, Word.RightShift(x,8));
  END IOCGROUP;

<* INLINE *> PROCEDURE IOC(inout: Word.T; group: Word.T; num: Word.T; len : Word.T): T =
  BEGIN
    (* 
       #define _IOC(inout,group,num,len) \
       (inout | ((len & IOCPARM_MASK) << 16) | ((group) << 8) | (num))
    *)
    RETURN
      Word.Or(inout,
              Word.Or(Word.LeftShift(Word.And(len,IOCPARM_MASK),16), 
                      Word.Or(Word.LeftShift(group,8), num)));
  END IOC;

<* INLINE *> PROCEDURE IO(group: CHAR; num: T): T =
  BEGIN
    RETURN IOC(IOC_VOID, ORD(group),num,0);
  END IO;

<* INLINE *> PROCEDURE IOR(group: CHAR; num: T; sizeoftype:T): T =
  BEGIN
    RETURN IOC(IOC_OUT, ORD(group),num,sizeoftype);
  END IOR;

<* INLINE *> PROCEDURE IOW(group: CHAR; num: T; sizeoftype:T): T =
  BEGIN
    RETURN IOC(IOC_IN, ORD(group),num,sizeoftype);
  END IOW;


<* INLINE *> PROCEDURE IOWR(group: CHAR; num: T; sizeoftype:T): T =
  BEGIN
    RETURN IOC(IOC_INOUT, ORD(group),num,sizeoftype);
  END IOWR;

<* INLINE *> PROCEDURE IORW(group: CHAR; num: T; sizeoftype:T): T =
  BEGIN
    RETURN IOWR(group, num, sizeoftype);
  END IORW; 

PROCEDURE Init() = 
  BEGIN
    IOC_INOUT    := Word.Or(IOC_IN,IOC_OUT);
    IOCPARM_MAX  := 0; (* XXX *)
  END Init;

BEGIN
END Ioctl.
