(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 3-Mar-98  David Becker at the University of Washington
 *	Changed Selinfo to hold a Sema instead of Thread
 *
 * 29-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* FreeBSD select implementation. *)
INTERFACE Select;
IMPORT Thread;
IMPORT Ctypes;
IMPORT Sema;

TYPE
  (* XXX THIS STRUCTURE MUST BE SAME AS struct selinfo IN sys/select.h. *)
  Selinfo = RECORD
    sema: Sema.T; (* Originally a pid_t. Assuming that
		     sizeof(pid_t)=sizeof(pointer). *)
    flags: Ctypes.short;
  END;

(* Impl. of "selrecord" orig. in "sys_generic.c".  Pre: SPL is high. *)  
PROCEDURE Record (curThread: Thread.T; VAR selector: Selinfo);

(* Impl. of "selwakeup" orig. in "sys_generic.c".  Pre: SPL is high. *)    
PROCEDURE Wakeup (VAR selector: Selinfo);
  
END Select.
