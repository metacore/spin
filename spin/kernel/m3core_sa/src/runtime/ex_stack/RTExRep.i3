(* Copyright (C) 1993, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu Jan 26 12:53:10 PST 1995 by kalsow     *)
(*      modified on Thu Jun 25 18:20:47 PDT 1992 by muller     *)

UNSAFE INTERFACE RTExRep;

IMPORT RTMachine;

(* This interface defines the low-level routines and data structures
   used by the exception runtime's stack walker.
*)

(*--------------------------------------------------------- runtime hooks ---*)
(* Each machine must define "Frame", "CurrentFrame", "PreviousFrame",
   "Unwind", and "ProcName".  The "Frame" type must minimally include fields
   named "pc" and "sp".  It is ok for "ProcName" to always return NIL.
*)

TYPE Frame = RTMachine.FrameInfo;

<*EXTERNAL "RTException__CurFrame" *>
PROCEDURE CurrentFrame (VAR(*OUT*) f: Frame);
(* Return in "f" the frame of its caller.  Returns with pc = NIL on failure. *)

<*EXTERNAL "RTException__PrevFrame" *>
PROCEDURE PreviousFrame (READONLY callee: Frame;  VAR(*OUT*)caller: Frame);
(* Return in "caller" the stack frame that called "callee".
   Returns with pc = NIL if  "callee" is the first frame on
   the stack or its predecessor is ill-formed. *)

<*EXTERNAL "RTException__Unwind" *>
PROCEDURE Unwind (READONLY f: Frame);
(* Restore the machine state back to the frame "f".  All callee-saved
   registers must be restored to the state they were in when frame "f"
   made its last call.  Note that if the unwind operation encounters a
   signal handler frame, it must also restore the caller-saved registers. *)

<*EXTERNAL "RTException__ProcName" *>
PROCEDURE ProcName (READONLY f: Frame): ADDRESS;
(* Return the null-terminated constant string that names the procedure
   corresponding to the stack frame "f".  Returns NIL if no name is
   known. *)

(*----------------------------------------- compiler generated descriptors --*)

TYPE
  ScopeKind = { Except, ExceptElse,
                Finally, FinallyProc,
                Raises, RaisesNone,
                Lock };

TYPE
  Scope = UNTRACED REF RECORD
    kind        : CHAR;    (* ScopeKind *)
    outermost   : CHAR;    (* BOOLEAN => last scope that covers [start..stop]*)
    end_of_list : CHAR;    (* BOOLEAN => last scope in module list *)
    pad         : CHAR;
    start       : ADDRESS; (* first PC of the handled scope *)
    stop        : ADDRESS; (* last PC of the handled scope *)
    excepts     : ADDRESS; (* NIL-terminated list of handled exceptions *)
    offset      : INTEGER; (* frame offset of ExceptionInfo *)
  END;

TYPE
  ExceptionInfo = RECORD
    exception : ADDRESS;
    arg       : ADDRESS;
  END;

END RTExRep.

