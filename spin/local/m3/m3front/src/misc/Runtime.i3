(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Runtime.i3                                            *)
(* Last Modified On Mon Jul 25 15:22:56 PDT 1994 By kalsow     *)

(* This interface provides handle on the runtime procedures
   that the compiler calls directly. *)

(*
 * HISTORY
 * 21-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	write barrier hook
 *
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added hooks for reference counting
 *
 * 04-Feb-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added hooks for VIEW faults
 *
 *)

INTERFACE Runtime;

IMPORT M3ID, Module, Procedure;

TYPE
  Hook = {
    Raise, ResumeRaise, PushEFrame, PopEFrame,
    Lock, Unlock,
    Concat,
    NewTracedRef, NewTracedArray,
    NewUntracedObj, NewUntracedRef, NewUntracedArray,
    DisposeRef, DisposeObj,
    AssertFault, ReturnFault, CaseFault, TypecaseFault, RangeFault,
    SubscriptFault, NarrowFault, NilFault, ShapeFault,
    (* for VIEW *)
    SizeFault, AlignFault,
    (* for reference counting *)
    AssignTracedToPossibleGlobal,
    AssignTracedToGlobal,
    AssignKnown,
    IsShared,
    (* for pointer tracking *)
    TraceRef, TraceRefPossible, TraceCount
  };

PROCEDURE Import ();
(* Import the standard interfaces containing runtime hooks. *)

PROCEDURE Bind (dest: Module.T;  VAR runtime: Module.T;  VAR id: M3ID.T);
(* bind the runtime interface as an import of 'dest' *)

PROCEDURE LookUpProc (h: Hook): Procedure.T;
(* return a handle on the procedure that implements hook 'h' *)

PROCEDURE Reset ();

END Runtime.
