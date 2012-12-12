(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Machine dependent part of the event dispatcher.
 *)

UNSAFE INTERFACE MachineDispatcher;
IMPORT DispatcherRep;

(* these constants contain offsets to the interface descriptor for           *)
(* DispatcherPrivate.i3 for procedures DebugDispatch and ForkAsynchDispatch *)
(* respectively                                                              *)
CONST
  DebugDispatchOffset  =  96;
  AsynchDispatchOffset = 104;

CONST
  NumSavedRegisters = 9;  (* s0-s6 plus sp and pc *)

PROCEDURE DebugDispatchStub (desc: DispatcherRep.AliasDescT;
                             sp: ADDRESS;
                             a3, a4, a5, a6: INTEGER) : INTEGER
                            RAISES ANY;

PROCEDURE AsynchDispatchStub (desc: DispatcherRep.AliasDescT;
                              sp: ADDRESS;
                              a3, a4, a5, a6: INTEGER);

END MachineDispatcher.
