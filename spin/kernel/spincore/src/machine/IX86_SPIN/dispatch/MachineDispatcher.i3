(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Oct-96  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Whisted.
 *)

UNSAFE INTERFACE MachineDispatcher;
IMPORT DispatcherRep;

(* these constants contain offsets to the interface descriptor for           *)
(* DispatcherPrivate.i3 for procedures DebugDispatch and ForkAsynchDispatch *)
(* respectively                                                              *)
CONST
  DebugDispatchOffset  = 48;
  AsynchDispatchOffset = 52;

CONST
  NumSavedRegisters = 1; (* a pointer to the saved registers is the only *)
                         (* extra argument to the Stop handler *)

PROCEDURE DebugDispatchStub (desc: DispatcherRep.AliasDescT;
                             sp: ADDRESS) : INTEGER
                            RAISES ANY;

PROCEDURE AsynchDispatchStub (desc: DispatcherRep.AliasDescT;
                              sp: ADDRESS);

END MachineDispatcher.







