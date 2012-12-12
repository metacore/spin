(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Jun-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Machine dependent part of the event dispatcher.
 *)

UNSAFE MODULE MachineDispatcher;
IMPORT DispatcherPrivate, DispatcherTypes, DispatcherRep, Thread, CPU;

(* size of activation record created by the dispatcher stub *)
(* has to be manually synchronized with dispStub.s *)
CONST
  DISP_STUB_FRAMESIZE = 88;

(******************************************************************************
 *
 * code for debugging disptch
 *
 *****************************************************************************)

(*
 * debug dispatch stub
 *)

PROCEDURE DebugDispatchStub (desc: DispatcherRep.AliasDescT;
                             sp: ADDRESS;
                             arg3, arg4, arg5, arg6: INTEGER): INTEGER 
                            RAISES ANY =
  VAR
    regs      : UNTRACED REF CPU.CalleeSavedRegs;
    argPtr    : UNTRACED REF INTEGER;
    args      : DispatcherPrivate.DebugDispatchArgs;
    eventDesc : DispatcherRep.EventDescT := desc.eventDesc;
  BEGIN
    (* get the first two arguments stored on stack by the stub *)
    regs := LOOPHOLE(sp-DISP_STUB_FRAMESIZE, 
                     UNTRACED REF CPU.CalleeSavedRegs);
    args[1] := regs.extra1;
    args[2] := regs.extra2;
    args[3] := arg3;
    args[4] := arg4;
    args[5] := arg5;
    args[6] := arg6;

    (* get the remaining arguments stored on stack by the caller *)
    argPtr := sp;
    FOR i := 7 TO DispatcherPrivate.MaxNumOfArgumentsForDebug DO
      args[i] := argPtr^;  
      INC(argPtr, ADRSIZE(INTEGER));
    END;

    (* add a reference to the callee-saved register state to the argument *)
    (* list if the hander has asked for them *) 
    IF eventDesc.saveRegs THEN
      WITH nArgs = DispatcherTypes.NumberOfArgs(eventDesc) DO
        args[nArgs+1] := LOOPHOLE(regs, INTEGER);
      END;
    END;

    RETURN DispatcherPrivate.DebugDispatch(desc, args);
  END DebugDispatchStub;

(*
 * create a thread that will asynchronously dispatch the event
 *)

PROCEDURE AsynchDispatchStub (desc: DispatcherRep.AliasDescT;
                              sp: ADDRESS;
                              arg3, arg4, arg5, arg6: INTEGER) =
  VAR
    cl     : DispatcherPrivate.AsynchClosure;
    thread : Thread.T;
    regs   : UNTRACED REF CPU.CalleeSavedRegs;
    argPtr : UNTRACED REF INTEGER;
  BEGIN
    cl := NEW (DispatcherPrivate.AsynchClosure);
    cl.aliasDesc := desc;

    (* get the first two arguments stored on stack by the stub *)
    regs := LOOPHOLE(sp-DISP_STUB_FRAMESIZE, 
                     UNTRACED REF CPU.CalleeSavedRegs);
    cl.args[1] := regs.extra1;
    cl.args[2] := regs.extra2;

    (* get the arguments passed in registers *)
    cl.args[3] := arg3;
    cl.args[4] := arg4;
    cl.args[5] := arg5;
    cl.args[6] := arg6;

    (* get the remaining arguments stored on stack by the caller *)
    argPtr := sp - ((DispatcherPrivate.MaxNumOfHandlersForDebug-6) *
                    ADRSIZE(INTEGER));
    argPtr := sp;
    FOR i := 7 TO DispatcherPrivate.MaxNumOfArgumentsForDebug DO
      cl.args[i] := argPtr^;  
      INC(argPtr, ADRSIZE(INTEGER));
    END;

    thread := Thread.Fork(cl);
  END AsynchDispatchStub; 

BEGIN
END MachineDispatcher.


