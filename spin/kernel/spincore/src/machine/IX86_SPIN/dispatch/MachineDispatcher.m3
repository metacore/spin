(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Oct-96  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Whisted.
 *)

UNSAFE MODULE MachineDispatcher;
IMPORT DispatcherPrivate, Thread, AliasDesc, EventDesc, DispatcherTypes;
IMPORT CPU;

(******************************************************************************
 *
 * code for debugging disptch
 *
 *****************************************************************************)

(* debugging dispatch which is completely unoptimized and goes through       *)
(* Modula3 dispatch procedure cannot dispatch to more than                   *)
(* MaxNumOfHandlersForDebug handlers because the right size                  *)
(* of array has to be allocated on the stack. Also, the procedure has to     *)
(* dispatches to events with different number of arguments.  Since there is  *)
(* no support in Modula3 for arbitrary number of arguments the limit is set  *)
(* by MaxNumOfArgumentsForDebug. FuncH and FuncG are the                     *)
(* procedure types used for debug dispatching, FuncH for handlers, FuncG for *)
(* guards.  The extra argument (dummy) is reserved for passing closure,      *)
(* closure is passed as the first argument and all caller's arguments are    *)
(* shifted by one so that the last one is passed in the dummy argument       *)
(* If the event is set to use callee saved registers then its number of      *)
(* arguments must be less by the number of saved registers NumSavedRegisters *)

(*
 * dispatch routine
 *)

PROCEDURE DebugDispatchStub (desc: AliasDesc.T;
                               sp: ADDRESS) : INTEGER
                             RAISES ANY =
  VAR 
    eventDesc: EventDesc.T := desc.eventDesc;
    regs   : UNTRACED REF CPU.CalleeSavedRegs;
    argPtr : UNTRACED REF INTEGER;
    args: ARRAY [1..DispatcherPrivate.MaxNumOfArgumentsForDebug] OF INTEGER;
  BEGIN
    regs := LOOPHOLE(sp-5*4, UNTRACED REF CPU.CalleeSavedRegs);

    (* get the arguments stored on stack by the caller *)
    argPtr := sp;
    FOR i := 1 TO DispatcherPrivate.MaxNumOfArgumentsForDebug DO
        INC(argPtr, ADRSIZE(INTEGER));
        args[i] := argPtr^;  
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

PROCEDURE AsynchDispatchStub(desc: AliasDesc.T;
                             sp: ADDRESS) =
  VAR
    cl     : DispatcherPrivate.AsynchClosure;
    thread : Thread.T;
    argPtr: UNTRACED REF INTEGER;
  BEGIN
    cl := NEW (DispatcherPrivate.AsynchClosure);
    cl.aliasDesc := desc;

    (* get the arguments stored on stack by the caller *)
    argPtr := sp;
    FOR i := 1 TO DispatcherPrivate.MaxNumOfArgumentsForDebug DO
        INC(argPtr, ADRSIZE(INTEGER));
        cl.args[i] := argPtr^;  
    END;

    thread := Thread.Fork(cl);
    
  END AsynchDispatchStub; 

BEGIN
END MachineDispatcher.
