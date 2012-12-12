(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 28-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Moved InRanges out of Domain.
 *
 * 01-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	NIL guards are treated as if they both took and did not take
 *	a closure (in TypecheckBinding).
 *
 * 29-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fix signature for AsynchDispatch
 *
 * 20-Sep-96  Przemek Pardyak (pardy) at the University of Washington
 *	Check that all guards are FUNCTIONAL. Result handler takes all
 *	the event arguments.
 *
 * 27-Aug-96  Wilson Hsieh (whsieh) at the University of Washington
 *	integrate with change to compiler: word for result count also
 *	 encodes FUNCTIONAL and EPHEMERAL now, so have to mask out those
 *	 bits to get result count
 *
 * 25-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added the Last installation option.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added RecordStubs.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added Create and Swap.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Disable dispatcher optimizations when call graph profiling is on.
 *
 * 03-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed memory corruption in UpdateHandlerLists.
 *
 * 26-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 24-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Put back support for bypassing.
 *
 * 20-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 07-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed the fix for callee-saved registers to pass the registers
 *	to a procedure with an arbitrary number of arguments.
 *
 * 01-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed a bug in the DebugDispatch routine to pass correctly all of
 *	the callee-save save registers to the thread stop handler.
 *
 * 01-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for turning non-Modula-3 procedures into legal
 *	events, handlers, etc. 
 *
 * 10-Mar-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added third level of optimization (unrolled dispatch loop).
 *
 * 26-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Moved the call to SetSave for Strand.Stop to Strand.m3. 
 *	Added second level of optimization (stiching from code snippets).
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Authorization services.
 *
 * 23-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Removed a couple places where procedure pointers were put
 *	in place of traced references.
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Removed uses of obsolete interface Clib.
 *
 * 09-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Replaced calls to Clib with calls to IO. Switched to a single 
 *	dispatcher exception.  DebugDispatch uses 15 arguments, first
 *	step towards arbitrary number of arguments.
 *
 * 22-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	All guards are evaluated before any handler is invoked.
 *	DebugDispatch RAISES clause changed to pass on all exceptions.
 *
 * 21-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Fixed a bug in DebugDispatch.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *      Use Textify.
 *
 * 10-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Reduced memory overhead by splitting EventDesc.T into EventDesc.T
 *	and ProcDesc.T.
 *
 * 02-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned up the code.  Changed some names to conform to our
 *	naming convention. Got rid of SetNumOfArgs and SetNoResult.
 *
 * 20-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added GetHandlers.
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added dynamic typechecking of handler installation.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Switched to the simplified StrongRef interface.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use PROCANY instead of REFANY
 *	use REF PROCEDURE instead of PROCEDURE
 *
 * 17-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added the DefaultOptions const to assure that clients remain in
 *	sync with changes to the dispatcher.
 *
 * 12-Aug-95  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned the code.
 *
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Changed {ED,AD}Table to Tbl to allow the makefile to generate the
 *	generic directly
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)

UNSAFE MODULE Dispatcher EXPORTS Dispatcher, DispatcherPrivate, DispatcherRep,
                                 MachineDispatcher;

IMPORT RTModule, RT0, StrongRef, Word;
IMPORT RTProcedureSRC, RTProcDescF, RTProcDesc;
IMPORT EventDesc, EDTbl, AliasDesc, ADTbl;
IMPORT Stitcher, Fmt, IO, SpinException, Textify, MachineDispatcher;
IMPORT SafeConvert, RTCode, Auth, RTIO, ProfileSupport;
(* IMPORT DomainChecks; FIXME *)
IMPORT Binding AS DispBinding;
IMPORT DomainPrivate; (* FIXME: this violates modularity, change it *)
IMPORT MachineStitcherExtern;

IMPORT DispatcherTypes;
FROM DispatcherTypes IMPORT TypecheckResultHandler,
                            TypecheckHandler,
                            TypecheckGuard,
                            TypecheckBinding,
                            IsLegalAsynchronous,
                            NumberOfArgs,
                            HasResult;

(* an event is identified by a pointer to the procedure that defines it *)
(* it may be also aliased by stubs created for dispatch (alias id) *)
VAR
  EventLock: MUTEX;
  EventTable: EDTbl.T;      (* a hash from event id to event descriptors *)
  AliasTable: ADTbl.T;      (* a hash from alias id to alias descriptors *)

(* dispatcher stubs can be generated with different levels of optimization   *)
(* the level can be set for each event separately by calling SetOptLevel     *)
(* if the level for an event is not set by SetOptLevel then the default      *)
(* level (DefaultOptLevel) is used, the default optimization can             *)
(* be changed by calling SetDefaultOptLevel                                  *)
(*                                                                           *)
(* supported optimization levels:                                            *)
(*                                                                           *)
(*   0 - debugging in Modula-3, no optimization, simple stub routine jumps   *)
(*       to a Modula3 procedure DebugDispatch which does fully general       *)
(*       dispatch, fixed number of arguments, identical stub is cloned for   *)
(*       each event. All guards executed before handlers.                    *)
(*                                                                           *)
(*   1 - debugging in assmbler, no optimization, dispatch stub does fully    *)
(*       general dispatch directly to handlers, fixed number of arguments,   *)
(*       identical stub is cloned for each event. All guards executed before *)
(*       handlers.                                                           *)
(*                                                                           *)
(*   2 - specialized for the number of arguments, existence of closures,     *)
(*       cancellation, and passing callee-saved registers, the stubs are     *)
(*       patched from code snippets. All guards executed before handlers.    *)
(*                                                                           *)
(*   3 - unroll the dispatch loop and embed pointers to guards and handlers  *)
(*       in the stub code, a separate stub is generated for each             *)
(*       configuration of handlers. All guards executed before handlers.     *)
(*                                                                           *)
(*   4 - like 3 but guards are executed right before their handlers.         *)
(*                                                                           *)
(*   5 - 4 plus inlining of small procedures                                 *)
(*                                                                           *)

(******************************************************************************
 *
 * single binding handling
 *
 *****************************************************************************)

(*
 * create an inactive binding
 *)
PROCEDURE Create (event          : PROCANY;
                  guard          : PROCANY;
                  handler        : PROCANY;
                  guardClosure   : REFANY   := NIL;
                  handlerClosure : REFANY   := NIL;
                  options        : Options  := DefaultOptions;
                  key            : Auth.Key := NIL
                 ): Binding
                 RAISES { Error } =
  BEGIN
    RETURN RealCreate(GetEventDesc(event),  guard, GetEventDesc(handler).event,
                      guardClosure, handlerClosure, options, key);
  END Create;

(* 
 * install the given handler on the event it is bound to
 * empty operation if the handler is not currently installed
 *)
PROCEDURE Install (binding: Binding)
                  RAISES { Error } =
  VAR
    eventDesc: EventDesc.T;
  BEGIN
    IF binding = NIL THEN 
      RETURN; 
    END;
    eventDesc := TryGetEventDesc(binding);
    LOCK eventDesc.lock DO
      IF RealInstall(binding, eventDesc) THEN
        Activate(eventDesc);
      END;
    END;
  END Install;

(*
 * uninstall the given handler from the event it is bound to
 * empty operation if the handler is not currently installed
 *)
PROCEDURE Uninstall (binding: Binding) RAISES { Error } =
  VAR
    eventDesc: EventDesc.T;
  BEGIN
    IF binding = NIL THEN 
      RETURN; 
    END;
    eventDesc := TryGetEventDesc(binding);
    LOCK eventDesc.lock DO
      IF RealUninstall(binding, eventDesc) THEN
        Activate(eventDesc);
      END;
    END;
  END Uninstall;

(* 
 * create an active binding: a composition of Create and Install
 *)
(* FIXME: allow extraction of the default guard and postguard *)
PROCEDURE InstallHandler (event          : PROCANY;
                          guard          : PROCANY;
                          handler        : PROCANY;
                          guardClosure   : REFANY   := NIL;
                          handlerClosure : REFANY   := NIL;
                          options        : Options  := DefaultOptions;
                          key            : Auth.Key := NIL
                         ): Binding
                         RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
    binding: Binding;
  BEGIN
    binding := RealCreate(eventDesc, guard, GetEventDesc(handler).event,
                          guardClosure, handlerClosure, options, key);
    LOCK eventDesc.lock DO
      IF RealInstall(binding, eventDesc) THEN
        Activate(eventDesc);
      END;
    END;
    RETURN binding;
  END InstallHandler;

(******************************************************************************
 *
 * handler sets manipulation
 *
 *****************************************************************************)

(* 
 * obtain a list of all handlers installed on an event 
 *)
PROCEDURE GetHandlers (event  : PROCANY;
                       <*UNUSED*>module : RTCode.Module): REF ARRAY OF Binding
                      RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
    bindings  : REF ARRAY OF Binding;
    binding: Binding;
    i: INTEGER;
  BEGIN
    IF eventDesc.nHandlers = -1 THEN
      RETURN NIL;
    END;

    bindings := NEW (REF ARRAY OF Binding, eventDesc.nHandlers);
    binding := eventDesc.bindings;
    i := 0;
    WHILE binding # NIL DO
      bindings[i] := binding;
      binding := binding.next;
      INC(i);
    END;
    <* ASSERT i = eventDesc.nHandlers *>
    RETURN bindings;
  END GetHandlers;

(*
 * verify whether the binding is active or not
 *) 
PROCEDURE IsInstalled (<* UNUSED *> binding: Binding): BOOLEAN =
  BEGIN
    (* FIXME *)
    <* ASSERT FALSE *>
  END IsInstalled;

(*
 * return a binding corresponding to the original handler
 *)
PROCEDURE GetOriginalHandler (event: PROCANY): Binding
                              RAISES { Error } =
  VAR
    eventDesc: EventDesc.T := GetEventDesc(event);
  BEGIN
    IF eventDesc.nHandlers = -1 THEN
      RegisterEvent(eventDesc);
    END;

    RETURN eventDesc.defBinding;
  END GetOriginalHandler; 

(******************************************************************************
 *
 * Default handler and result handling
 *
 *****************************************************************************)

PROCEDURE CreateResultHandlers (event            : PROCANY;
                                resultHandler    : PROCANY := NIL;
                                defaultHandler   : PROCANY := NIL;
                                resultClosure    : REFANY  := NIL;
                                defaultClosure   : REFANY  := NIL; 
                                defaultResultPtr : REFANY  := NIL;
                                module           : RTCode.Module := NIL;
                               ): ResultHandlers
                               RAISES { Error } =
  VAR
    eventDesc         : EventDesc.T := GetEventDesc(event);
  BEGIN
    RETURN RealCreateResultHandlers(eventDesc.event,
                                    resultHandler,
                                    defaultHandler,
                                    resultClosure,
                                    defaultClosure,
                                    defaultResultPtr,
                                    module);
  END CreateResultHandlers;

PROCEDURE InstallResultHandlers (resultHandlers: ResultHandlers)
                                 RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(resultHandlers.event);
  BEGIN
    eventDesc.resultHandlers := resultHandlers;
    Activate(eventDesc);
  END InstallResultHandlers;

PROCEDURE UninstallResultHandlers (resultHandlers: ResultHandlers)
                                   RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(resultHandlers.event);
  BEGIN
    IF eventDesc.resultHandlers # resultHandlers THEN
      RETURN;
    END;
    eventDesc.resultHandlers := NIL;
    Activate(eventDesc);
  END UninstallResultHandlers;

PROCEDURE ImposeResultHandlers (event            : PROCANY;
                                resultHandler    : PROCANY := NIL;
                                defaultHandler   : PROCANY := NIL;
                                defaultResultPtr : REFANY  := NIL;
                                resultClosure    : REFANY  := NIL;
                                defaultClosure   : REFANY  := NIL; 
                                module           : RTCode.Module;
                               ): ResultHandlers
                               RAISES { Error } =
  VAR
    eventDesc      : EventDesc.T := GetEventDesc(event);
    resultHandlers : ResultHandlers;
  BEGIN
    resultHandlers := RealCreateResultHandlers(eventDesc.event,
                                               resultHandler,
                                               defaultHandler,
                                               resultClosure,
                                               defaultClosure,
                                               defaultResultPtr,
                                               module);
    eventDesc.resultHandlers := eventDesc.resultHandlers;
    Activate(eventDesc);
    RETURN resultHandlers;
  END ImposeResultHandlers;

PROCEDURE RealCreateResultHandlers (event            : PROCANY;
                                    resultHandler    : PROCANY;
                                    defaultHandler   : PROCANY;
                                    defaultResultPtr : REFANY;
                                    resultClosure    : REFANY;
                                    defaultClosure   : REFANY; 
                                    <* UNUSED *> module : RTCode.Module;
                                    ): ResultHandlers
                                    RAISES { Error } =
  VAR
    useResultClosure  : BOOLEAN := FALSE;
    useDefaultClosure : BOOLEAN := FALSE;
    resultPassArgs    : BOOLEAN := TRUE;
    resultHandlers    : ResultHandlers;
  BEGIN
    IF resultHandler # NIL THEN
      TypecheckResultHandler(GetProcDesc(event), GetProcDesc(resultHandler),
                             resultClosure, useResultClosure, resultPassArgs);
    END;
    IF defaultHandler # NIL THEN
      TypecheckHandler(GetProcDesc(event), GetProcDesc(defaultHandler), FALSE,
                       defaultClosure, useDefaultClosure);
    END;

    resultHandlers := NEW (ResultHandlers);

    resultHandlers.resultHandler     := resultHandler;
    resultHandlers.resultClosure     := resultClosure;
    resultHandlers.useResultClosure  := useResultClosure;
    resultHandlers.resultPassArgs    := resultPassArgs;
    resultHandlers.defaultHandler    := defaultHandler;
    resultHandlers.defaultClosure    := defaultClosure;
    resultHandlers.useDefaultClosure := useDefaultClosure;
    IF defaultResultPtr # NIL THEN
      resultHandlers.defaultResult     := LOOPHOLE(defaultResultPtr,
                                                   REF INTEGER)^; (* FIXME *)
    ELSE
      resultHandlers.defaultResult     := 0;
    END;
    resultHandlers.event             := event;

    RETURN resultHandlers;
  END RealCreateResultHandlers;

(*
 * install a result handler 
 *)
PROCEDURE InstallResultHandler (event   : PROCANY;
                                handler : PROCANY;
                                <* UNUSED *> module  : RTCode.Module;
                                closure : REFANY := NIL) 
                               RAISES { Error } = 
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
    useClosure : BOOLEAN := FALSE;
    passArgs   : BOOLEAN := TRUE;
  BEGIN
    IF handler # NIL THEN
      TypecheckResultHandler(GetProcDesc(event), GetProcDesc(handler),
                             closure, useClosure, passArgs);
    END;

    IF eventDesc.resultHandlers = NIL THEN
      eventDesc.resultHandlers := NEW (ResultHandlers);
    END;

    eventDesc.resultHandlers.resultHandler    := handler;
    eventDesc.resultHandlers.resultClosure    := closure;
    eventDesc.resultHandlers.useResultClosure := useClosure;
    eventDesc.resultHandlers.resultPassArgs := passArgs;
    Activate(eventDesc);
  END InstallResultHandler;

(*
 * install a default handler 
 *)
PROCEDURE InstallDefaultHandler (event   : PROCANY;
                                 handler : PROCANY;
                                 <* UNUSED *>module  : RTCode.Module;
                                 closure : REFANY := NIL) 
                                RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
    useClosure : BOOLEAN := FALSE;
  BEGIN
    IF handler # NIL THEN
      TypecheckHandler(GetProcDesc(event), GetProcDesc(handler), FALSE,
                       closure, useClosure);
    END;

    IF eventDesc.resultHandlers = NIL THEN
      eventDesc.resultHandlers := NEW (ResultHandlers);
    END;

    eventDesc.resultHandlers.defaultHandler    := handler;
    eventDesc.resultHandlers.defaultClosure    := closure;
    eventDesc.resultHandlers.useDefaultClosure := useClosure;
    Activate(eventDesc);
  END InstallDefaultHandler;

(*
 * set a default result
 *)
PROCEDURE SetDefaultResult (event     : PROCANY;
                            resultPtr : REFANY;
                            <* UNUSED *> module    : RTCode.Module)
                           RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);  
  BEGIN
    IF eventDesc.resultHandlers = NIL THEN
      eventDesc.resultHandlers := NEW (ResultHandlers);
    END;
    eventDesc.resultHandlers.defaultResult := LOOPHOLE(resultPtr, REF INTEGER)^;
    Activate(eventDesc);
  END SetDefaultResult;

(******************************************************************************
 *
 * Asynchrony
 *
 *****************************************************************************)

(* 
 * make an event asynchronous if "asynchronouns" is TRUE and synchronous
 * if it is FALSE
 *)
PROCEDURE SetAsynchronousEvent (<* UNUSED *>event        : PROCANY;
                                <* UNUSED *>asynchronous : BOOLEAN;
                                <* UNUSED *>module: RTCode.Module)
                               <* NOWARN *> RAISES { Error } =
  BEGIN
    <* ASSERT FALSE *>
  END SetAsynchronousEvent;

(*
 * make a handler asynchronous if "asynchronouns" is TRUE and synchronous
 * if it is FALSE
 *)
PROCEDURE SetAsynchronousHandler (<* UNUSED *>binding: Binding;
                                  <* UNUSED *>asynchronous : BOOLEAN)
                                 <* NOWARN *> RAISES { Error } =
  BEGIN
    <* ASSERT FALSE *>
  END SetAsynchronousHandler;

(******************************************************************************
 *
 * Authorization support
 *
 *****************************************************************************)

PROCEDURE InstallAuthorizerForInterface (<*UNUSED*>auth     : Auth.T;
                                         interface: RTCode.Interface;
                                         module   : RTCode.Module)
                                         RAISES {Error} =
  BEGIN
    (* DOES NOT WORK *)
    IF NOT RTCode.ModuleExportsInterface(module, interface) THEN
      RAISE Error(ErrorCode.AuthError);
    END;
  END InstallAuthorizerForInterface;


PROCEDURE InstallAuthorizerForEvent (auth  : Auth.T;
                                     event : PROCANY;
                                     module: RTCode.Module) 
                                     RAISES {Error} =
  VAR eventDesc: EventDesc.T;
  BEGIN
    eventDesc := GetEventDesc(event);
    IF NOT RTCode.ModuleImplementsProcedure(module, eventDesc.event) THEN
      RAISE Error(ErrorCode.AuthError);
    END;
    LOCK eventDesc.lock DO
      eventDesc.auth := auth;
    END;
  END InstallAuthorizerForEvent;

PROCEDURE AuthCheck (binding: Binding; key: Auth.Key; event: PROCANY)
                     RAISES {Error} =
  VAR 
    eventDesc: EventDesc.T;
    auth: Auth.T;
  BEGIN
    eventDesc := GetEventDesc(event);
    LOCK eventDesc.lock DO
      (* Check the authorizer *)
      <* ASSERT eventDesc.auth # NIL *>
      auth := eventDesc.auth;
    END;
    IF NOT auth.authorize(key, binding) THEN <* NOWARN *> (* FIXME *)
      RAISE Error(ErrorCode.AuthError);
    END;
  END AuthCheck;

(******************************************************************************
 *
 * Imposed guards
 *
 *****************************************************************************)

(*
 * create an imposed guard for a binding
 *)
PROCEDURE CreateImposedGuard(binding    : Binding; <* NOWARN *>
                             guardProc        : PROCANY := NIL; <* NOWARN *>
                             guardClosure     : REFANY := NIL; <* NOWARN *>
                             postGuard        : PROCANY := NIL; <* NOWARN *>
                             postGuardClosure : REFANY := NIL; <* NOWARN *>
                            ): ImposedGuard
                            RAISES { Error } = <* NOWARN *>
  BEGIN <* NOWARN *>
    (* FIXME *)
  END CreateImposedGuard;

(*
 * install an imposed guard
 *)
PROCEDURE InstallGuard(guard: ImposedGuard) RAISES { Error } = <* NOWARN *>
  BEGIN
    (* FIXME *)
  END InstallGuard;

(* 
 * uninstall an imposed guard
 *)
PROCEDURE UninstallGuard(guard: ImposedGuard) RAISES { Error } =
  VAR 
    ptr, prev: ImposedGuard;
    binding: Binding;
    eventDesc: EventDesc.T;
  BEGIN
    binding   := guard.binding;
    eventDesc := RealGetEventDesc(binding.event);

    LOCK eventDesc.lock DO
      (* find the guard *)
      ptr := binding.imposedGuards;
      WHILE ptr # NIL DO
        IF ptr = guard THEN EXIT; END;
        prev := ptr;
        ptr := ptr.next;
      END;
      
      (* if found, remove it and update the event if necessary *)
      IF ptr # NIL THEN
        IF prev = NIL THEN
          binding.imposedGuards := ptr.next;
        ELSE
          prev.next := ptr.next;
        END;
        DEC(binding.nImposed);
        UpdateIfInstalled(binding, eventDesc, FALSE);
      END;
    END;
  END UninstallGuard; 

(* 
 * install an imposed guard
 *)
PROCEDURE ImposeGuard (binding          : Binding; 
                       guardProc        : PROCANY := NIL;
                       guardClosure     : REFANY  := NIL;
                       postGuardProc    : PROCANY := NIL;
                       postGuardClosure : REFANY  := NIL;
                      ): ImposedGuard
                      RAISES { Error } =
  VAR
    guard               : ImposedGuard;
    useGuardClosure     : BOOLEAN := FALSE;
    passArgs            : BOOLEAN := TRUE;
    usePostGuardClosure : BOOLEAN := FALSE;
  BEGIN
    (* typecheck the guard *)
    IF postGuardProc = NIL THEN
    IF guardProc # NIL THEN
      TypecheckGuard(GetProcDesc(binding.event), GetProcDesc(guardProc),
                     guardClosure, useGuardClosure, passArgs);
    END;
    ELSE
    (* FIXME: no typechecking to accomodate linearized arguments *)
    (*
    IF postGuardProc # NIL THEN
      TypecheckHandler(GetProcDesc(binding.event), GetProcDesc(postGuardProc),
                       FALSE, postGuardClosure, usePostGuardClosure);
    END;
    *)
    (* FIXME: if the postguard is non-nil assume we want a closure *)
    useGuardClosure := guardClosure # NIL;
    usePostGuardClosure := postGuardClosure # NIL;
    END;

    (* create a guard *)
    guard := NEW(ImposedGuard);
    guard.guard               := guardProc;
    guard.useGuardClosure     := useGuardClosure;
    guard.guardClosure        := guardClosure;
    guard.postGuard           := postGuardProc;
    guard.usePostGuardClosure := usePostGuardClosure;
    guard.postGuardClosure    := postGuardClosure;
    guard.binding             := binding;

    (* add it to the binding and update event if necessary *)
    WITH eventDesc = RealGetEventDesc(binding.event) DO
      LOCK eventDesc.lock DO
        VAR
          ptr := binding.imposedGuards;
        BEGIN
          IF postGuardProc = NIL OR ptr = NIL THEN
            binding.imposedGuards := guard;
            guard.next            := ptr;
          ELSE
            BEGIN
              LOOP
                IF ptr.next = NIL THEN
                  ptr.next := guard;
                  guard.next := NIL;
                  EXIT;
                END;
                ptr := ptr.next;
              END;
            END;
          END;
        END;
        INC(binding.nImposed);
        IF guardClosure # NIL THEN StrongRef.Add(guardClosure); END;
        UpdateIfInstalled(binding, eventDesc, TRUE);
      END;
    END;

    RETURN guard;
  END ImposeGuard; 

(*
 * change the event state only if that binding is already installed
 * must be called with the event lock
 *)
PROCEDURE UpdateIfInstalled(binding: Binding; 
                            eventDesc: EventDesc.T; 
                            add: BOOLEAN) RAISES { Error } =
  VAR
    ptr: Binding;
  BEGIN
    ptr := eventDesc.bindings;
    WHILE ptr # NIL DO
      IF ptr = binding THEN
        IF add THEN
          INC(eventDesc.nImposed, ptr.nImposed);
        ELSE
          DEC(eventDesc.nImposed, ptr.nImposed);
        END;
        Activate(eventDesc);
        EXIT;
      END;
      ptr := ptr.next;
    END;
  END UpdateIfInstalled;
      
(******************************************************************************
 *
 *
 *
 *****************************************************************************)

(*
 * actually create a binding
 *)
PROCEDURE RealCreate (eventDesc      : EventDesc.T;
                      guard          : PROCANY;
                      handler        : PROCANY;
                      guardClosure   : REFANY;
                      handlerClosure : REFANY;
                      options        : Options;
                      key            : Auth.Key;
                     ): Binding
                     RAISES { Error } =
  VAR
    binding: Binding;
    useHandlerClosure, useGuardClosure, passGuardArgs: BOOLEAN;
  BEGIN
    (* typecheck the binding *)
    TypecheckBinding(GetProcDesc(eventDesc.event),
                     GetProcDesc(guard),
                     GetProcDesc(handler),
                     eventDesc.saveRegs,
                     guardClosure, handlerClosure, 
                     useGuardClosure, useHandlerClosure, passGuardArgs);

    (* allocate a binding descriptor *)
    binding := NEW (Binding, 
                    event := eventDesc.event, 
                    guard := guard, 
                    handler := handler, 
                    useGuardClosure := useGuardClosure,
                    useHandlerClosure := useHandlerClosure,
                    guardClosure := guardClosure, 
                    handlerClosure := handlerClosure, 
                    cancel := Opt.Cancel IN options,
                    nImposed := 0,
                    options := options,
                    next := NIL);

    (* try to authorize it *)
    AuthCheck(binding, key, eventDesc.event);

    RETURN binding;
  END RealCreate;
    
PROCEDURE RealInstall (binding: Binding; eventDesc: EventDesc.T): BOOLEAN =
  VAR
    ptr: Binding;
  BEGIN
    IF eventDesc.nHandlers = -1 THEN
      RegisterEvent(eventDesc);
    END;
    
    ptr := eventDesc.bindings;
    WHILE ptr # NIL DO
      IF ptr = binding THEN
        RETURN FALSE;
      END;
      ptr := ptr.next;
    END;

    (* keep track of the number of handlers with closures or cancelations *)
    IF binding.useGuardClosure OR binding.useHandlerClosure THEN
      INC(eventDesc.nUseClosure);
    END;
    IF Opt.Cancel IN binding.options THEN INC(eventDesc.nUseCancel); END;
    
    (* update the event descriptor *)
    INC(eventDesc.nHandlers);
    INC(eventDesc.nImposed, binding.nImposed);
    
    (* figure out ordering *)
    IF Opt.First IN binding.options THEN
      (* First goes first *) 
      binding.next := eventDesc.bindings;
      eventDesc.bindings := binding;
      IF eventDesc.lastBinding = NIL THEN
        eventDesc.lastBinding := binding;
      END;
    ELSIF Opt.Last IN binding.options THEN
      (* if it is Last, find the end of the chain *)	
      ptr := eventDesc.lastBinding;
      IF ptr = NIL THEN
        eventDesc.bindings := binding;
      ELSE
        WHILE ptr.next # NIL DO
          ptr := ptr.next;
        END;
	ptr.next := binding;
      END;	
    ELSE
      (* all other (not last and not first) go anywhere else which is *)
      (* after lastBinding which points right before all Last *)
      IF eventDesc.lastBinding # NIL THEN
        binding.next := eventDesc.lastBinding.next;
        eventDesc.lastBinding.next := binding;
      ELSE
        eventDesc.bindings := binding;
      END;
      eventDesc.lastBinding := binding;
    END;

    (* freeze closures *)
    (* FIXME: too conservative, has to do it only for optLevel 3 *)
    IF binding.guardClosure # NIL THEN 
      StrongRef.Add(binding.guardClosure); 
    END;
    IF binding.handlerClosure # NIL THEN 
      StrongRef.Add(binding.handlerClosure); 
    END;
    RETURN TRUE;
  END RealInstall;

(*
 * update the state of the event to reflect uninstallation 
 * return TRUE if the binding was actually insalled before
 *)
PROCEDURE RealUninstall (binding: Binding; eventDesc: EventDesc.T): BOOLEAN =
  VAR
    ptr, prev: Binding;
  BEGIN
    IF eventDesc.nHandlers = -1 THEN
      RegisterEvent(eventDesc);
    END;
    
    (* find the handler to be removed *)
    ptr := eventDesc.bindings;
    prev := NIL;
    WHILE ptr # NIL DO
      IF ptr = binding THEN
        IF prev = NIL THEN
          eventDesc.bindings := binding.next;
        ELSE
          prev.next := binding.next;
        END;
        EXIT;
      END;
      prev := ptr;
      ptr := ptr.next;
    END;

    (* make sure it is not chained with other bindings *)
    binding.next := NIL;
    
    (* binding was not installed *)
    IF ptr = NIL THEN RETURN FALSE; END;
    
    (* keep track of the number of handlers with closures or cancelations *)
    IF binding.useGuardClosure OR binding.useHandlerClosure THEN
      DEC(eventDesc.nUseClosure);
    END;
    IF binding.cancel THEN DEC(eventDesc.nUseCancel); END;
    
    (* update the list *)
    IF ptr = eventDesc.lastBinding THEN
      eventDesc.lastBinding := prev;
    END;
    
    (* update the event descriptor *)
    DEC(eventDesc.nHandlers);
    DEC(eventDesc.nImposed, ptr.nImposed);
    
    RETURN TRUE;
  END RealUninstall;
  
(*
 * update the systems state to reflect the changes done in an event 
 *)
PROCEDURE Activate (eventDesc: EventDesc.T) RAISES { Error } =
  BEGIN
    (* check the state of the event *)
    CheckRegistration (eventDesc);
    
    (* activate the new handler list *)
    UpdateHandlerLists(eventDesc);

    (* try to find the best stub *)
    InstallStub(eventDesc, FindOptLevel(eventDesc), FindTrace(eventDesc));
  END Activate;

(*
 * check whether the event should be in the default or active state
 *)
PROCEDURE CheckRegistration (eventDesc: EventDesc.T) =
  BEGIN
    IF eventDesc.nHandlers = 1 AND 
      eventDesc.bindings.handler = eventDesc.defBinding.handler AND
      eventDesc.nImposed = 0 AND eventDesc.resultHandlers = NIL AND
      NOT eventDesc.keepStub
     THEN
      UnregisterEvent(eventDesc);
    ELSE
      IF eventDesc.nHandlers = -1 THEN
        RegisterEvent(eventDesc);
      END;
    END;
  END CheckRegistration;

(*
 * extract event descriptor from a binding
 *)
PROCEDURE TryGetEventDesc (binding: Binding): EventDesc.T =
  VAR 
    eventDesc: EventDesc.T;
  BEGIN
    TRY
      eventDesc := GetEventDesc(binding.event);
    EXCEPT
    | Error => 
      IO.PutError("Dispatcher inconsistency: invalid event inside binding\n");
      <* ASSERT FALSE *>
    END;
    RETURN eventDesc;
  END TryGetEventDesc;

(*
 * create simplified lists of handlers and update handler information
 * in all the descriptors
 *)

PROCEDURE UpdateHandlerLists (eventDesc: EventDesc.T) =
  VAR
    oldHandlers : REF ARRAY OF Word.T;
    handlers    : REF ARRAY OF Word.T;
    handlerPtr  : ADDRESS;
    size        : INTEGER;
    alias       : AliasDesc.T;
    idx         : INTEGER;
    binding     : Binding;
    guard       : ImposedGuard;
  BEGIN
    (* create the new list of handlers *)
    size := ADRSIZE(DispatchDesc);
    IF eventDesc.nHandlers = -1 THEN
      INC(size, 2 * ADRSIZE(HandlerDesc));
    ELSE
      INC(size, (eventDesc.nHandlers+1) * ADRSIZE(HandlerDesc) +
        eventDesc.nImposed * ADRSIZE(ImposedDesc));
    END;
    handlers := NEW(REF ARRAY OF Word.T, size DIV ADRSIZE(Word.T));
      
    (* initialize the list of handlers with dispatch information *)
    WITH rhs = eventDesc.resultHandlers DO
      IF rhs # NIL THEN
        handlers[0] := rhs.defaultResult;
        handlers[1] := SafeConvert.AdrToWord(rhs.defaultHandler);
        handlers[2] := SafeConvert.RefAnyToWord(rhs.defaultClosure);
        handlers[3] := ORD(rhs.useDefaultClosure);
        handlers[4] := SafeConvert.AdrToWord(rhs.resultHandler);
        handlers[5] := SafeConvert.RefAnyToWord(rhs.resultClosure);
        handlers[6] := ORD(rhs.useResultClosure);
      ELSE
        handlers[0] := 0;
        handlers[1] := 0;
        handlers[2] := 0;
        handlers[3] := 0;
        handlers[4] := 0;
        handlers[5] := 0;
        handlers[6] := 0;
      END;
    END;
    idx := 7;

    binding := eventDesc.bindings;
    WHILE binding # NIL DO
      handlers[idx+0] := binding.nImposed;
      handlers[idx+1] := SafeConvert.AdrToWord(binding.guard);
      handlers[idx+2] := SafeConvert.AdrToWord(binding.handler);
      handlers[idx+3] := ORD(binding.cancel);
      handlers[idx+4] := ORD(binding.useGuardClosure);
      handlers[idx+5] := ORD(binding.useHandlerClosure);
      handlers[idx+6] := SafeConvert.RefAnyToWord(binding.guardClosure);
      handlers[idx+7] := SafeConvert.RefAnyToWord(binding.handlerClosure);
      INC(idx, 8);
      guard := binding.imposedGuards;
      WHILE guard # NIL DO
        handlers[idx+0] := SafeConvert.AdrToWord(guard.guard);
        handlers[idx+1] := ORD(guard.useGuardClosure);
        handlers[idx+2] := SafeConvert.RefAnyToWord(guard.guardClosure);
        handlers[idx+3] := SafeConvert.AdrToWord(guard.postGuard);
        handlers[idx+4] := ORD(guard.usePostGuardClosure);
        handlers[idx+5] := SafeConvert.RefAnyToWord(guard.postGuardClosure);
        INC(idx, 6);
        guard := guard.next;
      END;
      binding := binding.next;
    END;

    (* sentinel *)
    handlers[idx+2] := 0;
    
    (* make sure that GC does not move the list of handlers *)
    oldHandlers := eventDesc.handlers;
    StrongRef.Add(handlers);
    handlerPtr := ADR(handlers[0]);

    (* update the lists in the event descriptor and all alias descriptors *)
    eventDesc.handlers := handlers;
    alias := eventDesc.aliases;
    WHILE alias # NIL DO
      alias.handlers := handlerPtr;
      alias := alias.next;
    END;
      
    (* enable moving of old list of handlers, the only outstanding *)
    (* references are those grabbed by dispatcher stubs and are kept *)
    (* on a stack for the duration of dispatch. The list will be collected *)
    (* after all dispatches are completed *)
    StrongRef.Remove(oldHandlers);

    handlers := NIL;
    oldHandlers := NIL;
  END UpdateHandlerLists;

PROCEDURE FindTrace (eventDesc: EventDesc.T): BOOLEAN =
  BEGIN
    RETURN traceAll OR eventDesc.trace;
  END FindTrace;

PROCEDURE GetOptLevel (event: PROCANY): INTEGER RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
  BEGIN
    RETURN FindOptLevel(eventDesc);
  END GetOptLevel;

PROCEDURE FindOptLevel (eventDesc: EventDesc.T): INTEGER RAISES { Error } =
  VAR
    optLevel: INTEGER;
  BEGIN
    (* findout the optimization level to be used for this event *)
    optLevel := eventDesc.optLevel;
    IF optLevel = -1 THEN
      optLevel := DefaultOptLevel;
    END;

    (* always keep the simplest stub *) 
    IF eventDesc.keepStub THEN
      optLevel := 0;
    END;

    (* higher levels of optimizations available for only up to 64 handlers *)
    IF eventDesc.nHandlers > 64 AND optLevel > 2 THEN
      optLevel := 2;
    END;

    (* profiling won't work with levels 5 stubs *)
    IF ProfileSupport.CompiledIn THEN optLevel := MIN(optLevel, 4); END;

    (* FIXME: add support for default handling to all optimization levels *)
    IF eventDesc.resultHandlers # NIL AND
      (eventDesc.resultHandlers.defaultHandler # NIL OR
      eventDesc.resultHandlers.defaultResult # 0)
     THEN
      optLevel := 0;
    END;
    
    (* FIXME: a gross hack for Robert *)
    IF eventDesc.resultHandlers # NIL AND
      eventDesc.resultHandlers.resultHandler # NIL
     THEN
      IF eventDesc.resultHandlers.resultPassArgs THEN
        optLevel := 0;
      ELSIF optLevel # 5 THEN
        optLevel := 0; 
      END;
    END;

    (* FIXME *)
    IF eventDesc.resultHandlers # NIL THEN
      optLevel := 0;
    END;

    (* FIXME: no support for imposed guards at level 1 and 2 *)
    IF (optLevel = 1 OR optLevel = 2) AND eventDesc.nImposed # 0 THEN
      IO.Put("IMPOSED\n");
      optLevel := 0;
    END;

    (* FIXME: postguards *)
    IF eventDesc.nImposed # 0 THEN
      VAR
        binding := eventDesc.bindings;
      BEGIN
        WHILE optLevel # 0 AND binding # NIL DO
          VAR
            ptr := binding.imposedGuards;
          BEGIN
            WHILE ptr # NIL DO
              IF ptr.postGuard # NIL THEN
                IF optLevel # 5 AND optLevel # 4 THEN
                  optLevel := 0;
                END;
                EXIT;
              END;
              ptr := ptr.next;
            END;
          END;
          binding := binding.next;
        END;
      END;
    END;

    IF optLevel < 0 AND optLevel > MaxOptLevel THEN
      IO.PutError("Dispatcher: unsupported optimization level\n");
      <* ASSERT FALSE *>
    END;

    IF optLevel <= 1 AND eventDesc.nHandlers > MaxNumOfHandlersForDebug THEN
      IO.PutError (
          "Dispatcher: maximum number of handlers per event exceeded\n");
      RAISE Error(ErrorCode.CodeGenerationError);
    END;

    IF optLevel <= 1 AND
      NumberOfArgs(eventDesc) > MaxNumOfArgumentsForDebug
     THEN
      IO.PutError (
          "Dispatcher: maximum number of arguments of an event exceeded\n");
      RAISE Error(ErrorCode.CodeGenerationError);
    END;

    RETURN optLevel;
  END FindOptLevel;

(*
 * generate and install optimal stub for dispatch
 * assumes that the lock on the record is held
 *)

PROCEDURE InstallStub (eventDesc: EventDesc.T; 
                       optLevel: INTEGER; trace: BOOLEAN) RAISES { Error } =
  VAR
    stub: PROCANY := NIL;
  BEGIN
    IF Debug THEN
      RTIO.PutText ("InstallStub:: event: " & Textify.Address(eventDesc.event)&
        " no args: " & Fmt.Int(NumberOfArgs(eventDesc)) & 
        " opt level: "  & Fmt.Int(eventDesc.optLevel) & 
        " no handlers: " & Fmt.Int(eventDesc.nHandlers) & "\n");
    END;

    (*
     * choose which stub should be used for the given event 
     * given its options and optimization level, most of this code
     * is for debugging and measurement purposes and could removed
     * from a production system
     *)
    IF NOT eventDesc.keepStub THEN
      IF eventDesc.nHandlers = -1 THEN
        (* unused event, return to the original procedure *)
        stub := eventDesc.event;
        (* FIXME: make a clone of the procedure 
           ELSIF eventDesc.nHandlers = 0 THEN
           (* always raise an exception *)
              stub := RaiseNoHandlerInvoked;
        *)
      ELSIF eventDesc.nHandlers = 1 AND NOT eventDesc.saveRegs
       THEN
        VAR 
          binding := eventDesc.bindings;
          procDesc: RTProcDescF.T;
        BEGIN
          procDesc := GetProcDescNoLock(binding.handler);
          IF binding.guard = NIL AND 
            NOT (binding.useGuardClosure OR binding.useHandlerClosure) AND
            procDesc.type = NIL
           THEN
            stub := eventDesc.bindings.handler;
          END;
        END;
      END;
    END;

    IF stub = NIL THEN
      stub := GetStub(eventDesc, optLevel, trace);
    END;
    
    (* overwrite the entry in the jtbl *)
    IF stub # eventDesc.stub THEN
      eventDesc.stub := stub;
      IF eventDesc.procDesc.jtblPtr = NIL THEN
        RTIO.PutText(
            "ERROR >> Dispatcher: internal procedure used as event: ");
        RTIO.PutAddr(eventDesc.event); RTIO.PutText("\n");
        <* ASSERT FALSE *>
      END;

      IF Debug THEN
        RTIO.PutText("InstallStub:: event: "); RTIO.PutAddr(eventDesc.event);
        RTIO.PutText(" jtblPtr: "); RTIO.PutAddr(eventDesc.procDesc.jtblPtr);
        RTIO.PutText(" old stub: "); RTIO.PutAddr(eventDesc.procDesc.jtblPtr^);
        RTIO.PutText(" new stub: "); RTIO.PutAddr(stub);
        RTIO.PutText("\n");
      END;

      eventDesc.procDesc.jtblPtr^ := stub;
    END;

    IF Debug THEN
      RTIO.PutText("InstallStub:: event: " & Textify.Address(eventDesc.event) &
        " stub: " & Textify.Address(stub) & "\n");
    END;
  END InstallStub;

(******************************************************************************
 *
 * aliases
 *
 *****************************************************************************)

(*
 * cons up a descriptor for an alias
 *)

PROCEDURE CreateAliasDesc (eventDesc: EventDesc.T): AliasDesc.T =
  VAR
    desc: AliasDesc.T;
  BEGIN
    (* create an alias for the event dispach stub *)
    desc := NEW (AliasDesc.T);
    desc.eventDesc := eventDesc;
    desc.lock := NEW (MUTEX);
    
    (* add it to the alias list *)
    desc.next := eventDesc.aliases;
    eventDesc.aliases := desc;

    (* set its handler list *)
    IF eventDesc.handlers # NIL THEN
      desc.handlers := ADR(eventDesc.handlers[0]);
    END;

    RETURN desc;
  END CreateAliasDesc;

PROCEDURE SetOptLevel (event: PROCANY; optLevel: INTEGER)
                       RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
  BEGIN
    IF Debug THEN
      RTIO.PutText ("Dispatcher.SetOptLevel: " & 
        Textify.Address(eventDesc.event) & 
        " " & Fmt.Int(optLevel) & "\n");
    END;

    IF optLevel < 0 OR optLevel > MaxOptLevel THEN
      IO.PutError("Dispatcher.SetOptLevel:: incorrect optimization level " &
        Textify.Address(eventDesc.event) & " " & Fmt.Int(optLevel) & "\n");
      RAISE Error(ErrorCode.CodeGenerationError);
    END;

    LOCK eventDesc.lock DO
      (* set the level for the event *)
      eventDesc.optLevel := optLevel;
      
      (* reinstall a stub if it already exist *)
      IF Initialized(eventDesc) THEN 
        InstallStub(eventDesc, FindOptLevel(eventDesc), FindTrace(eventDesc));
      END;
    END;
  END SetOptLevel;

PROCEDURE SetDefaultOptLevel(optLevel: INTEGER)
                             RAISES { Error } = 
  VAR
    eventDesc: EventDesc.T;
    procPtr: RT0.ProcPtr;
    proc: PROCANY;
    module: RT0.ModulePtr;
  BEGIN
    IF Debug THEN
      RTIO.PutText ("Dispatcher.SetDefaultOptLevel: " & 
        Textify.Address(eventDesc.event) & " " & Fmt.Int(optLevel) & "\n");
    END;

    IF optLevel < 0 OR optLevel > MaxOptLevel THEN
      IO.PutError(
          "Dispatcher.SetDefaultOptLevel:: incorrect optimization level " &
          Textify.Address(eventDesc.event) & " " & Fmt.Int(optLevel) & "\n");
      RAISE Error(ErrorCode.CodeGenerationError);
    END;

    DefaultOptLevel := optLevel;

    LOCK EventLock DO
      (* scan module descriptors for exported procedures *)
      FOR i := 0 TO RTModule.Count() - 1 DO
        module := RTModule.Get(i);
        procPtr := module.proc_info;
        IF procPtr # NIL THEN
          WHILE (procPtr.proc # NIL) DO
            proc := procPtr.proc;

            (*
             * reinstall the stub for those events for which one already exists
             *  and which use the default optimization level
             *)
            IF EventTable.get(proc, eventDesc) AND 
              Initialized(eventDesc) AND eventDesc.optLevel = -1
             THEN
              TRY
                InstallStub(eventDesc, 
                            FindOptLevel(eventDesc), FindTrace(eventDesc));
              EXCEPT
              | Error => 
                IO.PutError(
                  "Dispatcher.SetDefaultOptLevel: stub installation failed\n");
              END;
            END;

            (* next procedure *)
            INC (procPtr, ADRSIZE (procPtr^));
          END;
        END;
      END;
    END;
  END SetDefaultOptLevel;


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
(* procedure types used for debug dispatching, FuncH for handlers, FuncB for *)
(* guards.  The extra argument (dummy) is reserved for passing closure,      *)
(* closure is passed as the first argument and all caller's arguments are    *)
(* shifted by one so that the last one is passed in the dummy argument       *)
(* If the event is set to use callee saved registers then its number of      *)
(* arguments must be less by the number of saved registers NumSavedRegisters *)

TYPE
  FuncH  = PROCEDURE (a1:   INTEGER; a2:   INTEGER; a3:   INTEGER;
                      a4:   INTEGER; a5:   INTEGER; a6:   INTEGER; 
                      a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                      a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                      a13:  INTEGER; a14:  INTEGER; a15:  INTEGER; 
                      dummy: INTEGER
                     ): INTEGER;

  FuncG = PROCEDURE (a1:  INTEGER; a2:  INTEGER; a3:  INTEGER;
                     a4:  INTEGER; a5:  INTEGER; a6:  INTEGER;
                     a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                     a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                     a13:  INTEGER; a14:  INTEGER; a15:  INTEGER; 
                     dummy: INTEGER
                    ): BOOLEAN;

  FuncR = PROCEDURE (VAR finalResult: INTEGER; thisResult: INTEGER; 
                     last: BOOLEAN; VAR arg: REFANY;
                     a1:   INTEGER; a2:   INTEGER; a3:   INTEGER;
                     a4:   INTEGER; a5:   INTEGER; a6:   INTEGER; 
                     a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                     a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                     a13:  INTEGER; a14:  INTEGER; a15:  INTEGER);

  FuncRCl = PROCEDURE (closure: REFANY;
                       VAR finalResult: INTEGER; thisResult: INTEGER; 
                       last: BOOLEAN; VAR arg: REFANY;
                       a1:   INTEGER; a2:   INTEGER; a3:   INTEGER;
                       a4:   INTEGER; a5:   INTEGER; a6:   INTEGER; 
                       a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                       a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                       a13:  INTEGER; a14:  INTEGER; a15:  INTEGER);

  FuncNR = PROCEDURE (last: BOOLEAN; VAR arg: REFANY;
                      a1:   INTEGER; a2:   INTEGER; a3:   INTEGER;
                      a4:   INTEGER; a5:   INTEGER; a6:   INTEGER; 
                      a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                      a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                      a13:  INTEGER; a14:  INTEGER; a15:  INTEGER);

  FuncNRCl = PROCEDURE (closure: REFANY;
                        last: BOOLEAN; VAR arg: REFANY;
                        a1:   INTEGER; a2:   INTEGER; a3:   INTEGER;
                        a4:   INTEGER; a5:   INTEGER; a6:   INTEGER; 
                        a7:   INTEGER; a8:   INTEGER; a9:   INTEGER; 
                        a10:  INTEGER; a11:  INTEGER; a12:  INTEGER; 
                        a13:  INTEGER; a14:  INTEGER; a15:  INTEGER);

(*
 * debug dispatch routine
 *)
PROCEDURE DebugDispatch (    desc : AliasDesc.T;
                         VAR args : DebugDispatchArgs): INTEGER 
                        RAISES ANY =
  VAR 
    eventDesc   : EventDesc.T := desc.eventDesc;
    wc          : FuncG;
    hdl         : FuncH;
    cnt         : INTEGER;
    invoked     : BOOLEAN;
    invokeFlags : ARRAY [0..MaxNumOfHandlersForDebug-1] OF BOOLEAN;

    (* these are pointers into packed dispatch information, OK to
       have untraced to traced pointers because we use them only locally
       in this procedure and the real referent is strongref-ed *)
    dispDesc    : UNTRACED REF DispatchDesc; <* NOWARN *>
    handlers    : UNTRACED REF HandlerDesc; <* NOWARN *>
    guards      : UNTRACED REF ImposedDesc; <* NOWARN *>

    (* result handling procedures *)
    defaultHandler                      : FuncH;
    resultHandler                       : FuncR;
    resultHandlerCl                     : FuncRCl;
    resultHandlerNR                     : FuncNR;
    resultHandlerNRCl                   : FuncNRCl;
    resultClosure, defaultClosure       : REFANY;
    useResultClosure, useDefaultClosure : BOOLEAN;

    (* variables passed to result handling procedures *)
    result, finalResult : INTEGER;
    last                : BOOLEAN;
    invocationArgument  : REFANY;

    (* FIXME: postguard stuff *)
    pwc: PROCANY;
    pwcuc: BOOLEAN;
    pwcc: REFANY;
    dontCall: BOOLEAN;
  BEGIN
    (* no handler list indicates that the event is in the default state *)
    (* so call the original handler, otherwise iterate over the list *)
    (* of handlers and call the guards and handlers in turn *)
    handlers := desc.handlers;

    (* the event may be dispatched without being initialized *)
    IF handlers = NIL THEN
      result := LOOPHOLE(eventDesc.event,FuncH)(args[1], args[2], args[3], 
                                                args[4], args[5], args[6], 
                                                args[7], args[8], args[9],
                                                args[10], args[11], args[12],
                                                args[13], args[14], args[15],
                                                0);
      RETURN result;
    END;

    dispDesc          := desc.handlers;
    finalResult       := dispDesc.defaultResult;
    defaultHandler    := dispDesc.defaultHandler;
    useDefaultClosure := dispDesc.useDefaultClosure;
    defaultClosure    := dispDesc.defaultClosure;
    resultHandler     := dispDesc.resultHandler;
    resultHandlerCl   := dispDesc.resultHandler;
    resultHandlerNR   := dispDesc.resultHandler;
    resultHandlerNRCl := dispDesc.resultHandler;
    resultClosure     := dispDesc.resultClosure;
    useResultClosure  := dispDesc.useResultClosure;

    INC(handlers, ADRSIZE(DispatchDesc));

    (* evaluate all the guards *)
    cnt := 0;
    invoked := FALSE;
    LOOP
      hdl := LOOPHOLE(handlers.handler, FuncH);
      IF hdl = NIL THEN EXIT; END;
      invokeFlags[cnt] := TRUE;
      IF handlers.nImposed # 0 THEN
        guards := handlers + ADRSIZE(handlers^);
        FOR i := 1 TO handlers.nImposed DO
          (* FIXME: postguards *)
          IF guards.postGuard = NIL THEN
            wc := LOOPHOLE(guards.guard, FuncG);
            IF ((wc = NIL) OR 
              (guards.useGuardClosure AND
              wc(SafeConvert.RefAnyToWord(guards.guardClosure),
                 args[1], args[2], args[3], args[4], args[5], args[6],
                 args[7], args[8], args[9], args[10], 
                 args[11], args[12], args[13], args[14], args[15])) OR
                 (NOT guards.useGuardClosure AND
              wc(args[1], args[2], args[3], args[4], args[5], args[6],
                 args[7], args[8], args[9], args[10], 
                 args[11], args[12], args[13], args[14], args[15], 0)))
             THEN
            ELSE
              invokeFlags[cnt] := FALSE;
              EXIT;
            END;
          END;
          INC(guards, ADRSIZE(guards^));
        END;
      END;
      IF invokeFlags[cnt] THEN
        wc := LOOPHOLE(handlers.guard, FuncG);
        IF ((wc = NIL) OR 
            (handlers.useGuardClosure AND 
             wc(SafeConvert.RefAnyToWord(handlers.guardClosure),
                args[1], args[2], args[3], args[4], args[5], args[6],
                args[7], args[8], args[9], args[10], 
                args[11], args[12], args[13], args[14], args[15])) OR
            (NOT handlers.useGuardClosure AND
             wc(args[1], args[2], args[3], args[4], args[5], args[6],
                args[7], args[8], args[9], args[10], 
                args[11], args[12], args[13], args[14], args[15], 0)))
         THEN
          invoked := TRUE;
          IF handlers.cancel THEN 
            INC(cnt);
            EXIT; 
          END;
        ELSE
          invokeFlags[cnt] := FALSE;
        END;    
      END;
      INC(cnt);
      INC(handlers, 
          ADRSIZE(HandlerDesc) + handlers.nImposed * ADRSIZE(ImposedDesc));
    END;

    (* no handler will be invoked *)
    (* FIXME: postguards *)
    IF NOT invoked THEN
      IF defaultHandler # NIL THEN
        IF useDefaultClosure THEN
          result := defaultHandler(SafeConvert.RefAnyToWord(defaultClosure),
                                   args[1], args[2], args[3], args[4], args[5],
                                   args[6], args[7], args[8], args[9], 
                                   args[10], args[11], args[12], args[13], 
                                   args[14], args[15]);
          RETURN result;
        ELSE
          result := defaultHandler(args[1], args[2], args[3], args[4], args[5],
                                   args[6], args[7], args[8], args[9], 
                                   args[10], args[11], args[12], args[13],
                                   args[14], args[15], 0);
          RETURN result;
        END;
      ELSE
        IF HasResult(eventDesc) THEN
          RAISE SpinException.Exception(
                  SpinException.ExceptionInfo{
                       code := SpinException.ExceptionCode.NoHandlerInvoked,
                       msg  := SpinException.ExceptionNames[
                               SpinException.ExceptionCode.NoHandlerInvoked]});
        ELSE
          (* the event actually does not require a result *)
          RETURN 0;
        END;
      END;
    END;

    (* evaluate the handlers that should be invoked *)
    handlers := desc.handlers;
    INC(handlers, ADRSIZE(DispatchDesc));
    FOR i := 0 TO cnt-1 DO
      hdl := LOOPHOLE(handlers.handler, FuncH);
      IF invokeFlags[i] THEN

        (* FIXME: postguards *)
        guards := handlers + ADRSIZE(handlers^);
        pwc := NIL;
        dontCall := FALSE;
        FOR i := 1 TO handlers.nImposed DO
          pwc := LOOPHOLE(guards.postGuard, FuncG);
          IF pwc # NIL THEN
            pwcuc := guards.usePostGuardClosure;
            pwcc  := guards.postGuardClosure;
            wc := LOOPHOLE(guards.guard, FuncG);
            IF ((wc = NIL) OR 
              (guards.useGuardClosure AND
              wc(SafeConvert.RefAnyToWord(guards.guardClosure),
                 args[1], args[2], args[3], args[4], args[5], args[6],
                 args[7], args[8], args[9], args[10], 
                 args[11], args[12], args[13], args[14], args[15])) OR
                 (NOT guards.useGuardClosure AND
              wc(args[1], args[2], args[3], args[4], args[5], args[6],
                 args[7], args[8], args[9], args[10], 
                 args[11], args[12], args[13], args[14], args[15], 0)))
             THEN
            ELSE
              dontCall := TRUE;
            END;
            (* FIXME: we assume only one postguard *)
            EXIT;
          END;
          INC(guards, ADRSIZE(guards^));
        END;

        (* FIXME: postguards *)
        IF NOT dontCall THEN
        IF handlers.useHandlerClosure THEN
          result := hdl(SafeConvert.RefAnyToWord(handlers.handlerClosure),
                        args[1], args[2], args[3], args[4], args[5], args[6], 
                        args[7], args[8], args[9], args[10], 
                        args[11], args[12], args[13], args[14], args[15]);
        ELSE
          result := hdl(args[1], args[2], args[3], args[4], args[5], args[6], 
                        args[7], args[8], args[9], args[10], 
                        args[11], args[12], args[13], args[14], args[15], 0);
        END;
        END;

        (* FIXME: postguards *)
        IF pwc # NIL AND NOT dontCall THEN
          hdl := LOOPHOLE(pwc, FuncH);
          IF pwcuc THEN
            EVAL hdl(SafeConvert.RefAnyToWord(pwcc),
                     args[1], args[2], args[3], args[4], args[5], args[6],
                     args[7], args[8], args[9], args[10], 
                     args[11], args[12], args[13], args[14], args[15]);
          ELSE
            EVAL hdl(args[1], args[2], args[3], args[4], args[5], args[6],
                     args[7], args[8], args[9], args[10], 
                     args[11], args[12], args[13], args[14], args[15], 0);
          END;
        END;

        IF resultHandler # NIL THEN
          last := i = cnt-1;
          IF HasResult(eventDesc) THEN
            IF useResultClosure THEN
              resultHandlerCl(resultClosure, finalResult, result, 
                              last, invocationArgument,
                              args[1], args[2], args[3], args[4], args[5],
                              args[6], args[7], args[8], args[9], args[10], 
                              args[11], args[12], args[13], args[14], 
                              args[15]);

            ELSE
              resultHandler(
                  finalResult, result, last, invocationArgument,
                  args[1], args[2], args[3], args[4], args[5],
                  args[6], args[7], args[8], args[9], args[10], 
                  args[11], args[12], args[13], args[14], args[15]);
            END;
            (* FIXME: this requires synchronization *)
            (* FIXME: require installation with the right flag *)
            IF eventDesc.resultHandlers # NIL AND
              eventDesc.resultHandlers.resultHandler # NIL AND
              NOT eventDesc.resultHandlers.resultPassArgs
             THEN
              finalResult := result;
            END;
          ELSE
            IF useResultClosure THEN
              resultHandlerNRCl(resultClosure, 
                                last, invocationArgument,
                                args[1], args[2], args[3], args[4], args[5],
                                args[6], args[7], args[8], args[9], args[10], 
                                args[11], args[12], args[13], args[14], 
                                args[15]);
            ELSE
              resultHandlerNR(last, invocationArgument,
                              args[1], args[2], args[3], args[4], args[5],
                              args[6], args[7], args[8], args[9], args[10], 
                              args[11], args[12], args[13], args[14], 
                              args[15]);
            END;
          END;
        ELSE
          finalResult := result;
        END;
      END;   
      INC(handlers, 
          ADRSIZE(HandlerDesc) + handlers.nImposed * ADRSIZE(ImposedDesc));
    END;
    RETURN finalResult;
  END DebugDispatch;

(******************************************************************************
 * 
 * asynchronous dispatch
 *
 *****************************************************************************)

(*
 * asynchronous dispatch routine
 *)

PROCEDURE AsynchDispatch(self: AsynchClosure): REFANY =
  BEGIN
    TRY
      EVAL DebugDispatch(self.aliasDesc, self.args);
    EXCEPT
    ELSE
      (* FIXME *)
    END;
    RETURN NIL;
  END AsynchDispatch;


(*
 * create a default binding
 * assumes that the lock on the record has already been taken
 *)

PROCEDURE CreateDefaultBinding (eventDesc: EventDesc.T) =
  BEGIN
    eventDesc.defBinding := NEW (Binding, 
                                 event := eventDesc.event, guard := NIL, 
                                 handler := eventDesc.event, next := NIL);
  END CreateDefaultBinding;

(*
 * an event used for the first time, time to do lazy initialization
 *)

PROCEDURE RegisterEvent (eventDesc: EventDesc.T) =
  BEGIN
    <* ASSERT eventDesc.nHandlers = -1 *>

    (* initialize the event if necessary *)
    IF NOT Initialized(eventDesc) THEN 
      <* ASSERT eventDesc.stubs[0] = NIL *>
      InitializeEvent(eventDesc);
    END;

    (* we don't have to initialize the default handler entires because either*)
    (* they are initialized by InitializeEvent() or left correct by Uninstall*)
    eventDesc.nHandlers := 1;
  END RegisterEvent;

(*
 * event returning to the default state
 *)

PROCEDURE UnregisterEvent (eventDesc: EventDesc.T) =
  BEGIN
    IF eventDesc.procDesc.jtblPtr = NIL THEN
      RTIO.PutText("ERROR >> Dispatcher: internal procedure used as event: ");
      RTIO.PutAddr(eventDesc.event); RTIO.PutText("\n");
      <* ASSERT FALSE *>
    END;

    (* overwrite the entry in the jtbl with the original procedure *)
    eventDesc.procDesc.jtblPtr^ := eventDesc.event;
    eventDesc.nHandlers := -1;
  END UnregisterEvent;

(*
 * Saving state can be set only once for an event and no more aliases 
 * are allowed. The reason is so that someone cannot bypass saving of the
 * registers by using an alias with different properties
 *)

PROCEDURE SetSave (event: PROCANY) 
                   RAISES { Error } =
  VAR
    eventDesc: EventDesc.T := GetEventDesc(event);
    max := MaxNumOfArgumentsForDebug - MachineDispatcher.NumSavedRegisters;
  BEGIN
    LOCK eventDesc.lock DO
      IF Initialized(eventDesc) THEN
        IO.PutError("Dispatcher: SetSave has to be called " &
                    "before any other operation on that event\n");
        RAISE Error(ErrorCode.SaveRegsError);
      END;
      IF NumberOfArgs(eventDesc) > max THEN
        IO.PutError("Dispatcher: event that uses callee saved registers " &
                    "can have at most " & Fmt.Int(max) & " arguments\n");
        RAISE Error(ErrorCode.SaveRegsError);
      END;

      eventDesc.saveRegs := TRUE;
      InitializeEvent(eventDesc);
    END;
  END SetSave;


(*
 * create a stub and a default binding for an event 
 * assumes that the lock on the record has already been taken
 *)

PROCEDURE InitializeEvent (eventDesc: EventDesc.T) =
  VAR
    aptr: AliasDesc.T;
  BEGIN
    (* create a default binding *)
    IF eventDesc.defBinding = NIL THEN
      CreateDefaultBinding (eventDesc);
    END;

    (* initialize the record *)
    eventDesc.handlers := NEW (REF ARRAY OF Word.T, 2);
    eventDesc.handlers[0] := 0;
    eventDesc.handlers[0] := SafeConvert.AdrToWord(eventDesc.event);
    eventDesc.handlers[0] := 0;
    eventDesc.handlers[0] := 0;
    eventDesc.handlers[0] := 0;
    eventDesc.handlers[0] := 0;
    eventDesc.handlers[0] := 0;
    StrongRef.Add(eventDesc.handlers);

    eventDesc.bindings := eventDesc.defBinding;
    eventDesc.lastBinding := eventDesc.defBinding;

    (* initialize the existing alias descriptors *)
    aptr := eventDesc.aliases;
    WHILE aptr # NIL DO
      (* IMPLICIT SYNCHRONIZATION *)
      aptr.handlers := ADR(eventDesc.handlers[0]);
      aptr := aptr.next;
    END;
  END InitializeEvent;

PROCEDURE Initialized(eventDesc: EventDesc.T): BOOLEAN =
  BEGIN
    RETURN eventDesc.defBinding # NIL;
  END Initialized;

(******************************************************************************
 * 
 * generation of stubs
 *
 *****************************************************************************)

PROCEDURE GetStub(eventDesc: EventDesc.T; 
                  optLevel: INTEGER; trace: BOOLEAN): PROCANY
                  RAISES { Error } = 
  VAR
    desc: AliasDesc.T;
    stub: PROCANY;
  BEGIN
    (* stubs for optimization level less than 3 are cached *)
    (* stubs with tracing are not cached *)
    IF optLevel < 3 AND eventDesc.stubs[optLevel] # NIL AND NOT trace THEN
      RETURN eventDesc.stubs[optLevel];
    END;

    (*
     * create a new stub
     *)
    desc := CreateAliasDesc(eventDesc);
    StrongRef.Add(desc);
    stub := Stitcher.CloneStub(desc, 
                               optLevel,
                               NumberOfArgs(eventDesc), 
                               HasResult(eventDesc),
                               eventDesc.saveRegs,
                               trace);
    IF optLevel < 3 AND NOT trace THEN
      eventDesc.stubs[optLevel] := stub;
    END;
    desc.aid := stub;
    IF AliasTable.put(stub, desc) THEN
      IO.PutError("Dispatcher.InitializeEvent:: duplicate alias\n");
      RAISE Error(ErrorCode.InternalError);
    END;
    RETURN stub;
  END GetStub; 

(*
 *
 *)

PROCEDURE KeepStub(event: PROCANY; flag: BOOLEAN := TRUE) 
                   RAISES { Error } =
  VAR
    eventDesc: EventDesc.T := GetEventDesc(event);
  BEGIN
    IF Debug THEN
      IO.Put("Dispatcher.KeepStub: " & 
        Textify.Address(eventDesc.event) & "\n");
    END;

    LOCK eventDesc.lock DO
      eventDesc.keepStub := flag;

      (* make sure the stub exists *)
      IF NOT Initialized(eventDesc) THEN
        InitializeEvent(eventDesc);
      END;

      Activate(eventDesc);
    END;
  END KeepStub;

(*
 * return event descriptor for a procedure pointer
 *)
PROCEDURE GetEventDesc (event: PROCANY): EventDesc.T
                        RAISES { Error } =
  BEGIN
    IF event = NIL THEN 
      IO.PutError ("Dispatcher.GetEventDesc: invalid procedure\n");
      RAISE Error(ErrorCode.InvalidProcedure);
    END;
    RETURN RealGetEventDesc (event);
  END GetEventDesc;

PROCEDURE RealGetEventDesc (event: PROCANY): EventDesc.T
                            RAISES { Error } =
  VAR
    aliasDesc : AliasDesc.T;
  BEGIN
    IF event = NIL THEN 
      IO.PutError ("Dispatcher.GetEventDesc: invalid procedure\n");
      RAISE Error(ErrorCode.InvalidProcedure);
    END;
    LOCK EventLock DO
      (* check aliases because event may be actually an alias *)
      IF NOT AliasTable.get(event, aliasDesc) THEN
        RETURN CreateEventDesc(event);
      END;
    END;
    RETURN aliasDesc.eventDesc;
  END RealGetEventDesc;

(* 
 * Create event and alias descriptor for a given event.
 * Assumes that the lock is already taken and event is the original
 * procedure and not an alias.
 *)

PROCEDURE CreateEventDesc(event: PROCANY): EventDesc.T 
                          RAISES { Error } =
  VAR
    procDesc  : RTProcDescF.T;
    eventDesc : EventDesc.T;
    aliasDesc : AliasDesc.T;
  BEGIN
    procDesc := GetProcDescNoLock(event);

    (* create event descriptor *)
    eventDesc := NEW (EventDesc.T);

    eventDesc.event := event;
    eventDesc.procDesc := procDesc;
    eventDesc.nHandlers := -1;
    eventDesc.handlers := NIL;
    eventDesc.optLevel := -1;
    eventDesc.nImposed := 0;
    eventDesc.lock := NEW (MUTEX);
    eventDesc.auth := NEW(Auth.AuthAlways);   (* eventually should be never *)
    IF EventTable.put(event, eventDesc) THEN
      IO.PutError("Dispatcher inconsistency: duplicate event\n");
      <* ASSERT FALSE *>
    END;
          
    (* create default alias descriptor *)
    <* ASSERT eventDesc.aliases = NIL *>
    aliasDesc := CreateAliasDesc(eventDesc);
    aliasDesc.aid := event;
    IF AliasTable.put(event, aliasDesc) THEN
      IO.PutError("Dispatcher inconsistency: duplicate alias\n");
      <* ASSERT FALSE *>
    END;
    RETURN eventDesc;
  END CreateEventDesc;

(*
 * Return procedure descriptor for a procedure pointer.
 * This is a wrapper for GetProcDescNoLock, it checks for NIL and aliases
 * and takes the lock.
 *)

PROCEDURE GetProcDesc (event: PROCANY): RTProcDescF.T
                      RAISES { Error } =
  VAR
    aliasDesc : AliasDesc.T;
    proc: PROCANY;
  BEGIN
    IF event = NIL THEN RETURN NIL; END;
    proc := event;
    LOCK EventLock DO
      IF AliasTable.get(proc, aliasDesc) THEN
        proc := aliasDesc.eventDesc.event;
      END;
      RETURN GetProcDescNoLock(proc);
    END;
  END GetProcDesc; 

(*
 * Return procedure descriptor for a procedure pointer.
 * Assumes that the lock is already taken and proc is the original
 * procedure and not an alias.
 *)

PROCEDURE GetProcDescNoLock(proc: PROCANY): RTProcDescF.T
                            RAISES { Error } =
  VAR
    procDesc: RTProcDescF.T;
  BEGIN
    IF proc = NIL THEN RETURN NIL; END;
    procDesc := RTProcDescF.GetDesc(proc);
    IF procDesc = NIL THEN
      IO.PutError("Dispatcher: procedure descriptor not found for " &
        Textify.Address(proc) & "\n");
      RAISE Error(ErrorCode.InvalidProcedure);
    END;
    RETURN procDesc;
  END GetProcDescNoLock;

(*
 *  raises the exception meaning that no handler was invoked
 *)
PROCEDURE RaiseNoHandlerInvoked () RAISES { SpinException.Exception } =
  BEGIN
    RAISE SpinException.Exception(
              SpinException.ExceptionInfo{
                     code := SpinException.ExceptionCode.NoHandlerInvoked,
                     msg  := SpinException.ExceptionNames[
                             SpinException.ExceptionCode.NoHandlerInvoked]});
  END RaiseNoHandlerInvoked;

(******************************************************************************
 *
 * 
 *
 *****************************************************************************)

PROCEDURE GetAsynchAlias (event: PROCANY): PROCANY
                          RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
    aliasDesc : AliasDesc.T;
  BEGIN
    IF NOT IsLegalAsynchronous(eventDesc.procDesc) THEN
      IO.PutError ("Dispatcher.GetAsynchAlias: illegal asynchronous event\n");
      RAISE Error(ErrorCode.IllegalAsynchEvent);
    END;

    aliasDesc := eventDesc.asynchAlias;
    IF aliasDesc = NIL THEN
      aliasDesc := CreateAliasDesc(eventDesc);
      StrongRef.Add(aliasDesc);
      aliasDesc.aid := 
          Stitcher.CloneStub(aliasDesc, -1, 0, FALSE, FALSE, FALSE);
      eventDesc.asynchAlias := aliasDesc;
      IF AliasTable.put(aliasDesc.aid, aliasDesc) THEN
        IO.PutError("Dispatcher.InitializeEvent:: duplicate alias\n");
        RAISE Error(ErrorCode.InternalError);
      END;
    END;
    RETURN aliasDesc.aid;
  END GetAsynchAlias;

(******************************************************************************
 *
 * Initialization
 *
 *****************************************************************************)

PROCEDURE DestroyEvents(modules: UNTRACED REF ARRAY OF RT0.ModulePtr)
                       : BOOLEAN =
  VAR
    module: RT0.ModulePtr;
    procPtr: RT0.ProcPtr;
    proc: PROCANY;
    eventDesc: EventDesc.T;
    aliasDesc: AliasDesc.T;
  BEGIN
    IF Debug THEN
      RTIO.PutText("Dispatcher.DestroyUnits:\n"); 
    END;

    FOR i := FIRST(modules^) TO LAST(modules^) DO
      module := modules[i];
      
      IF Debug THEN
        RTIO.PutText("module: "); 
        RTIO.PutString(module.file); 
        RTIO.PutText("\n");
      END;

      procPtr := module.proc_info;
      IF procPtr # NIL THEN
        WHILE (procPtr.proc # NIL) DO
          proc := procPtr.proc;

          IF Debug THEN
            RTIO.PutText("  procedure: "); 
            RTIO.PutString(procPtr.name); 
            RTIO.PutText("\n");
          END;
          
          (* destroy an event *)
          IF EventTable.get(proc, eventDesc) THEN
            (* destroy all of its aliases *)
            aliasDesc := eventDesc.aliases;
            WHILE aliasDesc # NIL DO
              (* enable its collection *)
              IF StrongRef.Exists(aliasDesc) THEN
                StrongRef.Remove(aliasDesc);
              END;

              (* forget about it *)
              IF NOT AliasTable.delete(aliasDesc.aid, aliasDesc) THEN
                IO.PutError("Dispatcher: could not remove an alias\n");
                RETURN FALSE;
              END;

              (* clean it *)
              aliasDesc.aid       := NIL;
              aliasDesc.eventDesc := NIL;
              aliasDesc.handlers  := NIL;
              aliasDesc := aliasDesc.next;
            END;

            (* release the handler list *)
            StrongRef.Remove(eventDesc.handlers);
            
            (* clean it *)
            eventDesc.event          := NIL;
            eventDesc.procDesc       := NIL;
            eventDesc.aliases        := NIL;
            eventDesc.asynchAlias    := NIL;
            eventDesc.auth           := NIL;
            eventDesc.resultHandlers := NIL;
            eventDesc.handlers       := NIL;
            eventDesc.bindings       := NIL;
            eventDesc.lastBinding    := NIL;
            eventDesc.defBinding     := NIL;

            (* destroy the event descriptor *)
            IF NOT EventTable.delete(proc, eventDesc) THEN
              IO.PutError("Dispatcher: could not remove a event descriptor\n");
              RETURN FALSE;
            END;
          END;

          (* next procedure *)
          INC (procPtr, ADRSIZE (procPtr^));
        END;
      END;
    END;
    RETURN TRUE;
  END DestroyEvents;

PROCEDURE DestroyHandlers(<* UNUSED *>ranges: DomainPrivate.RangeT): BOOLEAN =
  VAR
    iter      := EventTable.iterate();
    event     : PROCANY;
    eventDesc : EventDesc.T;
    list      : REF ARRAY OF Binding := NIL;
    num       : INTEGER := 0;
    binding   : Binding;
    guard     : ImposedGuard;

  <* UNUSED *>
  PROCEDURE AddForUninstallation(binding: Binding) =
    VAR
      new: REF ARRAY OF Binding := NIL;
    BEGIN
      IF list = NIL THEN list := NEW(REF ARRAY OF Binding, 10); END;
      IF NUMBER(list^) = num THEN
        new := NEW(REF ARRAY OF Binding, 2 * num); 
        SUBARRAY(new^, 0, num) := SUBARRAY(list^, 0, num);
        list := new;
      END;
      list[num] := binding;
      INC(num);
    END AddForUninstallation;

  BEGIN
    IF Debug THEN
      IO.Put("Dispatcher.DestroyHandlers:\n"); 
    END;

    (* go over all active events in the system *)
    WHILE iter.next(event, eventDesc) DO
      IF Debug THEN
        IO.Put("event: " & Textify.Address(event) & "\n"); 
      END;

      (* reinitialize the uninstallation list *)
      list := NIL;
      num  := 0;

      (* go over all bindings for this event, if any part of it belongs *)
      (* to the domain represented by the ranges, mark it for uninstallation *)
      binding := eventDesc.bindings;
      WHILE binding # NIL DO
        IF Debug THEN
          IO.Put("binding: " & Textify.Address(binding.handler) & " " &
            Textify.Address(binding.guard) & "\n"); 
        END;

        (* FIXME
        IF DomainChecks.InRanges(binding.handler, ranges) OR 
          DomainChecks.InRanges(binding.guard, ranges) 
         THEN
          IF Debug THEN
            IO.Put("uninstall\n");
          END;
          AddForUninstallation(binding);
        ELSE
          guard := binding.imposedGuards;
          WHILE guard # NIL DO
            IF Debug THEN
              IO.Put("imposed guard: " & Textify.Address(guard.guard) & "\n");
            END;
            IF DomainChecks.InRanges(guard.guard, ranges) THEN
              IF Debug THEN
                IO.Put("uninstall\n");
              END;
              AddForUninstallation(binding);
              EXIT;
            END;
            guard := guard.next;
          END;
        END;
        *)
        binding := binding.next;
      END;

      (* actually uninstall what you accumulated *)
      IF num > 0 THEN
        FOR i := 0 TO num-1 DO
          IF NOT RealUninstall(list[i], eventDesc) THEN
            IO.PutError("Dispatcher.DestroyHandlers: could not uninstall\n");
            RETURN FALSE;
          END;
          list[i].event   := NIL;
          list[i].guard   := NIL;
          list[i].handler := NIL;
          guard := list[i].imposedGuards;
          WHILE guard # NIL DO
            guard.guard   := NIL;
            guard.binding := NIL;
            guard := guard.next;
          END;
          list[i].imposedGuards := NIL;
        END;
        TRY
          Activate(eventDesc);
        EXCEPT
        | Error => RETURN FALSE;
        END;
      END;
    END;
    Debug := FALSE;

    RETURN TRUE;
  END DestroyHandlers;


(* 
 * reinitialization of the dispatcher after dynamic loading of M3 code
 *)

PROCEDURE Reinitialize() =
  BEGIN

  END Reinitialize;

(*
 * communicate the list of stubs to the profiling code.
 *)

PROCEDURE RecordStubs(): UNTRACED REF ARRAY OF StubEvent =
  VAR
    event: PROCANY;
    desc: EventDescT;
    iter: EDTbl.Iterator := EventTable.iterate();
    nStubs := 0;
    result : UNTRACED REF ARRAY OF StubEvent;
  BEGIN
    LOCK EventLock DO
      WHILE iter.next(event, desc) DO
        IF desc.stub # NIL THEN
          INC(nStubs);
        END;
        FOR i := FIRST(desc.stubs) TO LAST(desc.stubs) DO
          IF desc.stubs[i] # NIL THEN
            INC(nStubs);
          END;
        END;
      END;
      
      result := NEW(UNTRACED REF ARRAY OF StubEvent, nStubs);
      iter.reset();
      nStubs := 0;
      WHILE iter.next(event, desc) DO
        IF desc.stub # NIL THEN
          result[nStubs] := StubEvent{desc.stub, event};
          INC(nStubs);
        END;
        FOR i := FIRST(desc.stubs) TO LAST(desc.stubs) DO
          IF desc.stubs[i] # NIL THEN
            result[nStubs] := StubEvent{desc.stubs[i], event};
            INC(nStubs);
          END;
        END;
      END;
    END;
    RETURN result;
  END RecordStubs;

(* 
 * initialization of the dispatcher during boot time
 *)

PROCEDURE Init (verbose: BOOLEAN) =
  BEGIN
    (* create and initialize hash tables *)
    EventLock  := NEW (MUTEX);
    EventTable := NEW (EDTbl.Default).init(100);
    AliasTable := NEW (ADTbl.Default).init(100);

    (* initialize typechecking *)
    DispatcherTypes.Init();

    (* initialize run-time code generation *)
    Stitcher.Init();

    IF verbose THEN IO.Put ("Dispatcher initialized...\n") END;
 END Init;

PROCEDURE Trace (event: PROCANY; on: BOOLEAN) RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(event);
  BEGIN
    (* ignore if already done *)
    IF on = eventDesc.trace THEN
      RETURN;
    END;

    (* ignore uninitialized events *)
    IF eventDesc.nHandlers = -1 THEN
      RETURN;
    END;
    
    (* put into effect *)
    eventDesc.trace := on;
    InstallStub(eventDesc, FindOptLevel(eventDesc), on);
  END Trace;

(* FIXME: I can probably just iterate over the EventTable *)
PROCEDURE TraceAll (on: BOOLEAN) =
  VAR
    eventDesc: EventDesc.T;
    procPtr: RT0.ProcPtr;
    proc: PROCANY;
    module: RT0.ModulePtr;
  BEGIN
    IF traceAll = on THEN
      RETURN;
    END;
    traceAll := on;
    LOCK EventLock DO
      (* scan module descriptors for exported procedures *)
      FOR i := 0 TO RTModule.Count() - 1 DO
        module := RTModule.Get(i);
        procPtr := module.proc_info;
        IF procPtr # NIL THEN
          WHILE (procPtr.proc # NIL) DO
            proc := procPtr.proc;
            IF EventTable.get(proc, eventDesc) AND Initialized(eventDesc) THEN
              TRY
                InstallStub(eventDesc, FindOptLevel(eventDesc), on);
              EXCEPT
              | Error => 
                IO.PutError("Dispatcher.TraceAll: stub installation failed\n");
              END;
            END;

            (* next procedure *)
            INC (procPtr, ADRSIZE (procPtr^));
          END;
        END;
      END;
    END;
  END TraceAll;

PROCEDURE ResetTrace () =
  BEGIN
    MachineStitcherExtern.trace_counts_reset();
  END ResetTrace;

PROCEDURE DumpTrace () =
  BEGIN
    MachineStitcherExtern.trace_counts_dump();
  END DumpTrace;

PROCEDURE GetName (proc: PROCANY): ADDRESS RAISES { Error } =
  VAR
    eventDesc : EventDesc.T := GetEventDesc(proc);
    desc: REF RTProcedureSRC.ProcDesc;
  BEGIN
    proc := eventDesc.event;
    IF proc = NIL THEN
      (* RETURN "NIL";*)
    ELSE
      desc    := RTProcedureSRC.GetByPC(proc);
      IF desc # NIL THEN 
        (* FIXME
        RETURN M3toC.TtoS(desc.file & "." & desc.name);
        *)
      ELSE
        (* RETURN "<UNKNOWN>";*)
      END;
    END;
    RETURN NIL;
  END GetName;

(* FIXME: interface to lock and unlock sets of events to perform complex
   event operations *)
<* UNUSED *>
PROCEDURE LockEvents (<* UNUSED *> events : REF ARRAY OF PROCANY) =
  BEGIN
  END LockEvents;

<* UNUSED *>
PROCEDURE UnlockEvents (<* UNUSED *> events : REF ARRAY OF PROCANY) =
  BEGIN
  END UnlockEvents;

(*
 * atomically change the state of many bindings: uninstall handlers on 
 * the deactivationList, install handlers on the activationList
 *)
(* FIXME: allow any event, do guards, result handlers *)
PROCEDURE AtomicSwap (removeHandlers      : REF ARRAY OF Binding := NIL;
                      addHandlers         : REF ARRAY OF Binding := NIL;
          <* UNUSED *>removeGuards        : REF ARRAY OF ImposedGuard := NIL;
          <* UNUSED *>addGuards           : REF ARRAY OF ImposedGuard := NIL;
          <* UNUSED *>removeResultHandlers: REF ARRAY OF ResultHandlers := NIL;
          <* UNUSED *>addResultHandlers   : REF ARRAY OF ResultHandlers := NIL;
                     ) RAISES { Error } =
  VAR
    eventDesc : EventDesc.T;
    event     : PROCANY;
    change    : BOOLEAN;

  PROCEDURE CheckEventsInList (list: REF ARRAY OF Binding) RAISES { Error } =
    BEGIN
      IF list # NIL THEN
        FOR i := FIRST(list^) TO LAST(list^) DO
          IF event = NIL THEN
            event := list[i].event;
            eventDesc := TryGetEventDesc(list[i]);
          ELSIF event # list[i].event THEN
            IO.PutError ("more than one event in Dispatcher.AtomicSwap\n");
            RAISE Error(ErrorCode.InvalidProcedure);
          END;
        END;
      END;
    END CheckEventsInList;

  BEGIN
    IF Debug THEN
      RTIO.PutText (">>> Dispatcher.Swap\n");
      (* FIXME: write an iteration when you care
         ":: event1: " & 
         Textify.Address(binding1.event) & "; event2: " & 
         Textify.Address(binding2.event) &
         "; binding1: " & Textify.Ref(binding1) &
         "; binding2: " & Textify.Ref(binding2) &
         "\n");
      *)
    END;

    (* check that the bindings are for the same event *)
    event := NIL;
    CheckEventsInList(removeHandlers);
    CheckEventsInList(addHandlers);

    IF event = NIL THEN
      IO.Put (">>> Dispatcher.AtomicSwap: no event\n");
    END;

    (* nothing to do *)
    IF event = NIL THEN RETURN; END;

    (* swap them *)
    change := FALSE;
    LOCK eventDesc.lock DO
      IF removeHandlers # NIL THEN
        FOR i := FIRST(removeHandlers^) TO LAST(removeHandlers^) DO
          change := RealUninstall(removeHandlers[i], eventDesc) OR change;
        END;
      END;
      IF addHandlers # NIL THEN
        FOR i := FIRST(addHandlers^) TO LAST(addHandlers^) DO
          change := RealInstall(addHandlers[i], eventDesc) OR change;
        END;
      END;
      IF change THEN
        Activate(eventDesc);
      END;
    END;
  END AtomicSwap; 

PROCEDURE Bypass (event:PROCANY; proc:PROCANY) RAISES { Error } =
  VAR
    default := NEW(REF ARRAY OF Binding, 1);
    new     := NEW(REF ARRAY OF Binding, 1);
  BEGIN
    RTProcDescF.Legalize(proc);
    default[0] := GetOriginalHandler(event);
    new[0] := Create(event, NIL, proc);
    AtomicSwap(default, new);
  END Bypass;

PROCEDURE CreateLinear(proc    : PROCANY;
                       nArgs   : INTEGER;
                       closure : REFANY := NIL): PROCANY
                       RAISES { Error } = <* NOWARN *>
  VAR
    stub       : PROCANY;
    useClosure : BOOLEAN;
  BEGIN
    (* FIXME: typecheck *)
    useClosure := RTProcDesc.NumberOfArgs(RTProcDesc.GetType(proc)) = 2;
    IF useClosure AND closure # NIL THEN
      StrongRef.Add(closure); (* FIXME: remove *)
    END;
    stub := Stitcher.GetLinear(proc, nArgs, useClosure, closure);
    RTProcDescF.Legalize(stub);
    RETURN stub;
  END CreateLinear; 

BEGIN
END Dispatcher.
