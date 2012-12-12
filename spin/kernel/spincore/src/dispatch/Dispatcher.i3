(* 
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	fix signature for SetDefaultResult
 *
 * 25-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added the Last installation option.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added Create and Swap.
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Auth services.
 *
 * 09-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added GetHandlers.
 *
 * 31-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	use PROCANY instead of REFANY
 *
 * 17-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added the DefaultOptions const to assure that clients remain in
 *	sync with changes to the dispatcher.
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)

INTERFACE Dispatcher;
IMPORT Auth, RTCode;

TYPE
  BindingT = OBJECT
  METHODS
    getEvent(): PROCANY;
    getHandler(): PROCANY;
    getGuard(): PROCANY;
    getHandlerClosure(): REFANY;
    getGuardClosure(): REFANY;
  END;

TYPE
  ResultHandlersT = OBJECT
  METHODS
    getResultHandler(): PROCANY;
    getResultClosure(): REFANY;
    getDefaultHandler(): PROCANY;
    getDefaultClosure(): REFANY;
    getDefaultResult(): REFANY;
  END;

TYPE
  Binding        <: BindingT;
  ImposedGuard   <: REFANY;
  ResultHandlers <: ResultHandlersT;

(*
 * exceptions
 *) 

TYPE
  ErrorCode = {
    InternalError,       (* dispatcher data structures are corrupted         *)
    InvalidProcedure,    (* argument is not a valid Modula3 procedure        *)
    TypeError,           (* dynamic type checking error                      *)
    CodeGenerationError, (* run-time code generator could not generate code  *)
    IllegalAsynchEvent,  (* argument event cannot be raised asynchronously   *)
    SaveRegsError,       (* event cannot use callee saved registers          *)
    AuthError		 (* authorization error *)
  };

EXCEPTION
  Error(ErrorCode);

(*
 * handler installation options
 *)

TYPE
  Opt = { First, Last, Cancel };

  Options = SET OF Opt;

CONST
  DefaultOptions = Options{};

(******************************************************************************
 *
 * single binding handling
 *
 *****************************************************************************)

PROCEDURE Create (event          : PROCANY;
                  guard          : PROCANY;
                  handler        : PROCANY;
                  guardClosure   : REFANY   := NIL;
                  handlerClosure : REFANY   := NIL;
                  options        : Options  := DefaultOptions;
                  key            : Auth.Key := NIL
                 ): Binding
                 RAISES { Error };
(* create an inactive binding *)

PROCEDURE Install (binding: Binding) 
                  RAISES { Error };
(* install the given handler on the event it is bound to *)
(* empty operation if the handler is not currently installed *)

PROCEDURE Uninstall (binding: Binding) 
                    RAISES { Error };
(* uninstall the given handler from the event it is bound to *)
(* empty operation if the handler is not currently installed *)

PROCEDURE InstallHandler (event          : PROCANY;
                          guard          : PROCANY;
                          handler        : PROCANY;
                          guardClosure   : REFANY   := NIL;
                          handlerClosure : REFANY   := NIL;
                          options        : Options  := DefaultOptions;
                          key            : Auth.Key := NIL
                         ): Binding
                         RAISES { Error };
(* create an active binding: a composition of Create and Install *)

(******************************************************************************
 *
 * handler sets manipulation
 *
 *****************************************************************************)

PROCEDURE GetHandlers (event  : PROCANY;
                       module : RTCode.Module := NIL): REF ARRAY OF Binding
                      RAISES { Error };
(* obtain a list of all handlers installed on an event *)

PROCEDURE IsInstalled (binding: Binding): BOOLEAN;
(* verify whether the binding is active or not *)

PROCEDURE GetOriginalHandler (event: PROCANY): Binding
                              RAISES { Error };
(* return a binding corresponding to the original handler *)

(******************************************************************************
 *
 * Default handler and result handling
 *
 *****************************************************************************)

PROCEDURE CreateResultHandlers (event          : PROCANY;
                                resultHandler  : PROCANY := NIL;
                                defaultHandler : PROCANY := NIL;
                                resultPtr      : REFANY  := NIL;
                                resultClosure  : REFANY  := NIL;
                                defaultClosure : REFANY  := NIL; 
                                module         : RTCode.Module := NIL;
                               ): ResultHandlers
                               RAISES { Error };

PROCEDURE InstallResultHandlers (resultHandlers: ResultHandlers)
                                 RAISES { Error };

PROCEDURE UninstallResultHandlers (resultHandlers: ResultHandlers)
                                   RAISES { Error };

PROCEDURE ImposeResultHandlers (event          : PROCANY;
                                resultHandler  : PROCANY := NIL;
                                defaultHandler : PROCANY := NIL;
                                resultPtr      : REFANY  := NIL;
                                resultClosure  : REFANY  := NIL;
                                defaultClosure : REFANY  := NIL; 
                                module         : RTCode.Module := NIL;
                               ): ResultHandlers
                               RAISES { Error };

PROCEDURE InstallResultHandler (event   : PROCANY;
                                handler : PROCANY;
                                module  : RTCode.Module;
                                closure : REFANY := NIL) 
                               RAISES { Error };
(* install a result handler *)

PROCEDURE InstallDefaultHandler (event   : PROCANY;
                                 handler : PROCANY;
                                 module  : RTCode.Module;
                                 closure : REFANY := NIL) 
                                RAISES { Error };
(* install a default handler *)

PROCEDURE SetDefaultResult (event     : PROCANY;
                            resultPtr : REFANY;
                            module    : RTCode.Module)
                           RAISES { Error };
(* set a default result *)

(******************************************************************************
 *
 * Asynchrony
 *
 *****************************************************************************)

PROCEDURE SetAsynchronousEvent (event        : PROCANY;
                                asynchronous : BOOLEAN;
                                module: RTCode.Module := NIL)
                               RAISES { Error };
(* make an event asynchronous if "asynchronouns" is TRUE and synchronous
   if it is FALSE *)

PROCEDURE SetAsynchronousHandler (binding: Binding;
                                  asynchronous : BOOLEAN)
                                 RAISES { Error };
(* make a handler asynchronous if "asynchronouns" is TRUE and synchronous
   if it is FALSE *)

(******************************************************************************
 *
 * Authorization support
 *
 *****************************************************************************)

PROCEDURE InstallAuthorizerForInterface (auth      : Auth.T;
                                         interface : RTCode.Interface;
                                         module    : RTCode.Module := NIL)
                                         RAISES { Error };
(* InstallAuthorizerForInterface affixes a new Auth.T onto the procedures
   declared in the named interface. The caller must have a "controlling
   view" on the interface, such that they either implement the interface
   directly (eg, RTCode.ModuleExportsInterface(module, interface), or
   are tightly coupled with a module that does. *)

PROCEDURE InstallAuthorizerForEvent (auth  : Auth.T;
                                     event : PROCANY;
                                     module: RTCode.Module := NIL)
                                     RAISES { Error };
(* InstallAuthorizerForEvent affixes a new Auth.T onto the named event. 
   The caller must have a controlling view of the event, as given by 
   the predicate RTCode.ModuleImplementsProcedure(module, event). *)

(******************************************************************************
 *
 * Imposed guards
 *
 *****************************************************************************)

PROCEDURE CreateImposedGuard(binding    : Binding;
                             guardProc        : PROCANY := NIL;
                             guardClosure     : REFANY  := NIL;
                             postGuard        : PROCANY := NIL;
                             postGuardClosure : REFANY  := NIL;
                            ): ImposedGuard
                            RAISES { Error };
(* create an imposed guard for a binding *)

PROCEDURE InstallGuard(guard: ImposedGuard)
                       RAISES { Error };
(* install an imposed guard *)

PROCEDURE UninstallGuard(guard: ImposedGuard)
                       RAISES { Error };
(* uninstall an imposed guard *)

PROCEDURE ImposeGuard (binding          : Binding; 
                       guardProc        : PROCANY := NIL;
                       guardClosure     : REFANY  := NIL;
                       postGuard        : PROCANY := NIL;
                       postGuardClosure : REFANY  := NIL;
                      ): ImposedGuard
                      RAISES { Error };
(* create and install an imposed guard *)

(******************************************************************************
 *
 * Atomic operations
 *
 *****************************************************************************)

PROCEDURE AtomicSwap (removeHandlers      : REF ARRAY OF Binding := NIL;
                      addHandlers         : REF ARRAY OF Binding := NIL;
                      removeGuards        : REF ARRAY OF ImposedGuard := NIL;
                      addGuards           : REF ARRAY OF ImposedGuard := NIL;
                      removeResultHandlers: REF ARRAY OF ResultHandlers := NIL;
                      addResultHandlers   : REF ARRAY OF ResultHandlers := NIL;
                     ) RAISES { Error };
(* atomically change the state of many bindings: uninstall handlers on 
   the deactivationList, install handlers on the activationList *)

(******************************************************************************
 *
 * Imposed guards
 *
 *****************************************************************************)

<* OBSOLETE *>
PROCEDURE GetAsynchAlias(event: PROCANY): PROCANY RAISES { Error };
(* return an alias for asynchronous event raise, events that return results  *)
(* or take VAR or READONLY parameters cannot have asynchronous aliases       *)

PROCEDURE GetName (event: PROCANY): ADDRESS RAISES { Error };

(******************************************************************************
 *
 * ???
 *
 *****************************************************************************)

PROCEDURE CreateLinear(proc    : PROCANY;
                       nArgs   : INTEGER;
                       closure : REFANY := NIL): PROCANY
                       RAISES { Error };

END Dispatcher.
