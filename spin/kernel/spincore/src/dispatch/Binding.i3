(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.  Renamed to Binding.
 *
 * 08-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added options to the spindle descriptor.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)
INTERFACE Binding;
FROM Dispatcher IMPORT Binding, ResultHandlers, BindingT, ResultHandlersT,
                       ImposedGuard, Options;

REVEAL
  Binding = BindingT BRANDED OBJECT
    event             : PROCANY;
    guard             : PROCANY;
    handler           : PROCANY;
    guardClosure      : REFANY;
    handlerClosure    : REFANY;
    useGuardClosure   : BOOLEAN;
    useHandlerClosure : BOOLEAN;
    cancel            : BOOLEAN;
    imposedGuards     : ImposedGuard;
    nImposed          : INTEGER;
    next              : Binding;
    options           : Options;
  OVERRIDES
    getEvent          := GetEvent; 
    getHandler        := GetHandler;
    getGuard          := GetGuard;
    getHandlerClosure := GetHandlerClosure;
    getGuardClosure   := GetGuardClosure;
  END;

PROCEDURE GetEvent(binding: Binding): PROCANY;
PROCEDURE GetHandler(binding: Binding): PROCANY;
PROCEDURE GetGuard(binding: Binding): PROCANY;
PROCEDURE GetHandlerClosure(binding: Binding): REFANY;
PROCEDURE GetGuardClosure(binding: Binding): REFANY;

REVEAL
  ImposedGuard = BRANDED OBJECT
    guard               : PROCANY;
    useGuardClosure     : BOOLEAN;
    guardClosure        : REFANY;
    postGuard           : PROCANY;
    usePostGuardClosure : BOOLEAN;
    postGuardClosure    : REFANY;
    binding             : Binding;
    next                : ImposedGuard;
  END;

REVEAL
  ResultHandlers = ResultHandlersT BRANDED OBJECT
    defaultResult     : INTEGER; (* initial value of the result *)
    resultHandler     : PROCANY; (* called after each regular handler *)
    resultClosure     : REFANY;  (* its closure *)
    useResultClosure  : BOOLEAN; (* should it be passed *)
    resultPassArgs    : BOOLEAN; (* should arguments be passed to it *)
    defaultHandler    : PROCANY; (* called if no other handler is called *)
    defaultClosure    : REFANY;  (* its closure *)
    useDefaultClosure : BOOLEAN; (* should it be passed *)
    defaultPassArgs   : BOOLEAN; (* should arguments be passed to it *)
    event             : PROCANY;
  OVERRIDES
  END;

PROCEDURE GetResultHandler(resultHandlers: ResultHandlers): PROCANY;
PROCEDURE GetResultClosure(resultHandlers: ResultHandlers): REFANY;
PROCEDURE GetDefaultHandler(resultHandlers: ResultHandlers): PROCANY;
PROCEDURE GetDefaultClosure(resultHandlers: ResultHandlers): REFANY;
PROCEDURE GetDefaultResult(resultHandlers: ResultHandlers): REFANY;

END Binding.
