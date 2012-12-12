(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 07-Jul-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *
 *)

MODULE Binding;
FROM Dispatcher IMPORT Binding, ResultHandlers;

PROCEDURE GetEvent(binding: Binding): PROCANY =
  BEGIN
    RETURN binding.event;
  END GetEvent;

PROCEDURE GetHandler(binding: Binding): PROCANY =
  BEGIN
    RETURN binding.handler;
  END GetHandler;

PROCEDURE GetGuard(binding: Binding): PROCANY =
  BEGIN
    RETURN binding.guard;
  END GetGuard; 

PROCEDURE GetHandlerClosure(binding: Binding): REFANY =
  BEGIN
    RETURN binding.handlerClosure;
  END GetHandlerClosure;

PROCEDURE GetGuardClosure(binding: Binding): REFANY =
  BEGIN
    RETURN binding.guardClosure;
  END GetGuardClosure;

PROCEDURE GetResultHandler(resultHandlers: ResultHandlers): PROCANY =
  BEGIN
    RETURN resultHandlers.resultHandler;
  END GetResultHandler;

PROCEDURE GetResultClosure(resultHandlers: ResultHandlers): REFANY =
  BEGIN
    RETURN resultHandlers.resultClosure;
  END GetResultClosure;

PROCEDURE GetDefaultHandler(resultHandlers: ResultHandlers): PROCANY =
  BEGIN
    RETURN resultHandlers.defaultHandler;
  END GetDefaultHandler;

PROCEDURE GetDefaultClosure(resultHandlers: ResultHandlers): REFANY =
  BEGIN
    RETURN resultHandlers.defaultClosure;
  END GetDefaultClosure;

PROCEDURE GetDefaultResult(<*UNUSED*>resultHandlers: ResultHandlers): REFANY =
  BEGIN
    RETURN NIL; (* FIXME *)(*resultHandlers.defaultResult*)
  END GetDefaultResult;

BEGIN
END Binding.
