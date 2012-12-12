(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 11-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

GENERIC MODULE Trusted(Protocol,Interface);
IMPORT Dispatcher;
IMPORT IO;

PROCEDURE Install (
    <* UNUSED *> event: Protocol.PacketArrivedEvent; 
    whenClause: Protocol.PacketArrivedEvent; 
    handler: Protocol.PacketArrivedEvent): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding := Dispatcher.InstallHandler(Protocol.PacketArrived,
                                             whenClause, handler);
      EXCEPT
      | Dispatcher.Error(ec) => 
        CASE ec OF
        | Dispatcher.ErrorCode.InvalidProcedure =>
          IO.Put("Trusted invalid procedure installed.\n");
        ELSE
          IO.Put("Trusted dispatcher install error.\n");
        END;
      END;
    FINALLY
      RETURN binding;
    END;
  END Install;

PROCEDURE InstallWithClosure (
    <* UNUSED *> event: Protocol.PacketArrivedEvent; 
    whenClause: Protocol.PacketArrivedEventWithClosure; 
    handler: Protocol.PacketArrivedEventWithClosure;
    guardClosure: REFANY; 
    handlerClosure: REFANY): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding :=  Dispatcher.InstallHandler(Protocol.PacketArrived,
                                              whenClause, handler,
                                              guardClosure,handlerClosure);
      EXCEPT
      | Dispatcher.Error(ec) => 
        CASE ec OF
        | Dispatcher.ErrorCode.InvalidProcedure =>
          IO.Put("Trusted invalid procedure installed.\n");
        ELSE
          IO.Put("Trusted dispatcher install error.\n");
        END;
      END;
    FINALLY
      RETURN binding;
    END;
  END InstallWithClosure; 


PROCEDURE Uninstall(binding: REFANY) =
  BEGIN
    WITH s = NARROW(binding,Dispatcher.Binding) DO
      Dispatcher.Uninstall(s);
    END;
  END Uninstall;

PROCEDURE Init() = 
  BEGIN
    TRY 
      Dispatcher.Uninstall(Dispatcher.GetDefault(Protocol.PacketArrived));
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.Put("Trusted invalid procedure uninstalled.\n");
      ELSE
        IO.Put("Trusted dispatcher install error.\n");
      END;
    END;
    VAR t : T;
    BEGIN
      TRY
        t := NEW(T);
        EVAL Interface.Export(t);
      EXCEPT
      | NameServer.Error (* (ec) *)  => 
        IO.PutError("Link init FAILED\n");
      END;
    END
  END Init;

IMPORT Auth;
IMPORT Identity;
IMPORT NameServer;
IMPORT IO;

TYPE
  T = Auth.T OBJECT
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (<* UNUSED *> self: T; <*UNUSED*>key: Auth.Key): BOOLEAN =
  VAR id: Identity.T := Identity.GetCurrent();
  BEGIN
    (*
    IO.Put("Request to authorize ");
    IO.Put(Identity.GetName(id));
    IO.Put(" -- OK\n");
    *)
    RETURN TRUE;
  END Authorize;

BEGIN
END Trusted.
