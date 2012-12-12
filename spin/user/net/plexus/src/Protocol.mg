(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Don't uninstall the default handler until we integrate with the
 *	new dispatcher.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 27-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Whisted.
 *
 *)


GENERIC MODULE Protocol(Interface);
IMPORT Auth;
IMPORT Dispatcher;
IMPORT IO;
IMPORT Mbuf;
IMPORT NameServer;

PROCEDURE PacketArrived(
    <* UNUSED *> packet,curr: Mbuf.T;
    <* UNUSED *> offset:CARDINAL):BOOLEAN =
  BEGIN  
    RETURN FALSE; (* TRUE if consuming packet *) 
  END PacketArrived;

PROCEDURE ExceptionPrint(ec:Dispatcher.ErrorCode) = 
  BEGIN
    CASE ec OF
    | Dispatcher.ErrorCode.InvalidProcedure =>
      IO.Put("Trusted invalid procedure installed.\n");
    ELSE
      IO.Put("Trusted dispatcher install error.\n");
    END;
  END ExceptionPrint;

PROCEDURE Install (
    <* UNUSED *> event: PacketArrivedEvent; 
    whenClause: PacketArrivedEvent; 
    handler: PacketArrivedEvent): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding := Dispatcher.InstallHandler(PacketArrived,
                                             whenClause, handler);
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    FINALLY
      RETURN binding;
    END;
  END Install;

PROCEDURE InstallWithClosure (
    <* UNUSED *> event: PacketArrivedEvent; 
    whenClause: PacketArrivedEventWithClosure; 
    handler: PacketArrivedEventWithClosure;
    guardClosure: REFANY; 
    handlerClosure: REFANY): Dispatcher.Binding =
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      TRY
        binding :=  Dispatcher.InstallHandler(PacketArrived,
                                              whenClause, handler,
                                              guardClosure,handlerClosure);
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    FINALLY
      RETURN binding;
    END;
  END InstallWithClosure; 


PROCEDURE Uninstall(binding: REFANY) =
  BEGIN
    WITH s = NARROW(binding,Dispatcher.Binding) DO
      TRY
        Dispatcher.Uninstall(s);
      EXCEPT
      | Dispatcher.Error(ec) => ExceptionPrint(ec);
      END;
    END;
  END Uninstall;

PROCEDURE Init() = 
  BEGIN
(*
    TRY 
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(PacketArrived));
    EXCEPT
    | Dispatcher.Error(ec) => ExceptionPrint(ec);
    END;
*)
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

TYPE
  T = Auth.T OBJECT
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (<* UNUSED *> self: T; 
                     <* UNUSED *> key: Auth.Key;
                     <* UNUSED *> arg: REFANY): BOOLEAN =
  BEGIN
    (*
    IO.Put("Request to authorize ");
    IO.Put(Identity.GetName(id));
    IO.Put(" -- OK\n");
    *)
    RETURN TRUE;
  END Authorize;

BEGIN
END Protocol.
