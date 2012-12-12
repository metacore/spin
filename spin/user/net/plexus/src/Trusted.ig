(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 11-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)

GENERIC INTERFACE Trusted(Protocol);

(* (Un)install untrusted client event handler *)

IMPORT Dispatcher;

PROCEDURE Install (
    event      : Protocol.PacketArrivedEvent; 
    whenClause : Protocol.PacketArrivedEvent; 
    handler    : Protocol.PacketArrivedEvent): Dispatcher.Spindle;

PROCEDURE InstallWithClosure (
    event          : Protocol.PacketArrivedEvent;
    whenClause     : Protocol.PacketArrivedEventWithClosure; 
    handler        : Protocol.PacketArrivedEventWithClosure;
    guardClosure   : REFANY; 
    handlerClosure : REFANY): Dispatcher.Spindle;

PROCEDURE Uninstall (spindle: REFANY);

PROCEDURE Init();
END Trusted.
