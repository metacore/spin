(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 27-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Whisted.
 *
 *)


GENERIC INTERFACE Protocol();
IMPORT Mbuf;
IMPORT Dispatcher;

(* event definition *)
TYPE PacketArrivedEvent = PROCEDURE (packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN;
PROCEDURE PacketArrived(packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN;
TYPE PacketArrivedEventWithClosure = PROCEDURE (closure: REFANY;
      packet, curr: Mbuf.T; offset: CARDINAL):BOOLEAN;


PROCEDURE Install (
    event      : PacketArrivedEvent; 
    whenClause : PacketArrivedEvent; 
    handler    : PacketArrivedEvent): Dispatcher.Binding;

PROCEDURE InstallWithClosure (
    event          : PacketArrivedEvent;
    whenClause     : PacketArrivedEventWithClosure; 
    handler        : PacketArrivedEventWithClosure;
    guardClosure   : REFANY; 
    handlerClosure : REFANY): Dispatcher.Binding;

PROCEDURE Uninstall (spindle: REFANY);

PROCEDURE Init();
END Protocol.
