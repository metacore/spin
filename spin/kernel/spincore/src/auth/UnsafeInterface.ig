(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	New auth interface.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 *
 * Generic interface for exporting a particular interface via the name server.
 *
 * "I" is an interface name that is to be exported.  Idefs is where we expect
 * to find the compile-time constants describing the interface.
 *
 * Contraints:
 *    I     | WHERE <I.Brand = "Some Name">
 *
 *    Idefs | WHERE Idefs exports domainSymbols, domainSize, name
 *
 * NOTE: I and Idefs are UNUSED in this interface (although they are used in
 *	the implementation. Need an <*UNUSED*> pragma for interfaces.
 * 
 *
 *)

GENERIC INTERFACE UnsafeInterface();
IMPORT NameServer, Auth;
TYPE Authorizer = Auth.T;

PROCEDURE Export (auth: Authorizer := NIL): Authorizer 
  RAISES {NameServer.Error};

END UnsafeInterface.
