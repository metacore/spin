(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 25-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Using new nameserver interface to register domains.  Not using
 *	Auth anymore.
 *
 * 09-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Use the Brand name that is automatically generated in the
 *       interface files.
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Use new Auth/NS interface.
 *
 * 01-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

(* Generic module for accessing and exporting a particular interface
  through the name server and the domain linkage facilities.  We
  intend to access raw symbol information made available at COMPILE
  TIME.  This requires that we have access to direct linkage symbols,
  as well as auxiliary information on number of symbols and enveloping
  domain name. *)


GENERIC MODULE Interface(Idefs);
IMPORT InterfaceSub;
IMPORT Auth;

PROCEDURE Export (auth: Auth.T := NIL): Auth.T =
  BEGIN
    InterfaceSub.Export(auth, THIS_MODULE(),
			Idefs.Brand, Idefs.domainSymbols);
    RETURN auth;
  END Export;

BEGIN
END Interface.
