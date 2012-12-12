(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
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



GENERIC UNSAFE MODULE UnsafeInterface(Idefs);

IMPORT Domain, NameServer, Debugger;
IMPORT Auth; <* NOWARN *>
IMPORT IO, Fmt; <* NOWARN *>

PROCEDURE Export (auth: Authorizer := NIL): Authorizer
  RAISES {NameServer.Error} =
  VAR 
    domain    : Domain.T;
    domainDir : NameServer.T;
    component : TEXT;
    parent    : NameServer.TBase;
  BEGIN
    (* XXX broken by mef.
    IF auth = NIL THEN auth := NEW(Auth.AuthAlways); END;
    *)

    domain := Domain.CreateFromSymbolsArray(
                     THIS_MODULE(),
                     Idefs.Brand, 
                     Idefs.domainSymbols);
    Debugger.RegisterDomain(domain);

    domainDir := NameServer.Lookup(NIL,"/svc/domains",component,parent);
    domainDir.attach(Idefs.Brand,domain);
    RETURN auth;
  END Export;

BEGIN
END UnsafeInterface.
