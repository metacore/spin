(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Use newer Auth interface.
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use new IO interface.
 *
 * 18-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Exports the Plexus domain to the Nameserver.
 *
 * 16-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)


GENERIC MODULE Link(Interface);
IMPORT Auth;

TYPE
  T = Auth.T OBJECT
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (<* UNUSED *> self: T;
                     <* UNUSED *> key: Auth.Key;
                     <* UNUSED *> arg: REFANY): BOOLEAN =
  (*
  VAR id: Identity.T := Identity.GetCurrent();
  *)
  BEGIN
    (*
    IO.Put("Request to authorize ");
    IO.Put(Identity.GetName(id));
    IO.Put(" -- OK\n");
    *)
    RETURN TRUE;
  END Authorize;


PROCEDURE Init() = 
  VAR t : T;
  BEGIN
    t := NEW(T);
    EVAL Interface.Export(t);
  END Init;

BEGIN
END Link.

