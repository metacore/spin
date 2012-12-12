(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
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


GENERIC MODULE Plexus(Interface);
IMPORT Auth;
IMPORT Identity;
IMPORT NameServer;
IMPORT IO;

TYPE
  T = Auth.T OBJECT
    all: BOOLEAN;
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (self: T; <*UNUSED*>key: Auth.Key): BOOLEAN =
  VAR b: BOOLEAN;
      id: Identity.T := Identity.GetCurrent();
  BEGIN
    (*
    IO.Put(
      "Request to authorize " & Identity.GetName(id) & " \n" );
    *)
    IF self.all OR Identity.IsPrivileged(id) THEN
      (*
      IO.Put(" -- OK\n");
      *)
      b := TRUE;
    ELSE
      IO.PutError(" -- Auth Failed\n");
      b := FALSE;
    END;
    RETURN b;
  END Authorize;


VAR t : T;
BEGIN
  TRY
    t := NEW(T);
    t.all := TRUE;
    EVAL PlexusInterface.Export(t);
  EXCEPT
  | NameServer.Error(ec)  => 
    IO.PutError("Plexus init FAILED\n");
  END;

  IO.Put("Plexus init done.\n");
END Plexus.

