(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE InterfaceSub;
IMPORT Domain, NameServer, Debugger;
IMPORT Auth; <* NOWARN *>
IMPORT IO, Fmt; <* NOWARN *>
IMPORT SymbolEntry;

PROCEDURE Export(<*UNUSED*>auth: Auth.T;
		 module: ADDRESS;
		 name: TEXT;
		 READONLY symbols: ARRAY OF SymbolEntry.E) =
  VAR
    domain: Domain.T;
    domainDir: NameServer.T;
  BEGIN
    (* XXX broken by mef.
    IF auth = NIL THEN auth := NEW(Auth.AuthAlways); END;
    *)
    TRY
      domain := Domain.CreateFromSymbolsArray(module, name, symbols);
      Debugger.RegisterDomain(domain);

      domainDir := NameServer.Lookup(NIL, "/svc/domains");

      (* Just remove my name from the domain dir. This is needed to
	 override the "nanny" entry in some cases. *)
      TRY
	NameServer.Detach(domainDir, name);
      EXCEPT
      ELSE
      END;
      NameServer.Attach(domainDir, name, domain);
    EXCEPT
    | NameServer.Error(ec) =>
      IO.PutError("Interface.Export: " & name & ": "
		  & NameServer.Fmt(ec) & ".\n");
    END;
  END Export;
  
BEGIN
END InterfaceSub.
