(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 *)

MODULE CoreNS;
IMPORT IO, Dispatcher, NameServer, Auth, Domain; <* NOWARN *>
IMPORT NSName;

CONST NameSpace = ARRAY OF TEXT { "domains", "events", "threads", "types" };
PROCEDURE Init(verbose:BOOLEAN) = 
  VAR
    svcNS: NameServer.T;
    name: NameServer.Name;
  BEGIN
    TRY 
      (* find the spin services namespace *)
      svcNS := NameServer.Lookup(NIL, "/svc");

      FOR i := FIRST(NameSpace) TO LAST(NameSpace) DO
	(* attach the new namespace to the spin services namespace *)
	name := NSName.FromText(NameSpace[i]);
	svcNS.attach(name, NEW(NameServer.Default).init());
      END;

      (* find the domain namespace and add the debug domain *)
      name := NSName.FromText("domains");
      WITH domainNS = NARROW(svcNS.lookup(name), NameServer.T) DO
        (* attach the debug domain into the domain namespace *)
	name := NSName.FromText(Domain.debugName);
        domainNS.attach(name, Domain.debug);
      END;

      IF verbose THEN IO.Put("Core namespace management initialized\n"); END;
    EXCEPT
    | NameServer.Error =>
      IF verbose THEN IO.PutError("Core management initialization failed\n"); END;
    END;
  END Init;

BEGIN
END CoreNS.

