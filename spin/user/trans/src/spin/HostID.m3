(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
UNSAFE MODULE HostID;
IMPORT NetDb;
IMPORT Shell, Glob;

PROCEDURE HostNameToT (host: TEXT): T =
  BEGIN
    TRY
      WITH addr = NetDb.GetHostByName(host) DO 
	(* XXX I don't know how to get my host name - y *)
	RETURN VIEW(addr, T);
      END;
    EXCEPT
    ELSE
    END;
    RETURN 0;
  END HostNameToT;
  
PROCEDURE Init () =
  VAR
    hostName := Glob.GetVariable(Shell.Vars(), "hostname");
  BEGIN
    IF myID = 0 THEN 
      myID := HostNameToT(hostName);
    END;
  END Init;


BEGIN
END HostID.
