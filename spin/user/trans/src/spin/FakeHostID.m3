(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
MODULE FakeHostID EXPORTS HostID;
VAR MyHostID : T; (* caches the value *)

PROCEDURE HostNameToT (host : TEXT) : T =
  BEGIN
    RETURN 0;
  END HostNameToT;
  
PROCEDURE GetT () : T =
  BEGIN
    RETURN 0;
  END GetT;

PROCEDURE Init () =
  BEGIN
  END Init;
  
BEGIN
END FakeHostID.
