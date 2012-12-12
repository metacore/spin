(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
UNSAFE MODULE HostID EXPORTS HostID, UNIXHostID;
IMPORT Rd, IO, IP, Process;

PROCEDURE IPAddrToHostID (ipAddr : IP.Address) : T =
  BEGIN
    (* HostID has to be byte order independent. Thus we don't use
     VIEW here *)
    RETURN (  ipAddr.a[0] * 16_1000000
	    + ipAddr.a[1] * 16_10000 +
	    + ipAddr.a[2] * 16_100 + 
	    + ipAddr.a[3] * 16_1);
  END IPAddrToHostID;

PROCEDURE HostIDToIPAddr (hid : T) : IP.Address =
  VAR ipAddr : IP.Address;
  BEGIN
    ipAddr.a[0] := hid DIV 16_1000000;
    ipAddr.a[1] := (hid DIV 16_10000) MOD 16_100;
    ipAddr.a[2] := (hid DIV 16_100) MOD 16_100;
    ipAddr.a[3] := hid MOD 16_100;

    RETURN ipAddr;
  END HostIDToIPAddr;



(* Get the port number of the server running on the host "host".
 The server creates a file named "host" on startup, and stores the
 ASCII representation of the port number. This proc just reads it.
 *)
PROCEDURE GetPortForServer (hid : T) : INTEGER =
  VAR
    host : TEXT;
    rd : Rd.T;
    port : INTEGER;
  BEGIN
    host := IP.GetCanonicalByAddr(HostIDToIPAddr(hid));
    
    rd := IO.OpenRead(host & "/ADDR");
    IF rd = NIL THEN
      IO.Put(host & " is not running the server\n");
      Process.Exit(1);
    END;
    port := IO.GetInt(rd);
    Rd.Close(rd);
    RETURN port;
  END GetPortForServer;

PROCEDURE HostNameToT (host : TEXT) : T =
  VAR ipAddr : IP.Address;
  BEGIN
    TRY 
      IF NOT IP.GetHostByName (host, ipAddr) THEN
	IO.Put("I can't resolve the name " & host & ".\n");
	Process.Exit(1);
      END;
    EXCEPT
    | IP.Error =>
      IO.Put("I can't resolve the name " & host & ".\n");
      Process.Exit(1);
    END;
    RETURN IPAddrToHostID(ipAddr);
  END HostNameToT;
  
PROCEDURE Init () =
  BEGIN
    IF myID = 0 THEN
      myID := IPAddrToHostID(IP.GetHostAddr());
    END;
  END Init;

BEGIN
END HostID.
