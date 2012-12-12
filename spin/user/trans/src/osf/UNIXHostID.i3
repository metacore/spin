(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *)
INTERFACE UNIXHostID;
IMPORT IP, HostID;

PROCEDURE IPAddrToHostID(addr : IP.Address) : HostID.T;
PROCEDURE HostIDToIPAddr(hid : HostID.T) : IP.Address;
  
PROCEDURE GetPortForServer(hid : HostID.T) : INTEGER;
  
END UNIXHostID.
