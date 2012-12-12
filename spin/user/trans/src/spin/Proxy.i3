(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE Proxy;
IMPORT HostID, ConnFD, BufferPool;
IMPORT TransStub;

TYPE Server = REF RECORD
  hid : HostID.T;
  fd : Socket.T; (* TCP channel *)
  pool : BufferPool.T;
END;

PROCEDURE Open(hid : HostID.T) : Server;
PROCEDURE Close(server : Server);
PROCEDURE DoRPC (server : Server; buf : TransStub.T);
  
END Proxy.
