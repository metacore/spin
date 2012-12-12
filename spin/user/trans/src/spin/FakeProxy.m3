(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE FakeProxy EXPORTS Proxy;
IMPORT HostID;
IMPORT TransRPC;

REVEAL Server = ServerPublic BRANDED OBJECT END;

PROCEDURE Open(hid : HostID.T) : Server =
  BEGIN
    RETURN NIL;
  END Open;
PROCEDURE Close(server : Server) =
  BEGIN
    <*ASSERT FALSE*>
  END Close;
PROCEDURE DoRPC(server : Server; buf : TransRPC.T): BOOLEAN =
  BEGIN
    <*ASSERT FALSE*>
  END DoRPC;
PROCEDURE QueueRPC(server : Server; buf : TransRPC.T) =
  BEGIN
  END QueueRPC;

BEGIN
END FakeProxy.
