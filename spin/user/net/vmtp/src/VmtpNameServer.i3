(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE VmtpNameServer;

(*
   VMTP name server is supposed to maintain a mapping between a textual name
   and an entity.

   However, it's a fake now. It only supports names of the form
   "disc@host", where disc is one of "trans" and "foo", host is
   the internet domain name of the host that the principal resides.
*)

FROM VmtpPktFormat IMPORT Entity;

PROCEDURE Lookup(name: TEXT; VAR e: Entity): BOOLEAN;
  
END VmtpNameServer.
