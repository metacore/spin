(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Eric Christoffersen (ericc) at the University of Washington
 *	Created.  Domain holds common types, constant and objects for use in 
 * the LFS.
 *
 *)
MODULE LFSCore;


IMPORT LFSCoreInterface, NameServer, Auth,IO;

BEGIN

  TRY 
    EVAL LFSCoreInterface.Export(NEW (Auth.AuthAlways));
  EXCEPT
    NameServer.Error =>
    IO.Put("Error when exporting LFSCoreInterface, NameServer.Error\n");
  ELSE
    IO.Put("LFSCore exported.\n");
  END;

END LFSCore.
