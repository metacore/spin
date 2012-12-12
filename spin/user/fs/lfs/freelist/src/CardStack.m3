(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Tim Bradley (tbradley) at the University of Washington
 *	Whisted.
 *
 *)
MODULE CardStack = Stack(Cardinal)
IMPORT CardStackInterface,Auth,NameServer;
BEGIN
  TRY
    EVAL CardStackInterface.Export(NEW(Auth.AuthAlways));
    IO.Put("Exported disk extents...\n");
  EXCEPT
    NameServer.Error => IO.Put("Failed to export extents.\n");
  END;	
END CardStack.
