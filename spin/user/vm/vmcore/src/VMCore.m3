(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 18-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created to export the domain.
 *
 *)

MODULE VMCore;
IMPORT VMCoreInterface, NameServer, IO;

BEGIN
  TRY
    EVAL VMCoreInterface.Export();
  EXCEPT
  | NameServer.Error => 
    IO.PutError("VMCore: interface export failed\n");
  END;
END VMCore.

