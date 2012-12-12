(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE EFSCmd;
IMPORT ParseParams;

CONST CommandName = "efs";
CONST CommandHelp = "nuke DIR\ncreate DIR NAME SIZE\nzero NAME\n";

PROCEDURE Run(pp : ParseParams.T) : BOOLEAN;
  
END EFSCmd.
