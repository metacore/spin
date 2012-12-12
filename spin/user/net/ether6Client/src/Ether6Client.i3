(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 * 
 * HISTORY
 *)

INTERFACE Ether6Client;
PROCEDURE Init(verbose:BOOLEAN); 
CONST Brand  = "EtherClient";
CONST timing = FALSE;
(* shell command support *)
CONST Ether6ClientCommandName = "ether6client";
      Ether6ClientCommandHelp = " debug level | uninstall";
CONST Ether6GenCommandName = "ether6gen";
      Ether6GenCommandHelp = " -debug level";
END Ether6Client.
