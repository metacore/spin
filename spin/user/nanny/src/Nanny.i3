(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 26-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *)

(* "Nanny" is the "domain control" interface.  It's purpose is much
   like inetd, providing a friendly on-demand loader for many popular
   SPIN services.  It works by registering domain names with the
   nameserver, using the auth callback service to on-demand load a
   domain when someone asks for it.  At that point, nanny takes itself
   out of the naming loop for the loaded domain and waits until the
   loaded domain is unloaded, at which point, nanny kicks back in for
   the domain.  *)

INTERFACE Nanny;
IMPORT ParseParams;

CONST CommandName = "nanny";
CONST CommandHelp = " commands\n  install [-f] [-n alias] name script\n  touch alias\n  delete\n  zap";
  
PROCEDURE Run(pp: ParseParams.T): BOOLEAN; 
END Nanny.


 

  

