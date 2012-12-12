(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 18-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

INTERFACE NameServerPrivate;
IMPORT NameServer;

VAR curRoot : NameServer.T; (* current root (aka filesystem root) *)
VAR root: NameServer.T; (* real root. *)  
VAR svc: NameServer.T; (* spin services namespace(aka "svc") *)

PROCEDURE SetRoot(dir: NameServer.T);
(* Set "curRoot". *)
  

PROCEDURE Init(verbose:BOOLEAN);
END NameServerPrivate.
