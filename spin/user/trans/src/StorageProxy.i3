(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(* Client side storage module. *)

INTERFACE StorageProxy;
IMPORT Storage;
IMPORT Error;

PROCEDURE Open (host: TEXT; port: CARDINAL; fileName: TEXT): Storage.T
  RAISES {Error.E};
(* Open a storage "fileName" on the host "host". *)

END StorageProxy.
