(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
        FetchDep.i3

        Loads the initial extensions ether from tftp or http.
 *)
(*
 * HISTORY
 * 20-Mar-97  Tsutomu Owa (owa) at University of Washington
 *	Moved from ALPHA_SPIN as stcp now works on both PC and alpha.
 *)

INTERFACE Fetch;

PROCEDURE Fetch(path:TEXT; VAR buf:REF ARRAY OF CHAR): INTEGER;
  (* fetch a file ether from tftp or http *)

PROCEDURE Config(READONLY line : TEXT);
  (* parse line and execute 'boot' command, if exists. *)

PROCEDURE Init();
  (* Initialize variables for extension fetching *)

PROCEDURE FreeMemory();
  (* force simple tcp to release his buf *)

END Fetch.
