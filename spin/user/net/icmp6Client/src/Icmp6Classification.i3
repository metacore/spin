(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *)
INTERFACE Icmp6Classification;
IMPORT Icmp6PktFormat, Net;
TYPE T = Icmp6PktFormat.T;
VAR debug_level : Net.Level := Net.oLevel.NODEBUG;

PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);
END Icmp6Classification.
