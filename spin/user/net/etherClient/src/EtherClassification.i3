(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Switching to new type.
 *)

INTERFACE EtherClassification;
IMPORT EtherPktFormat, Net;
TYPE T = EtherPktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);
VAR debug_level : Net.Level := Net.oLevel.NODEBUG;
END EtherClassification.

