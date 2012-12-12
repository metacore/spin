(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

INTERFACE Ip6Classification;
IMPORT Ip6PktFormat, Mbuf, Net;
VAR debug_level := Net.oLevel.NODEBUG;
CONST debug  = FALSE;
TYPE T = Ip6PktFormat.T;
PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(verbose:BOOLEAN);
END Ip6Classification.



