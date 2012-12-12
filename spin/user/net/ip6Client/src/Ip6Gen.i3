(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)

INTERFACE Ip6Gen;
IMPORT Ip6PktFormat, Mbuf, Net;
IMPORT Ctypes;
IMPORT Ip6Route;

VAR debug_level := Net.oLevel.NODEBUG;
CONST debug = FALSE;

PROCEDURE OverlayChecksum(
    READONLY ip : Ip6PktFormat.Header;
    paylen      : Ctypes.unsigned_int;
    next_head   : Ctypes.unsigned_char := 0) : Ctypes.unsigned_short;

PROCEDURE PacketSend(
	READONLY ip	: Ip6PktFormat.Header;
	READONLY data	: Mbuf.T;
                 rt     : Ip6Route.T := NIL);

PROCEDURE Init(verbose:BOOLEAN);
PROCEDURE Uninit(vebose:BOOLEAN);
END Ip6Gen.


