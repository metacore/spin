(*
 * Copyright 1995-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 12-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src/IX86_SPIN.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made routines FUNCTIONAL
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up. Made routines ephemeral.
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Added subfree which frees things allocated by subarray.
 *
 * 11-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *      Created.  Interface to assembly language routines.
 *)

UNSAFE (* for externals *)
INTERFACE StcpNetExtern;

IMPORT Ctypes; (* m3core *)

(* Interface for network/external data conversion function. *)
<* EXTERNAL byte_swap_word *> 
EPHEMERAL FUNCTIONAL
PROCEDURE htons(x:Ctypes.unsigned_short):Ctypes.unsigned_short;

<* EXTERNAL byte_swap_word *> 
EPHEMERAL FUNCTIONAL
PROCEDURE nstoh(x:Ctypes.unsigned_short):Ctypes.unsigned_short;

<* EXTERNAL byte_swap_long *> 
EPHEMERAL FUNCTIONAL
PROCEDURE htonl(x:Ctypes.unsigned_int):Ctypes.unsigned_int;

<* EXTERNAL byte_swap_long *> 
EPHEMERAL FUNCTIONAL
PROCEDURE nltoh(x:Ctypes.unsigned_int):Ctypes.unsigned_int;

<* EXTERNAL in_checksum *> 
EPHEMERAL 
PROCEDURE in_checksum(
    packet   : ADDRESS; 
    len      : Ctypes.unsigned_int; 
    prevcsum : Ctypes.unsigned_short := 0):Ctypes.unsigned_short;

END StcpNetExtern.
