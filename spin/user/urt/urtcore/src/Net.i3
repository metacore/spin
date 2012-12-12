(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 20-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Fixed checksum()
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made routines FUNCTIONAL
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Made routines ephemeral.  Took out in_checksum.
 *
 * 05-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *    Converted Net.BYTE to CHAR, so that network input can easily be
 *    turned into texts.
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Added subfree which frees things allocated by subarray.
 *
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *      Created.
 *)

INTERFACE Net;

IMPORT Ctypes; (* m3core *)

TYPE nible = BITS 4 FOR [16_0 .. 16_f];
TYPE BYTE  = CHAR;

TYPE Payload = UNTRACED REF ARRAY OF CHAR;

(* Interface for network/external data conversion function. *)
EPHEMERAL FUNCTIONAL
PROCEDURE htons(x:Ctypes.unsigned_short):Ctypes.unsigned_short;

EPHEMERAL FUNCTIONAL
PROCEDURE nstoh(x:Ctypes.unsigned_short):Ctypes.unsigned_short;

EPHEMERAL FUNCTIONAL
PROCEDURE htonl(x:Ctypes.unsigned_int):Ctypes.unsigned_int;

EPHEMERAL FUNCTIONAL
PROCEDURE nltoh(x:Ctypes.unsigned_int):Ctypes.unsigned_int;

(* EPHEMERAL *)
PROCEDURE checksum(
    READONLY 
    packet   : ARRAY OF CHAR;
    size     : CARDINAL := 0; 
    prevcsum : Ctypes.unsigned_short := LAST(Ctypes.unsigned_short))
  :Ctypes.unsigned_short;

(* Debugging support *)

TYPE oLevel={NODEBUG, INFO, NOTICE, DEBUG, WARNING, ERROR, CRITIICAL, ALERT, EMERGENCY};
TYPE Level = [ oLevel.NODEBUG .. oLevel.EMERGENCY ];
PROCEDURE MapDebug(i:INTEGER):Level;
PROCEDURE Debug(debug_level,priority: Level; READONLY x:TEXT);

PROCEDURE Init();
(* Module Initialization routine. *)

END Net.
