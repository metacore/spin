(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *       Added Configure facility to allow access to CAM device.
 *
 * 20-Jul-95  Brian Bershad (bershad) at the University of Washington
 *       Created.
 *
 *)

(* CAMPrivate facilities exported by the OS dependent CAM
   implementation. *)

INTERFACE CAMPrivate;
IMPORT Word, Ctypes;

(* Register routines are called for autoconf devs from
   sal/io/dec/tc/tc.c:tc_config_cont() *)
PROCEDURE RegisterRZ(str: Ctypes.char_star; unit:CARDINAL; id:Word.T) ;
PROCEDURE RegisterTZ(str: Ctypes.char_star; unit:CARDINAL; id:Word.T) ;
END CAMPrivate.


