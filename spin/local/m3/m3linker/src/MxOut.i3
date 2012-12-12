(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxOut.i3                                              *)
(* Last Modified On Thu Jul  7 09:49:09 PDT 1994 By kalsow     *)

(*
 * HISTORY
 *
 * 24-Aug-97  Przemek Pardyak (pardy) at the University of Washington
 *	Added compile flags to link information.
 *)

INTERFACE MxOut;

IMPORT Wr, Mx;

(*------------------------------------------------------------------------*)

PROCEDURE WriteUnits (units: Mx.UnitList;  output: Wr.T;  
                      flags: REF ARRAY OF TEXT);
(* write the linker info for the 'units' on 'output'.  This is
   the inverse of 'MxIn.ReadUnits' *)

PROCEDURE UnitsToText (units: Mx.UnitList;  flags: REF ARRAY OF TEXT): TEXT;
(* same as the above but instead of writing into a file text is returned *)

END MxOut.
