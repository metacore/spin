(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* File: MxIn.i3                                               *)
(* Last Modified On Mon Aug  1 10:46:23 PDT 1994 By kalsow     *)

(*
 * HISTORY
 * 10-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	change so that unit stores domain
 *      add SpinReadUnits
 *)

INTERFACE MxIn;

IMPORT Mx, Wr;
IMPORT Domain;

PROCEDURE StartReading ();
PROCEDURE ReadUnits (info: UNTRACED REF CHAR;
                     errors: Wr.T; d: Domain.T): INTEGER;
PROCEDURE GetUnits (): Mx.UnitList;

(* SPIN *)
PROCEDURE SpinReadUnits (info: UNTRACED REF CHAR; errors: Wr.T;
                         d: Domain.T; VAR (* OUT *) u: Mx.Unit) : INTEGER;

(* read and parse the link-info in 'input' and return the resulting UnitList.
   Each unit will be marked as coming from 'filename'.  If there are errors,
   messages will be written to 'errors' and NIL is returned.  If 'errors' is
   NIL, the error messages are silently dropped. *)

(*------------------------------------------------------------------------*)
(*
   A 'link-info' file is a sequence of possibly blank filled lines
   with the following contents:

     Nx y     --- define id number 'x' to be name 'y'
     Vx a b c --- define version stamp number 'x' for symbol 'a.b' to be 'c'

     In a b c d e f g h i j k l  --- start unit: interface 'n'
     Mn a b c d e f g h i j k l  --- start unit: module 'n'

     Am       --- current unit exports interface m
     Bm       --- current unit imports interface m
     Cm       --- current unit "uses" magic info from interface m
     Dm       --- current unit "uses" magic info from module m
     gm       --- current unit imports generic unit m
     ix       --- import version stamp 'x'
     Jx       --- import & implement version stamp 'x'
     ex       --- export version stamp 'x'
     Ex       --- export & implement version stamp 'x'
     Rn x y   --- export REVEAL 'x' = 'y' to unit n
     rn x y   --- import REVEAL 'x' = 'y' from interface n
     Xn x y   --- export REVEAL 'x' <: 'y' to unit n
     xn x y   --- import REVEAL 'x' <: 'y' from interface n
     Qt s     --- define opaque type 't' with supertype 's'.
     tx       --- import type 'x'
     Tx       --- export type 'x'
     wt       --- wish to know the object type 't'.
     on t s ds da ms -- import object type from interface unit n
     pn t s ds da ms -- import object type from module unit n
     Ot s ds da ms -- export object type 't' with supertype 's',
                  data size 'ds', data alignment 'da', and
                  method size 'ms' from unit #n
     Fn f1-fn --- define m3options to be used in recompilation during import

         ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz
  USED:  *****   **  ***  * * * *       * * *     **** *  **
*)

PROCEDURE Init ();

END MxIn.
