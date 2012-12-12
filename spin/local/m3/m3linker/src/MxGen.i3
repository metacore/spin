(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: MxGen.i3                                              *)
(* Last Modified On Fri Nov 11 13:00:26 PST 1994 By kalsow     *)

(* HISTORY
 * 28-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	Added linkinfo_ok to turn off the generation of dynamic link
 *	information.
 *
 *
 * 06-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Added GenerateConf procedure to write out conf files with objects
 *	listed in the right order.
 *
 *)

INTERFACE MxGen;

IMPORT Mx, Wr;

VAR linkinfo_ok : BOOLEAN := TRUE;

PROCEDURE GenerateMain (base       : Mx.LinkSet;
                        output     : Wr.T;
                        procname   : TEXT;	(* SPIN *)
                        verbose    : BOOLEAN;
                        windowsGUI : BOOLEAN);
(* write the list of compilation units in 'base' on 'output' in a
   correct Modula-3 initialization order.  It is an error to pass a 'base'
   that hasn't successfully passed through 'MxCheck.IsProgram'. *)

PROCEDURE GenerateConf (base       : Mx.LinkSet;
                        output     : Wr.T;
                        verbose    : BOOLEAN;
                        windowsGUI : BOOLEAN);
(* write the list of object files in a conf file in a 
   correct Modula-3 initialization order.  It is an error to pass a 'base'
   that hasn't successfully passed through 'MxCheck.IsProgram' or
   `MxCheck.IsLibrary'. *)

END MxGen.
