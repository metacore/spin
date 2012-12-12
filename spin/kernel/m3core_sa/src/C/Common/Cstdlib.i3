(* Copyright (C) 1989, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Cstdlib.i3                                            *)
(* Last modified on Tue Nov 20 04:03:35 1990 by muller         *)
(*      modified on Sat Jan 20 22:12:33 1990 by jerome         *)

(*
 * HISTORY (SPIN)
 * 01-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Some of these procedures are no longer defined in the spin
 *      kernel, and we wish to discourage their use, so they are
 *      commented out here.
 *
 * 06-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Changed malloc and free to use spin_malloc and spin_free
 *)

INTERFACE Cstdlib;

FROM Ctypes IMPORT void_star;
FROM Cstddef IMPORT size_t;

(*
 * PROCEDURE abort ();
 *
 * PROCEDURE atexit (func: PROCEDURE ()): int;
 *
 * PROCEDURE exit (status: int);
 *
 * PROCEDURE getenv (name: const_char_star): char_star;
 *
 * PROCEDURE system (string: const_char_star): int;
*)

PROCEDURE malloc (size: size_t): void_star;

PROCEDURE free (ptr: void_star);

(*
 * PROCEDURE strtod (str: const_char_star; ptr: char_star_star): double;
 *
 * PROCEDURE atof (str: const_char_star): double;
 *)

END Cstdlib.
