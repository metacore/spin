(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Unsafe conversion interfaces for trusted clients.
 *)
UNSAFE INTERFACE UnsafeConvert;
IMPORT Ctypes;

EXCEPTION LengthMismatch;

(* 
 * The routines below are unsafe because thye trust the client to
 * specify a valid length.
 *)
   
(*
 * convert an array of bytes into a ref array of char.  
 * Caller is responsible for freeing the bytes pointed to by cp when 
 * cp is freed. (should also have less costly interface!)
 *)
PROCEDURE BytesToRefArrayOfChar (    cp  : Ctypes.char_star;
                                     len : CARDINAL; 
                                 VAR ra  : REF ARRAY OF CHAR) RAISES {LengthMismatch};

PROCEDURE BytesToArrayOfChar (    cp  : Ctypes.char_star;
                                  len : CARDINAL; 
                               VAR ra  : ARRAY OF CHAR) RAISES {LengthMismatch};

PROCEDURE StoT(s: Ctypes.char_star; len: INTEGER := 0) : TEXT;

END UnsafeConvert.

