(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* "NSName.T" is a standard reprenentation of names used in name server and
   file systems. It is designed in such a way that
   substring operation is very fast.

   Note that the lifetime of the "str" field is usually only during
   one lookup.
   Thus, if you want to save away a name, 
   you must call "DeepCopy" to create your own copy of the name.

   Legacy codes still use NameServer.Name to mean NSName.T, but
   the use of the former should be phased out.
   
   *)

INTERFACE NSName;

TYPE T = RECORD
  str: REF ARRAY OF CHAR;
  from, end: CARDINAL;
END;
  
PROCEDURE ToText(READONLY name: T): TEXT;
  (* Creates a deep copy. *)
  
PROCEDURE FromText(str: TEXT): T;
  (* Creates a deep copy. *)
  
PROCEDURE FromRefArrayChar(str: REF ARRAY OF CHAR): T;
  (* Creates a shallow copy. *)
  
PROCEDURE FromArrayChar(READONLY str: ARRAY OF CHAR): T;
  (* Creates a deep copy. *)
  
PROCEDURE DeepCopy(READONLY src: T): T;
  (* Create a copy of "src" that doesn't share memory with "src". *)

(* Below are for the table generic. *)
PROCEDURE Hash (READONLY name: T): INTEGER;
PROCEDURE Equal (READONLY a, b: T): BOOLEAN;
CONST Brand = "NSName";
    
END NSName.
