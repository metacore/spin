(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 03-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE UNIXUtils;
PROCEDURE Perror(msg: TEXT);
PROCEDURE Tokenize (line: TEXT): REF ARRAY OF TEXT;  
END UNIXUtils.
