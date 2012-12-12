(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(* This is a stub to isolate idiosyncrasies in M3 I/O *)
INTERFACE RawIO;
IMPORT Error;
TYPE
  T <: REFANY;
  FileStat = RECORD
    size: INTEGER;
  END;
  
CONST BlockSize = 8192;
(* Basic disk block size. All I/O has to be multiple of this size.
   XXX this might be arch dependent. *)
  
PROCEDURE Open(file: TEXT; size: CARDINAL): T RAISES {Error.E};
(* Open the file "file". If the file does not exists, then it is created
   with the byte "size". *)
  
PROCEDURE Stat(f: T): FileStat;
(* Get some file attributes. *)
  
PROCEDURE Write(f: T; READONLY buf: ARRAY OF CHAR; off: CARDINAL);
(* Write the array "buf" from byte offset "off" of the file "f".
   Write is synchronous, i.e., when this procedure returns,
   the data is guaranteed to be written to the disk.
   "off" must be "BlockSize" aligned, and "NUMBER(buf)" must be
   multiple of "BlockSize. *)
  
PROCEDURE Read(f: T; VAR buf: ARRAY OF CHAR; off: CARDINAL): INTEGER;
(* Read from the byte offset "off" of the file "f" into "buf".
   Same restrictions as "write" apply to "buf" and "off". *)
  
PROCEDURE Close(f: T);
(* Close the file. *)		

PROCEDURE PrintStat();
END RawIO.
