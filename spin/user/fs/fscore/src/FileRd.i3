(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new file system interface.
 *	
 * 15-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

(* A FileRd.T is a reader connected to a FileIO.T file. It is buffered,
   and seekable. FileRd.T will have to maintain its own
   1 character buffer for now, but GetSub will bypass it. *)

INTERFACE FileRd;
IMPORT Rd;
IMPORT File;

TYPE
  T <: Public;
  Public = Rd.T OBJECT METHODS init(p: File.T): T END;

PROCEDURE Open(p: TEXT): T RAISES {Rd.Failure};
(* Equivalent to "NEW(T).init(FileIO.open_file(FileIO.new_fp(), p))". *)

END FileRd.  
