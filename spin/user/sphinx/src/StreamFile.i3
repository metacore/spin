(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      cleaned up imports
 *)


(* Stream file handle is a pseudo file object for stream devices.
   Since we use File.T as an abstraction for any I/Os, we have to
   create such thing as fake stream file. *)

INTERFACE StreamFile;
IMPORT Types;
IMPORT File;
IMPORT BSDtty;

TYPE
  Info = REF RECORD
    (* We store "struct tty*" information here. *)
    name : TEXT; (* just for debugging purpose *)
    gid : INTEGER; (* the foreground pgrp *)
  END;

  T <: Public;
  Public = File.T OBJECT
        info: Info;
	dev: BSDtty.T;
        async: BOOLEAN; (* TRUE if FIOASYNC is set *)
     END;

  (* NullFH corresponds to "/dev/null" *)
  Null <: Public;

  (* Text file handle is used only in regresion tests. This is write only,
   and all the outputs go into a unique text buffer. By calling
   GetTextFHContents(), you can get the accumulated contents.

   Calling close, and the write on TextFH clears the previous contents. 
   *)
  Text <: File.T;
    
PROCEDURE InternTty(path : TEXT) : Info;
PROCEDURE SetFG(fh : T; gid : Types.Pid);
PROCEDURE GetTextFHContents() : TEXT;

END StreamFile.
