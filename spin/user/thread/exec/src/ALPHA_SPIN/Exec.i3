(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Program exec
 *
 * HISTORY
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated exec cmd and Exec itself.
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 * html
 *)

(*

   This module provides a way to load the contents of UNIX a.out file
   into address space.

 *)
 
INTERFACE Exec;
IMPORT Error, Space, UserSpaceThread;
IMPORT AoutParser;

TYPE Info = AoutParser.Info;
  
PROCEDURE Execute(filename: TEXT;
		  READONLY argv: ARRAY OF TEXT;
		  READONLY environ: ARRAY OF TEXT;
		  space: Space.T; thread : UserSpaceThread.T;
		  VAR info: Info) RAISES {Error.E};
(* "Execute" doesn't execute the program itself; it merely loads the
   contents into space and register packet.

   "filename" is the absolute path of the OSF/1 executable. It can de either
   statically linked or dynamically linked. "argv" and "environ" are same as
   in UNIX. Each entry in "environ" should look like "SHELL=/bin/sh".
   "space" is the space on which the contents are loaded. "Thread" is the
   [thread:index] on whose register record the initial values are set.
   "Info" will hold the various [information:AoutParser] about the
   executable file on successful return.

   When "filename" is a dynamically linked executable, the dynamic loader
   is started instead of the "filename" itself. The loader path is
   "/spin/bin/loader" by default, but it can de changed by setting
   the [shell] variable "LOADER_PATH"(the path has to de absolute).

   See [SphinxCmd.i3:SphinxCmd.Execute] for how to use this procedure to
   actually start a new process.
   *)
  
  
END Exec.
