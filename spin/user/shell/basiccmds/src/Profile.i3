(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Jul-96  Charles Garrett (garrett) at the University of Washington
 *	Added command to set the number of arcs in profiling data array.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added cmd and flush options for profile command.
 *
 * 05-May-96  Charles Garrett (garrett) at the University of Washington
 *	Shell interface to profiling. In order to profile something from
 *	 the shell, call "profile on", run your program and then call
 *	 "profile off". Then attach to the crash machine with m3gdbttd
 *	 and read out the information. Be sure to read out the
 *	 information before turning on profiling again because the
 *	 internal buffers will be overwritten. 
 *
 *)
INTERFACE Profile;

IMPORT ParseParams;

CONST CommandName = "profile";
CONST CommandHelp = " on|off|cmd|flush|dump|arcs <int>";

PROCEDURE Run (pp: ParseParams.T): BOOLEAN;

END Profile.


