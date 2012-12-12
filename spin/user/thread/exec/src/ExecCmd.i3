(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Program exec
 *
 * HISTORY
 * 12-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed TtySignal handler so that it won't signal the shell unless
 *	^C is typed 2 types rapidly.
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated exec cmd and Exec itself.
 * 10-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *)

(*

 This module defines a shell command "exec".

 Exec is used to spawn off a user space process. It may take two options.

 "-noreturn" suspends the shell after the child proc is started. Therefore,
 you can't do anything after that. This is an early hack by David Dion
 to execute the OSF/1 server without the keyboard interference from shell.

 "-w" is more sophisticated option aimed at the same goal. With this option,
 the shell waits for an internal condition variable. The user process is
 supposed to signal the condition variable using "Signal" procedure on exit.
 You can also signal the shell by typing "^C" two times within a second.
 
 At least one argument, the program path, is required except in case
 "exec" is called with "-w".
 All the arguments following the program path are passed to the user
 space process using C argc/argv protocol. In addition to that, all the
 shell variables are passed to as environment variables.
 Dlib applications can read argc/argv and environment variables.

 | exec /spin/caveman/hello Hello World

 In the above example, the program "/spin/caveman/hello" is created with
 arguments "Hello" and "World".
 
 *)
 
INTERFACE ExecCmd;
IMPORT ParseParams;


CONST Brand = "ExecCmd";

CONST CommandName = "exec";
CONST CommandHelp = " [-noreturn] [-w] filename [args...]";

PROCEDURE Wait();
PROCEDURE Signal();
PROCEDURE Run (pp: ParseParams.T) : BOOLEAN;

END ExecCmd.
