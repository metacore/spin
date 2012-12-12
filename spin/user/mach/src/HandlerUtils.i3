(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 10-Mar-96  David Dion (ddion) at the University of Washington
 *	Export Debug var to improve efficiency in other syscall extensions.
 *	Add SimpleWait and EndWait for user-level shell.
 *
 * 14-Jan-96  David Dion (ddion) at the University of Washington
 *	Commented out SetDebugSpace, which is not implemented in 
 *      HandlerUtils.m3.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 17-Aug-95  David Dion (ddion) at the University of Washington
 *       Created.
 *
 *)

(* "HandlerUtils" interface -- dynamically linked extension *)

(* The "HandlerUtils" interface exports some utility procedures used by
   the Unix syscall handling extensions. *)

INTERFACE HandlerUtils;

IMPORT Space, Word;
IMPORT UserSpaceThread;

VAR 
  Debug : BOOLEAN := FALSE;
(*
CONST
  Debug = FALSE;
*)


EXCEPTION
  BadArg;
  BadBuf;
  BadSpace;
  BadAddr;
  Failure;

PROCEDURE GetNArgsOffStack(space: Space.T; 
                           sp: Word.T; 
                           N: CARDINAL;
                           VAR extraArgs: ARRAY OF Word.T) : BOOLEAN;
(* Get N arguments off the stack.  These will be arg6-arg6+N. *)

PROCEDURE WordToArray(data: Word.T; VAR array: ARRAY OF CHAR; 
  start: CARDINAL; finish: CARDINAL);
(* Convert a Word.T into an ARRAY OF CHAR. *)

PROCEDURE ArrayToWord(VAR data: Word.T; READONLY array: ARRAY OF CHAR;
  start: CARDINAL; finish: CARDINAL);
(* Convert an ARRAY OF CHAR into a Word.T. *)

PROCEDURE RoundUpToPage(VAR x: Word.T);
(* Round up to VM page size. *)

PROCEDURE RoundDownToPage(VAR x: Word.T);
(* Round down to VM page size. *)

PROCEDURE DebugPrint(mesg: TEXT);
(* Print a debugging message only if the debugging switch is turned on. *)

PROCEDURE Print(mesg: TEXT);
(* Print TEXT to the console. *)

PROCEDURE PrintError(mesg: TEXT);
(* Print TEXT to the console as an error message. *)

PROCEDURE SetDebugSyscall(switch: Word.T) : Word.T;
(* Set the debugging switch. *)

PROCEDURE SimpleWait();
(* Wait on a process to complete before returning.  This procedure is
 * VERY dumb -- there are no provisions for multiple processes, etc. *)

PROCEDURE EndWait();
(* Release the thread which called SimpleWait. *)

PROCEDURE Externalize(<*UNUSED*>uthread: UserSpaceThread.T;
                      intptr: REFANY) : Word.T;
(* Wrapper to change extern refs approach easily *)

PROCEDURE Internalize(<*UNUSED*>uthread: UserSpaceThread.T;
                      extptr: Word.T) : REFANY;
(* Wrapper to change extern refs approach easily *)

PROCEDURE CopyIn(space: Space.T; 
                 source: Word.T; 
                 VAR dest: ARRAY OF CHAR;
                 len: CARDINAL) : BOOLEAN;
(* Read data into the kernel *)

PROCEDURE CopyOut(space: Space.T; 
                  READONLY source: ARRAY OF CHAR; 
                  dest: Word.T;
                  len: CARDINAL) : BOOLEAN;
(* Write data out of the kernel *)

END HandlerUtils.
