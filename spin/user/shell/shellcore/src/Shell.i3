(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Shell interface.
 * Need to revise to export all services as extensions.
 *
 *
 * HISTORY
 * 14-May-96  Brian Bershad (bershad) at the University of Washington
 *	Backed out Exit Exception from OneShellCommand. Served no
 *	 purpose. Removed Init.
 *
 * 09-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 05-Jan-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added explicit uninstallation of commands.
 *	Changed return type of CommandNewInstall to an opaque descriptor.
 *
 * 22-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Removed ShowDomainsText and ShowThreadsText.
 *
 * 03-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Added ShowThreadsText and ShowDomainsText which return text
 *      strings containing the information.
 *
 * 01-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *      Added dispatcher exceptions to the list of exceptions raised
 *      by procedures that interface clients with handler installation.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Whisted. Copyright.
 *
 *)

INTERFACE Shell;

IMPORT ParseParams, Glob;

CONST Brand = "Shell";  (* Use this name to link with this extension *)

TYPE
  T <: REFANY;


(*
 * EVENTS
 *	Run -- somebody parse the input
 *	Help -- what do you do?
 *)

PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

PROCEDURE Help(command: TEXT := NIL;
	       detailed := FALSE);

PROCEDURE ParseArgs(line: TEXT) : ParseParams.T;
	(* Gen a pp from a ws-delimited text *)


PROCEDURE OneShellCommand(t: TEXT) : BOOLEAN;
			(* Execute one shell command from 
		           the input stream wr; *)


PROCEDURE FailStop() : BOOLEAN;
(*
 * By default, DO NOT FAIL when we find a command that can not execute correctly.
 *)

PROCEDURE Verbose() : BOOLEAN;
PROCEDURE Status(): TEXT;
	(* Returns any shell status.  NIL if none. *)

(*
 * SHELL VARIABLES
 *)


PROCEDURE Vars(): Glob.T;	(* Return calling thread's shell vars *)
PROCEDURE SysVars() : Glob.T;	(* Return the global system variables *)


CONST
      SHELLVARS = "shell-vars";	(* Identity property name of shell variables *)

CONST
     (* Some convenient shell variables *)
      HOME        = "home";
      USER        = "user";
      LOADPATH    = "loadpath";
      VERSION     = "version";
      BUILDDIR    = "builddir";
      TARGET      = "target";
      HOSTID      = "hostid";
      IPADDR      = "ipaddr";
      IPADDR0     = "ipaddr0";
      IPADDR1     = "ipaddr1";
      IPADDR2     = "ipaddr2";
      IPADDR3     = "ipaddr3";
      STATUS      = "status";
      EXIT_STATUS = "exit-status";
      VERBOSE     = "verbose";
      FAILSTOP    = "failstop";
      MINUSONE    = "-1";
      ZERO        = "0";
      PROMPT      = "prompt";

PROCEDURE CommandLoop (showPrompt: BOOLEAN := TRUE;
                       echoChars : BOOLEAN := TRUE  );


END Shell.
