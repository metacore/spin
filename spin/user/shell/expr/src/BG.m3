(*
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY 
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed over to new security management
 *
 * 5-oct-96  becker at the University of Washington
 *	Make child inherit parents shell variables
 *
 * 2-Mar-96  Brian Bershad (bershad) at the University of Washington
 *	Created
 *)


MODULE BG;
IMPORT Shell, ParseParams, SpinException, ThreadExtra, IO, ExprCmds;
IMPORT Glob, SecurityManager;

(*
 * ACK. WE don't catch the SpinException if we are asked to BG something
 * that doesn't exist. We should catch this through the default handler.
 * Sort of becoming a mess.
 *)

PROCEDURE RunThread (ra: ThreadExtra.ArgT): ThreadExtra.ResultT =
  BEGIN
    TRY
      EVAL Shell.Run(NARROW(ra, ParseParams.T));
    EXCEPT SpinException.Exception(ei) => ExprCmds.Error(ei); END;
    RETURN NIL;
  END RunThread;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR cmd: TEXT;
  BEGIN
    pp.reset();
    TRY
      EVAL pp.getNext();	(* Skip BG *)
      cmd := pp.peekNext();
      IO.Put("Backgrounding " & cmd & "\n");

      (* Set the console variable's shell variables to inherit the parents. *)
      EVAL SecurityManager.SetCurrentProperty(Shell.SHELLVARS,
                                              Glob.New(Shell.Vars()));

      EVAL ThreadExtra.PFork(RunThread, pp.dup());
    EXCEPT
      ParseParams.Error => IO.Put("bg what?\n"); RETURN FALSE;
    END;
    RETURN TRUE;
  END Run;

BEGIN
END BG.
