(* Copyright 1993 by Digital Equipment Corp.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Jan 26 14:03:08 PST 1995 by kalsow     *)
(*      modified on Wed Apr 21 09:07:52 PDT 1993 by mcjones    *)
(*      modified on Tue Mar  9 11:57:?? PDT 1993 by mjordan    *)

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 25-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	OpenRead doesn't go to SALtftp anymore.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 4-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Made deleting of tabulation move the cursor correctly and
 *	added control-c to cancel the whole line in ReadLine.
 *
 * 22-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Make faithful to IO.i3 and raise Error in GetLine on Rd failure.
 *
 * 09-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Copy the SRC standard library IO interface.
 *)

MODULE FileIO;
IMPORT IO, Dispatcher;
IMPORT Rd;
IMPORT FileRd;

PROCEDURE OpenRead(f: TEXT): Rd.T=
  VAR
    rd: Rd.T;
  BEGIN
    TRY 
      (* Attempt to open as directly mounted file. *)
      rd := FileRd.Open(f);
    EXCEPT
    ELSE
      IO.PutError("Unknown exception raised by FileRd.Open()\n");
      rd := NIL;
    END;
    RETURN rd;
  END OpenRead;

BEGIN
  TRY
    Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(IO.OpenRead));
    EVAL Dispatcher.InstallHandler(IO.OpenRead, NIL, OpenRead);
  EXCEPT
  ELSE
    IO.PutError("FileIO:  handler installation failed\n");
  END;
END FileIO.
