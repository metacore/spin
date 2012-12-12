(* Copyright 1993 by Digital Equipment Corp.                   *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(* Last modified on Thu Sep 23 12:17:09 PDT 1993 by mcjones    *)
(*      modified on Fri Jun 18 13:27:27 PDT 1993 by wobber     *)
(*      modified on Sat Jan 26 14:38:00 PST 1993 by gnelson    *)

(* The "IO" interface provides textual input and output for simple
   programs.  For more detailed control, use the interfaces "Rd",
   "Wr", "Stdio", "FileRd", "FileWr", "Fmt", and "Lex". *)

(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleanup.
 *
 * 23-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Enabled wantText for ReadLine./
 *
 * 25-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Added echo parameter to ReadLine which controls whether
 *	 characters are printed on the standard writer. We set echo
 *	to false when telnetting in to a shell.
 *
 * 4-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Made deleting of tabulation move the cursor correctly and
 *	added control-c to cancel the whole line in ReadLine.
 *
 * 09-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Copy the SRC standard library IO interface.
 *)

INTERFACE IO;

IMPORT Rd, Wr;

CONST
    NUL = '\000';
    BS  = '\010';
    DEL = '\177';
    LF  = '\n';
    RET = '\r';
    TAB = '\t';
    CTRL_C = '\003';

PROCEDURE Put(txt: TEXT; wr: Wr.T := NIL);
(* Output "txt" to "wr" and flush "wr". *)

PROCEDURE Putx (n: INTEGER; width := 0; wr: Wr.T := NIL);
(* Output "Fmt.UnsignedInt(n)" to "wr" and flush "wr". *)
   
PROCEDURE PutInt (n: INTEGER; width := 0; wr: Wr.T := NIL);
(* Output "Fmt.Int(n)" to "wr" and flush "wr". *)

(* PROCEDURE PutReal(r: REAL; wr: Wr.T := NIL); *)
(* Output "Fmt.Real(r)" to "wr" and flush "wr". *)

PROCEDURE PutError(txt: TEXT; wr: Wr.T := NIL);
(* Output "txt" to "wr" and enter the debugger. *)

PROCEDURE EOF(rd: Rd.T := NIL): BOOLEAN;
(* Return "TRUE" iff "rd" is at end-of-file. *)

EXCEPTION Error;

(* The exception "Error" is raised whenever a "Get" procedure
   encounters syntactically invalid input, including unexpected
   end-of-file. *)

PROCEDURE GetLine(rd: Rd.T := NIL): TEXT RAISES {Error};
(* Read a line of text from "rd" and return it. *)

(* A line of text is either zero or more characters terminated by a
   line break, or one or more characters terminated by an end-of-file.
   In the former case, "GetLine" consumes the line break but does not
   include it in the returned value.  A line break is either {\tt
   \char'42\char'134n\char'42} or {\tt
   \char'42\char'134r\char'134n\char'42}. *)

PROCEDURE ReadLine(rd: Rd.T := NIL; position: INTEGER := 0;
                   echo : BOOLEAN := TRUE;
	           wantText : BOOLEAN := TRUE): TEXT RAISES {Error};
(* ReadLine differs from GetLine in that it permits editting
   with BackSpace. ReadLine is appropriate for getting input
   from humans typing on the terminal. Either GetLine or 
   ReadLine is appropriate for getting input from other
   sources. "position" argument should indicate position
   in the line of the first character to be read, it allows correct
   displaying in case of deleted tabulation characters.

   If wantText is TRUE, then nothing is returned until the first
   non nil newline terminated line is entered. Otherwise, nil can
   be returned to indicate a blank line entered.  *)

PROCEDURE GetChar(rd: Rd.T := NIL): CHAR RAISES {Error};
(* Read the next character from "rd" and return it. *)

PROCEDURE GetInt(rd: Rd.T := NIL): INTEGER RAISES {Error};
(* Read a decimal numeral from "rd" using "Lex.Int" and return its
   value. *)

(* PROCEDURE GetReal(rd: Rd.T := NIL): REAL RAISES {Error}; *)
(* Read a real number from "rd" using "Lex.Real" and return its value.
   *)
  
(* OpenRead is hijacked from the FileIO extension *)
PROCEDURE OpenRead(f: TEXT): Rd.T;
(* Open the file name "f" for reading and return a reader on its
   contents. If the file doesn't exist or is not readable, return
   "NIL". *)

(*
PROCEDURE OpenWrite(f: TEXT): Wr.T;
(* Open the file named "f" for writing and return a writer on its
   contents.  If the file does not exist it will be created.  If the
   process does not have the authority to modify or create the file,
   return "NIL". *) 
*)
END IO.

