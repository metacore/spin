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
 *)
(*
 * HISTORY
 * 19-Mar-97  Przemek Pardyak (pardy) at the University of Washington
 *	Locked the reader in ReadLine to avoid problems caused by
 *      multiple shells.
 *	
 * 07-Aug-96  Marc Fiuczynski (mef) at the University of Washington
 *	Removed use of SALtftp in IO.OpenRead.  It was very confusing
 *	when this happened without explicitly mounting the
 *	tftpfilesystem.
 *
 * 23-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Enabled wantText for ReadLine.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Fixed GetWr to use BootIO writer when strand isn't a Thread.T 
 *	(from becker)
 *
 * 25-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Added echo parameter to ReadLine which controls whether
 *	 characters are printed on the standard writer. We set echo
 *	to false when telnetting in to a shell.
 *
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Got rid of concatenation on the PutError path.
 *
 * 04-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed KThread to Thread.
 *
 * 04-Jan-95  Przemek Pardyak (pardy) at the University of Washington
 *	Made deleting of tabulation move the cursor correctly and
 *	added control-c to cancel the whole line in ReadLine.
 *
 * 22-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Make faithful to IO.i3 and raise Error in GetLine on Rd failure.
 *
 * 09-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Copy the SRC standard library IO interface.
 *)

(* "IO.m3" implements the default standard output and input for M3
   kernel code, both the SPIN core and extensions.

   In the DEC SRC M3 distribution, IO is implemented in libm3

   See Boot.m3 in the start library for a comments on output during
   the boot process.
 *)


MODULE IO;

IMPORT Rd, RdClass, UnsafeRd, Wr, Fmt, Lex, Text;
IMPORT Strand, ThreadRep, Thread, BootIO;

(* the following line is necessary to satisfy teh constrain that revelations
   must be totally ordered (see libm3/src/rd/Common/UnsafeRd.i3 *)
REVEAL RdClass.Private <: MUTEX;

<* FATAL Wr.Failure, Thread.Alerted *>

PROCEDURE GetRd (VAR rd: Rd.T) =
  VAR s: Strand.T;
  BEGIN
    IF rd # NIL THEN RETURN END;

    (* The common case is to use the thread's standard reader *)
    s := Strand.GetCurrent();
    IF s # NIL AND ISTYPE(s, Thread.T) THEN
      rd := NARROW(s, Thread.T).reader;
      IF rd # NIL THEN RETURN END;
      (* Put("thread.reader NIL. defaulting to BootIO.reader\n",BootIO.Writer()); *)
    END;

    (* Use the default otherwise.  Occurs when booting or in a trap handler. *)
    rd := BootIO.Reader();
  END GetRd;

PROCEDURE GetWr (VAR wr: Wr.T) =
  VAR s: Strand.T;
  BEGIN
    IF wr # NIL THEN RETURN END;

    (* The common case is to use the thread's standard writer *)
    s := Strand.GetCurrent();
    IF s # NIL AND ISTYPE(s, Thread.T) THEN
      wr := NARROW(s, Thread.T).writer;
      IF wr # NIL THEN RETURN END;
      (* Put("thread.writer NIL. defaulting to BootIO.writer\n",BootIO.Writer()); *)
    END;

    (* Use the default otherwise.  Occurs when booting or in a trap handler. *)
    wr := BootIO.Writer();
  END GetWr;


PROCEDURE Put(txt: TEXT; wr: Wr.T := NIL)=
  BEGIN
    GetWr(wr);
    Wr.PutText(wr, txt); Wr.Flush(wr); 
  END Put;

PROCEDURE Putx (n: INTEGER; width := 0; wr: Wr.T := NIL) =
  BEGIN
    GetWr(wr);
    Wr.PutText(wr, Fmt.Pad(Fmt.Unsigned(n), width)); Wr.Flush(wr); 
  END Putx;

PROCEDURE PutInt (n: INTEGER; width := 0; wr: Wr.T := NIL) =
  BEGIN
    GetWr(wr);
    Wr.PutText(wr, Fmt.Pad(Fmt.Int(n),width)); Wr.Flush(wr); 
  END PutInt;

(* PROCEDURE PutReal(r: REAL; wr: Wr.T := NIL)=
  VAR
    s : Strand.T;
  BEGIN
    GetWr(wr);
    IF wr # NIL THEN Wr.PutText(wr, Fmt.Real(r)); Wr.Flush(wr); END;  
  END PutReal; 
*)

PROCEDURE PutError(txt: TEXT; wr: Wr.T := NIL)=
  BEGIN
    GetWr(wr);
    Wr.PutText(wr, "ERROR >>> ");
    Wr.PutText(wr, txt);
    Wr.Flush(wr);
  END PutError;

PROCEDURE EOF (rd: Rd.T := NIL): BOOLEAN =
  VAR res: BOOLEAN;
  BEGIN
    GetRd(rd);
    TRY res := Rd.EOF(rd); EXCEPT ELSE RETURN TRUE; END;
    RETURN res;
  END EOF;


PROCEDURE GetLine (rd: Rd.T := NIL): TEXT RAISES {Error} =
  BEGIN
    GetRd(rd);
    TRY RETURN Rd.GetLine(rd);
    EXCEPT
    | Rd.EndOfFile, Rd.Failure => RAISE Error;
    END;
  END GetLine;

PROCEDURE ReadLine (rd      : Rd.T    := NIL;
                    position: INTEGER := 0;
                    echo    : BOOLEAN := TRUE;
                    wantText: BOOLEAN := TRUE  ): TEXT RAISES {Error} =
  VAR
    cmd   : ARRAY [0 .. 1023] OF CHAR;
    pos   : ARRAY [0 .. 1023] OF INTEGER;
    nchars: INTEGER                      := 0;
    ndel  : INTEGER;
    c     : CHAR;
  BEGIN
    GetRd(rd);

(*
  Version that assumes stream is cooked and comes a line at a time.
    TRY
      nchars := Rd.GetSub(rd, cmd);
      IF nchars # 0 AND cmd[FIRST(cmd)+nchars-1] = '\n' THEN DEC(nchars) END;
      RETURN Text.Sub(Text.FromChars(cmd), FIRST(cmd), nchars);
    EXCEPT
      Rd.Failure => RAISE Error;
    END;
*)

(*
  This version reads the stream one char at a time and
  does a minimal amount of  cooking on the input.
 *)
    TRY
      TRY
        RdClass.Lock(rd);
        LOOP
          c := UnsafeRd.FastGetChar(rd);     (* can RAISE Error *)
          IF c > NUL THEN
            pos[nchars] := position;
            IF c = LF THEN
              (* A linefeed only matters after some other characters. *)
              IF NOT wantText OR nchars > 0 THEN
                EXIT;
              END;
            ELSIF c = RET THEN
              (* A return indicates the end of a line no matter where it
                 occurs *)
              (* TEMP TEST -- XX --- DO NOT ECHO RETURN CHARACTERS. *)
              (* EAT IT *)
            ELSIF c = DEL OR c = BS THEN
              IF nchars > 0 THEN
                DEC(nchars);
                ndel := position - pos[nchars];
                FOR i := 1 TO ndel DO DeleteChar(); DEC(position); END;
              END;
            ELSIF c = CTRL_C THEN
              nchars := 0;
              IF echo THEN Put(Fmt.Char(LF)); END;
            EXIT;
          ELSE
            IF echo THEN Put(Fmt.Char(c)); END;
            cmd[nchars] := c;
            INC(nchars);
            IF c = TAB THEN
              INC(position, 8 - (position MOD 8));
            ELSE
              INC(position);
            END;
          END;
        END;
        END;
      EXCEPT
        Rd.EndOfFile, Rd.Failure => RAISE Error;
      END;
    FINALLY
      RdClass.Unlock(rd);
    END;
    
    IF nchars = 0 AND NOT wantText THEN RETURN NIL; END;

    cmd[nchars] := NUL;
    RETURN Text.FromChars(SUBARRAY(cmd, 0, nchars));
  END ReadLine;

PROCEDURE DeleteChar () =
  BEGIN
    Put(Fmt.Char(BS));
    Put(Fmt.Char(' '));
    Put(Fmt.Char(BS));
  END DeleteChar;

PROCEDURE GetChar (rd: Rd.T := NIL): CHAR RAISES {Error} =
  BEGIN
    GetRd(rd);
    TRY
      RETURN Rd.GetChar(rd);
    EXCEPT
    | Rd.EndOfFile, Rd.Failure => RAISE Error;
    END;
  END GetChar;

PROCEDURE GetInt(rd: Rd.T := NIL): INTEGER RAISES {Error}=
  BEGIN
    GetRd(rd);
    TRY RETURN Lex.Int(rd);
    EXCEPT
    | Lex.Error, Rd.Failure => RAISE Error;
    END;
  END GetInt;

(* PROCEDURE GetReal(rd: Rd.T := NIL): REAL RAISES {Error}=
    GetRd(rd);
    TRY RETURN Lex.Real(rd);
    EXCEPT
    | Lex.Error => RAISE Error;
    END;
  END GetReal; *)

(* OpenRead and OpenWrite are just dummies and not support by SPIN *)
PROCEDURE OpenRead(<*UNUSED*> f: TEXT): Rd.T=
  BEGIN
    RETURN NIL;
  END OpenRead;

(*
PROCEDURE OpenWrite(<*UNUSED*>f: TEXT): Wr.T=
  BEGIN
    RETURN NIL;
  END OpenWrite;
*)
BEGIN
END IO.
