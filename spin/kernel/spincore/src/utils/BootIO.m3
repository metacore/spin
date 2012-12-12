(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
  	BootIO.m3
  
  	The Boot reader is null.  No console reading is supported at boottime.
  
  	The Boot writer is implemented with Sal.PutChar(char)
  
  	The reader and writer are created on the first calls to Reader
  	and Wr respectively.
 *)
(*
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Sal and CPU interfaces
 *
 * 24-Feb-97  Emin Gun Sirer (egs) at the University of Washington
 *	Cleaned up warnings.
 *)

MODULE BootIO;

IMPORT WrClass, Sal, Thread;
IMPORT Wr AS WrBase;

REVEAL
  Wr = WrBase.T BRANDED OBJECT
	  OVERRIDES
	    seek      := Seek;
	    putString := PutString;
	    flush     := Flush;
	  END;

VAR
	defaultWr:Wr;
	defaultReader:Rr;
        redirectedWr:WrBase.T;

PROCEDURE ObjInit (wr: Wr): Wr =
  BEGIN
    WrClass.Lock(wr);
    TRY
      wr.buff := NEW(REF ARRAY OF CHAR, 1);
      wr.st := 0;
      wr.cur := 0;
      wr.lo := 0;
      wr.hi := 1;
      wr.closed := FALSE;
      wr.seekable := FALSE;
      wr.buffered := FALSE;
    FINALLY
      WrClass.Unlock(wr);
    END;
    RETURN wr;
  END ObjInit;

PROCEDURE Seek(wr: Wr; n: CARDINAL) RAISES {} =
  BEGIN
    (* Only the PutChar procedure uses the buffer and it will
       increment wr.cur by 1. We adjust the lo and hi values
       to make the wr.cur offset always equal wr.lo so that 
       PutChar always write to wr.buff[0]. *)
    wr.lo := n;
    wr.hi := n + 1;
  END Seek;

PROCEDURE PutString(<*UNUSED*> wr: Wr; READONLY a: ARRAY OF CHAR) RAISES {} =
  BEGIN
    (* Send straight to output *)
    (* Because the ConsoleWr.T is unbuffered, there won't be any unflushed
       characters in the buffer, so we can ignore it. *)
    TRY
      IF redirectedWr # NIL THEN
        WrBase.PutString(redirectedWr, a);
      ELSE
        FOR i := FIRST(a) TO LAST(a) DO
          Sal.PutChar(a[i]);
        END;
      END;
    EXCEPT
      Thread.Alerted, WrBase.Failure => RETURN;
    END;
  END PutString;

PROCEDURE Flush(wr: Wr) RAISES {} =
  BEGIN
    TRY
      (* Send straight to output *)
      IF wr.cur > wr.lo THEN
        IF redirectedWr # NIL THEN
          WrBase.PutChar(redirectedWr, wr.buff[0]);
        ELSE
          Sal.PutChar(wr.buff[0]);
        END;
        wr.lo := wr.cur;
      END;
    EXCEPT
      Thread.Alerted, WrBase.Failure => RETURN;
    END;
  END Flush;

PROCEDURE Writer(): Wr =
  BEGIN
    IF defaultWr # NIL THEN RETURN defaultWr END;

    defaultWr := NEW(Wr);
    RETURN ObjInit(defaultWr);
  END Writer;

PROCEDURE Reader(): Rr =
  BEGIN
    IF defaultReader # NIL THEN RETURN defaultReader END;

    defaultReader := NEW(Rr);
    RETURN defaultReader.init();
  END Reader;

PROCEDURE Redirect(wr:WrBase.T) = 
  BEGIN
    redirectedWr := wr;
  END Redirect; 

BEGIN
(* PROCs in this module are used before the runtime can run this main body. *)
END BootIO. 
