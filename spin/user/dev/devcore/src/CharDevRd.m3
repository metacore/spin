(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   CharDevRd.i3

   Implement a reader using the methods in CharDevice.T.
 *)
MODULE CharDevRd;

IMPORT Rd, RdClass, Error, CharDevice, BSDtty;
IMPORT IO, Atom, AtomList;

REVEAL
  T = Public BRANDED OBJECT
	  dev:CharDevice.T;
	  OVERRIDES
	    seek   := Seek;
	    getSub := GetSub;
	    length := Length;
	    init   := ObjInit;
	    close     := Close;
	  END;


PROCEDURE ObjInit (rd: T; d:CharDevice.T): T =
  BEGIN
    rd.dev := d;
    rd.buff := NEW(REF ARRAY OF CHAR, 1);
    rd.st := 0;
    rd.cur := 0;
    rd.lo := 0;
    rd.hi := 0;
    rd.closed := FALSE;
    rd.seekable := FALSE;
    rd.intermittent := TRUE;
    RETURN rd;
  END ObjInit;

PROCEDURE Close(rd: T) RAISES {} =
  BEGIN
    rd.dev := NIL;
    rd.buff := NIL;
  END Close;


PROCEDURE ReadException(msg: TEXT) RAISES {Rd.Failure} =
  VAR
    a : AtomList.T := NIL;
  BEGIN
    IO.Put(msg & "\n");
    RAISE Rd.Failure(AtomList.Cons(Atom.FromText(msg),a));
  END ReadException;

PROCEDURE Seek(rd: T; n: CARDINAL; 
               dontBlock: BOOLEAN): RdClass.SeekResult RAISES {Rd.Failure} =
  VAR
    tty: BSDtty.T;
    len: CARDINAL;
  BEGIN
    TRY
      IF dontBlock THEN
        (* Rd.CharsReady is asking if there are any characters to be read *)
	tty := NARROW(rd.dev, BSDtty.T);
        IF tty.charsAvail() > 0 THEN
          RETURN RdClass.SeekResult.Ready;
        ELSE
          RETURN RdClass.SeekResult.Eof;
        END;
      ELSE
        len := rd.dev.read(SUBARRAY(rd.buff^,0,NUMBER(rd.buff^)));
        rd.lo := n;
        rd.hi := n + len;
      END;
    EXCEPT
    | Error.E(e) =>
      ReadException("CharDevRd.Seek: "& e.message());
    END;
    RETURN RdClass.SeekResult.Ready;
  END Seek;

PROCEDURE GetSub(rd: T; VAR a: ARRAY OF CHAR): CARDINAL RAISES {Rd.Failure} =
  BEGIN
    TRY
      IF rd.cur < rd.hi THEN
        (* One character was put back in the buffer by UnGetChar *)
        a[0] := rd.buff[0];
        INC(rd.cur);
        RETURN 1 + rd.dev.read( SUBARRAY(a, 1, NUMBER(a)-1));
      ELSE
        RETURN rd.dev.read(a);
      END;
    EXCEPT
    | Error.E(e) =>
      ReadException("CharDevRd.Seek: "& e.message());
    END;

    (* NOT REACHED -- this is to quiet compiler *)
    RETURN 0;
  END GetSub; 

PROCEDURE Length(<*UNUSED*> rd: T): INTEGER =
  BEGIN
    RETURN -1;
  END Length;

BEGIN
END CharDevRd.
