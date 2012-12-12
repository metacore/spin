(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
   CharDevWr.i3

   Implement a writer using the methods in CharDevice.T.
 *)
MODULE CharDevWr;

IMPORT Wr, WrClass, Error, CharDevice;
IMPORT IO, Fmt, Atom, AtomList;

REVEAL
  T = Public BRANDED OBJECT
	  dev:CharDevice.T;
	  OVERRIDES
	    seek      := Seek;
	    putString := PutString;
	    flush     := Flush;
	    init      := ObjInit;
	    close     := Close;
	  END;

PROCEDURE ObjInit (wr: T; d:CharDevice.T): T =
  BEGIN
    WrClass.Lock(wr);
    TRY
      wr.dev := d;
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

PROCEDURE Close(wr: T) RAISES {} =
  BEGIN
    wr.dev := NIL;
    wr.buff := NIL;
  END Close;

PROCEDURE Seek(wr: T; n: CARDINAL) RAISES {} =
  BEGIN
    (* Only the PutChar procedure uses the buffer and it will
       increment wr.cur by 1. We adjust the lo and hi values
       to make the wr.cur offset always equal wr.lo so that 
       PutChar always write to wr.buff[0]. *)
    wr.lo := n;
    wr.hi := n + 1;
  END Seek;

PROCEDURE WriteException(msg: TEXT) RAISES {Wr.Failure} =
  VAR
    a : AtomList.T := NIL;
  BEGIN
    IO.Put(msg & "\n");
    RAISE Wr.Failure(AtomList.Cons(Atom.FromText(msg),a));
  END WriteException;

PROCEDURE PutString(wr: T; READONLY a: ARRAY OF CHAR) RAISES {Wr.Failure} =
  VAR
    bytes :INTEGER;
  BEGIN
    (* Send straight to output *)
    (* Because the ConsoleWr.T is unbuffered, there won't be any unflushed
       characters in the buffer, so we can ignore it. *)
    TRY
      bytes := wr.dev.write(a);
      IF bytes # NUMBER(a) THEN
        WriteException("CharDevWr.PutString: Short write of " &
		  Fmt.Int(bytes) & " bytes.  Requested " &
		  Fmt.Int(NUMBER(a)) & " bytes");
      END;
    EXCEPT
    | Error.E(e) =>
      WriteException("CharDevWr.Seek: "& e.message());
    END;
  END PutString;

PROCEDURE Flush(wr: T) RAISES {Wr.Failure} =
  VAR
    bytes :INTEGER;
  BEGIN
    TRY
      (* Send straight to output *)
      IF wr.cur > wr.lo THEN
	bytes := wr.dev.write(SUBARRAY(wr.buff^,0,1));
        IF bytes # 1 THEN
          WriteException("CharDevWr.Flush: Short write of " &
		  Fmt.Int(bytes) & " bytes");
	END;
        wr.lo := wr.cur;
      END;
    EXCEPT
    | Error.E(e) =>
      WriteException("CharDevWr.Seek: "& e.message());
    END;
  END Flush;

BEGIN
END CharDevWr.
