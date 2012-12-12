(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 02-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	File.read and write now takes var array of char instead of ref
 *	array of char
 *	
 * 02-Jul-96  Yasushi Saito (yasushi) at the University of Washington
 *	File.read and write now takes var array of char instead of ref
 *	array of char
 *	
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new file system interface.
 * 15-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)
UNSAFE MODULE FileRd;

IMPORT Rd, RdClass, FileSystem, File, FileStat, Error;
IMPORT Atom, AtomList;

REVEAL
  T = Public BRANDED OBJECT
    fp : File.T;
  OVERRIDES
    seek   := Seek;
    getSub := GetSub;
    length := Length;
    close  := Close;
    init   := Init;
  END;

CONST
  BuffSize = 512;

PROCEDURE Init (rd: T; fp: File.T): T =
  BEGIN
    rd.fp := fp;
    rd.buff := NEW(REF ARRAY OF CHAR, BuffSize);
    rd.st := 0;
    rd.cur := 0;
    rd.lo := 0;
    rd.hi := 0;
    rd.closed := FALSE;
    rd.seekable := TRUE;
    rd.intermittent := FALSE;
    RETURN rd;
  END Init;

PROCEDURE Open(p: TEXT): T RAISES {Rd.Failure} = 
  VAR
    entry: REFANY;
  BEGIN 
    (* Try to open as a directly mounted file *)
    TRY 
      entry := FileSystem.Open(p);
    EXCEPT
    | Error.E(e) =>
      RAISE Rd.Failure(ExplainError(e));
    END;
    TYPECASE entry OF 
    | NULL =>
      RAISE Rd.Failure(AtomList.List1(Atom.FromText(p & " not a file")));
    | File.T(fp) =>
      EVAL fp.open(0);
      RETURN NEW(T).init(fp);
    ELSE
      RAISE Rd.Failure(AtomList.List1(Atom.FromText(p & " not a file")));
    END;
  END Open;

PROCEDURE Seek(rd: T; offset: CARDINAL; 
               dontBlock: BOOLEAN): RdClass.SeekResult RAISES {Rd.Failure} =
  VAR
    nread: CARDINAL := BuffSize;
  BEGIN
    IF dontBlock THEN
      (* This is a trick. Only CharsReady calls with dontBlock set to
         TRUE. Whether we return Eof or not, CharsReady will always
         return 1 which is the number of characters in the buffer. *)
      RETURN RdClass.SeekResult.Eof;
    ELSE
      (* Read the next character *)
      TRY
	nread := rd.fp.read(SUBARRAY(rd.buff^, 0, nread), offset);
      EXCEPT
	| Error.E(e) =>
	  RAISE Rd.Failure(ExplainError(e));
      END;
	
      IF nread = 0 THEN
	RETURN RdClass.SeekResult.Eof;
      END;

      rd.lo := offset;
      rd.hi := offset + nread;
      RETURN RdClass.SeekResult.Ready;
    END;
  END Seek;

PROCEDURE GetSub(rd: T; VAR a: ARRAY OF CHAR): CARDINAL RAISES {Rd.Failure} =
  VAR
    nread: CARDINAL := NUMBER(a);
  BEGIN
    IF rd.cur < rd.hi THEN
      (* One character was put back in the buffer by UnGetChar *)
      a[0] := rd.buff[0];
      INC(rd.cur);
      DEC(nread);
      TRY 
	nread := rd.fp.read(SUBARRAY(a, 1, nread), rd.cur);
      EXCEPT
	| Error.E(e) =>
	  RAISE Rd.Failure(ExplainError(e));
      END;
    ELSE
      TRY 
	nread := rd.fp.read(a, rd.cur);
      EXCEPT
	| Error.E(e) =>
	  RAISE Rd.Failure(ExplainError(e));
      END;
    END;
    RETURN nread;
  END GetSub; 

PROCEDURE Close(rd: T) RAISES {Rd.Failure} =
BEGIN
  TRY 
    rd.fp.close();
  EXCEPT
    | Error.E(e) =>
      RAISE Rd.Failure(ExplainError(e));
  END;
END Close;

PROCEDURE Length(rd: T): INTEGER =
VAR stat : FileStat.T;
BEGIN
  TRY 
    rd.fp.stat(stat);
  EXCEPT 
  | Error.E =>
    (* XXX should print an error message here. (mef) *)
    stat.size := 0;
  END;
  RETURN stat.size;
END Length;

PROCEDURE ExplainError(e : Error.T): AtomList.T =
  BEGIN
    RETURN AtomList.List1(Atom.FromText(e.message()));
  END ExplainError;

BEGIN
END FileRd. 
