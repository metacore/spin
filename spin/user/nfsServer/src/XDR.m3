(*
 * HISTORY
 * 22-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Changed GetBytes to traverse through an mbuf chain.
 *	
 *	Changed GetText to allocate a Text.T and use GetBytes to copy
 *	into that buffer directly.
 *
 *)


MODULE XDR;
IMPORT Word, Text, TextF; (* M3 LIBS *)
IMPORT Ctypes, IO;
IMPORT Mbuf;

(*
 * Sources.
 *)
PROCEDURE GetWord32 (VAR m: Mbuf.T; VAR pos: CARDINAL): Ctypes.unsigned_int
  RAISES {Failed} =
  VAR 
    v: Ctypes.unsigned_int;
    size: INTEGER;
    buf : UNTRACED REF ARRAY OF CHAR;
  BEGIN
    
    LOOP
      buf := Mbuf.Array(m);
      size := NUMBER(buf^)-pos;
      IF size >= BYTESIZE(Ctypes.unsigned_int) THEN 
        EXIT; 
      ELSIF size = 0 THEN
        pos := 0;
        m := m.mh_hdr.mh_next;
        IF m = NIL THEN
          IO.Put("xdr failure 1");
          RAISE Failed(Failure.SourceNoSpace);
        END;
      ELSE
          IO.Put("xdr failure 2");
        RAISE Failed(Failure.SourceNoSpace);
      END;
    END;

    (* Charlie: This code actually measured slightly faster than VIEWing
       the array as an unsigned_int. You can substitute + for Word.Or
       here and get the same quality of code. *)
    WITH word_buf = VIEW(SUBARRAY(buf^, pos, 4), ARRAY [0..3] OF CHAR) DO
      v := Word.Or(Word.Or(Word.LeftShift(ORD(word_buf[0]), 24), 
                           Word.LeftShift(ORD(word_buf[1]), 16)), 
                   Word.Or(Word.LeftShift(ORD(word_buf[2]), 8),
                           ORD(word_buf[3])));
    END;

    INC(pos, BYTESIZE(Ctypes.unsigned_int));
    RETURN v;
  END GetWord32;

(* Get n bytes and realign. *)
PROCEDURE GetBytes (VAR m: Mbuf.T; VAR pos: CARDINAL; VAR v: ARRAY OF CHAR)
  RAISES {Failed} =
  VAR
    size: INTEGER;
    buf : UNTRACED REF ARRAY OF CHAR;
    got,wanted,bytes: CARDINAL;
  BEGIN
    buf := Mbuf.Array(m);
    got := 0;
    wanted := NUMBER(v);

    WHILE wanted > 0 DO 
      (* trailingsize of mbuf starting at pos *)
      size := NUMBER(buf^)-pos;

      (* number of bytes we can copy from this mbuf *)
      (* Charlie: Could we change bytes to an INTEGER to omit a check here? *)
      bytes := MIN(wanted,size);

      IF bytes # 0 THEN
        SUBARRAY(v,got,bytes) := SUBARRAY(buf^,pos,bytes);
        INC(pos,bytes);
        DEC(wanted,bytes); (* Charlie: Check for a negative number here. *)
      ELSE

        (* size must be equal to zero if we get here, indicating that
           we are at the end of the mbuf. *)
        <*ASSERT size = 0*>

        WHILE size = 0 DO
          pos := 0;
          m := m.mh_hdr.mh_next;
          IF m = NIL THEN
            IO.Put("xdr failure 3");
            RAISE Failed(Failure.SourceNoSpace);
          END;
          buf := Mbuf.Array(m);
          (* trailingsize of mbuf starting at pos *)
          size := NUMBER(buf^)-pos;
        END;
      END;        
    END;
  END GetBytes;

PROCEDURE GetTextAsChars (VAR m: Mbuf.T; VAR pos: CARDINAL; VAR len: CARDINAL; VAR v: ARRAY OF CHAR)
  RAISES {Failed} =
  VAR
    alignBytes : CARDINAL;  (* number of alignment bytes *)
  BEGIN
    len := GetWord32(m,pos);
    IF len > NUMBER(v) THEN
      IO.Put("xdr failure in GetTextAsChars\n");
      RAISE Failed(Failure.SourceNoSpace);
    END;
    GetBytes(m,pos,SUBARRAY(v,0,len));
    alignBytes := 3 - (len + 3) MOD 4; (* Charlie: Test for negative here. *)
    INC(pos,alignBytes);
  END GetTextAsChars;

PROCEDURE GetText (VAR m: Mbuf.T; VAR pos: CARDINAL): TEXT 
  RAISES {Failed} =
  VAR
    len        : INTEGER;        (* length of the string *)
    t          : TEXT    := NIL; (* the result *)
    alignBytes : INTEGER;        (* number of alignment bytes *)
  BEGIN
    len := GetWord32(m,pos);
    IF len > 0 THEN
      alignBytes := 3 - (len + 3) MOD 4;
      IF len > 1024 THEN
        IO.PutError("WARNING: XDR.GetText getting large text buffer of size ");
        IO.PutInt(len);
        IO.Put("\n");
      END;
      t := NEW(Text.T,len+1);
      GetBytes(m,pos,SUBARRAY(t^,0,len));
      t[len] := '\000';
      INC(pos,alignBytes);
    ELSE
      t := ""; 
    END;
    RETURN t;
  END GetText;

PROCEDURE GetBoolean(VAR m: Mbuf.T; VAR pos: CARDINAL): BOOLEAN
  RAISES {Failed} = 
  BEGIN
    RETURN GetWord32(m,pos) # 0;
  END GetBoolean;

(*
 * Sinks.
 *)

PROCEDURE PutWord32 (VAR m: Mbuf.T; VAR pos: CARDINAL; v: INTEGER) : CARDINAL
  RAISES {Failed} <*NOWARN*>=
  BEGIN
    WITH buf = Mbuf.Array(m) DO 
      IF (NUMBER(buf^)-pos) >= 4 THEN
        
        (* Charlie: This code measured the fastest of the PutWord
           variations that I tested. It assumes that the position in
           the array is 32-bit aligned. *)
        VIEW(SUBARRAY(buf^, pos, 4), Ctypes.unsigned_int) := 
            Word.Or(Word.Or(Word.RightShift(Word.And(v, 16_ff000000), 24),
                            Word.RightShift(Word.And(v, 16_ff0000), 8)),
                    Word.Or(Word.LeftShift(Word.And(v, 16_ff00), 8),
                            Word.LeftShift(Word.And(v, 16_ff), 24)));

        INC(pos, 4);
        RETURN 4;
      END;
    END;
    RAISE Failed(Failure.SinkNoSpace);
  END PutWord32;

PROCEDURE PutBytes (VAR m: Mbuf.T; VAR pos: CARDINAL; READONLY v: ARRAY OF CHAR): CARDINAL
  RAISES {Failed} = 
  VAR len : CARDINAL := NUMBER(v);
  BEGIN
    WITH buf = Mbuf.Array(m) DO 
      IF NUMBER(buf^)-pos >= len THEN
        SUBARRAY(buf^, pos, len) := v;
        INC(pos, len);
        RETURN len;
      END;
    END;
    RAISE Failed(Failure.SinkNoSpace);
  END PutBytes;

PROCEDURE PutCharsAsText (VAR m: Mbuf.T; VAR pos: CARDINAL; READONLY v: ARRAY OF CHAR): CARDINAL
  RAISES {Failed} =
  VAR
    len        : CARDINAL := NUMBER(v);
    alignBytes : CARDINAL;
    size : CARDINAL;
  BEGIN
    size := PutWord32(m,pos,len);
    alignBytes := 3 - (len + 3) MOD 4; (* Charlie: CARDINAL causes a check here *)
    WITH buf = Mbuf.Array(m) DO 
      IF NUMBER(buf^)-pos >= len+alignBytes THEN
        SUBARRAY(buf^,pos,len) := v;
        (* increment and realign on word boundry *)
        INC(pos, len+alignBytes);
        RETURN size+len+alignBytes;
      END;
    END;
    IO.Put("PutCharsAsText no space in mbuf.\n");
    RAISE Failed(Failure.SinkNoSpace);
  END PutCharsAsText;

PROCEDURE PutText (VAR m: Mbuf.T; VAR pos: CARDINAL; v: TEXT): CARDINAL
  RAISES {Failed} =
  VAR
    len : CARDINAL := Text.Length(v);
  BEGIN
    RETURN PutCharsAsText(m,pos,SUBARRAY(v^,0,len));
  END PutText;

PROCEDURE PutBoolean(VAR m: Mbuf.T; VAR pos: CARDINAL; v: BOOLEAN) : CARDINAL
  RAISES {Failed} = 
  VAR 
  BEGIN
    IF v THEN 
      RETURN PutWord32(m,pos,1); 
    ELSE
      RETURN PutWord32(m,pos,0); 
    END;
  END PutBoolean;


BEGIN
END XDR.
