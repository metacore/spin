INTERFACE XDR;
IMPORT Ctypes, Mbuf;

EXCEPTION Failed(Failure);
TYPE Failure = {
  SourceNoSpace, SinkNoSpace, 
  SourceBadPos,  SinkBadPos,
  SourceBadLen,  SinkBadLen};

CONST 
  Error = ARRAY Failure OF TEXT {
  "Not enough data in Source",
  "Not enough space in Sink",
  "Bad pos for Source",
  "Bad pos for Sink",
  "Bad Len for Source",
  "Bad len for Sink"};

PROCEDURE GetWord32 (VAR m: Mbuf.T; VAR pos: CARDINAL): Ctypes.unsigned_int
  RAISES {Failed};

(* Get n bytes and realign. *)
PROCEDURE GetBytes (VAR m: Mbuf.T; VAR pos: CARDINAL; VAR v: ARRAY OF CHAR)
  RAISES {Failed};

PROCEDURE GetTextAsChars (VAR m: Mbuf.T; VAR pos: CARDINAL; VAR len: CARDINAL; VAR v: ARRAY OF CHAR)
  RAISES {Failed};

PROCEDURE GetText (VAR m: Mbuf.T; VAR pos: CARDINAL): TEXT 
  RAISES {Failed};

PROCEDURE GetBoolean(VAR m: Mbuf.T; VAR pos: CARDINAL): BOOLEAN
  RAISES {Failed};

PROCEDURE PutWord32 (VAR m: Mbuf.T; VAR pos: CARDINAL; v: INTEGER): CARDINAL
  RAISES {Failed};

PROCEDURE PutBytes (VAR m: Mbuf.T; VAR pos: CARDINAL; READONLY v: ARRAY OF CHAR): CARDINAL
  RAISES {Failed};

PROCEDURE PutCharsAsText (VAR m: Mbuf.T; VAR pos: CARDINAL; READONLY v: ARRAY OF CHAR): CARDINAL
  RAISES {Failed};

PROCEDURE PutText (VAR m: Mbuf.T; VAR pos: CARDINAL; v: TEXT): CARDINAL
  RAISES {Failed};

PROCEDURE PutBoolean(VAR m: Mbuf.T; VAR pos: CARDINAL; v: BOOLEAN): CARDINAL
  RAISES {Failed};

END XDR.
