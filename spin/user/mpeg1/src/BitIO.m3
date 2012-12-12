(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

UNSAFE MODULE BitIO;

IMPORT FileSystem, File, NameServer, Directory, Word, IO, Ctypes;

TYPE
  Byte = BITS 8 FOR [0..255];
  Buffer = ARRAY [0..65535] OF Ctypes.unsigned_int;
  ByteBuffer = ARRAY [0..BYTESIZE(Buffer)-1] OF Byte;
    
REVEAL T = BRANDED REF RECORD
  file:          File.T;
  pos:           INTEGER;
  length:        INTEGER; 
  buffer:        Buffer;
  currentWord:   CARDINAL;
  currentBit:    INTEGER;
  wordsInBuffer: CARDINAL;
  readAheadWord: Ctypes.unsigned_int;
  readAhead:     BOOLEAN;
END;

(* These procedures allow mixed bit and byte IO on a file *)

PROCEDURE OpenRead(name: TEXT): T =
VAR
  root: Directory.TPublic;
  component: NameServer.Name;
  parent: NameServer.TBase;
  entry: REFANY;
  new := NEW(T);
BEGIN
  root := FileSystem.GetRoot();
  (* Need to change this to grab any file "name" along any path rather than mjackson from my directory. *)
  entry := root.lookup(name, component, parent);

  TYPECASE entry OF
  | File.TPublic(file) => (* Find the size of the file. *)
                          CONST BufferSize = 8 * 1024;
                          VAR
                            buf     : REF ARRAY OF CHAR;
                            from    : CARDINAL;
                            bufsize : CARDINAL := BufferSize;
                            size    : CARDINAL := 0;
                          BEGIN
                            file := file.open(0);
                            TRY
                              WHILE bufsize = BufferSize DO
                              file.readRef(size, bufsize, buf, from);
                              size := size + bufsize;
                              END;
                            FINALLY
                              file.close();
                              new.length := LOOPHOLE(size, INTEGER);
                              IO.Put("Size = "); IO.PutInt(size);
                            END;
                          END;
                          
                          (* Now re-open the file for reading. *)
                          new.file := file.open(0);
  ELSE
    (* not a file, do nothing. *)
    IO.Put("Bad file name.\n");
  END;

  new.pos := 0;
  new.readAhead := FALSE;
  new.wordsInBuffer := 0;
  RefillBuffer(new);
  new.currentBit := 32;
  RETURN new;
END OpenRead;

PROCEDURE RefillBuffer(self: T) =
VAR
  bytes : CARDINAL;
  temp, temp2, temp3, temp4: Ctypes.unsigned_int; 
BEGIN
  INC(self.pos, self.wordsInBuffer * BYTESIZE(Ctypes.unsigned_int));
  self.currentWord := 0;

  IF self.readAhead = FALSE THEN
    bytes := self.file.read(LOOPHOLE(self.pos, CARDINAL), LOOPHOLE(self.buffer, ARRAY OF CHAR));
  ELSE
    self.buffer[0] := self.readAheadWord;
    bytes := self.file.read(LOOPHOLE(self.pos, CARDINAL), 
                            SUBARRAY(LOOPHOLE(self.buffer, ARRAY OF CHAR), BYTESIZE(Ctypes.unsigned_int), 
                                     BYTESIZE(self.buffer) - BYTESIZE(Ctypes.unsigned_int)));
    INC(bytes, BYTESIZE(Ctypes.unsigned_int));
    self.readAhead := FALSE;
  END;

  self.wordsInBuffer := bytes DIV BYTESIZE(Ctypes.unsigned_int);

  (* Reorder the bytes to deal with a little endian machine (Pentium and Alpha). *)
  FOR i := 0 TO (self.wordsInBuffer-1) DO
    temp  := Word.RightShift(self.buffer[i], 24);
    temp2 := Word.LeftShift(Word.RightShift(Word.LeftShift(self.buffer[i], 40), 56), 8);
    temp3 := Word.LeftShift(Word.RightShift(Word.LeftShift(self.buffer[i], 48), 56), 16);
    temp4 := Word.RightShift(Word.LeftShift(self.buffer[i], 56), 32);
    self.buffer[i] := Word.Or(Word.Or(Word.Or(temp, temp2), temp3), temp4);
  END;  
END RefillBuffer;

<*INLINE*>
PROCEDURE GetBits(self: T; length: CARDINAL): Ctypes.unsigned_int RAISES {EOF} =
BEGIN
  IF self.currentBit - length > 0 THEN
    DEC(self.currentBit, length);
    RETURN Word.Extract(self.buffer[self.currentWord], self.currentBit, length)
  ELSE
    RETURN GetMoreBits(self, length);
  END;
END GetBits;

PROCEDURE GetMoreBits(self: T; length: CARDINAL): Ctypes.unsigned_int RAISES {EOF} =
VAR
  bits : Ctypes.unsigned_int;
BEGIN
  bits := Word.Extract(self.buffer[self.currentWord], 0, self.currentBit);
  bits := Word.LeftShift(bits, length - self.currentBit);
  INC(self.currentWord);
  IF self.currentWord >= self.wordsInBuffer THEN
    RefillBuffer(self);
  END;
  self.currentBit := self.currentBit - length + 32;
  IF self.currentBit < 32 THEN
    IF self.wordsInBuffer = 0 THEN RAISE EOF; END;
    bits := Word.Or(bits, Word.RightShift(self.buffer[self.currentWord], self.currentBit));
  END;
  RETURN bits;
END GetMoreBits;

<*INLINE*>
PROCEDURE ShowBits(self: T; length: CARDINAL): Ctypes.unsigned_int =
BEGIN
  IF self.currentBit - length >= 0 THEN
    RETURN Word.Extract(self.buffer[self.currentWord], self.currentBit-length, length);
  ELSE
    RETURN ShowMoreBits(self, length);
  END;
END ShowBits;

PROCEDURE ShowMoreBits(self: T; length: CARDINAL): Ctypes.unsigned_int =
VAR
  bits : Ctypes.unsigned_int;
  nextWord : Ctypes.unsigned_int;
  bytes : CARDINAL;
BEGIN
  bits := Word.Extract(self.buffer[self.currentWord], 0, self.currentBit);
  bits := Word.LeftShift(bits, length - self.currentBit);
  IF self.currentWord+1 >= self.wordsInBuffer THEN
    IF self.readAhead = FALSE THEN
      self.readAheadWord := 0;
      bytes := self.file.read(LOOPHOLE(self.pos, CARDINAL), LOOPHOLE(self.readAheadWord, 
                              ARRAY [0..BYTESIZE(Ctypes.unsigned_int) - 1] OF CHAR)); 
      INC(self.pos, bytes);
      IF bytes > 0 THEN
        self.readAhead := TRUE;
      END;
    END;
    nextWord := self.readAheadWord;
  ELSE
    nextWord := self.buffer[self.currentWord + 1];
  END;
  bits := Word.Or(bits, Word.RightShift(nextWord, self.currentBit - length + 32));
  RETURN bits;
END ShowMoreBits;

<*INLINE*>
PROCEDURE SkipBits(self: T; length: CARDINAL) RAISES {EOF} =
BEGIN
  DEC(self.currentBit, length);
  IF self.currentBit <= 0 THEN
    INC(self.currentBit, 32);
    INC(self.currentWord);
    IF self.currentWord >= self.wordsInBuffer THEN
       RefillBuffer(self);
       IF self.wordsInBuffer = 0 THEN
         RAISE EOF;
       END;
    END;
  END;
END SkipBits;

<*INLINE*>
PROCEDURE ByteAligned(self: T): BOOLEAN =
BEGIN
  RETURN self.currentBit MOD 8 = 0;
END ByteAligned;

PROCEDURE Length(self: T): INTEGER =
BEGIN
  RETURN self.length;
END Length;

PROCEDURE Close(self: T) =
BEGIN
  self.file.close();
END Close;

BEGIN
END BitIO.

