(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to implementation using low level syscalls.
 *	M3 library doesn't know char special file can be seeked. Crap!
 * 13-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE RawIO; (* we need loophole to interface with raw Unix procs. *)
IMPORT IO, Fmt;
IMPORT Unix;
IMPORT Uuio, Umman;
IMPORT M3toC;
IMPORT CPU;
IMPORT TransOS;
IMPORT Word;
IMPORT Ctypes;
IMPORT Cerrno;
IMPORT RawIOExtern;
IMPORT Debugger;
FROM TransUtils IMPORT Msg, Warn;

REVEAL T = BRANDED REF RECORD 
  fd: CARDINAL;
  pos: INTEGER;
  name: TEXT;
END;


PROCEDURE Stat (f: T): FileStat =
  BEGIN
    RETURN FileStat{size := RawIOExtern.get_file_size(f.fd)};
  END Stat;

PROCEDURE Write (f: T; READONLY log: ARRAY OF CHAR; pos: CARDINAL) =
  VAR
    len := NUMBER(log);
    nWrite: INTEGER;
    adr := LOOPHOLE(ADR(log[0]), Ctypes.char_star);
  BEGIN
    <*ASSERT pos MOD BlockSize = 0*>
    EVAL Seek(f, pos);
    nWrite := Uuio.write(f.fd, adr, len);
    IF nWrite = -1 AND Cerrno.errno = 14 THEN
      (* Efault. the requested page might be invalidated. *)
      EVAL Umman.mprotect(adr, CPU.PAGESIZE,
		     Word.Or(Umman.PROT_READ, Umman.PROT_WRITE));
      nWrite := Uuio.write(f.fd, adr, len);
      EVAL Umman.mprotect(adr, CPU.PAGESIZE, 0);
    END;
    IF nWrite # len THEN
      Warn("rawio.write: errno=", Fmt.Int(Cerrno.errno), " ",
	   Fmt.Int(nWrite), "<->", Fmt.Int(len));
      EVAL Seek(f, f.pos+len);
    END;
    INC(f.pos, len);
  END Write;

PROCEDURE Read (f: T; VAR log: ARRAY OF CHAR; pos: CARDINAL): INTEGER =
  VAR
    nRead: INTEGER;
    adr := LOOPHOLE(ADR(log[0]), Ctypes.char_star);    
  BEGIN
    <*ASSERT pos MOD BlockSize = 0*>
    EVAL Seek(f, pos);
    nRead := Uuio.read(f.fd, adr, NUMBER(log));
    IF nRead < NUMBER(log) THEN
      Msg("rawio.read: errno=", Fmt.Int(Cerrno.errno),
	  "at ", Fmt.Int(pos));
    END;
    
    INC(f.pos, nRead);
    RETURN nRead;
  END Read;

PROCEDURE Seek (f: T; pos: INTEGER): INTEGER =
  BEGIN
    <*ASSERT pos MOD BlockSize = 0*>
    IF f.pos = pos THEN RETURN pos; END;

    pos := Unix.lseek(f.fd, pos, 0);
    IF pos < 0 THEN
      Msg("rawio.seek: errno=", Fmt.Int(Cerrno.errno));
    END;
    f.pos := pos;
    RETURN f.pos;
  END Seek;

PROCEDURE Close (f: T) =
  BEGIN
    EVAL Unix.close(f.fd);
    f.fd := 999;
  END Close;
  
PROCEDURE Open (file: TEXT; size: CARDINAL): T =
  VAR
    t := NEW(T);
  BEGIN
    IF Unix.access(M3toC.TtoS(file), 0) # 0 THEN
      (* File doesn't exist. Create it. *)
      Msg("rawio.open: creating ", file, ", size 16_",
	  Fmt.Int(size, 16), ".\n");
      t.fd := Unix.open(M3toC.TtoS(file),
		      Word.Or(Unix.O_RDWR, Unix.O_CREAT),
		      Unix.Mrwrwrw);
      IF t.fd < 0 THEN
	IO.Put("rawio.open: error while opening.\n");
	TransOS.Exit(1);
      END;
      EVAL Unix.ftruncate(t.fd, size);
      EVAL Unix.close(t.fd);
    END;
    t.fd := Unix.open(M3toC.TtoS(file),
		      Word.Or(Unix.O_RDWR, Unix.O_CREAT),
		      Unix.Mrwrwrw);
    t.pos := 0;
    t.name := file;
    IF t.fd < 0 THEN
      IO.Put("rawio.open:" & file & ".\n");
      t := NIL;
    END;
    RETURN t;
  END Open;
PROCEDURE PrintStat () =
  BEGIN
  END PrintStat;
  
BEGIN
END RawIO.
