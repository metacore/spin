(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 27-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Direct data copying.
 * 18-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Made Read() and Write() faster.
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 *)
MODULE PipeFile EXPORTS Sphinx;
IMPORT Word;
IMPORT Thread;
IMPORT File;
IMPORT StreamFile;
IMPORT Ctypes;
IMPORT Error, Errno, ErrnoDep;
IMPORT Proc;
IMPORT OpenFile;
IMPORT Fcntl AS URTFcntl;
IMPORT IoctlPosix;
IMPORT SphinxMachineDep;
IMPORT Strand;
IMPORT Translation;
IMPORT CPU;
IMPORT ProcRep;
TYPE Mode = {Read, Write, Closed};

CONST BufferSize = 8192+1;
  
TYPE RingBuffer = REF RECORD
  (* used when writer arrives before reader, or
     when read is used instead of readRef. *)
  mu: MUTEX;
  readPtr, writePtr: [0 .. BufferSize-1];
  (* readPtr = writePtr => buf empty.
     writePtr = readPtr-1 => buf full *)
  full, empty: Thread.Condition;
  
  directBuf: REF ARRAY OF CHAR; (* used only when readRef is waiting
				   on the empty buffer. *)
  directFrom, directLen: CARDINAL; (* the region used in directBuf. *)
  buf: ARRAY [0 .. BufferSize-1] OF CHAR;
END;

TYPE T = StreamFile.T BRANDED OBJECT
  peer: T;
  mode: Mode;
  async: BOOLEAN;
  buf: RingBuffer;
OVERRIDES
  read := PipeRead;
  write := PipeWrite;
  readRef := ReadRef;
  close := PipeClose;
  ioctl := PipeIoctl;
END;

PROCEDURE PipeClose (f: T) =
  BEGIN
    f.mode := Mode.Closed;
    LOCK f.buf.mu DO
      Thread.Signal(f.buf.full);
      Thread.Signal(f.buf.empty);
    END;
  END PipeClose;

(* Pre: b is locked. *)
PROCEDURE ReadSub (f: T; VAR data: ARRAY OF CHAR): CARDINAL =
  VAR
    i: CARDINAL := 0;
    bytesToRead := NUMBER(data);
    needSignal: BOOLEAN;
    size: CARDINAL;
    b := f.buf;
  BEGIN
    <*ASSERT bytesToRead > 0*>
    WHILE i < bytesToRead DO
      WHILE b.readPtr = b.writePtr DO
	(* Buffer is empty. *)
	IF f.peer.mode = Mode.Closed OR f.async THEN
	  RETURN i;
	END;
	Thread.Wait(b.mu, b.empty);
      END;

      needSignal := (b.writePtr = b.readPtr-1
		     OR (b.readPtr = 0 AND b.writePtr = BufferSize-1));
      IF b.readPtr < b.writePtr THEN
	(* |-------R-------------W----| *)
	(*    copy <------------>       *)
	size := MIN(b.writePtr - b.readPtr, bytesToRead - i);
	SUBARRAY(data, i, size) := SUBARRAY(b.buf, b.readPtr, size);
	INC(b.readPtr, size);
	INC(i, size);
      ELSE
	(* |-------W-------------R----| *)
	(*                  copy <----> *)
	size := MIN(BufferSize - b.readPtr, bytesToRead - i);
	SUBARRAY(data, i, size) := SUBARRAY(b.buf, b.readPtr, size);
	INC(i, size);
	IF (b.readPtr + size >= BufferSize) THEN 
	  b.readPtr := 0;
	ELSE
	  INC(b.readPtr, size);
	END;
	
	
	(* |-------W-------------R----| *)
	(* <------> copy                *)
	size := MIN(b.writePtr, bytesToRead - i);
	IF size # 0 THEN
	  SUBARRAY(data, i, size) := SUBARRAY(b.buf, 0, size);
	  INC(i, size);
	  b.readPtr := size;
	END;
      END;
      IF needSignal THEN Thread.Signal(b.full); END;
    END;
    RETURN i;
  END ReadSub;
  
PROCEDURE ReadRef (f: T;
		   VAR buf: REF ARRAY OF CHAR;
		   bytes: CARDINAL;
		   <*UNUSED*>offset: CARDINAL;
		   VAR from: CARDINAL): CARDINAL =
  VAR
    b := f.buf;
    n: CARDINAL;
  BEGIN
    IF buf = NIL THEN
      buf := NEW(REF ARRAY OF CHAR, bytes);
      from := 0;
    END;
    LOCK b.mu DO
      IF FALSE AND b.readPtr = b.writePtr AND b.directBuf = NIL THEN
	b.directBuf := buf;
	b.directFrom := from;
	b.directLen := bytes;
	REPEAT
	  IF f.peer.mode = Mode.Closed OR f.async THEN
	    EXIT;
	  END;
	  Thread.Wait(b.mu, b.empty);
	UNTIL b.directLen = 0;
	b.directBuf := NIL;
	n := bytes - b.directLen;
      ELSE
	n := ReadSub(f, SUBARRAY(buf^, from, bytes));
      END;
    END;
    RETURN n;
  END ReadRef;

PROCEDURE PipeRead (f: T;
		    VAR data: ARRAY OF CHAR;
		    <*UNUSED*>offset: File.OffsetT): CARDINAL =
  BEGIN
    LOCK f.buf.mu DO
      RETURN ReadSub(f, data);
    END;
  END PipeRead;

PROCEDURE PipeWrite (f: T;
		     READONLY data: ARRAY OF CHAR;
		     <*UNUSED*>offset: File.OffsetT): CARDINAL =
  VAR
    i: CARDINAL := 0;
    b := f.buf;
    bytesToWrite := NUMBER(data);
    needSignal: BOOLEAN;
    size: CARDINAL;
  BEGIN
    <*ASSERT bytesToWrite > 0*>
    LOCK b.mu DO
      WHILE i < bytesToWrite DO
	IF b.directBuf # NIL THEN
	  size := MIN(b.directLen, bytesToWrite-i);
	  SUBARRAY(b.directBuf^, b.directFrom, size) :=
	    SUBARRAY(data, i, size);
	  DEC(b.directLen, size);
	  INC(b.directFrom, size);
	  INC(i, size);
	  Thread.Signal(b.empty);
	  IF i >= bytesToWrite THEN EXIT; END;
	END;

	WHILE (b.directBuf = NIL OR b.directLen = 0)
	  AND (b.writePtr = b.readPtr-1
	       OR (b.readPtr = 0 AND b.writePtr = BufferSize-1)) DO
	  (* Buffer is full. *)
	  IF f.peer.mode = Mode.Closed OR f.async THEN
	    RETURN i;
	  END;
	  Thread.Wait(b.mu, b.full);
	END;

	needSignal := b.readPtr = b.writePtr;
	
	IF b.readPtr > b.writePtr THEN
	  (* |-------W-------------R----| *)
	  (*    copy <----------->        *)
	  size := MIN(b.readPtr - 1 - b.writePtr, bytesToWrite - i);
	  SUBARRAY(b.buf, b.writePtr, size) := SUBARRAY(data, i, size);
	  INC(b.writePtr, size);
	  INC(i, size);
	ELSIF b.readPtr = 0 THEN
	  (* R---------------------W----| *)
	  (*                  copy <--->  *)
	  size := MIN(BufferSize - b.writePtr - 1, bytesToWrite - i);
	  SUBARRAY(b.buf, b.writePtr, size) := SUBARRAY(data, i, size);
	  INC(b.writePtr, size);
	  INC(i, size);
	ELSE 
	  (* |-------R-------------W----| *)
	  (*                  copy <----> *)
	  size := MIN(BufferSize - b.writePtr, bytesToWrite - i);
	  SUBARRAY(b.buf, b.writePtr, size) := SUBARRAY(data, i, size);
	  INC(i, size);
	  IF (b.writePtr + size >= BufferSize) THEN
	    b.writePtr := 0;
	  ELSE
	    INC(b.writePtr, size);
	  END;
	  
	  (* |-------R-------------W----| *)
	  (* <------> copy                *)
	  size := MIN(b.readPtr-1, bytesToWrite - i);
	  IF size > 0 THEN
	    SUBARRAY(b.buf, 0, size) := SUBARRAY(data, i, size);
	    INC(i, size);
	    b.writePtr := size;
	  END;
	END;
	IF needSignal THEN Thread.Signal(b.empty); END;
      END;
    END;
    RETURN i;
  END PipeWrite;
    
PROCEDURE PipeIoctl (f: T;
		     cmd: INTEGER;
		     VAR arg: ARRAY OF CHAR;
		     <*UNUSED*>mode: INTEGER) RAISES {Error.E} =
  VAR i: Word.T;
  BEGIN
    IF cmd = IoctlPosix.FIOASYNC OR cmd = IoctlPosix.FIONBIO THEN
      i := VIEW(arg, Ctypes.int);
      f.async := (i # 0);
      LOCK f.buf.mu DO
	Thread.Signal(f.buf.full);
	Thread.Signal(f.buf.empty);
      END;
    ELSE
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EINVAL));
    END;
  END PipeIoctl;

(*
 Start of public procedures
 *)
PROCEDURE Pipe (<*UNUSED*>us: Strand.T; VAR ss: CPU.SavedState)
  RAISES {Errno.E} =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    p: ARRAY [0 .. 1] OF T;
    fh: ARRAY [0 .. 1] OF OpenFile.T;
    fd: ARRAY [0 .. 1] OF INTEGER;
    b: RingBuffer;
  BEGIN

    p[0] := NEW(T);
    p[1] := NEW(T);
    b := NEW(RingBuffer,
	     readPtr := 0,
	     writePtr := 0,
	     mu := NEW(MUTEX),
	     full := NEW(Thread.Condition),
	     empty := NEW(Thread.Condition));
    p[0].peer := p[1];
    p[0].mode := Mode.Read;
    p[0].buf := b;
    p[0].async := FALSE;
    p[1].peer := p[0];
    p[1].mode := Mode.Write;
    p[1].buf := b;
    p[1].async := FALSE;

    fh[0] := NEW(OpenFile.T);
    fh[0].seekable := FALSE;
    fh[0].flags := URTFcntl.O_RDONLY;
    fh[0].path := "pipe-read";
    fh[0].h := p[0];
    
    fh[1] := NEW(OpenFile.T);
    fh[1].seekable := FALSE;
    fh[1].flags := URTFcntl.O_WRONLY;
    fh[1].path := "pipe-write";
    fh[1].h := p[1];
    
    LOCK proc.mu DO
      fd[0] := Proc.AllocateFD(proc, fh[0]);
      fd[1] := Proc.AllocateFD(proc, fh[1]);
    END;
    SphinxMachineDep.GetpidReturn(ss, fd[0], fd[1]);
  END Pipe;

BEGIN
END PipeFile.
