(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added log truncation.
 * 07-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE WAL EXPORTS WAL, WALPrivate;

IMPORT Word;
IMPORT TID;
IMPORT SID;
IMPORT Round;
IMPORT Ctypes;
IMPORT Fmt;
IMPORT IO;
IMPORT RawIO;
IMPORT TextRefTbl;
IMPORT Debugger;
IMPORT TransUtils;
IMPORT StorageLocal;
IMPORT TransPrivate;
IMPORT Spy;
<*NOWARN*>FROM TransUtils IMPORT Debug, DebugMsg, Msg, SpyTime;

VAR
  openLogs := NEW(TextRefTbl.Default).init();
  openLogsMu := NEW(MUTEX);
  logTimer, logFlushTimer: Spy.T;

CONST
  IntSize = BYTESIZE(INTEGER);
  
<*INLINE*>
PROCEDURE GetNextLSN (READONLY hdr: RawHeader): LSN =
  VAR n := Round.Up8(hdr.lsn + BYTESIZE(RawHeader) + hdr.size);
  BEGIN
    IF n MOD BlockSize + BYTESIZE(RawHeader) > BlockSize THEN
      n := Round.Up(n, BlockSize);
    END;
    RETURN n;
  END GetNextLSN;

VAR typeToName :=
  ARRAY Type OF TEXT {"Redo", "Undo",
		      "S1PCommit", "SPrepare", "SCommit", "SAbort",
		      "TPrepared", "TAbort", "TCommitted",
		      "CheckPoint", "ShutDown", "EOB"};
  
PROCEDURE TypeName (t: Type): TEXT =
BEGIN
  RETURN typeToName[t];
END TypeName;



(* Read the contents of the log block "pos" into the crash buffer, 
  scan the block and set the buf-position of each records within it to
   "t.crashRecords". The end of "t.crashRecords" is marked by "BlockSize".
 *)
PROCEDURE ReadIntoCrashBuf (t: T; pos: INTEGER) =
VAR
  hdr: RawHeader;
  recordIdx: [0 .. BlockSize-1];
  lsn: INTEGER;
BEGIN
  WITH len = RawIO.Read(t.fh, t.crashBuf^, pos) DO
    IF len < BYTESIZE(RawHeader) THEN
      (* special case: log is empty *)
      t.crashRecords[0] := BlockSize; (* mark the end *)
      RETURN;
    END;
  END;

  IF NOT ViewRawHeader(t.crashBuf^, hdr, 0) THEN
    (* special case: log is empty *)
    t.crashRecords[0] := BlockSize;
    RETURN;
  END;
    
  t.crashRecords[0] := 0;
  recordIdx := 1;
  t.crashFirstLSN := hdr.lsn;
  lsn := GetNextLSN(hdr);
  
  LOOP
    WITH off = lsn MOD BlockSize DO 
      IF off + BYTESIZE(RawHeader) > BlockSize
	 OR NOT ViewRawHeader(t.crashBuf^, hdr, off)
	 OR hdr.lsn # lsn THEN
	(* Mark the last entry void *)
	t.crashRecords[recordIdx] := BlockSize;
	t.crashLastLSN := lsn;
	RETURN; (* reached the end of the log *)
      END;
      t.crashRecords[recordIdx] := off;
      INC(recordIdx);
    END;
    lsn := GetNextLSN(hdr);
  END;
END ReadIntoCrashBuf;

(* Find the log record with miminum LSN. Such record is always the first
   record of some log block. Returns TRUE if log file is not empty, and
   "hdr" holds the first log record info. 
   
   As a side effect, this procedure sets t.delta value also.
   *)
PROCEDURE FindFirstRecord (t: T; (*OUT*)VAR hdr: RawHeader): BOOLEAN =
  VAR
    lastHdr: RawHeader;
    buf: ARRAY [0 .. RawIO.BlockSize-1] OF CHAR;
    pos := t.size - BlockSize;
    len: CARDINAL;
  BEGIN
    EVAL RawIO.Read(t.fh, buf, 0);
    IF NOT ViewRawHeader(buf, lastHdr, 0) THEN
      RETURN FALSE;
    END;

    pos := t.size - BlockSize;
    LOOP
      len := RawIO.Read(t.fh, buf, pos);
      IF NOT ViewRawHeader(buf, hdr, 0)
	 OR hdr.lsn # lastHdr.lsn-BlockSize THEN
	(* The very block at "pos" is not logically contiguous with
	   the block at "pos+BlockSize"(modulo t.size). This means that the
	   block at "pos+BlockSize" is the logical first block. *)
	hdr := lastHdr;
	t.delta := hdr.lsn - (pos + BlockSize) MOD t.size;
	RETURN TRUE;
      END;
      lastHdr := hdr;
      DEC(pos, BlockSize);
    END;
  END FindFirstRecord;
  
(* Find the very last log record in the file T. Put the
   header of that record on "hdr", and return TRUE. This proc returns
   FALSE if log is empty.

 *)
PROCEDURE FindLastRecord (t: T; (*OUT*)VAR hdr: RawHeader): BOOLEAN =
  VAR
    buf: ARRAY [0 .. RawIO.BlockSize-1] OF CHAR;    
    pos := 0;
    len: CARDINAL;
    prevLSN := -1;
  BEGIN
    (*
       First, we find the last block. This can be looking at the
       LSN of the first record of each block, and check if
       LSN is monotonically increasing. If not, then we know that
       we've reached the end.
       
       (this is the virtue of not letting log records cross the block
       boundary)
    *)
    EVAL RawIO.Read(t.fh, buf, 0);
    IF NOT ViewRawHeader(buf, hdr, 0) THEN
      (* The very first record of the log is invalid. *)
      RETURN FALSE;
    END;
  
    prevLSN := hdr.lsn;
  
    LOOP
      IF pos+BlockSize >= t.size THEN
	EXIT;	(* reached EOF. *)
      END;
      
      len := RawIO.Read(t.fh, buf, pos + BlockSize);
      <*ASSERT len = NUMBER(buf)*>
      IF NOT ViewRawHeader(buf, hdr, 0)
	 OR hdr.lsn <= prevLSN THEN
	(* The LSN of the first record of the next block is
	   smaller than the last blocks'. This means that
	   the last block is the latest. *)
	EXIT;
      END;
      
      prevLSN := hdr.lsn;
      INC(pos, BlockSize);
    END;
    
    (* Read the last log block into the crash buffer, and
       find the last log record within that block *)
    ReadIntoCrashBuf(t, pos);


    (* Get the delta value. Delta is the difference between disk position
       and LSN value. It is usually 0, but may become positive when
       the log wrapped round. *)
    <*ASSERT t.crashRecords[0] # BlockSize*>  (* We must have at least one
						 record in the last block. *)
    EVAL ViewRawHeader(t.crashBuf^, hdr, 0);
    t.firstBlock := pos;
    t.firstBlockLSN := hdr.lsn;
    FOR i := 0 TO LAST(t.crashRecords^) DO
      IF t.crashRecords[i] = BlockSize THEN
	(* we've reached the end of the block *)
	IF i = 0 THEN
	  (* log was empty *)
	  RETURN FALSE;
	ELSE
	  EVAL ViewRawHeader(t.crashBuf^, hdr, t.crashRecords[i-1]);
	  RETURN TRUE;
	END;
      END;
    END;
    EVAL ViewRawHeader(t.crashBuf^, hdr,
		       t.crashRecords[LAST(t.crashRecords^)]);
    RETURN TRUE;
  END FindLastRecord;

(* Read the log at LSN. *)  
PROCEDURE Read (t: T; lsn: LSN; buf: REF ARRAY OF CHAR;
		from := 0): INTEGER =
  VAR
    bufOff : [0 .. BlockSize-1];
  BEGIN
    LOCK t DO
      (* "Read" is used only on transaction abort and startup log analysis.
	 Therefore, we don't pay much penalty by flushing every time here. *)
      FlushWithoutLock(t, LAST(LSN));

      IF lsn < t.crashFirstLSN OR lsn >= t.crashLastLSN THEN
	(* We are outside the the crash buffer.
	   Read in the correct one. *)
	<*ASSERT t.crashFirstLSN MOD BlockSize = 0*>
	ReadIntoCrashBuf(t, LSNToFileOffset(t, Round.Down(lsn, BlockSize)));
      
	IF t.crashFirstLSN # Round.Down(lsn, BlockSize) THEN
	  (* log wraps round. *)
	  <*ASSERT FALSE*>
	END;
      END;

      <*ASSERT Sane(t)*>
      <*ASSERT lsn >= t.crashFirstLSN AND lsn < t.crashLastLSN *>
      <*ASSERT t.crashFirstLSN MOD BlockSize = 0*>

      bufOff := lsn - t.crashFirstLSN;

      WITH hdr = VIEW(SUBARRAY(t.crashBuf^, bufOff, BYTESIZE(RawHeader)),
		      RawHeader) DO
	<*ASSERT hdr.lsn = lsn*>
	IF hdr.size+BYTESIZE(RawHeader) < NUMBER(buf^)-from THEN 
	  (* buf big enough *)
	  SUBARRAY(buf^, from, hdr.size+BYTESIZE(RawHeader))
	      := SUBARRAY(t.crashBuf^, bufOff, hdr.size+BYTESIZE(RawHeader));
	END;
	RETURN hdr.size+BYTESIZE(RawHeader);
      END;
    END;
  END Read;

PROCEDURE ReadPrevLog (t: T; lsn: LSN; VAR hdr: RawHeader): BOOLEAN =
  VAR 
    newFirstLSN: LSN;
    newLSN, nextLSN: LSN;
  BEGIN
    IF lsn <= t.firstLSN  THEN
      (* We can't go back beyond the first record *)
      RETURN FALSE; END;
    
    <*ASSERT lsn > 0*>
    
    IF lsn <= t.crashFirstLSN OR lsn > t.crashLastLSN THEN
      (* We are outside the the crash buffer.
	 Read in the correct one.
      *)
      <*ASSERT t.crashFirstLSN MOD BlockSize = 0*>
      IF lsn MOD BlockSize = 0 THEN
	newFirstLSN := lsn - BlockSize;
      ELSE
	newFirstLSN := Round.Down(lsn, BlockSize);
      END;

      IF newFirstLSN < t.firstLSN THEN
	(* No more old logs. *)
	RETURN FALSE;
      END;
      
      ReadIntoCrashBuf(t, LSNToFileOffset(t, newFirstLSN));
    END;
    
    (* Begin from the beginning of the crash buf, search
       forward until we get just before "lsn".
    *)
    IF NOT (lsn > t.crashFirstLSN AND lsn <= t.crashLastLSN) THEN
      Debugger.Enter();
    END;

    <*ASSERT t.crashFirstLSN MOD BlockSize = 0*>
    EVAL ViewRawHeader(t.crashBuf^, hdr, 0);
    nextLSN := GetNextLSN(hdr);
  
    WHILE nextLSN # lsn DO 
      newLSN := nextLSN;
      IF NOT ViewRawHeader(t.crashBuf^, hdr, newLSN MOD BlockSize) THEN
	IO.Put("LOG is seriously broken: Can't find the prev log of "
	       & Fmt.Int(lsn) & ".\n");
	<*ASSERT FALSE*>
      END;	     
      nextLSN := GetNextLSN(hdr);
      <*ASSERT nextLSN > t.crashFirstLSN AND nextLSN <= t.crashLastLSN*>
    END;
    
    RETURN TRUE;
  END ReadPrevLog;

(* Read the log for "sid" that precedes "lsn".

 Pre: "t" is locked.
 *)
PROCEDURE ReadPrevLogWRTStorage (t: T; sid: SID.T; lsn: LSN;
				 VAR hdr: RawHeader): BOOLEAN =
  BEGIN
    LOCK t DO 
      LOOP
	IF NOT ReadPrevLog(t, lsn, hdr) THEN
	  RETURN FALSE;
	END;
	IF hdr.sid = sid THEN
	  RETURN TRUE;
	END;
	lsn := hdr.lsn;
      END;
    END;
  END ReadPrevLogWRTStorage;

PROCEDURE Sane (t: T): BOOLEAN =
  BEGIN
    IF t.delta MOD BlockSize # 0 THEN
      Debugger.Enter();
    END;
    IF (t.nextLSN - t.logTail) MOD BlockSize # 0 THEN
      Debugger.Enter();
    END;
    IF t.crashFirstLSN MOD BlockSize # 0 THEN
      Debugger.Enter();
    END;
    RETURN TRUE;
  END Sane;
(* 
   Public Procedures
 *)
PROCEDURE Open (fileName: TEXT): T =
VAR
  t: T;
  r: REFANY;
  hdr: RawHeader;
BEGIN
  LOCK openLogsMu DO
    IF openLogs.get(fileName, r) THEN
      t := NARROW(r, T);
      INC(t.refCnt);
    ELSE
      t := NEW(T,
	       refCnt := 1,
	       name := fileName, 
	       fh := RawIO.Open(fileName, DefaultLogSize),
	       log := NEW(REF ARRAY OF CHAR, BlockSize), 
	       crashBuf := NEW(REF ARRAY OF CHAR, BlockSize),
	       crashRecords := NEW(REF ARRAY OF [0..BlockSize],
				   BlockSize DIV BYTESIZE(RawHeader)),
	       crashLSNIdx := 0,
	       cpCount := 0,
	       cpMu := NEW(MUTEX));
      t.size := RawIO.Stat(t.fh).size;
      IF t.size MOD BlockSize # 0 THEN
	VAR oldSize := t.size;
	BEGIN
	  t.size := Round.Down(t.size, BlockSize);
	  Msg("wal.open: rounding down the log size from ",
	      Fmt.Int(oldSize), " to ", Fmt.Int(t.size));
	END;
      END;
      
      EVAL openLogs.put(fileName, t);
      
      IF FindLastRecord(t, hdr) THEN
	t.lastFlushedLSN := GetNextLSN(hdr);
	t.startLSN := t.lastFlushedLSN;
      ELSE
	(* log is empty *)
	t.startLSN := -1;
	t.lastFlushedLSN := 0;
      END;
      IF FindFirstRecord(t, hdr) THEN
	t.firstLSN := hdr.lsn;
      ELSE
	t.firstLSN := 0;
      END;
      IF DebugMsg THEN
	Msg("wal.open: first log=", Fmt.Int(t.firstLSN),
	    "last log=", Fmt.Int(t.startLSN));
      END;
      t.nextLSN := t.lastFlushedLSN;
      
      (* Read the last log block into the memory and prepare to append.*)
      t.logFirstLSN := Round.Down(t.lastFlushedLSN, BlockSize);
      EVAL RawIO.Read(t.fh, SUBARRAY(t.log^, 0, BlockSize),
		      LSNToFileOffset(t, t.logFirstLSN));
      t.logTail := t.lastFlushedLSN - t.logFirstLSN;
      <*ASSERT Sane(t)*>
      Recover(t);
      <*ASSERT Sane(t)*>
    END;
  END;

  RETURN t;
END Open;

PROCEDURE FileName (fd: T): TEXT =
BEGIN
  RETURN fd.name;
END FileName;

(* Write a log record from "fd.logTail" *)
PROCEDURE WriteCommonHeader (fd: T; sid: SID.T;
			     type: Type; size: INTEGER;
			     tid: TID.T; prevLSN: LSN) =
  CONST HeaderSize = BYTESIZE(CommonHeader);
  BEGIN
    WITH hdr = VIEW(SUBARRAY(fd.log^, fd.logTail, HeaderSize), CommonHeader) DO
      hdr.lsn := fd.nextLSN;
      hdr.sid := sid;
      hdr.type := ORD(type);
      hdr.size := size;
      WITH x = VIEW(hdr, ARRAY [0..1] OF INTEGER) DO
	hdr.ivsum := Word.And(Word.Not(Word.Plus(x[0], x[1])),
			      16_FFFFFFFF);
      END;
      hdr.tid := tid;
      hdr.prevLSN := prevLSN;
      INC(fd.logTail, HeaderSize);
    END;
  END WriteCommonHeader;

PROCEDURE WriteRawHeader (fd: T; sid: SID.T;
			  type: Type; size: INTEGER) =
  CONST HeaderSize = BYTESIZE(RawHeader);
  BEGIN
    WITH hdr = VIEW(SUBARRAY(fd.log^, fd.logTail, HeaderSize), RawHeader) DO
      hdr.lsn := fd.nextLSN;
      hdr.sid := sid;
      hdr.type := ORD(type);
      hdr.size := size;
      WITH x = VIEW(hdr, ARRAY [0..1] OF INTEGER) DO
	hdr.ivsum := Word.And(Word.Not(Word.Plus(x[0], x[1])),
			      16_FFFFFFFF);
      END;
      INC(fd.logTail, HeaderSize);
    END;
  END WriteRawHeader;

  
PROCEDURE WriteRedo (t: T;
		     sid: SID.T;
		     tid: TID.T;
		     type: Type;
		     prevLSN: LSN;
		     pos: INTEGER;
		     READONLY content: ARRAY OF CHAR): LSN =
  VAR
    recordSize := Round.Up8(BYTESIZE(CommonHeader)
			    +IntSize
			    +NUMBER(content));
    lsn: LSN;
    orgLogTail: INTEGER;
  BEGIN
    IF SpyTime THEN Spy.Enter(logTimer); END;
      
    <*ASSERT type = Type.Undo OR type = Type.Redo*>
    LOCK t DO
      <*ASSERT Sane(t)*>
      (* No log record can cross the log block boundary. If it's going to
	 cross the boundary, we first flush the currently buffered log records
      *)
      IF t.logTail + recordSize >= BlockSize THEN
	FlushAndClearLog(t);
	<*ASSERT Sane(t)*>
      END;
    
      <*ASSERT t.logTail + recordSize < BlockSize*>
      orgLogTail := t.logTail;
      lsn := t.nextLSN;
      WriteCommonHeader(t, sid, type,
			CommonHeaderSize+IntSize+NUMBER(content),
			tid, prevLSN);
      VIEW(SUBARRAY(t.log^, t.logTail, IntSize), INTEGER) := pos;
      SUBARRAY(t.log^, t.logTail+IntSize, NUMBER(content)) := content;
      <*ASSERT orgLogTail + recordSize = Round.Up8(t.logTail+NUMBER(content)+8)*>
      t.logTail := orgLogTail + recordSize;
      INC(t.nextLSN, recordSize);
      <*ASSERT Sane(t)*>
    END; (* lock t *)
    IF SpyTime THEN Spy.Exit(logTimer); END;
    RETURN lsn;
  END WriteRedo;
  
PROCEDURE WriteCommit (t: T; sid: SID.T; tid: TID.T;
		       type: Type; prevLSN: LSN;
		       READONLY aux: ARRAY OF CHAR): LSN =
  VAR
    lsn: LSN;
    orgLogTail: INTEGER;
    recordSize := BYTESIZE(CommonHeader) + NUMBER(aux);
    (* assuming "aux" is always 8 byte aligned. *)
  BEGIN
    IF SpyTime THEN Spy.Enter(logTimer); END;
    <*ASSERT NUMBER(aux) MOD 8 = 0*>
    
    LOCK t DO
      <*ASSERT Sane(t)*>
      IF t.logTail + recordSize >= BlockSize THEN 
	FlushAndClearLog(t);
	<*ASSERT Sane(t)*>
      END;
      
      <*ASSERT t.logTail + recordSize < BlockSize*>
      orgLogTail := t.logTail;
      lsn := t.nextLSN;
      WriteCommonHeader(t, sid, type, CommonHeaderSize + NUMBER(aux),
			tid, prevLSN);
      IF NUMBER(aux) > 0 THEN
	SUBARRAY(t.log^, t.logTail, NUMBER(aux)) := aux;
	INC(t.logTail, NUMBER(aux));
      END;
      
      <*ASSERT orgLogTail + recordSize = t.logTail *>
      INC(t.nextLSN, recordSize);
      <*ASSERT Sane(t)*>
    END;
    IF SpyTime THEN Spy.Exit(logTimer); END;
    RETURN lsn;
  END WriteCommit;
  
PROCEDURE WriteOther (t: T;
		      sid: SID.T;
		      type: Type;
		      READONLY buf: ARRAY OF CHAR): LSN =
VAR 
  recordSize := Round.Up8(BYTESIZE(RawHeader) +NUMBER(buf));
  lsn: LSN;
  orgLogTail: INTEGER;
BEGIN
  IF SpyTime THEN Spy.Enter(logTimer); END;
  LOCK t DO
    <*ASSERT Sane(t)*>
    IF t.logTail + recordSize >= BlockSize THEN 
      FlushAndClearLog(t);
      <*ASSERT Sane(t)*>
    END;

    <*ASSERT t.logTail + recordSize < BlockSize*>
    orgLogTail := t.logTail;

    lsn := t.nextLSN;
    WriteRawHeader(t, sid, type, NUMBER(buf));
    SUBARRAY(t.log^, t.logTail, NUMBER(buf)) := buf;
    INC(t.logTail, NUMBER(buf));
    t.logTail := Round.Up8(t.logTail);
    
    <*ASSERT orgLogTail + recordSize = t.logTail*>
    INC(t.nextLSN, recordSize);
  END;
  IF SpyTime THEN Spy.Exit(logTimer); END;
  RETURN lsn;
END WriteOther;

(* Convert LSN to file offset *)
<*INLINE*>
PROCEDURE LSNToFileOffset (t: T; lsn: LSN): CARDINAL =
  BEGIN
    <*ASSERT lsn >= t.firstLSN*>
    WITH voff = lsn - t.delta DO
      IF voff < t.size THEN RETURN voff; END;
      (* wraps round.. *)
      <*ASSERT voff - t.size < t.size*>
      RETURN voff - t.size;
    END;
  END LSNToFileOffset;

(* Free enough log space *)  
PROCEDURE FreeupLogSpace (t: T) =
  VAR minLSN: LSN;
  BEGIN
    <*ASSERT t.nextLSN >= t.firstBlockLSN*>
    IF t.nextLSN - t.firstBlockLSN >= t.size THEN
      Msg("!!!! no log space.");
      Debugger.Enter();
    END;
    
    IF t.nextLSN - t.firstBlockLSN >= (t.size DIV 4) * 3 THEN
      (* Find out the minimum LSN from which the records may be used.
	 In other words, the records appearing before "minLSN" can be
	 never used, so it is safe to delete them. *)
      minLSN := MIN(StorageLocal.FreeupLogSpace(),
		    TransPrivate.FreeupLogSpace());
      IF DebugMsg THEN
	Msg("truncating the log ", Fmt.Int(t.firstBlockLSN), "->",
	    Fmt.Int(minLSN));
      END;
      <*ASSERT minLSN >= t.firstBlockLSN*>
      minLSN := Round.Down(MIN(minLSN, t.nextLSN), BlockSize);
      t.firstBlock := LSNToFileOffset(t, minLSN);
      t.firstBlockLSN := minLSN;
      t.delta := t.firstBlockLSN - t.firstBlock;
    END;
  END FreeupLogSpace;
  
(* Flush everything that is held currently, and clear the log.
   If there's a empty space in the buffer, 0 is output to the device for that
   part
   
   Pre: "t" is locked *)
PROCEDURE FlushAndClearLog (t: T) =
  VAR
    logStartOffset := t.lastFlushedLSN - t.logFirstLSN;
    storageStartOffset: CARDINAL;
  BEGIN
    IF SpyTime THEN Spy.Enter(logFlushTimer); END;
    
    <*ASSERT t.logTail = t.nextLSN MOD BlockSize*>
    <*ASSERT t.lastFlushedLSN <= t.nextLSN *>
    <*ASSERT Sane(t)*>    

    FreeupLogSpace(t);
    storageStartOffset := LSNToFileOffset(t, t.lastFlushedLSN);
    <*ASSERT (logStartOffset - storageStartOffset) MOD BlockSize = 0*>
    
    IF BlockSize - t.logTail >= BYTESIZE(RawHeader) THEN
      (* Append the end-of-block record to make the next
	 log start from the head of the buffer *)
       WriteRawHeader(t, SID.Void, Type.EOB,
		      BlockSize-t.logTail-BYTESIZE(RawHeader));
    ELSE
      t.logTail := BlockSize;
    END;

    (* Round down both offsets to RawIO.BlockSize. *)
    logStartOffset := Round.Down(logStartOffset, RawIO.BlockSize);
    storageStartOffset := Round.Down(storageStartOffset, RawIO.BlockSize);

    RawIO.Write(t.fh,
		SUBARRAY(t.log^, logStartOffset, BlockSize-logStartOffset),
		storageStartOffset);
    
    t.nextLSN := Round.Up(t.nextLSN, BlockSize);
    IF DebugMsg AND t.nextLSN - t.delta >= t.size THEN
      Msg("log wraps round.");
    END;
    t.lastFlushedLSN := t.nextLSN;
    t.logFirstLSN := t.nextLSN;
    t.logTail := 0;
    IF SpyTime THEN Spy.Exit(logFlushTimer); END;
    <*ASSERT Sane(t)*>
  END FlushAndClearLog;

PROCEDURE FlushWithoutLock (t: T; lsn: LSN) =
  VAR
    logStartOffset := t.lastFlushedLSN - t.logFirstLSN;
    (* the 1st offset of the region in the log buffer we need to write out. *)
    logEndOffset: CARDINAL;
    (* the last offset of the region in the log buffer we need to write out. *)
    storageStartOffset: CARDINAL;
    (* the first offset of the log storage we need to write out. *)
    size: CARDINAL;
  BEGIN
    <*ASSERT Sane(t)*>
    lsn := MIN(lsn, t.nextLSN);
    IF lsn <= t.lastFlushedLSN THEN RETURN; END;
    
    IF SpyTime THEN Spy.Enter(logFlushTimer); END;
    FreeupLogSpace(t);

    storageStartOffset := LSNToFileOffset(t, t.lastFlushedLSN);
    logEndOffset := lsn - t.logFirstLSN;
    
    <*ASSERT (logStartOffset - storageStartOffset) MOD BlockSize = 0*>

    (* All I/Os have to RawIO.BlockSize aligned. *)
    logStartOffset := Round.Down(logStartOffset, RawIO.BlockSize);
    logEndOffset := Round.Up(logEndOffset, RawIO.BlockSize);
    storageStartOffset := Round.Down(storageStartOffset, RawIO.BlockSize);
    
    (* Since log is always flushed before the buffer gets full *)
    size := logEndOffset - logStartOffset;
    <*ASSERT size <= BlockSize *>

    RawIO.Write(t.fh, SUBARRAY(t.log^, logStartOffset, size),
		storageStartOffset);
    t.lastFlushedLSN := lsn;
    <*ASSERT Sane(t)*>
    IF SpyTime THEN Spy.Exit(logFlushTimer); END;
  END FlushWithoutLock;

PROCEDURE Flush (t: T; lsn: LSN) =
  BEGIN
    LOCK t DO
      FlushWithoutLock(t, lsn);
    END;
  END Flush;


PROCEDURE Close (t: T) =
VAR dummy: REFANY;
BEGIN
  LOCK openLogsMu DO
    FlushAndClearLog(t);
    DEC(t.refCnt);
    <*ASSERT t.refCnt >= 0*>
    IF t.refCnt = 0 THEN 
      RawIO.Close(t.fh);
      EVAL openLogs.delete(t.name, dummy);
    END;
  END;
END Close;

PROCEDURE TakeCheckPoint (log: T) =
BEGIN
  LOCK log.cpMu DO
    IF log.cpCount >= CheckPointInterval THEN
      log.cpCount := 0;
    ELSE
      INC(log.cpCount);
    END;
  END;
END TakeCheckPoint;

PROCEDURE AppendWord16 (VAR buf: ARRAY OF CHAR;
			VAR idx: INTEGER;
			val: INTEGER) =
  BEGIN
    VIEW(SUBARRAY(buf, idx, 2), Ctypes.unsigned_short) := val;
    INC(idx, 2);
  END AppendWord16;

PROCEDURE AppendWord32 (VAR buf: ARRAY OF CHAR;
			VAR idx: INTEGER;
			val: INTEGER) =
  BEGIN
    VIEW(SUBARRAY(buf, idx, 4), Ctypes.int) := val;
    INC(idx, 4);
  END AppendWord32;

PROCEDURE AppendWord64 (VAR buf: ARRAY OF CHAR;
			VAR idx: INTEGER;
			val: INTEGER) =
  BEGIN
    VIEW(SUBARRAY(buf, idx, 8), INTEGER) := val;
    INC(idx, 8);
  END AppendWord64;

PROCEDURE ViewCommonHeader (READONLY buf: ARRAY OF CHAR;
			    VAR hdr: CommonHeader; idx: CARDINAL): BOOLEAN =
  BEGIN
    hdr := VIEW(SUBARRAY(buf, idx, BYTESIZE(CommonHeader)), CommonHeader);
    
    WITH x = VIEW(hdr, ARRAY [0..1] OF INTEGER) DO
      IF hdr.ivsum # Word.And(Word.Not(Word.Plus(x[0], x[1])),
			      16_FFFFFFFF) THEN
	IO.Put("corrupt log!!\n");
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END ViewCommonHeader;

PROCEDURE ViewRawHeader (READONLY buf: ARRAY OF CHAR;
			 VAR hdr: RawHeader; idx: CARDINAL): BOOLEAN =
  BEGIN
    hdr := VIEW(SUBARRAY(buf, idx, BYTESIZE(RawHeader)), RawHeader);
    
    WITH x = VIEW(hdr, ARRAY [0..1] OF INTEGER) DO
      IF hdr.ivsum # Word.And(Word.Not(Word.Plus(x[0], x[1])),
			      16_FFFFFFFF) THEN
	IO.Put("corrupt log!!\n");
	RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END ViewRawHeader;

PROCEDURE ViewWord32 (READONLY buf: ARRAY OF CHAR;
		      VAR idx: INTEGER): INTEGER =
  VAR val: Ctypes.int;
  BEGIN
    val := VIEW(SUBARRAY(buf, idx, 4), Ctypes.int);
    INC(idx, 4);
    RETURN val;
  END ViewWord32;

PROCEDURE ViewWord64 (READONLY buf: ARRAY OF CHAR;
		      VAR idx: INTEGER): INTEGER =
  VAR val: INTEGER;
  BEGIN
    val := VIEW(SUBARRAY(buf, idx, 8), INTEGER);
    INC(idx, 8);
    RETURN val;
  END ViewWord64;

PROCEDURE FlushLog () =
  VAR
    itr: TextRefTbl.Iterator;
    blah: TEXT;
    r: REFANY;
  BEGIN
    LOCK openLogsMu DO
      itr := openLogs.iterate();
      WHILE itr.next(blah, r) DO
	Flush(r, LAST(LSN));
      END;
    END;
  END FlushLog;

PROCEDURE Nuke (fileName: TEXT) =
  VAR
    buf := NEW(REF ARRAY OF CHAR, BlockSize);
    fh: RawIO.T;
    r: REFANY;
    t: T;
  BEGIN
    IO.Put("Nuking the log " & fileName & ".\n");
    fh := RawIO.Open(fileName, 0);
    RawIO.Write(fh, buf^, 0); (* nuke the content. *)
    RawIO.Close(fh);

    LOCK openLogsMu DO 
      IF NOT openLogs.get(fileName, r) THEN RETURN; END;
      t := NARROW(r, T);

      t.startLSN := -1;
      t.lastFlushedLSN := 0;
      t.nextLSN := 0;
      t.logFirstLSN := 0;
      t.logTail := 0;
      t.firstLSN := 0;
    END;
  END Nuke;

PROCEDURE PrintStat (t: T) =
  VAR 
  BEGIN
    LOCK t DO
      IO.Put("log name: " & t.name & ".\n");
      IO.Put("size: 16_" & Fmt.Int(t.size, 16) & ".\n");
      IO.Put("lastFlushedLSN: 16_" & Fmt.Int(t.lastFlushedLSN, 16) & ".\n");
      IO.Put("nextLSN: 16_" & Fmt.Int(t.nextLSN, 16) & ".\n");
      IO.Put("startLSN: 16_" & Fmt.Int(t.startLSN, 16) & ".\n");
      IO.Put("delta: 16_" & Fmt.Int(t.delta, 16) & ".\n\n");
    END;
  END PrintStat;
  
BEGIN
  IF SpyTime THEN
    logTimer := Spy.Create("trans:log", FALSE);
    logFlushTimer := Spy.Create("trans:logflush", FALSE);
  END;
END WAL.
