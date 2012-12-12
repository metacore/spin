(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 * html
 *)

(*
   This module manages the transaction log.

   First, we define the terminology.
   A <dfn>log record</dfn> holds one logical block of information.
   It can be one of several types(either one of "Type" type).

   A <dfn>log block</dfn> is a sequence of log records. It's size is fixed at
   "BlockSize". The log records in a log blocks are lined in the
   increasing LSN order. 

 *)

INTERFACE WAL;
IMPORT TID, SID, Ctypes;

TYPE
  LSN = INTEGER;
  (* LSN is a Log Serial Number, as defined in Aries & Exodus.
   
     It is defined to be the disk offset of the log record.
     If the log records overlap, then the LSN is:
   
     (# of overlaps so far) * (size of the log device) + disk offset.
     
     It has to be >= 8 bytes long.
   *)
CONST
  VoidLSN = LAST(LSN);
  (* "VoidLSN" is used to indicate that the lsn is not valid. For example,
     "prevLSN" field of commonheader is VoidLSN if it is the first log
     record of the transaction. *)
CONST
  BlockSize = 8192*16;
(* BlockSize affects the performance quite a bit. If you make it bigger,
   you might have fewer # of log flushes, but it decreases the dcache
   locality. *)
  
TYPE
  RecoveryResult = {OK, NoSuchStorage, OtherError};
  (* This type is used as a return value for "StorageLocal.Redo" and "Undo"
     callbacks. *)
  
  T <: REFANY;
  (* T represents an open log device. *)

  RawHeader = RECORD
    lsn: LSN;		  (* LSN of this record*)
    sid: SID.T;
    type: Ctypes.unsigned_short;
    size: Ctypes.unsigned_short; (* # of bytes following excluding ivsum. *)
    ivsum: Ctypes.unsigned_int; (* bit negation of 32bit sum of above
				   fields *)
  END;
  (* Raw portion is common to all types of records *)

  CommonHeader = RECORD
    (* damn m3 doesn't allow us to extend a record *)
    lsn: LSN;		  (* LSN of this record*)
    sid: SID.T;
    type: Ctypes.unsigned_short;
    size: Ctypes.unsigned_short; (* # of bytes following excluding ivsum. *)
    ivsum: Ctypes.unsigned_int; (* bit negation of 32bit sum of above
				   fields *)
    tid: TID.T;
    prevLSN: LSN;
  END;
  
CONST
  CommonHeaderSize = BYTESIZE(TID.T) + BYTESIZE(LSN);
  (* Overhead of common header. This value has to be added
     to the "size" field in the raw header. *)
  
  (* CommonHeader portion is common to Undo/Redo/Commit/Abort records *)
TYPE
  PrepareLockEntry = RECORD
    from: Ctypes.unsigned_int; (* Actually, the type is "Buffer.Offset". *)
    end: Ctypes.unsigned_int;
    mode: Ctypes.unsigned_int;
    padding: Ctypes.unsigned_int;
  END;
    
    
  Type = {
	  Redo, (* TID:8 PREVLSN:8 POS:8 CHAR\[SIZE\] *)
	  Undo, (* TID:8 PREVLSN:8 POS:8, CHAR\[SIZE\] *)
	  S1PCommit, (* TID:8 PREVLSN:8 *)
	  SPrepare, (* TID:8 PREVLSN:8 nEntry:8 PrepareLockEntry[NPAGES]
		       nEntry records number of entries locked by the
		       transaction. *)
	  SCommit, (* TID:8 PREVLSN:8 *)
	  SAbort, (* TID:8 PREVLSN:8 *)
	  TPrepared, (* TID:8 PREVLSN:8 *) 
	  TAbort, (* TID:8 PREVLSN:8 *) 
	  TCommitted, (* TID:8 PREVLSN:8 *) 
	  CheckPoint, (* Storage check point
			 |t-record-size:8;
			 |<tid,lastLSN>*t-record-size;
			 |<dirty-page-pos,recoveryLSN>;
			 *)
	  ShutDown, (* *)
	  EOB (* end of log block filler. *)
  };
  (* Type of a log record *)

PROCEDURE TypeName(t: Type): TEXT;
(* Return a human understandable name for the type. *)
  
CONST DefaultLogSize = 4 * BlockSize;
  
(* Format of a log record:

   LSN:8, TID:8, Type:4, ...
   
   No log record crosses the BlockSize boundary
   (this is to find the end of the log easily).
*)
  
PROCEDURE Open(fileName: TEXT): T;
PROCEDURE FileName(fd: T): TEXT;

PROCEDURE WriteCommit(fd: T;
		      sid: SID.T;
		      tid: TID.T;
		      type: Type;
		      prevLSN: LSN;
		      READONLY aux: ARRAY OF CHAR): LSN;
(* "type" has to be Commit, Prepare or Abort.
 
 "prevLSN" specifies the log record that belong to the same
 transaction, and that immediately precedes this one.

   "aux" must be 8 byte aligned. This is now used only by Prepare to
   store lock info. 

 *)

PROCEDURE WriteRedo(fd: T;
		    storage: SID.T;
		    tid: TID.T;
		    type: Type;
		    prevLSN: LSN;
		    pos: INTEGER;
		    READONLY buf: ARRAY OF CHAR): LSN;
(* type has to be Redo or Undo *)

PROCEDURE WriteOther(fd: T;
		     storage: SID.T;
		     type: Type;
		     READONLY buf: ARRAY OF CHAR): LSN;

PROCEDURE Read(fd: T; lsn: LSN;
	       buf: REF ARRAY OF CHAR;
	       from := 0): INTEGER;
(* Read the log record specified by "lsn".
   Return the size of the log record.

 Read only the "CommonHeader" portion of the log record if the
 "buf" turns out to be too small to hold the whole record.
 You can check if you've read the whole record by checking if
 the retval > "NUMBER(buf) - from". 
 
 *)

(* The below two are used by recovery proc in the storage manager. *)

PROCEDURE ReadPrevLog(t: T; lsn: LSN; VAR hdr: RawHeader): BOOLEAN;
(* Read the log record just before "lsn".
 Pre: "t" is locked*)

PROCEDURE ReadPrevLogWRTStorage(t: T; sid: SID.T;
				lsn: LSN; VAR hdr: RawHeader): BOOLEAN;
(* Read the log for "sid" that precedes "lsn".

   Pre: "t" is *not* locked.
 *)

PROCEDURE GetNextLSN (READONLY hdr: RawHeader): LSN;

PROCEDURE Flush(fd: T; lsn: LSN := LAST(LSN));
(* Flush all the log records currently logged.
 "lsn" specifies the LSN of the last log record that is to be flushed.
 If it is omitted, all the records in the buffer are flushed. *)

PROCEDURE Close(fd: T);

PROCEDURE TakeCheckPoint(fd: T);
  
PROCEDURE FlushLog();
  (* Flush all the log files *)

PROCEDURE PrintStat(log: T);
  (* Print info. *)
  
(* Some utilities *)

PROCEDURE AppendWord16((*OUT*)VAR buf: ARRAY OF CHAR;
		       (*INOUT*)VAR idx: INTEGER;
		       val: INTEGER);
(*
 Write a 32bit integer "val" into buf\[idx\] .. buf\[idx+3\].
 *)
PROCEDURE AppendWord32((*OUT*)VAR buf: ARRAY OF CHAR;
		       (*INOUT*)VAR idx: INTEGER;
		       val: INTEGER);

(*
 Write a 64bit integer "val" into buf\[idx\] .. buf\[idx+7\].
 *)
PROCEDURE AppendWord64((*OUT*)VAR buf: ARRAY OF CHAR;
		       (*INOUT*)VAR idx: INTEGER;
		       val: INTEGER);

(* Get the header content from the buffer *)
PROCEDURE ViewCommonHeader(READONLY buf: ARRAY OF CHAR;
			   (*OUT*)VAR hdr: CommonHeader;
			   idx: CARDINAL): BOOLEAN;
  
PROCEDURE ViewRawHeader(READONLY buf: ARRAY OF CHAR;
			(*OUT*)VAR hdr: RawHeader;
			idx: CARDINAL): BOOLEAN;
			

PROCEDURE ViewWord32(READONLY buf: ARRAY OF CHAR;
		     (*INOUT*)VAR idx: INTEGER): INTEGER;

PROCEDURE ViewWord64(READONLY buf: ARRAY OF CHAR;
		     (*INOUT*)VAR idx: INTEGER): INTEGER;

END WAL.
