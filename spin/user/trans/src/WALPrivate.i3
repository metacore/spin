(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 28-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE WALPrivate;
IMPORT WAL, RawIO;

CONST CheckPointInterval = 3;

REVEAL WAL.T = MUTEX BRANDED OBJECT
  fh: RawIO.T;
  name: TEXT; (* name of the log file *)
  size: CARDINAL; (* byte size of the log device. This must be
		     multiple of "BlockSize". *)
  refCnt: CARDINAL; (* currently shared by this number of storages. *)

  lastFlushedLSN: WAL.LSN; (* last log record that is flushed into the
			      log file. *)
  nextLSN: WAL.LSN; (* the LSN to be assigned to next newcoming log
		       invariant: "lastFlushedLSN <= newLSN" *)
  startLSN: WAL.LSN; (* the last LSN at the time the log is first
			opened. *)
  firstLSN: WAL.LSN; (* the first valid LSN in the log. *)
  firstBlock: CARDINAL; (* byte offset of the log block on which "startLSN"
			   resides. *)
  firstBlockLSN: WAL.LSN; (* LSN of the first log record of "firstBlock". *)
  delta: CARDINAL; (* Difference betweeh LSN value and the byte location
		      of the record within the log file.
		      invariant: "delta = firstBlockLSN - firstBlock" *)
  
  log: REF ARRAY OF CHAR; (* append only buffer. *)
  logFirstLSN: WAL.LSN;  (* first LSN of the records in the log buf *)
  
  logTail: [0 .. WAL.BlockSize]; (* Current tail of the log. indexes "log" *)

  crashBuf: REF ARRAY OF CHAR; (* tmp used on recovery. *)
  crashRecords: REF ARRAY OF [0 .. WAL.BlockSize];
  (* crashRecords holds the start idx of each records in the crashBuf.
    this speeds up the backward record search  *)
  crashLSNIdx: INTEGER;
  (* crashLSNIdx serves as the hint from which to
   start the crashRecords search *)
  crashFirstLSN: WAL.LSN;  (* first LSN of the records in the crash buf
			     This is always equal to the lsn of
			     the record at crashRecords[0]. 
			     | t.crashFirstLSN MOD BlockSize = 0
			     *)
  crashLastLSN: WAL.LSN; (* last LSN of the records in the crash buf *)
  
  cpMu: MUTEX; (* checkpoint mutex *)
  cpCount: [0 .. CheckPointInterval];
END;

PROCEDURE Recover(log: WAL.T);
  
PROCEDURE Nuke(fileName: TEXT);
(* Erase all the contents. This has to be called then the log "fileName" is
   not opened. It will cause havoc otherwise. *)
  
END WALPrivate.
