(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE BufferPurge;
IMPORT PhysAddr;
IMPORT Buffer, BufferRep;
IMPORT BufferQ, ArrayUtils;<*NOWARN*>
IMPORT IO, Fmt;<*NOWARN*>

IMPORT TransRep;
IMPORT ActiveTrans, ActiveTransRep;

IMPORT TransServiceRep;
IMPORT Storage, StorageRep, StorageLocal;
IMPORT Debugger;
IMPORT WAL;
IMPORT VMDebug;
IMPORT TransMode;
FROM TransUtils IMPORT Debug, Msg;

PROCEDURE PurgePages (st: Storage.T; shadow: BOOLEAN;
		      READONLY pos: ARRAY OF INTEGER): BOOLEAN =
  VAR
    bufs: ARRAY [0 .. 63] OF Buffer.T;
    buf: Buffer.T;
    diff := Buffer.AllocDiff();
    diffSize: CARDINAL;
    at: ActiveTrans.T;
    lsn: WAL.LSN := 0;
  BEGIN
    LOCK st.mu DO 
      FOR i := 0 TO LAST(pos) DO
	bufs[i] := Buffer.LockWithoutPin(st, pos[i]);
	IF bufs[i] # NIL THEN
	  buf := bufs[i];
	  <*ASSERT buf.pos = pos[i]*>
	  <*ASSERT buf.st = st*>

	  (* Make sure that the frame is not mapped anywhere.
	     State may not be zombie when the shadow frame is chosen
	     as the victim. *)
	  WITH state = PhysAddr.GetState(buf.frame) DO
	    IF state # PhysAddr.State.Zombie THEN
	      IF Debug AND state = PhysAddr.State.Dead THEN
		Debugger.Enter();
	      END;
	      PhysAddr.ChangeState(buf.frame, PhysAddr.State.Reclaimed);
	    END;
	  END;

	  IF Debug THEN
	    (* Calculate the checksum for this page. The checksum is
	       compared when the page is brought back later. *)
	    IF st.checkSum = NIL THEN
	      st.checkSum := NEW(REF ARRAY OF INTEGER,
				 st.memObj.stat().virtualSize);
	    END;
	    IF buf.dirty THEN
	      (* Update the checksum *)
	      st.checkSum[buf.pos] := VMDebug.CalculateChecksum(buf.frame);
	    ELSE
	      (* Make sure that the checksum matches. *)
	      VAR sum := VMDebug.CalculateChecksum(buf.frame);
	      BEGIN
		IF st.checkSum[buf.pos] # 0
		   AND sum # st.checkSum[buf.pos] THEN
		  Msg("purge: checksum altered.\n");
		  Debugger.Enter();
		END;
	      END;
	    END;
	  END;
	  
	  lsn := MAX(lsn, buf.lsn);
	  
	  IF buf.at # NIL THEN
	    <*ASSERT buf.dirty*>
	    (* A transaction is currently trying to modify this page.
	       Create an undo record to satisfy WAL. *)
	    
	    at := buf.at;
	    INC(at.nPagesPurged);
	    at.needUndoLogScan := TRUE;
	    IF buf.dirty AND buf.shadow # NIL THEN
	      <*ASSERT NOT(TransMode.T.PageGrainLogging IN at.tr.mode)*>
	      diffSize := Buffer.ReverseDiff(buf, diff^);
	      INC(at.nUndoLogBytes, diffSize);
	      at.lastLSN := st.writeRedo(at.tr.tid, WAL.Type.Undo,
					 at.lastLSN, buf.pos,
					 SUBARRAY(diff^, 0, diffSize));
	      buf.lsn := at.lastLSN;
	    END;
	    
	    lsn := at.lastLSN;
	    IF Debug THEN
	      (* Make sure buf is linked to at. *)
	      VAR
		itr := BufferQ.Iterate(at.pages);
		tmp: Buffer.T;
		found := FALSE;
	      BEGIN
		WHILE BufferQ.NextItr(itr, tmp) DO
		  <*ASSERT tmp.at = at*>
		  IF tmp = buf THEN
		    found := TRUE;
		    EXIT;
		  END;
		END;
		IF NOT found THEN Debugger.Enter(); END;
	      END;
	    END;
	    (* Unlink buf from at *)
	    buf.at := NIL;
	    BufferQ.Remove(buf);
	  END;
	  IF shadow AND buf.shadow # NIL THEN
	    (* just throw away the shadow page. *)
	    PhysAddr.Deallocate(buf.shadow);
	    buf.shadow := NIL;
	    bufs[i] := NIL;
	    Buffer.Unlock(buf);
	  END;
	END;

      END;

      (* Flush the log before writing the actual page contents.
	 This is a must to satisfy WAL principle.*)
      IF NOT shadow THEN
	Buffer.FreeDiff(diff);
	st.flushLog(lsn);
      END;

      FOR i := 0 TO LAST(pos) DO
	buf := bufs[i];
	IF buf # NIL THEN 
	  IF buf.dirty THEN
	    IF ISTYPE(st, StorageLocal.T) THEN
	      Buffer.Write(buf);
	      buf.dirty := FALSE;
	    ELSE
	      Msg("remote storage not supported now.\n");
	      Debugger.Enter();
	    END;
	  ELSE
	    IF Debug AND ISTYPE(st, StorageLocal.T) THEN
	      (* Make sure that the contents are not changed since
		 it's read in from disk.*)
	      EVAL Buffer.CheckBufferIsNotModified(buf);
	    END; 
	  END;
	  IF Debug THEN 
	    WITH state = PhysAddr.GetState(buf.frame) DO
	      <*ASSERT state = PhysAddr.State.Reclaimed
	            OR state =PhysAddr.State.Zombie*>
	    END;
	  END;
	  Buffer.Delete(buf);
	END;
      END;	
    END;
    RETURN TRUE;
  END PurgePages;

BEGIN
END BufferPurge.

