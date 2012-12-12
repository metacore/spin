(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE ActiveTrans;
IMPORT TransT;
IMPORT ActiveTransRep, ActiveTransQ;
IMPORT WAL;
IMPORT CheckPoint;
IMPORT LockMan;
IMPORT BufferQ;

PROCEDURE Init (at: T; tr: TransT.T) =
  BEGIN
    at.tr := tr;
    at.lastLSN := -1;
    at.firstLSN := LAST(WAL.LSN);
    at.dirty := FALSE;
    at.state := CheckPoint.RState.Active;
    at.pages := BufferQ.NewHeader();
    LockMan.Init(at);
    at.nRedoLogBytes := 0;
    at.nUndoLogBytes := 0;
    at.nPagesMapped := 0;
    at.nPagesModified := 0;
    at.nPagesPagedIn := 0;
    at.nPagesPurged := 0;
  END Init;

PROCEDURE Destroy (at: T) =
  BEGIN
    <*ASSERT BufferQ.Empty(at.pages)*>
    BufferQ.Free(at.pages);

    (* Free myself *)
    ActiveTransQ.Remove(at);
    ActiveTransQ.Free(at);
  END Destroy;
  
BEGIN
END ActiveTrans.










