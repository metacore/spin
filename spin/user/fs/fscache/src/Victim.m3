(*
 * Copyright 1994-1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)

(* The default Victim implementation maintains a global LRU list of
   blocks in a double-linked list.  The head of the list is the
   most recently accessed, and the tail is the least recently accessed. *)

MODULE Victim;

IMPORT BaseMObjCache, FileMObjCache, Buffer;
IMPORT DoubleList;
IMPORT IO, Error;
(* IMPORT Fmt; *)

(* called when a buffer is allocated and granted to a file mem obj *)
PROCEDURE AllocBuffer(bcache: BaseMObjCache.T; 
  <*UNUSED*>fcache: FileMObjCache.T; buf: Buffer.T) =
  BEGIN
    IF bcache.victimlist = NIL THEN
      bcache.victimlist := NEW(DoubleList.T).init();
    END;
    TRY
      bcache.victimlist.addHead(buf);
    EXCEPT
    | DoubleList.InList =>
      IO.Put("Victim.AllocBuffer: buffer already in LRU list for " &
        bcache.print() & "!\n");
      TRY
        bcache.victimlist.moveToHead(buf);
      EXCEPT
      | DoubleList.NotInList =>
        IO.Put("Victim.AllocBuffer: victim list insanity for " &
          bcache.print() & "?\n");
      END;
    END;
  END AllocBuffer;

(* called when a buffer is returned to the base mem obj and placed in
   the free buffer pool *)
PROCEDURE FreeBuffer(bcache: BaseMObjCache.T; 
  <*UNUSED*>fcache: FileMObjCache.T;  buf: Buffer.T) =
  BEGIN
    IF bcache.victimlist = NIL THEN
      IO.Put("Victim.FreeBuffer: missing LRU list for " & 
        bcache.print() & "!\n");
    END;
    TRY
      bcache.victimlist.remove(buf);
    EXCEPT
    | DoubleList.NotInList =>
      IO.Put("Victim.FreeBuffer: buffer not in LRU list for " &
        bcache.print() & "!\n");
    END;
  END FreeBuffer;

(* called when a buffer in a file mem obj is referenced *)
PROCEDURE Ref(bcache: BaseMObjCache.T; <*UNUSED*>fcache: FileMObjCache.T;
  <*UNUSED*>blockno: FileMObjCache.BlockNo; buf: Buffer.T; 
  <*UNUSED*>reftype: RefType) =
  BEGIN
    IF bcache.victimlist = NIL THEN
      IO.Put("Victim.Ref: missing LRU list for " & 
        bcache.print() & "!\n");
    END;
    TRY
      bcache.victimlist.moveToHead(buf);
    EXCEPT
    | DoubleList.NotInList =>
      IO.Put("Victim.Ref: buffer not in LRU list for " &
        bcache.print() & "!\n");
    END;
  END Ref;

(* called by a file mem obj when a buffer is needed and the free buffer
   pool is empty. returns NIL if no buffers can be stolen. *)
PROCEDURE StealBuffer(bcache: BaseMObjCache.T; 
  fcache: FileMObjCache.T;  blockno: FileMObjCache.BlockNo) : Buffer.T =
  VAR
    buf: Buffer.T := NIL;
    relbuf: Buffer.T := NIL;
  BEGIN
    IF bcache.victimlist = NIL THEN
      IO.Put("Victim.StealBuffer: missing LRU list for " & 
        bcache.print() & "!\n");
      RETURN NIL;
    END;

    (* choose the least recently used - on tail of list *)
    TRY
      buf := bcache.victimlist.removeTail();
    EXCEPT
    | DoubleList.Empty =>
      IO.Put("Victim.StealBuffer: LRU list empty for " &
        bcache.print() & "!\n");
      RETURN NIL;
    END;

    (* steal it from the file mem obj where it now lives *)
    TRY
      relbuf := NARROW(buf.owner,FileMObjCache.T).releaseBlock(buf.index);
    EXCEPT
    | Error.E(ec) =>
      IO.Put("Victim.StealBuffer: unable to steal buffer for base cache " &
        bcache.print() & ": " & ec.message() & "\n");
      RETURN NIL;
    END;

    (* see if the file mem obj buf returned a different buffer. the
       current implementation will not do this, but you never know
       how the implementation might change. *)
    IF relbuf # buf THEN
      (* put buf back on the tail of the LRU list - should it be head? *)
      TRY
        bcache.victimlist.addTail(buf);
      EXCEPT
      | DoubleList.InList =>
        TRY
          bcache.victimlist.moveToTail(buf);
        EXCEPT
        | DoubleList.NotInList =>
          (* skip *)
        END;
      END;
      (* remove the released buffer from the LRU list *)
      TRY
        bcache.victimlist.remove(relbuf);
      EXCEPT
      | DoubleList.NotInList =>
        (* skip *)
      END;
      buf := relbuf;
    END;

    (* the victim buffer is now not in the LRU list.  put it in at the
       head of the list, since it is about to be accessed. *)
    TRY
      bcache.victimlist.addHead(buf);
    EXCEPT
    | DoubleList.InList =>
      TRY
        bcache.victimlist.moveToHead(buf);
      EXCEPT
      | DoubleList.NotInList =>
        (* skip *)
      END;
    END;

    (* reset the buffer container data *)
    buf.owner := fcache;
    buf.index := blockno;

    RETURN buf;
  END StealBuffer;

PROCEDURE Stat(bcache: BaseMObjCache.T; 
  VAR fcacheNextVictim: FileMObjCache.T; VAR nextIndex: INTEGER;
  VAR fcacheNotVictim: FileMObjCache.T; VAR notIndex: INTEGER) =
  VAR 
    nextbuf, notbuf: Buffer.T;
  BEGIN
    IF bcache.victimlist = NIL THEN
      IO.Put("Victim.Stat: missing LRU list for " & 
        bcache.print() & "!\n");
      fcacheNextVictim := NIL;
      fcacheNotVictim := NIL;
      RETURN;
    END;

    TRY
      nextbuf := bcache.victimlist.removeTail();
    EXCEPT
    | DoubleList.Empty =>
      fcacheNextVictim := NIL;
      nextIndex := 0;
      fcacheNotVictim := NIL;
      notIndex := 0;
      RETURN;
    END;
    TRY
      bcache.victimlist.addTail(nextbuf);
    EXCEPT
    | DoubleList.InList =>
      (* skip *)
    END;
    fcacheNextVictim := NARROW(nextbuf.owner, FileMObjCache.T);
    nextIndex := nextbuf.index;

    TRY
      notbuf := bcache.victimlist.removeHead();
    EXCEPT
    | DoubleList.Empty =>
      fcacheNotVictim := NIL;
      notIndex := 0;
      RETURN;
    END;
    TRY
      bcache.victimlist.addHead(notbuf);
    EXCEPT
    | DoubleList.InList =>
      (* skip *)
    END;
    fcacheNotVictim := NARROW(notbuf.owner, FileMObjCache.T);
    notIndex := notbuf.index;
  END Stat;

BEGIN
END Victim.
