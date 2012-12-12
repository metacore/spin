(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted.
 *	
 *)
MODULE TransPager;
IMPORT IO, Fmt;
IMPORT PagerObject;
IMPORT Storage, StorageProtected, StorageRep;
IMPORT TransGroup, TransProc;
IMPORT AddressSpace;
IMPORT ActiveTrans, ActiveTransRep;
IMPORT PhysAddr;
IMPORT Buffer;
IMPORT BufferPurge;
IMPORT Translation;
IMPORT CPU;
IMPORT Trap;
IMPORT LockMode;
IMPORT Protection;
IMPORT VMError;
IMPORT Strand;
IMPORT Spy;
IMPORT TransUtils;
IMPORT Transaction;
FROM TransUtils IMPORT DebugMsg, Msg;

TYPE
  PageNumber = PagerObject.PageNumber;
  PageCount = PagerObject.PageCount;
  
  MapInfo = RECORD
    space: AddressSpace.T;
    offset: CARDINAL; (* from what portion of memobj? *)
    from, len: CARDINAL; (* on what virtual address region? *)
    minMapped, maxMapped: CARDINAL;
  END;

CONST MaxMaps = 4;
REVEAL
  T = PagerObject.T BRANDED OBJECT
    st: Storage.T;
    maps: ARRAY [0 .. MaxMaps-1] OF MapInfo;
    (* XXX space can be mapped onto only 4 address space simultaneously. *)
  OVERRIDES
    pageIn := PageIn;
    pageOut := PageOut;
    pageMapNotify := PageMapNotify;
    mapNotify := MapNotify;
    unmapNotify := UnmapNotify;
  END;

VAR
  pageinSpy: Spy.T;

PROCEDURE Create (st: Storage.T): PagerObject.T =
  BEGIN
    RETURN NEW(T, st := st);
  END Create;

PROCEDURE PageIn (t: T; off: PagerObject.PageNumber;
		  type: INTEGER;
		  frame: PhysAddr.T;
		  VAR prot: Protection.T): PagerObject.ResultCode =
  VAR
    allocated := TRUE;
    buf: Buffer.T;
    proc: TransProc.T;
    tr: Transaction.T;
    at: ActiveTrans.T;
    lock: LockMode.T;
  BEGIN
    IF TransUtils.SpyTime THEN Spy.Enter(pageinSpy); END;
      
    proc := TransProc.Self();
    tr := proc.curTrans;
    IF tr = NIL THEN
      IO.Put("trans pageout while not in transaction.\n");
      RETURN PagerObject.ResultCode.DeviceFull;
    END;
    IF type = Trap.Write THEN
      prot := Protection.All;
      lock := LockMode.T.Write;
    ELSE
      prot := Protection.ReadOnly;
      lock := LockMode.T.Read;
    END;
    
    LOCK t.st.mu DO 
      at := StorageProtected.InternTrans(t.st, tr);
      TRY
	buf := t.st.pinFrame(at, lock, off, allocated, frame);
	Buffer.Unlock(buf);
	INC(at.nPagesPagedIn);
      EXCEPT
      | VMError.E(e) =>
	Msg("transpager.pagein: " & Fmt.Int(off) & ":" & VMError.Message(e));
	Strand.Yield();
	RETURN PagerObject.ResultCode.AlreadyPagedIn;
      END;
    END;

    IF TransUtils.SpyTime THEN Spy.Exit(pageinSpy); END;
    IF NOT allocated THEN
      IO.Put("pagein: already allocated.\n");
      RETURN PagerObject.ResultCode.AlreadyPagedIn;
    ELSE
      RETURN PagerObject.ResultCode.Success;
    END;
  END PageIn;

PROCEDURE PageOut (t: T; off: PagerObject.PageNumber;
		   <*UNUSED*>frame: PhysAddr.T;
		   <*UNUSED*>dirty: BOOLEAN): PagerObject.ResultCode =
  VAR
    offs: ARRAY [0 .. 16] OF INTEGER;
    frames: ARRAY [0 .. 15] OF PhysAddr.T;
    nFrames: CARDINAL;
  BEGIN
    offs[0] := off;
    nFrames := PhysAddr.GetVictims(t.st.memObj, frames);
    FOR i := 0 TO nFrames-1 DO
      offs[i+1] := PhysAddr.GetTag(frames[i]).off;
    END;
    IF BufferPurge.PurgePages(t.st, FALSE, SUBARRAY(offs, 0, nFrames+1)) THEN
      RETURN PagerObject.ResultCode.Success;
    ELSE
      IO.Put("pageout: already purged.\n");
      RETURN PagerObject.ResultCode.AlreadyPagedOut;
    END;
  END PageOut;

PROCEDURE PageMapNotify (t: T; <*UNUSED*>off: PagerObject.PageNumber;
			 space: REFANY; virt: PagerObject.PageNumber) =
  VAR
    i := FindMapIdx(t, space, virt);
  BEGIN
    WITH m = t.maps[i] DO
      m.minMapped := MIN(m.minMapped, virt);
      m.maxMapped := MAX(m.maxMapped, virt);
      <*ASSERT m.minMapped < m.from + m.len AND m.maxMapped >= m.from *>
    END;
  END PageMapNotify;

PROCEDURE FindMapIdx (t: T; space: REFANY;
		      virt: PagerObject.PageNumber): INTEGER =
  BEGIN
    FOR i := 0 TO LAST(t.maps) DO
      WITH m = t.maps[i] DO 
	IF m.space = space AND virt >= m.from AND virt < m.from + m.len THEN
	  RETURN i;
	END;
      END;
    END;
    <*ASSERT FALSE*>
  END FindMapIdx;
  
PROCEDURE MapNotify(t: T; from: PageNumber; len: PageCount;
		    space: REFANY; virtAddr: PageNumber) =
  BEGIN
    IF DebugMsg THEN
      Msg("transpager.mapnotify: off=", Fmt.Int(from*CPU.PAGESIZE,16), 
	  "@", Fmt.Int(virtAddr*CPU.PAGESIZE,16),
	  " len=", Fmt.Int(len*CPU.PAGESIZE,16));
    END;
    
    FOR i := 0 TO LAST(t.maps) DO
      WITH m = t.maps[i] DO 
	IF m.space = NIL THEN
	  m.space := space;
	  m.offset := from;
	  m.len := len;
	  m.from := virtAddr;
	  m.minMapped := LAST(CARDINAL);
	  m.maxMapped := FIRST(CARDINAL);
	  RETURN;
	END;
      END;
    END;
    <*ASSERT FALSE*>
  END MapNotify;
  
PROCEDURE UnmapNotify(t: T; from: PageNumber; len: PageCount;
		      space: REFANY; virtAddr: PageNumber) =
  VAR src, dst: [0 .. MaxMaps];
  BEGIN
    IF DebugMsg THEN
      Msg("transpager.unmapnotify: off=",
	  Fmt.Int(from*CPU.PAGESIZE,16), 
	  "@", Fmt.Int(virtAddr*CPU.PAGESIZE,16),
	  " len=", Fmt.Int(len*CPU.PAGESIZE,16));
    END;
    src := 0;
    dst := 0;
    WHILE src < MaxMaps DO
      IF t.maps[src].space = space AND 
	 t.maps[src].offset = from AND
	 t.maps[src].len = len AND
	 t.maps[src].from = virtAddr THEN
	INC(src);
      ELSE
	IF dst # src THEN
	  t.maps[dst] := t.maps[src];
	END;
	INC(src);
	INC(dst);
      END;
    END;
    <*ASSERT src # dst*>
    WHILE dst < MaxMaps DO
      t.maps[dst].space := NIL;
      INC(dst);
    END;

    (* Note: the memobj is not destroyed. *)
  END UnmapNotify;

PROCEDURE InvalidateMappingsForSpace (t: T; tr: TransGroup.T) =
  BEGIN
    TYPECASE tr OF
    | TransProc.T(proc) =>
      FOR i := 0 TO LAST(t.maps) DO
	WITH m = t.maps[i] DO
	  IF m.space = NIL THEN
	    EXIT;
	  ELSIF m.space = proc.proc THEN
	    IF DebugMsg THEN 
	      Msg("transpager: removing from 0x", Fmt.Int(m.minMapped),
		  " to 0x", Fmt.Int(m.maxMapped));
	    END;
	    Translation.RemoveMapping(m.space, m.minMapped, m.maxMapped+1);
	  END;
	END;
      END;
    ELSE
    END;
  END InvalidateMappingsForSpace;

  
BEGIN
  IF TransUtils.SpyTime THEN
    pageinSpy := Spy.Create("trans:pager-pagein", FALSE, TransUtils.NItr);
  END;
END TransPager.
