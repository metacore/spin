(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Added validity check.
 * 31-Aug-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE PhysAddr;
IMPORT Q;
IMPORT Word;
IMPORT IO;
IMPORT Fmt;
IMPORT CPU;
IMPORT MemoryObject;
IMPORT Umman, Utypes;
IMPORT TransOS;
IMPORT Thread;
IMPORT Debugger;
IMPORT Cstring, Ctypes;
IMPORT UNIXUtils;

FROM TransUtils IMPORT Debug;

VAR
  nPages : CARDINAL := 0;
  mu := NEW(MUTEX);
  cond := NEW(Thread.Condition);
  
REVEAL T = Q.T BRANDED OBJECT
  state: State;
  valid: BOOLEAN;
  tag: Tag;
END;

TYPE Closure = Thread.Closure OBJECT
  victim: T;
OVERRIDES
  apply := Apply;
END;

VAR
  queue: ARRAY State OF Q.T;

PROCEDURE Apply (c: Closure): REFANY =
  VAR
    vmObj: MemoryObject.T := c.victim.tag.obj;
  BEGIN
    EVAL vmObj.pager.pageOut(c.victim.tag.off, c.victim, TRUE);
    Deallocate(c.victim);
    Thread.Signal(cond);
    RETURN NIL;
  END Apply;

PROCEDURE InvalidatePageMapping (v: T) =
  VAR 
    mObj: MemoryObject.T := v.tag.obj;
    pos := v.tag.off * CPU.PAGESIZE;
  BEGIN
    IF Umman.mprotect(LOOPHOLE(mObj.base+pos, Utypes.caddr_t),
		      CPU.PAGESIZE, 0) # 0 THEN
      UNIXUtils.Perror("mprotect:invalidate:");
    END;
  END InvalidatePageMapping;

PROCEDURE EnableMapping (v: T) =
  VAR 
    mObj: MemoryObject.T := v.tag.obj;
    pos := v.tag.off * CPU.PAGESIZE;
  BEGIN
    IF Umman.mprotect(LOOPHOLE(mObj.base+pos, Utypes.caddr_t),
		      CPU.PAGESIZE,
		      Word.Or(Umman.PROT_READ, Umman.PROT_WRITE)) < 0 THEN
      UNIXUtils.Perror("mprotect:enable:");
    END;
  END EnableMapping;
  
PROCEDURE GetState (t: T): State=
  BEGIN
    RETURN t.state;
  END GetState;
  
PROCEDURE GetVictims (t: REFANY; VAR frames: ARRAY OF T): CARDINAL =
  VAR
    i: INTEGER;
    v: T;
  BEGIN
    WHILE i < NUMBER(frames) DO
      v := Q.RemoveTail(queue[State.Active]);
      IF v = NIL OR NOT ISTYPE(v, T) THEN
	EXIT;
      END;
      IF v.tag.obj = t THEN
	v.valid := FALSE;
	v.state := State.Zombie;
	frames[i] := v;
	Q.InsertHead(queue[State.Zombie], v);
	(*InvalidatePageMapping(v);*)
	INC(i);
      ELSE
	Q.InsertHead(queue[State.Active], v);
      END;
    END;
    RETURN i;    
  END GetVictims;
  
PROCEDURE GetTag (t: T): Tag =
  BEGIN
    RETURN t.tag;
  END GetTag;
PROCEDURE Allocate (VAR tag: Tag): T =
  VAR
    mObj: MemoryObject.T := tag.obj;
    pos := tag.off * CPU.PAGESIZE;
    t: T;
    victim: T;
  BEGIN
    LOCK mu DO 
      WHILE nPages >= MaxPages DO
	WITH v = Q.RemoveTail(queue[State.Active]) DO
	  IF NOT ISTYPE(v, T) THEN
	    IO.Put("Pageout victim queue is empty. Try increasing -bufsize.\n");
	    TransOS.Exit(1);
	  END;
	  victim := v;
	END;
	victim.valid := FALSE;
	victim.state := State.Zombie;
	Q.InsertHead(queue[State.Zombie], victim);
	(*InvalidatePageMapping(victim); -- to allow Access on victim pages efficiently*) 
	IF Debug THEN
	  IO.Put("purging " & Fmt.Int(victim.tag.off) & ".\n");
	END;
	EVAL Thread.Fork(NEW(Closure, victim := victim));

	WHILE nPages >= MaxPages DO
	  Thread.Wait(mu, cond);
	END;
      END;
      
      t := NEW(T, tag := tag, valid := TRUE, state := State.Active);
    
      EnableMapping(t);
      Q.InsertHead(queue[State.Active], t);
      INC(nPages);
    END;

    RETURN t;
  END Allocate;

PROCEDURE Deallocate (t: T) =
  VAR
    memObj := NARROW(t.tag.obj, MemoryObject.T);
    off := t.tag.off;
  BEGIN
    LOCK mu DO 
      IF t.state = State.Dead THEN
	RETURN;
      END;
      
      IF Debug THEN IO.Put("dealloc " & Fmt.Int(t.tag.off) & ".\n"); END;
      DEC(nPages);
      Q.Remove(t);
      InvalidatePageMapping(t);
      t.valid := FALSE;
      t.state := State.Dead;
    END;
  END Deallocate;

PROCEDURE Access (t: T; callback : PROCEDURE (VAR buf : Content)) =
  VAR
    ptr: Word.T;
    mObj := NARROW(t.tag.obj, MemoryObject.T);
  BEGIN
    ptr := mObj.base + t.tag.off*CPU.PAGESIZE;
    callback(LOOPHOLE(ptr, UNTRACED REF Content)^);
  END Access;

PROCEDURE Copy (dst, src: T) =
  VAR
    ptr1, ptr2: Word.T;
    mObj1 := NARROW(dst.tag.obj, MemoryObject.T);
    mObj2 := NARROW(src.tag.obj, MemoryObject.T);
  BEGIN
    ptr1 := mObj1.base + dst.tag.off*CPU.PAGESIZE;
    ptr2 := mObj2.base + src.tag.off*CPU.PAGESIZE;

    EVAL Cstring.memcpy(LOOPHOLE(ptr1, Ctypes.void_star),
		   LOOPHOLE(ptr2, Ctypes.const_void_star),
		   CPU.PAGESIZE);
  END Copy;

PROCEDURE Read (t: T; off: CARDINAL; VAR buf: ARRAY OF CHAR) =
  VAR
    ptr: Word.T;
    mObj := NARROW(t.tag.obj, MemoryObject.T);
  BEGIN
    ptr := mObj.base + t.tag.off*CPU.PAGESIZE;
    WITH src = LOOPHOLE(ptr, UNTRACED REF ARRAY [0..CPU.PAGESIZE-1] OF CHAR)^ DO
      FOR i := 0 TO LAST(buf) DO 
	buf[i] := src[i+off];
      END;
    END;
  END Read;
  
PROCEDURE Write (t: T; off: CARDINAL; READONLY buf: ARRAY OF CHAR) =
  VAR
    ptr: Word.T;
    mObj := NARROW(t.tag.obj, MemoryObject.T);
  BEGIN
    ptr := mObj.base + t.tag.off*CPU.PAGESIZE;
    WITH dst = LOOPHOLE(ptr, UNTRACED REF ARRAY [0..CPU.PAGESIZE-1] OF CHAR)^ DO
      FOR i := 0 TO LAST(buf) DO
	dst[i+off] := buf[i];
      END;
    END;
  END Write;
  
PROCEDURE ChangeState (t: T; state: State) =
  BEGIN
    LOCK mu DO
      IF NOT t.valid THEN
	Debugger.Enter();
      END;
      Q.Remove(t);
      t.state := state;
      Q.InsertHead(queue[state], t);
      IF state = State.Active THEN
	EnableMapping(t);
      END;
    END;
  END ChangeState;


BEGIN
  MaxPages := 10;
  FOR i := FIRST(queue) TO LAST(queue) DO
    queue[i] := Q.NewHeader();
  END;
END PhysAddr.
