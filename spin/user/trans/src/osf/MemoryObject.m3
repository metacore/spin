(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 06-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE MemoryObject;
IMPORT PagerObject;
IMPORT TransCache;
IMPORT Word;
IMPORT Utypes;
IMPORT Umman;
IMPORT Unix;
IMPORT TransOS;
IMPORT IO, Fmt;
IMPORT CPU;
IMPORT UNIXUtils;
IMPORT Debugger; <*NOWARN*>
IMPORT Cstring;

REVEAL T = TPublic BRANDED OBJECT
  size: CARDINAL; (* byte size *)
OVERRIDES
  init := Init;
  map := Map;
  unmap := Unmap;
  virtualSize := VirtualSize;
END;

PROCEDURE Init (t: T; size: INTEGER; pager: PagerObject.T;
		<*UNUSED*>cache: TransCache.T): T =
  VAR
    caddr: Utypes.caddr_t;
  BEGIN
    t.enabled := NEW(REF ARRAY OF BOOLEAN, size);
    t.size := size * CPU.PAGESIZE;
    t.pager := pager;
    caddr := Umman.mmap(LOOPHOLE(0, Utypes.caddr_t), t.size,
			Word.Or(Umman.PROT_READ, Umman.PROT_WRITE),
			Word.Or(Umman.MAP_ANONYMOUS,Umman.MAP_PRIVATE), -1, 0);
    IF caddr = LOOPHOLE(-1, Utypes.caddr_t) THEN
      UNIXUtils.Perror("MemoryObject.Init:mmap");
      TransOS.Exit(2);
    END;
    t.base := LOOPHOLE(caddr, Word.T);
    RETURN t;
  END Init;

PROCEDURE Map (t: T; addr: Word.T) =
  VAR
    caddr: Utypes.caddr_t := LOOPHOLE(addr, Utypes.caddr_t);
    buf: REF ARRAY OF CHAR;
  BEGIN
    IO.Put("brk = 16_" & Fmt.Int(LOOPHOLE(Unix.sbrk(0), INTEGER), 16));
    IF t.base # -1 THEN
      buf := NEW(REF ARRAY OF CHAR, CPU.PAGESIZE*2);
      (* Here comes a terrible hack. The only case (i know of) that
	 map is called twice is when trans_load_heap examines the
	 header(first two pages). Thus, we just copy the first two
	 pages of the old region into the new region. *)
      IF Umman.mprotect(LOOPHOLE(t.base, Utypes.caddr_t),
			CPU.PAGESIZE*2,
			Word.Or(Umman.PROT_READ, Umman.PROT_WRITE)) # 0 THEN
	UNIXUtils.Perror("MemoryObject.Map:mprotect");
      END;
      
      EVAL Cstring.memcpy(ADR(buf[0]), 
			  LOOPHOLE(t.base, Utypes.caddr_t), 
			  CPU.PAGESIZE*2);
      EVAL Umman.munmap(LOOPHOLE(t.base, Utypes.caddr_t), t.size);
    END;
    
    caddr := Umman.mmap(LOOPHOLE(addr, Utypes.caddr_t), t.size,
			Word.Or(Umman.PROT_READ, Umman.PROT_WRITE),
			Word.Or(Umman.MAP_ANONYMOUS,Umman.MAP_PRIVATE), -1, 0);
    
    IF LOOPHOLE(caddr, Word.T) = -1 THEN
      UNIXUtils.Perror("MemoryObject.Map:mmap");
      TransOS.Exit(1);
    END;

    IF t.base # -1 THEN
      (* copy back the old contents. *)
      EVAL Cstring.memcpy(caddr, ADR(buf[0]), CPU.PAGESIZE*2);
    END;
    
    IF Umman.mprotect(caddr, t.size, 0) < 0 THEN 
      UNIXUtils.Perror("MemoryObject.Map:mprotect");
    END;
    
    t.base := LOOPHOLE(caddr, Word.T);
  END Map;

PROCEDURE Unmap (t: T; addr: Word.T) =
  VAR caddr: Utypes.caddr_t := LOOPHOLE(addr, Utypes.caddr_t);
  BEGIN
  END Unmap;
  
PROCEDURE VirtualSize (self: T): CARDINAL =
  BEGIN
    RETURN self.size DIV CPU.PAGESIZE;
  END VirtualSize;
  
BEGIN
END MemoryObject.
