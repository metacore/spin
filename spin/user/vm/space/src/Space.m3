(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 07-Jan-97  becker at the University of Washington
 *	Removed UNSAFE
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made GetCurrent FUNCTIONAL
 *
 * 6-21-96  becker at the University of Washington
 *	Added Duplicate proc which returns a space identical to its arg.
 *	Added Clear to Deallocate entire space.
 *
 * 01-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to deal with arrays of size zero.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added GetTranslation (bershad) for debugger hack,
 *	and extra debug print on allocation.
 *
 * 07-Apr-96 Jan Sanislo (oystr) at the University of Washington
 *	Added MapPhysToVirt.  This is *not* intended as a general
 *	purpose routine.  See comments below.
 *
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added support for external refs
 *
 * 09-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Use IO and reader/writers.
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed Read and Write to update the current space. Restoring
 *      was too costly and useless for clients.
 *	Took out a check in Activate out of the double-locked region as
 *	per Brian's suggestion.
 *
 * 07-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Removed identity (doesn't belong in space)
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Console device.
 *
 * 31-Jul-95  Brian Bershad (bershad) at the University of Washington
 *	Added support for identity.
 *
 * 26-Jul-95  Stefan Savage (savage) at the University of Washington
 *	No longer external functions, space opaque
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed the implementation to call the external functions directly.
 *
 * 28-Dec-94  Stefan Savage (savage) at the University of Washington
 *      Created
 *
 *)
MODULE Space;

IMPORT AddressSpaceRep, PhysAddr, VirtAddr, Translation,
       Fmt, Word, IO;
IMPORT AddressMapQ, AddressMapEntry;
IMPORT ExternalRef, MemoryObject, SpaceInterface;
IMPORT PhysAddrPrivate;
IMPORT Protection;
IMPORT PagerObject;
IMPORT DefaultPager;
IMPORT VMTypes;
IMPORT VMError;
IMPORT MachineMem;
IMPORT ArrayUtils;
FROM MachineMem IMPORT PAGESIZE;

CONST Debug = FALSE;

PROCEDURE Create(): T =
  VAR
    newSpace: T;
  BEGIN
    newSpace := NEW(T).init(NIL);
    RETURN(newSpace);
  END Create;

PROCEDURE Duplicate (newSpace, space: T) RAISES {VMError.E} =
  VAR
    entry: AddressMapEntry.T;
    pageBuf:= NEW(REF ARRAY OF CHAR,PAGESIZE);
    len: CARDINAL;
    itr := AddressMapQ.Iterate(space.maps);
  BEGIN
    WHILE AddressMapQ.NextItr(itr, entry) DO
      len := entry.end - entry.from;
      newSpace.allocate(entry.from, len, FALSE);
      newSpace.map(entry.from, len,
		   NEW(MemoryObject.T).init(entry.from, DefaultPager.Create(len)),
		   0);
      FOR page := entry.from TO entry.end-1 DO
	Read(space, page*PAGESIZE, pageBuf^, PAGESIZE);
	Write(newSpace, pageBuf^, page*PAGESIZE, PAGESIZE);
      END;
    END;
  END Duplicate;

PROCEDURE Destroy (space: T) =
  BEGIN
    space.destroy();
  END Destroy;
    
PROCEDURE Activate(<*UNUSED*>space: T) =
  BEGIN
  END Activate;

PROCEDURE Protect(space: T; 
		  addr, size: Word.T; prot: Protection.T) RAISES {VMError.E} =
  BEGIN
    Translation.ChangeMapping(space, 
			      addr DIV PAGESIZE,
			      RoundUpToPage(Word.Plus(addr, size)), prot);
  END Protect; 

FUNCTIONAL PROCEDURE GetCurrent(): T = 
  BEGIN
    TYPECASE Translation.GetCurrent() OF
    | T(t) => RETURN t;
    ELSE RETURN NIL;
    END;
  END GetCurrent;

PROCEDURE RoundUpToPage (v: VirtAddr.Address): VirtAddr.Page =
  BEGIN
    RETURN (v-1) DIV PAGESIZE + 1;
  END RoundUpToPage;
  
PROCEDURE Allocate(space: T; VAR addr: Word.T; size: Word.T;
		   anywhere := FALSE) RAISES {VMError.E} =
  VAR
    mObj: MemoryObject.T;
    pageNum: VMTypes.PageNumber;
    pager: PagerObject.T;
    numPages: VMTypes.PageCount;
  BEGIN
    IF Debug THEN
      IO.Put("Allocating "&Fmt.Unsigned(size)&" bytes at "&Fmt.Unsigned(addr)&"\n");
    END;
    pageNum := MachineMem.BytesToPages(addr);
    numPages := MachineMem.BytesToPages(MachineMem.RoundToPage(size));
    pager := DefaultPager.Create(numPages);
    mObj := NEW(MemoryObject.T).init(numPages, pager);
    
    space.allocate(pageNum, numPages, anywhere);
    space.map(pageNum, numPages,mObj,0);
    addr := Word.Times(pageNum, PAGESIZE);
    IF Debug = TRUE THEN
      IO.Put("addr was "&Fmt.Unsigned(addr)&"\n");
    END;
  END Allocate;

PROCEDURE Deallocate(space: T; addr, size: Word.T) RAISES {VMError.E} =
  VAR
    pageNum: CARDINAL;
    numPages: CARDINAL;
  BEGIN
    IF Debug = TRUE THEN
      IO.Put("Deallocating "&Fmt.Unsigned(size)&" bytes from "&Fmt.Unsigned(addr)&"\n");
    END;
    pageNum := MachineMem.BytesToPages(addr);
    numPages := MachineMem.BytesToPages(MachineMem.RoundToPage(size));
    space.deallocate(pageNum, numPages);
  END Deallocate;

PROCEDURE Read(space: T; src: VirtAddr.Address; VAR dst: ARRAY OF CHAR;
	       size: INTEGER)
  	RAISES {VMError.E} =
  BEGIN
    IF size = -1 THEN size := NUMBER(dst); END;
    Translation.Read(space, src, SUBARRAY(dst, 0, size));
  END Read;

PROCEDURE Write(space: T;
		READONLY src: ARRAY OF CHAR;
		dst: VirtAddr.Address; size: INTEGER) RAISES {VMError.E} =
  BEGIN
    IF size = -1 THEN size := NUMBER(src); END;
    Translation.Write(space, SUBARRAY(src, 0, size), dst);
  END Write;

PROCEDURE Zero(space: T; dst, size: Word.T) RAISES {VMError.E} =
  VAR
    zero: PhysAddr.Content;
    end := dst + size;
  BEGIN 
    ArrayUtils.Clear(zero);
    WHILE dst < end DO
      size := MIN(BYTESIZE(zero), end - dst);
      Translation.Write(space, SUBARRAY(zero, 0, size), dst);
      INC(dst, size);
    END;
  END Zero;
    
PROCEDURE GetExternalRefTable(space: T): ExternalRef.T =
  BEGIN
    RETURN space.externs;
  END GetExternalRefTable;

PROCEDURE DeleteExtRef(space: T; extptr: Word.T) =
  BEGIN
    space.externs.delete(extptr);
  END DeleteExtRef;

PROCEDURE Externalize(space: T; intptr: REFANY; pos: Word.T): Word.T =
  BEGIN
    RETURN space.externs.externalize(intptr, pos);
  END Externalize;

PROCEDURE Internalize(space: T; extptr: Word.T): REFANY =
  BEGIN
    RETURN space.externs.internalize(extptr);
  END Internalize;

PROCEDURE CopyExterns(src: T; dest: T) =
  BEGIN
    src.externs.copy(dest.externs);
  END CopyExterns;

PROCEDURE Equal(space1, space2: T): BOOLEAN =
  BEGIN
    IF space1 = space2 THEN
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END Equal;

(*
 * MapPhysToVirt is not intended as a general-purpose routine.
 * Its only client is the io/dec/ws/ws_device.c code that
 * maps the graphics chip's frame buffer and event queue into
 * a user-space address.  The protection is always RW.  All arguments
 * are guaranteed to be well-formed.  You can try using it for other purposes
 * but no guarantees.
 *
 * This routine returns an integer precisely because it is
 * called from C routines that aren't up to fielding any
 * exceptions.
 *)
PROCEDURE MapPhysToVirt(space: T; phys: PhysAddr.Address;
			virt, size: Word.T): INTEGER =
  VAR
    numPages, roundBytes: Word.T;
    prot: Protection.T;
    newP: PhysAddr.T;
    physEnd: PhysAddr.Address;
  BEGIN
    prot.read  := TRUE;
    prot.write := TRUE;
    prot.execute := FALSE;
    roundBytes := Word.Minus( MachineMem.RoundToPage( Word.Plus(virt,size) ),
			      MachineMem.TruncToPage( virt ) );
    numPages := MachineMem.BytesToPages( roundBytes );
    physEnd := Word.Plus(phys,roundBytes);

    TRY
      newP := PhysAddrPrivate.AllocateIOSpace(phys, physEnd);
      Translation.AddMapping(space, newP, virt DIV PAGESIZE,
			     RoundUpToPage(Word.Plus(virt,roundBytes)), prot);
    EXCEPT
    ELSE
      RETURN -1;
    END;
    RETURN 0;
  END MapPhysToVirt;

BEGIN
  EVAL SpaceInterface.Export();
END Space.
