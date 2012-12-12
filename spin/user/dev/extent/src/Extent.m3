(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 08-Mar-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to use T itself in place of Region. Also, changed proc names
 *      so that they conform to the spin coding standard(or whatever it is).
 * 27-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed the representation to interval list from bitvec.
 *      This makes allocation much much much faster.
 * 30-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to consistently use either blocks or bytes.
 *
 * 10-Jul-96  Eric Christoffersen (ericc) at the University of Washington
 *	fixed bug in New.
 *
 * 24-Jun-96 Emin Gun Sirer at DEC
 *       Added guards against writing past the end of extents.
 *
 * 03-Jun-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed extent offset calculation.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created.
 *)

(* Extent based disk manager. Extents allow clients to safely
   space-multiplex disks. *)

MODULE Extent;
IMPORT Disk, Error, IO, Fmt, ExtentInterface, Auth,
       NameServer, ParseParams, Device;
IMPORT RefSeq;
IMPORT Debugger;

CONST Debug = FALSE;

CONST
  BLOCK_SIZE  = 512;

REVEAL T = TPublic BRANDED "Extent" OBJECT
    openCount  : CARDINAL := 0;
    lock       : MUTEX;
    parent     : T;        (* for security - see comment below *)
    children   : RefSeq.T; (* sub-extents. Used in allocation to make sure
			      I don't step onto sisters. *)
    end: CARDINAL; (* absolute position in disk; in bytes *)
    start: CARDINAL; (* absolute position in disk; in bytes *)
    blocksize  : CARDINAL; (* size of a block *)
    dev        : Disk.T;   (* the device that the extent is allocated on *)
  OVERRIDES
    open          := Open;
    close         := Close;
    read          := Read;
    write         := Write;
    init          := Init;
    allocate      := Allocate;
    deallocate    := Deallocate;
    getBaseOffset := GetBaseOffset;
    stat := Stat;
  END;    

TYPE
  ErrorT = Error.T OBJECT
    message := Message;
  END;

CONST
  OutOfRange = 1;
  NoSpace = 2;
  UnsupportedDevice = 3;
  ErrorMessages = ARRAY [OutOfRange .. UnsupportedDevice] OF TEXT
  {
   "Out of range",
   "No space left on device",
   "Unsupported device type"
   };
   
PROCEDURE Message (t: ErrorT): TEXT =
  BEGIN
    WITH code = t.resultCode() DO 
      IF code >= FIRST(ErrorMessages) AND code <= LAST(ErrorMessages) THEN
	RETURN ErrorMessages[code];
      ELSE
	RETURN Error.T.message(t);
      END;
    END;
  END Message;

PROCEDURE Raise (code: INTEGER) RAISES {Error.E} =
  BEGIN
    RAISE Error.E(NEW(ErrorT).init(code));
  END Raise;

PROCEDURE Open (self:T) RAISES {Error.E} = 
  BEGIN
    LOCK self.lock DO
      IF self.openCount = 0 THEN
        (* XXX what should we do here??? access check ??? *)
        self.dev.open();
      END;
      INC(self.openCount);
    END;
  END Open;

PROCEDURE Close(self:T) RAISES {Error.E} = 
  BEGIN
    LOCK self.lock DO 
      IF self.openCount = 1 THEN 
        self.dev.close();
      END;
      IF self.openCount > 0 THEN 
        DEC(self.openCount);
      END;
    END;
  END Close;

PROCEDURE Read (self: T;
		VAR data: ARRAY OF CHAR;
		offset: CARDINAL := 0): CARDINAL RAISES {Error.E} =
  BEGIN
    (* no need to lock self, dev and size are constant, we want concurrency. *)
    IF self.start + offset + NUMBER(data) > self.end THEN
      Raise(OutOfRange);
    END;
    RETURN self.dev.read(data, self.start + offset);
  END Read; 

PROCEDURE Write(self: T;
		READONLY data: ARRAY OF CHAR;
		offset: CARDINAL := 0): CARDINAL RAISES {Error.E} =
  BEGIN
    (* no need to lock self, dev and size are constant, we want concurrency. *)
    IF self.start + offset + NUMBER(data) > self.end THEN
      Raise(OutOfRange);
    END;
    RETURN self.dev.write(data,self.start + offset);
  END Write;

PROCEDURE Init(self:T): TPublic = 
  BEGIN
    self.lock := NEW(MUTEX);
    RETURN self;
  END Init;

(* Pre: "extent" is locked. See if the region "start" .. "end"
   is free within the extent. The region is absolute, i.e., they are
   counted from start of the disk. *)
PROCEDURE RegionIsFree (extent: T; start, end: CARDINAL): BOOLEAN =
  VAR
    nChildren := extent.children.size();
    child: T;
  BEGIN
    IF end >= extent.end THEN
      (* region past end. *)
      RETURN FALSE;
    END;
    
    FOR i := 0 TO nChildren-1 DO
      child := extent.children.get(i);
      IF child # NIL THEN
	IF start <= child.start AND end > child.start  THEN
	  RETURN FALSE;
	END;
	IF end >= child.end AND start < child.end THEN 
	  RETURN FALSE;
	END;
      END;
    END;
    RETURN TRUE;
  END RegionIsFree;
  
(* Allocates N contiguous blocks at a given offset
   This routine can be made O(N) instead of O(MN), where N is parent
   extent size and M is the size of the subextent that is getting allocated.
   But we expect that allocation will be done in small blocks (M=1).  *)
  
PROCEDURE AllocateRegion (extent: T; VAR offset, size: CARDINAL; 
			  flags: FlagSet) RAISES {Error.E} =
  VAR 
    found := FALSE;
    nChildren: CARDINAL;
    oldChild: T;
  BEGIN
    (* no need to lock extent, size is constant. *)
    IF extent.start + offset + size > extent.end THEN
      IO.PutError("extent.allocate: request (" & Fmt.Int(size)
		  & ") bigger than the extent size ("
		  & Fmt.Int(extent.end-extent.start) & ").\n");
      (* XXX need to raise with real exception code *)
      Raise(OutOfRange);
    END;
    
    (* set the amount of space used by this extent *)
    IF Flags.ExactSize IN flags THEN
      size := RoundUp(extent, size);
    ELSE
      size := extent.end - extent.start;
    END;

    LOCK extent.lock DO
      (* set the offset *)
      IF Flags.ExactOffset IN flags THEN
	offset := RoundDown(extent, offset);
	found := RegionIsFree(extent, offset, offset + size);
      ELSE
	nChildren := extent.children.size();
	IF RegionIsFree(extent, extent.start, size) THEN
	  (* Hey! found at the head. *)
	  offset := extent.start;
	  found := TRUE;
	ELSE
	  (* Look at place right behind each existing region, and
	     see if it's big enough to hold the requested size. *)
	  FOR i := 0 TO nChildren-1 DO
	    oldChild := extent.children.get(i);
	    IF oldChild # NIL
	      AND RegionIsFree(extent, oldChild.end, oldChild.end + size) THEN
	      offset := oldChild.end;
	      found := TRUE;
	      EXIT;
	    END;
	  END;
	END;
      END;
      IF found THEN RETURN; END;
    END;
    IO.PutError("extent: no space (request=" & Fmt.Int(size) & ").\n");
    Raise(NoSpace);
  END AllocateRegion;

(* XXX I rather pass in a Disk.T instead of a text devName.  However,
   I don't want to spend the time right now to figure out how to read
   the partition size information if I'm only given the Disk.T for a
   particular partition.  The extent code currently opens the entire
   disk to get the overall partition/label information. (mef) *)
PROCEDURE Allocate (self: T; 
		    devName: TEXT;
		    VAR size: CARDINAL;
		    offset: CARDINAL; flags: FlagSet) RAISES {Error.E} = 
  VAR dev: Device.T;
  BEGIN
    TRY
      dev := Device.Lookup(devName);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Extent.init couldn't find "&devName&" device.\n");
    END;
    
    TYPECASE dev OF
    | T(extent) =>
      (* create an extent in the parent "extent" *)
      AllocateRegion(extent, offset, size, flags);
      self.dev := extent.dev;
      self.start := offset;
      self.end := offset + size;
      self.blocksize := extent.blocksize;
      self.parent := extent;
      self.children := NEW(RefSeq.T).init();
      extent.children.addhi(self);
    | Disk.T(disk) =>
      (* create an extent on a raw disk device *)
      VAR
	stat: Disk.Stat;
      BEGIN
	disk.stat(stat);
	(* use the parition specfic disk device *)
	self.dev := dev;

	(* set the base offset in bytes,
	   but no large than extent size *)
	self.start := offset;
	  
	(* set size of extent in terms of bytes *)
	self.end := MIN(size, stat.secSize * stat.nSecs);

	(* block size of the extent in bytes *)
	self.blocksize := stat.secSize;

	(* base extent is its own parent *)
	self.parent := self;
	self.children := NEW(RefSeq.T).init();
      END;
    ELSE
      IO.PutError("Extent.init unsupported device type.\n");
      (* XXX need to raise with real exception code *)
      Raise(UnsupportedDevice);
    END;
    IF Debug THEN 
      IO.Put("Created extent on "&devName);
      IO.Put(" start=" & Fmt.Int(self.start) & " end=" & Fmt.Int(self.end)
	     & ".\n");
    END;
    size := self.end-self.start;
  END Allocate;

(* "Deallocate" deallocates me, and free up the space that my
   parent reserved for me.
   The very top level extent does not need to be deallocated. *)
PROCEDURE Deallocate (self: T) = 
  PROCEDURE Sub () = 
    VAR
      parent := self.parent;
      nChildren := parent.children.size();
    BEGIN
      (* Unlink myself from parent.children *)
      FOR i := 0 TO nChildren-1 DO
	IF self = parent.children.get(i) THEN 
	  parent.children.put(i, NIL);
	  RETURN;
	END;
      END;
      IO.Put("bogus Deallocate ??????");
      Debugger.Enter();
    END Sub;
  BEGIN
    LOCK self.lock DO
      IF self.parent # self THEN
        LOCK self.parent.lock DO
          Sub();
        END;
      ELSE
        Sub();
      END;
    END;
  END Deallocate;

PROCEDURE GetBaseOffset (self: T) : CARDINAL =
  BEGIN
    (* no need to lock self, baseoffset is constant. *)
    RETURN self.start;
  END GetBaseOffset;

PROCEDURE Stat (self:T; VAR buf: Disk.Stat) =
  BEGIN
    buf.secSize := BLOCK_SIZE;
    buf.nSecs := (self.end-self.start) DIV buf.secSize;
  END Stat;
  
PROCEDURE RoundUp (extent:T; bytes: CARDINAL): CARDINAL = 
  BEGIN
    RETURN (bytes + extent.blocksize - 1) DIV extent.blocksize
      * extent.blocksize;
  END RoundUp;

PROCEDURE RoundDown (extent:T; bytes: CARDINAL): CARDINAL =
  BEGIN
    RETURN bytes DIV extent.blocksize * extent.blocksize;
  END RoundDown;


PROCEDURE Mkdev (pp: ParseParams.T) RAISES {ParseParams.Error} = 
  VAR
    devName, extName : TEXT;
    size             : CARDINAL := LAST(CARDINAL);
    offset           : CARDINAL := 0;
    newExt           : T;
    done             : BOOLEAN;
    flags            : FlagSet  := FlagSet{};
  BEGIN
    devName := pp.getNext();
    extName := pp.getNext();
    REPEAT
      done := TRUE;
      IF pp.testNext("-size") THEN
        size := pp.getNextInt();
        done := FALSE;
        flags := flags + FlagSet{Flags.ExactSize};
      END;
      IF pp.testNext("-offset") THEN
        offset := pp.getNextInt();
        done := FALSE;
        flags := flags + FlagSet{Flags.ExactOffset};
      END;
    UNTIL done;
    TRY
      IF Device.Lookup(extName) = NIL THEN 
        newExt := NEW(T).init();
        newExt.allocate(devName,size,offset,flags);
        Device.Register(extName,newExt);
      ELSE
        IO.PutError(extName & " already exists.\n");
      END;
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Extent.Run problem looking up device " & extName & "\n");
    | Error.E =>
      IO.PutError("Extent.Run problem allocating extent for " & extName & "\n");
    END;
  END Mkdev;

PROCEDURE Test (pp: ParseParams.T) RAISES {ParseParams.Error} =
  VAR
    ext : T;
    data : REF ARRAY OF CHAR;
    fail : BOOLEAN := FALSE;
    devName : TEXT;
    bytes : CARDINAL;
  BEGIN
    devName := pp.getNext();
    TRY
      ext := Device.Lookup(devName);
      IF ext # NIL THEN 
        ext.open();
        data := NEW(REF ARRAY OF CHAR,ext.blocksize);

        IO.Put("extent test on " & devName & "\n");

        IO.Put("block ");
        FOR j := 0 TO ((ext.start-ext.end) DIV ext.blocksize)-1 DO
          IO.PutInt(j); IO.Put(" ");

          (* init data with 0xaa *)
          FOR i := FIRST(data^) TO LAST(data^) DO
            data[i] := VAL(16_aa,CHAR);
          END;
          (* write to disk *)
          bytes := ext.write(data^,ext.blocksize * j);
          IF bytes # NUMBER(data^) THEN
            IO.PutError("ext.write wrote "&Fmt.Int(bytes));
            IO.Put(" instead of "&Fmt.Int(NUMBER(data^)) &"\n");
            EXIT;
          END;
          (* read back from disk *)
          bytes := ext.read(data^,ext.blocksize * j);
          IF bytes # NUMBER(data^) THEN
            IO.PutError("ext.read read "&Fmt.Int(bytes));
            IO.Put(" instead of "&Fmt.Int(NUMBER(data^)) &"\n");
            EXIT;
          END;

          (* verify data *)
          FOR i := FIRST(data^) TO LAST(data^) DO
            IF data[i] # VAL(16_aa,CHAR) THEN
              IO.Put("Test failed @ block "&Fmt.Int(j));
              IO.Put(" byte "&Fmt.Unsigned(ORD(data[i])) & " # 16_aa\n");
              fail := TRUE;
              EXIT;
            END;
          END;
          IF fail THEN EXIT; END;
        END;
      ELSE
        IO.PutError(devName & " not found.\n");
      END;
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Extent.Run problem looking up device " & devName & "\n");
    | Error.E =>
      IO.PutError("Extent.Run problem allocating extent for " & devName & "\n");
    END;
  END Test;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  VAR
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* skip command name *)
      IF pp.testNext("mkdev") THEN
	Mkdev(pp);
      ELSIF pp.testNext("test") THEN
	Test(pp);
      ELSE
        IO.Put("Command unknown.\n");
        IO.Put(CommandName & CommandHelp & "\n");
        RETURN FALSE;
      END;
      RETURN TRUE;
    EXCEPT
      ParseParams.Error => IO.Put(CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
  END Run;

BEGIN
  TRY
    EVAL ExtentInterface.Export(NEW(Auth.AuthAlways));
    IO.Put("Exported disk extents...\n");
  EXCEPT
    NameServer.Error => IO.Put("Failed to export extents.\n");
  END;
END Extent.
