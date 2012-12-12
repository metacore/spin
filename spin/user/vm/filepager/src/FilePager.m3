
(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-97  Yasushi Saito (yasushi) at the University of Washington
 *	Use ArrayUtils.Zero
 * 06-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Introduced SerializeDiskIO flag.
 * 14-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	pagein/out take PhysAddr.T
 * 09-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed minor bug in SwapOn where dev.used needed to be calculated
 *	based on the page size, rather than in byte size.
 *
 *)

MODULE FilePager;

IMPORT PagerObject, MachineMem, IO, Fmt, Dispatcher, File, FileStat,
       Error, DefaultPager;
IMPORT FilePagerInterface, Auth;
IMPORT PhysAddr;
IMPORT VMStat;
IMPORT VMDebug;
IMPORT Debugger;
IMPORT Wr;
IMPORT Protection;
IMPORT ArrayUtils;

CONST
  MaxDevices = 8; (* max # of swap devices *)
  PAGESIZE = MachineMem.PAGESIZE;
TYPE
  T = PagerObject.T OBJECT
    id: INTEGER;
    used: [0 .. 100];
    valid: BOOLEAN;
    pages: REF ARRAY OF Page;
  OVERRIDES
    destroy := Destroy;
    pageIn := PageIn;
    pageOut := PageOut;
  END;


  DevNumber = [0 .. MaxDevices-1];

  Device = RECORD
    mu: MUTEX;
    used: REF ARRAY OF BOOLEAN; (* Shit."BITS 1 FOR" not allowed on src m3 *)
    file: File.T;
    lastAllocatedBlock: CARDINAL;
  END;

  State = {In,  (* page is in the memory *)
	   Out, (* page is in disk *)
	   Invalid (* never used *)
	    };
  
  Page = RECORD
    block: CARDINAL;
    devNumber: DevNumber;
    sum: INTEGER;
    state: State;
  END;
    
VAR
  idSeq := 0;
  devs: ARRAY DevNumber OF Device;
  
PROCEDURE Create (size: CARDINAL): PagerObject.T =
  VAR self: T;
  BEGIN
    self := NEW(T);
    self.id := idSeq;
    INC(idSeq);
    self.valid := TRUE;
    self.pages := NEW(REF ARRAY OF Page, size);
    FOR i := 0 TO size-1 DO
      self.pages[i].state := State.Invalid;
    END;
    RETURN self;
  END Create;

PROCEDURE Print (p: T): TEXT =
  BEGIN
    RETURN "defpager " & Fmt.Int(p.id) & ":" & Fmt.Int(NUMBER(p.pages^));
  END Print;
  
PROCEDURE PageIn (self: T; off: PagerObject.PageNumber;
		  <*UNUSED*>type: INTEGER;
		  frame: PhysAddr.T;
		  VAR prot: Protection.T): PagerObject.ResultCode =
  
  PROCEDURE Clear (VAR data: PhysAddr.Content) =
    BEGIN
      ArrayUtils.Clear(data);
    END Clear;
    
  PROCEDURE ReadPage (VAR data: PhysAddr.Content) =
    VAR nRead: CARDINAL;
    BEGIN
      TRY
	WITH page = self.pages[off] DO 
	  <*ASSERT NUMBER(data)=PAGESIZE*>
	  nRead := devs[page.devNumber].file.read(data, page.block * PAGESIZE);
	  <*ASSERT nRead = PAGESIZE*>
	END;
      EXCEPT 
      | Error.E =>
	IO.Put("FilePager caught exception.\n");
      END;
    END ReadPage;
    
  VAR
    rc := PagerObject.ResultCode.Success;
  BEGIN
    prot := Protection.All;
    <*ASSERT off < NUMBER(self.pages^)*>

    IF self.used > 0 THEN
      IO.Put("file pager access race!");
      Debugger.Enter();
    END;
    INC(self.used);
    
    WITH page = self.pages[off] DO
      IF NOT self.valid THEN
	rc := PagerObject.ResultCode.PagerDestroyed;
      ELSE
	CASE page.state OF
	| State.In =>
	  (* Someone already swapped in my page while I'm waiting *)
	  IO.Put("page in in:" & Print(self) & " off="
		 & Fmt.Int(off, 16) & "\n");
	  rc := PagerObject.ResultCode.AlreadyPagedIn;
	| State.Invalid =>
	  PhysAddr.Access(frame, Clear);
	  IF VMStat.Enabled THEN
	    INC(VMStat.zero);
	  END;
	  page.state := State.In;
	| State.Out =>
	  IF VMDebug.DebugMessage THEN 
	    IO.Put("page in:" & Print(self) & " off=" & Fmt.Int(off, 16));
	    IO.Put(" block =" & Fmt.Int(page.block) & ".\n");
	  END;
	  WITH dev = devs[page.devNumber] DO
	    PhysAddr.Access(frame, ReadPage);
	    WITH sum = VMDebug.CalculateChecksum(frame) DO
	      IF page.sum # sum THEN
		IO.Put("checksum mismatch.\n");
		Debugger.Enter();
	      END;
	    END;
	    FreeBlock(dev, page.block);
	  END;

	  page.state := State.In;
	END;
      END;
    END;
    
    DEC(self.used);
    <*ASSERT self.used = 0*>
    RETURN rc;
  END PageIn;
  
PROCEDURE PageOut (self: T; off: PagerObject.PageNumber;
		   frame: PhysAddr.T;
		   <*UNUSED*>dirty: BOOLEAN): PagerObject.ResultCode =
  PROCEDURE WritePage (VAR data: PhysAddr.Content) =
    VAR nWrite: CARDINAL;
    BEGIN
      TRY
	WITH page = self.pages[off] DO
	  <*ASSERT NUMBER(data)=PAGESIZE*>
	  nWrite := devs[page.devNumber].file.write(data, page.block*PAGESIZE);
	  <*ASSERT nWrite = PAGESIZE*>
	END;
      EXCEPT 
      | Error.E =>
      IO.Put("FilePager caught exception.\n");
      END;
    END WritePage;
  
  VAR
    rc := PagerObject.ResultCode.Success;
  BEGIN
    <*ASSERT off < NUMBER(self.pages^)*>
    IF self.used > 0 THEN
      IO.Put("file pager access race!");
      Debugger.Enter();
    END;
    INC(self.used);
    
    WITH page = self.pages[off] DO
      IF VMDebug.DebugMessage THEN 
	IO.Put("pageout:" & Print(self) & " off=" & Fmt.Int(off, 16));
	IO.Put("stat = "& Fmt.Int(ORD(page.state)) & ".\n");
      END;
      
      IF NOT self.valid THEN
	rc := PagerObject.ResultCode.PagerDestroyed;
      ELSE
	CASE page.state OF
	| State.Out =>
	  IO.Put("pageout out:" & Print(self)
		 & " off=" & Fmt.Int(off, 16) & "\n");
	  (* Someone already swapped out my page while I'm waiting *)
	  rc := PagerObject.ResultCode.AlreadyPagedOut;
	| State.In, State.Invalid =>
	  (* State.Invalid may happen when the cow'ed page is purged
	     for the first time. *)
	  IF FindBlock(page.devNumber, page.block) THEN
	    (* IO.Put("pageout: block=" & Fmt.Int(page.block) & ".\n"); *)
	    page.sum := VMDebug.CalculateChecksum(frame);
	    PhysAddr.Access(frame, WritePage);
	    page.state := State.Out;
	  ELSE
            IO.Put("PageOut: DeviceFull\n");
	    rc := PagerObject.ResultCode.DeviceFull;
	  END;
	END;
      END;
    END;
    DEC(self.used);
    <*ASSERT self.used = 0*>
    RETURN rc;
  END PageOut;

PROCEDURE Destroy (self: T) =
  BEGIN
    (* no need to lock self, since this is no longer used *)
    self.valid := FALSE;
    
    FOR i := 0 TO LAST(self.pages^) DO
      WITH page = self.pages[i] DO
	CASE page.state OF
	| State.Out =>
	  FreeBlock(devs[page.devNumber], page.block);
	ELSE
	END;
      END;
    END;
  END Destroy;
  
VAR
  devsMu := NEW(MUTEX); (* guards devs table *)
  lastDevNumber := 0;
  
FUNCTIONAL
PROCEDURE SwapOnGuard (entry: REFANY): BOOLEAN = 
  BEGIN
    TYPECASE entry OF 
    | NULL => 
      RETURN FALSE;
    | File.T => 
      RETURN TRUE;
    ELSE
      RETURN FALSE;
    END;
  END SwapOnGuard;

PROCEDURE SwapOnHandler (entry: REFANY) =
  VAR
    stat: FileStat.T;
    fp: File.TPublic;
  BEGIN
    fp := NARROW(entry,File.TPublic);
    TRY
      fp := fp.open(0);
      fp.stat(stat);
    EXCEPT
    | Error.E(e) =>
      IO.Put(e.message() & "\n");
      RETURN;
    END;
    
    LOCK devsMu DO
      FOR i := 0 TO MaxDevices-1 DO
        WITH dev = devs[i] DO 
          IF dev.file = NIL THEN
            dev.mu := NEW(MUTEX);
            dev.used := NEW(REF ARRAY OF BOOLEAN, stat.size DIV PAGESIZE);
            dev.file := fp;
            IO.Put("swap installed.\n");
            RETURN;
          END;
        END;
      END;
    END;
    IO.Put("def pager swapon: table full\n");
    TRY
      fp.close();
    EXCEPT
    | Error.E(e) =>
      IO.Put(e.message() & "\n");
      RETURN;
    END;
  END SwapOnHandler; 
  
PROCEDURE FreeBlock (VAR dev: Device; block: CARDINAL) =
  BEGIN
    LOCK dev.mu DO
      dev.used[block] := FALSE;
    END;
  END FreeBlock;
  
PROCEDURE NextDev (n: DevNumber): DevNumber =
  BEGIN
    IF n = MaxDevices-1 THEN
      RETURN 0;
    ELSE
      RETURN n+1;
    END;
  END NextDev;


(* Find a free block from any swap device.
   Returns true if found. *)
PROCEDURE FindBlock (VAR devNumber: DevNumber; VAR block: CARDINAL): BOOLEAN=
  BEGIN
    devNumber := lastDevNumber;
    lastDevNumber := NextDev(lastDevNumber);

    FOR i := 0 TO MaxDevices-1 DO
      WITH dev = devs[devNumber] DO
	IF dev.file # NIL THEN
	  LOCK dev.mu DO
	    FOR j := 0 TO LAST(dev.used^) DO
	      block := dev.lastAllocatedBlock;
	      IF dev.lastAllocatedBlock = LAST(dev.used^) THEN
		dev.lastAllocatedBlock := 0;
	      ELSE
		INC(dev.lastAllocatedBlock);
	      END;
	      
	      IF NOT dev.used[block] THEN
		dev.used[block] := TRUE;
		RETURN TRUE;
	      END;
	    END;
	  END;
	END;
	devNumber := NextDev(devNumber);
      END;
    END;

    RETURN FALSE;
  END FindBlock;


PROCEDURE Stat (wr: Wr.T) =
  BEGIN
    FOR i := 0 TO LAST(devs) DO
      WITH dev = devs[i] DO
	IF dev.file # NIL THEN
	  IO.Put("Swap device No." & Fmt.Int(i) & ":", wr);
	  FOR j := 0 TO LAST(dev.used^) DO
	    IF j MOD 64 = 0 THEN
	      IO.Put("\n" & Fmt.Int(j, 16) & ":", wr);
	    END;
	    IF dev.used[j] THEN
	      IO.Put("#", wr);
	    ELSE
	      IO.Put(".", wr);
	    END;
	  END;
	  IO.Put("\n");
	END;
      END;
    END;
  END Stat;
  
BEGIN
  TRY
    EVAL FilePagerInterface.Export(NEW(Auth.AuthAlways));
    EVAL Dispatcher.InstallHandler(DefaultPager.Create,
			      NIL, Create,
			      options:=Dispatcher.Options{Dispatcher.Opt.First,
						 Dispatcher.Opt.Cancel});
    EVAL Dispatcher.InstallHandler(DefaultPager.SwapOn,
                                   SwapOnGuard, SwapOnHandler,
                                   options:=Dispatcher.Options{Dispatcher.Opt.First,
						 Dispatcher.Opt.Cancel});
    EVAL Dispatcher.InstallHandler(DefaultPager.Stat, NIL, Stat,
                         options:=Dispatcher.Options{Dispatcher.Opt.First,
						     Dispatcher.Opt.Cancel});
    IO.Put("swapper installed.\n");
  EXCEPT
  | Dispatcher.Error(ec) =>
    IO.Put("swapper: dispatcher error " & Fmt.Int(ORD(ec)) & ".\n");
  END;
END FilePager.
