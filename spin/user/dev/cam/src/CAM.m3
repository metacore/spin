(*
 * Copyright 1994-1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 12-Apr-97  Yasushi Saito (yasushi) at the University of Washington
 *	Removed the SerializeDiskIO flag.
 * 06-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Introduced SerializeDiskIO flag.
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new device interface. Also, add strongref'ing.
 * 22-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Deleted CAMStub and code that interposes on it.
 *      Added exception raises and handling.
 *
 * 21-Dec-95  Yasushi Saito (yasushi) at the University of Washington
 *	Read & write used to ignore bytes_wanted.
 *	Removed readForFileIO because it's no longer used.
 *
 * 09-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Added exception handling for Dispatcher.Install
 *
 * 28-Nov-95  Marc Fiuczynski (mef) at the University of Washington
 *	Updated RZread to have correct argument type to compile module.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	Add Configure to hook into OSF cam services.
 *
 * 20-Jul-95  Brian Bershad (bershad) at the University of Washington
 *       Created.
 *
 *)

(* "CAM" - Common Access Method for DEC SCSI
   Interface to C facilities for accessing CAM. *)

UNSAFE (* to import CAMExtern *)
MODULE CAM EXPORTS CAM, CAMPrivate;
IMPORT CAMExtern, CAMInterface, Ctypes, Device, Disk, Fmt, IO,
       M3toC, NameServer, Tape, Word;
IMPORT Text;
IMPORT DiskLabel;
IMPORT Thread, Error;

VAR diskMu: MUTEX;  

TYPE RZDisk = Disk.T BRANDED OBJECT
    id: CAMExtern.dev_t;
    name: TEXT;
    opened: BOOLEAN;
  OVERRIDES
    open  := RZopen;
    close := RZclose;
    read  := RZread;
    write := RZwrite;
    stat := RZstat;
  END;

TYPE TZTape = Tape.T BRANDED OBJECT
    id: CAMExtern.dev_t;
  END;

  
PROCEDURE RZstat(dev: RZDisk; VAR buf: Disk.Stat) =
  CONST
    MAXPARTINFO = 512; (* max amount of bytes a disklabel can lie in *)
    BLOCK_SIZE  = 512;
  VAR 
    partition: CARDINAL;
    partitionchar : CHAR;
    bytes         : CARDINAL;
    fullName      : TEXT;
    partData: ARRAY [0 .. MAXPARTINFO-1] OF CHAR;
  BEGIN
    IF NOT dev.opened THEN RZopen(dev); END;
    partitionchar := Text.GetChar(dev.name, Text.Length(dev.name) - 1);
    partition := ORD(partitionchar) - ORD('a');
    fullName := Text.Sub(dev.name, 0, Text.Length(dev.name) - 1) & "c";

    TRY
      (* get a handle on the full disk *)
      dev := Device.Lookup(fullName);

      (* open the full disk *)
      dev.open();

      (* read the disklabel to find out partition info.
         where the partition starts, size, etc.  *)
      bytes := dev.read(partData, DiskLabel.LABELSECTOR*BLOCK_SIZE);
      WITH sector = VIEW(partData, DiskLabel.LabelSector),
	   label = sector.disklabel,
	   part = label.d_partitions[partition] DO
	IF label.d_magic # DiskLabel.DISKMAGIC OR
	  label.d_magic2 # DiskLabel.DISKMAGIC THEN
	  IO.PutError("Disk magic number mismatch.!");
	  RETURN;
	END;
	buf.nSecs := part.p_size;
	buf.secSize := label.d_secsize;
      END;
      (* done using the full disk -- close it *)
      dev.close();
    EXCEPT
    | Error.E(e) => 
      IO.PutError("Cam.Stat: " & e.message());
    | NameServer.Error => 
      IO.PutError("extent.init Device lookup failed!\n");
    END;
  END RZstat;
  
PROCEDURE RZopen(dev:RZDisk) =
  VAR rc: INTEGER;
  BEGIN
    IF NOT dev.opened THEN
      rc := CAMExtern.cdisk_open(dev.id,0,8_60000 (* S_IFBLK *));
      dev.opened := TRUE;
    END;
  END RZopen;

PROCEDURE RZread(dev:RZDisk;
		 VAR data: ARRAY OF CHAR;
		 offset: CARDINAL): CARDINAL RAISES {Error.E} =
  VAR
    rc: INTEGER;  
    bytes := NUMBER(data);
  BEGIN
    IF NOT dev.opened THEN RZopen(dev); END;
    rc := CAMExtern.cam_read(dev.id, data, offset, bytes);
    IF rc # 0 THEN RAISE Error.E(NEW(Device.Error).init(rc)) END;
    RETURN bytes;
  END RZread;

PROCEDURE RZwrite(dev: RZDisk;
		  READONLY data: ARRAY OF CHAR;
		  offset: CARDINAL): CARDINAL RAISES {Error.E} =
  VAR
    rc: INTEGER;
    bytes := NUMBER(data);
  BEGIN
    IF NOT dev.opened THEN RZopen(dev); END;
    rc := CAMExtern.cam_write(dev.id, data, offset, bytes);
    IF rc # 0 THEN RAISE Error.E(NEW(Device.Error).init(rc)) END;
    RETURN bytes;
  END RZwrite;

PROCEDURE RZclose(dev: RZDisk) =
  BEGIN
    EVAL CAMExtern.cdisk_close(dev.id,0,8_60000 (* S_IFBLK *));
  END RZclose;

PROCEDURE RegisterRZ(str: Ctypes.char_star; unit:CARDINAL; id:Word.T) =
  VAR dev: RZDisk;
      name: TEXT;
  BEGIN
    IO.Put("CAM registering scsi drives.\n");
    TRY
      name := M3toC.CopyStoT(str) & Fmt.Int(unit);
      dev := NEW(RZDisk, id := id, name := name&"a");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+1, name := name&"b");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+2, name := name&"c");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+3, name := name&"d");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+4, name := name&"e");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+5, name := name&"f");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+6, name := name&"g");
      Device.Register(dev.name, dev);
      dev := NEW(RZDisk, id := id+7, name := name&"h");
      Device.Register(dev.name, dev);
    EXCEPT
    | NameServer.Error =>
	IO.Put("nameserver error while registering disk drives.\n");
    END;
  END RegisterRZ;

PROCEDURE RegisterTZ(<*UNUSED*>str: Ctypes.char_star; <*UNUSED*>unit:CARDINAL; id:Word.T) =
  VAR dev: TZTape;
  BEGIN
    IO.Put("CAM registering tape drives.\n");
    TRY
      dev := NEW(TZTape, id := id); Device.Register("rmt0l",dev);
      dev := NEW(TZTape, id := id+2); Device.Register("rmt0h",dev);
      dev := NEW(TZTape, id := id+4); Device.Register("rmt0m",dev);
      dev := NEW(TZTape, id := id+6); Device.Register("rmt0a",dev);
    EXCEPT
    | NameServer.Error =>
	IO.Put("nameserver error while registering tape drives.\n");
    END;
  END RegisterTZ;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    IF verbose THEN IO.Put("CAM main body\n"); END;
    diskMu := NEW(MUTEX);
    CAMExtern.cam_attach();
    TRY
      EVAL CAMInterface.Export(NIL);
    EXCEPT
    | NameServer.Error =>
      IF verbose THEN
        IO.Put("FileSystem namespace initialization failed\n"); 
      END;
    END;
  END Init;

BEGIN
  Init(TRUE);
END CAM.
