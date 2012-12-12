UNSAFE MODULE BDev;
IMPORT Disk, BDevExtern, Error, Device, IO, Ctypes, NameServer, M3toC;
IMPORT DiskLabel;
IMPORT Text;
IMPORT BDevInterface, Fmt;

CONST
  MAXPARTINFO = 512; (* max amount of bytes a disklabel can lie in *)
  BLOCK_SIZE  = 512;

TYPE
  BDisk = Disk.T BRANDED OBJECT
    name: TEXT;
    id: BDevExtern.dev_t;
  OVERRIDES
    open  := BOpen;
    close := BClose;
    read  := BRead;
    write := BWrite;
    stat := BStat;
  END;
  
PROCEDURE BOpen(self:Disk.T) =
  VAR dev: BDisk;
       rc: INTEGER;
  BEGIN
    dev := NARROW(self, BDisk);
    rc := BDevExtern.blkdev_open(dev.id);
  END BOpen;

PROCEDURE BRead(self:Disk.T; 
		VAR data: ARRAY OF CHAR;
		offset: CARDINAL): CARDINAL RAISES {Error.E} =
VAR
  dev := NARROW(self, BDisk);
  rc: INTEGER;
  bytes := NUMBER(data);
BEGIN
  rc := BDevExtern.blkdev_read(dev.id, data, offset, bytes);
  IF rc # 0 THEN RAISE Error.E(NEW(Device.Error).init(rc)) END;
  RETURN bytes;
END BRead;

PROCEDURE BWrite(self:Disk.T;
		 READONLY data: ARRAY OF CHAR;
		 offset: CARDINAL): CARDINAL RAISES {Error.E} =
VAR
  dev := NARROW(self, BDisk);
  rc: INTEGER;
  bytes := NUMBER(data);
BEGIN
  rc := BDevExtern.blkdev_write(dev.id, data, offset, bytes);
  IF rc # 0 THEN RAISE Error.E(NEW(Device.Error).init(rc)) END;
  RETURN bytes;
END BWrite;

PROCEDURE BClose(self:Disk.T) =
VAR dev: BDisk;
BEGIN
  dev := NARROW(self, BDisk);
  EVAL BDevExtern.blkdev_close(dev.id);
END BClose;

PROCEDURE BStat(dev: BDisk; VAR stat: Disk.Stat) =
  VAR
    partition: INTEGER;
    partitionchar : CHAR;
    bytes         : CARDINAL;
    fullName      : TEXT;
    partData: ARRAY [0 .. MAXPARTINFO-1] OF CHAR;
  BEGIN
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
	  IO.PutError("Disk magic number mismatch.!\n");
	  IO.Put("    expect: " & Fmt.Int(DiskLabel.DISKMAGIC, 16) &
	         ", got: " & Fmt.Int(label.d_magic, 16) &
	         ", and: " & Fmt.Int(label.d_magic2, 16) & ".\n");
	  RETURN;
	END;
	stat.nSecs := part.p_size;
	stat.secSize := label.d_secsize;
      END;
      (* done using the full disk -- close it *)
      dev.close();
    EXCEPT
    | Error.E(e) => 
      IO.PutError("Cam.Stat: " & e.message());
    | NameServer.Error => 
      IO.PutError("extent.init Device lookup failed!\n");
    END;
  END BStat;

PROCEDURE RegisterDev(str: Ctypes.char_star; id: BDevExtern.dev_t) =
  VAR dev: BDisk;
      name: TEXT;
  BEGIN
    TRY
      name := M3toC.CopyStoT(str);
      dev := NEW(BDisk);
      dev.name := name;
      dev.id := id;
      (* IO.Put("Attaching " & name & " at 0x" & Fmt.Int(id, 16) & ".\n"); *)
      Device.Register(name, dev);
    EXCEPT
    | NameServer.Error =>
	IO.Put("nameserver error while registering disk drives.\n");
    END;
  END RegisterDev;

BEGIN
  (* register myself to NameServer *)
  EVAL BDevInterface.Export(NIL);
  IO.Put("Attaching block devices...\n");

  BDevExtern.blkdev_register(RegisterDev);
END BDev.
