(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Use sub-extent rather than a entire disk to prevent
 *	possible conflicts between lfs and other extents such as httpextent.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Added FastBuffer support.
 *
 * 30-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *
 *)
MODULE DiskExtents;
IMPORT LFSRep;
IMPORT Extent;
IMPORT IO, Fmt;
IMPORT Disk, Device;
(* IMPORT Error; *)
IMPORT NameServer;

CONST DEBUG = FALSE;

PROCEDURE SetupDiskExtent(mp: LFSRep.MP; devname: TEXT) RAISES {DiskNotFound} =
  CONST
  VAR
    dev: Disk.T;
    stat: Disk.Stat;
    userdev: Extent.T;
    size:CARDINAL;
  BEGIN

    IF DEBUG THEN
      IO.Put("SetupDiskExtent: Opening device " & devname & "\n");
    END;
    TRY
      dev := Device.Lookup(devname);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("device: " & devname & " not registered!\n");
      RAISE DiskNotFound;
    END;

    dev.stat(stat);

    size:= stat.nSecs * stat.secSize;

    TYPECASE dev OF
    | Extent.T =>
      (* use sub-extent made by "extent mkdev $EDISKDEV devname ..." *)
      userdev := dev;
    | Disk.T =>
      (* use the entire disk. this may conflicts with other sub-extents! *)
      userdev := NEW(Extent.T).init();
      userdev.allocate(devname,size,0);
    END;
      
    IF DEBUG THEN
      IO.Put("  number of sectors: " & Fmt.Int(stat.nSecs) & "\n");
      IO.Put("  sector size : " & Fmt.Int(stat.secSize) & "\n");
      IO.Put("  allocated " & Fmt.Int(size) & " bytes extent\n");
    END;

    mp.segInfo.bytesInBlock := stat.secSize;
    mp.blocksInExtent := stat.nSecs;
    mp.segInfo.fsextent := userdev;

  END SetupDiskExtent;

BEGIN
END DiskExtents.
