(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Whisted.
 *
 *)
INTERFACE DiskExtents;
IMPORT LFSRep;
EXCEPTION DiskNotFound;

PROCEDURE SetupDiskExtent(mp: LFSRep.MP; devname: TEXT) RAISES {DiskNotFound};

END DiskExtents.
