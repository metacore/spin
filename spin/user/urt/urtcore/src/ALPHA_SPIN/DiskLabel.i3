(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Sep-97  Tsutomu Owa (owa) at the University of Washington
 *	Created. From DiskLabel.i3.
 *)
INTERFACE DiskLabel;
IMPORT DiskLabelCommon;

CONST
   LABELSECTOR = 0;                       (* sector containing label *)
   LABELOFFSET = 64;                      (* offset of label in sector *)
   DISKMAGIC = DiskLabelCommon.DISKMAGIC; (* The disk magic number *)
   NDDATA=DiskLabelCommon.NDDATA;(* words of drive type specific information *)
   NSPARE=DiskLabelCommon.NSPARE;(* words reserved for future use *) 
   NNAME=DiskLabelCommon.NNAME;  (* bytes for the packname *)
   MAXPARTITIONS=DiskLabelCommon.MAXPARTITIONS;

TYPE LabelSector =  (* What the disk sector containing the label looks like *)
  RECORD
    skip:      ARRAY [0..LABELOFFSET-1] OF CHAR;
    disklabel: DiskLabelCommon.T;
  END;

TYPE Partition = DiskLabelCommon.Partition;

TYPE PartInfo = DiskLabelCommon.PartInfo;

TYPE T = DiskLabelCommon.T;

END DiskLabel.
