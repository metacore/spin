(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Sep-97  Tsutomu Owa (owa) at the University of Washington
 *	Created. From DiskLabel.h.
 *)
INTERFACE DiskLabel;
IMPORT DiskLabelCommon;

(*
 * Each disk has a label which includes information about the hardware
 * disk geometry, filesystem partitions, and drive specific information.
 * The label is in block 0 or 1, possibly offset from the beginning
 * to leave room for a bootstrap, etc.
 * In case of freebsd, it starts at block 1, offset 0.
 *)

(*
 * XXX the following will go away when conversion to the slice version is
 * complete: OURPART, RAWPART, readMSPtolabel, readMBRtolabel, dkminor,
 * the DOSified readdisklabel, DOS stuff in this file.
 *)
CONST
   LABELSECTOR = 1;                       (* sector containing label *)
   LABELOFFSET = 0;                       (* offset of label in sector *)
   DISKMAGIC = DiskLabelCommon.DISKMAGIC; (* The disk magic number *)
   NDDATA=DiskLabelCommon.NDDATA; (* words of drive type specific information *)
   NSPARE=DiskLabelCommon.NSPARE;(* words reserved for future use *) 
   NNAME=DiskLabelCommon.NNAME;  (* bytes for the packname *)
   MAXPARTITIONS=DiskLabelCommon.MAXPARTITIONS;

TYPE LabelSector =  (* What the disk sector containing the label looks like *)
  RECORD
    (* skip:      ARRAY [0..LABELOFFSET-1] OF CHAR; *)
    disklabel: DiskLabelCommon.T;
  END;

TYPE Partition = DiskLabelCommon.Partition;

TYPE PartInfo = DiskLabelCommon.PartInfo;

TYPE T = DiskLabelCommon.T;

END DiskLabel.
