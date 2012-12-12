(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 16-Sep-98  Tsutomu Owa (owa) at the University of Washington
 *	Moved machine dependanpart into each subdir.
 *
 * 08-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Created. From disklabel.h.
 *)
INTERFACE DiskLabelCommon;
IMPORT Ctypes;

CONST
   DISKMAGIC = 16_82564557;	          (* The disk magic number *)
   NDDATA=5; (* words of drive type specific information *)
   NSPARE=5; (* words reserved for future use *) 
   NNAME=16; (* bytes for the packname *)
   MAXPARTITIONS=8;

TYPE Partition =             (* the partition table *)
  RECORD
    p_size:   Ctypes.unsigned_int;       (* number of sectors in partition *)
    p_offset: Ctypes.unsigned_int;       (* starting sector *)
    p_fsize:  Ctypes.unsigned_int;       (* filesystem basic fragment size *)
    p_fstype: Ctypes.unsigned_char;      (* filesystem type, see below *)
    p_frag:   Ctypes.unsigned_char;      (* filesystem fragments per block *)
    p_cpg:    Ctypes.unsigned_short;     (* filesystem cylinders per group *)
  END;

TYPE T = RECORD
        d_magic:        Ctypes.unsigned_int; (* the magic number *)
        d_type:		Ctypes.short;        (* drive type *)
        d_subtype:	Ctypes.short;        (* controller/d_type specific *)
        d_typename:	ARRAY [0..NNAME-1] OF CHAR; (* type name, e.g. "eagle" *)
        d_packname: ARRAY [0..NNAME-1] OF CHAR;    (* pack identifier *)
        (* disk geometry: *)
        d_secsize:	Ctypes.unsigned_int; (* # of bytes per sector *)
        d_nsectors:	Ctypes.unsigned_int; (* # of data sectors per track *)
        d_ntracks:	Ctypes.unsigned_int; (* # of tracks per cylinder *)
        d_ncylinders:	Ctypes.unsigned_int; (* # of data cylinders per unit *)
        d_secpercyl:	Ctypes.unsigned_int; (* # of data sectors/cylinder *)
        d_secperunit:	Ctypes.unsigned_int; (* # of data sectors per unit *)
        (*
         * Spares (bad sector replacements) below
         * are not counted in d_nsectors or d_secpercyl.
         * Spare sectors are assumed to be physical sectors
         * which occupy space at the end of each track and/or cylinder.
         *)
        d_sparespertrack: Ctypes.unsigned_short; (* # of spare sectors/track *)
        d_sparespercyl:	  Ctypes.unsigned_short; (* # of spare sectors/cylinder *)
        (*
         * Alternate cylinders include maintenance, replacement,
         * configuration description areas, etc.
         *)
        d_acylinders:	  Ctypes.unsigned_int; (* # of alt. cylinders/unit *)

        (* hardware characteristics: *)
        (*
         * d_interleave, d_trackskew and d_cylskew describe perturbations
         * in the media format used to compensate for a slow controller.
         * Interleave is physical sector interleave, set up by the formatter
         * or controller when formatting.  When interleaving is in use,
         * logically adjacent sectors are not physically contiguous,
         * but instead are separated by some number of sectors.
         * It is specified as the ratio of physical sectors traversed
         * per logical sector.  Thus an interleave of 1:1 implies contiguous
         * layout, while 2:1 implies that logical sector 0 is separated
         * by one sector from logical sector 1.
         * d_trackskew is the offset of sector 0 on track N
         * relative to sector 0 on track N-1 on the same cylinder.
         * Finally, d_cylskew is the offset of sector 0 on cylinder N
         * relative to sector 0 on cylinder N-1.
         *)
        d_rpm:		Ctypes.unsigned_short;(* rotational speed *)
        d_interleave:	Ctypes.unsigned_short;(* hardware sector interleave *)
        d_trackskew:	Ctypes.unsigned_short;(* sector 0 skew, per track *)
        d_cylskew:	Ctypes.unsigned_short;(* sector 0 skew, per cylinder *)
        d_headswitch:	Ctypes.unsigned_int;  (* head switch time, usec *)
        d_trkseek:	Ctypes.unsigned_int;  (* track-to-track seek, usec *)
        d_flags:	Ctypes.unsigned_int;  (* generic flags *)
        (* drive-type specific information *)
        d_drivedata:    ARRAY [0..NDDATA-1] OF Ctypes.unsigned_int;
        (* reserved for future use *)
        d_spare:        ARRAY [0..NSPARE-1] OF Ctypes.unsigned_int;
        d_magic2:	Ctypes.unsigned_int;   (* the magic number (again) *)
        d_checksum:	Ctypes.unsigned_short; (* xor of data incl. partitions *)

        (* filesystem and partition information: *)
        d_npartitions:	Ctypes.unsigned_short; (* number of partitions in following *)
        d_bbsize:	Ctypes.unsigned_int;   (* size of boot area at sn0, bytes *)
        d_sbsize:	Ctypes.unsigned_int;   (* max size of fs superblock, bytes *)
        d_partitions:   BITS MAXPARTITIONS * BITSIZE(Partition) FOR
                        ARRAY [0..MAXPARTITIONS-1] OF Partition;
        d_xxx:          Ctypes.unsigned_int; (* to make struct=for all def's *)
      END;

TYPE PartInfo = RECORD
  disklab : UNTRACED REF T;
  part    : UNTRACED REF Partition;
END;

END DiskLabelCommon.
