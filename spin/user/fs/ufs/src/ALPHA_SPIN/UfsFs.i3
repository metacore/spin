(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *  Defines UFS file structures -- super blocks, inodes, and directories.
 *
 * HISTORY
 * 03-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Removed unimplemented procedure declarations.
 *	
 * 05-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	merged with DiskInode.i3
 *	
 * 09-Dec-95  Yasushi Saito (yasushi) at the University of Washington
 *	deleted unused procedure decls.
 *	
 * 30-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	fsT changed to T.
 *	
 * 21-Oct-95  Yasushi Saito (yasushi) at the University of Washington
 *	Converted from fs.h.
 *
 *)



(*
  This module defines UNIX FFS superblock structures and friends.


  Each disk drive contains some number of file systems.
  A file system consists of a number of cylinder groups.
  Each cylinder group has inodes and data.
 
  A file system is described by its super-block, which in turn
  describes the cylinder groups.  The super-block is critical
  data and is replicated in each cylinder group to protect against
  catastrophic loss.  This is done at `newfs' time and the critical
  super-block data does not change, so the copies need not be
  referenced further unless disaster strikes.
 
  For file system fs, the offsets of the various blocks of interest
  are given in the super block as:
 | 	[fs->fs_sblkno]		Super-block
 | 	[fs->fs_cblkno]		Cylinder group block
 | 	[fs->fs_iblkno]		Inode blocks
 | 	[fs->fs_dblkno]		Data blocks
  The beginning of cylinder group cg in fs, is given by
  the "cgbase(fs, cg)" macro.
 
  The first boot and super blocks are given in absolute disk addresses.
  The byte-offset forms are preferred, as they don't imply a sector size.
 *)

INTERFACE UfsFs;
IMPORT Ctypes, FileDefs, UfsFsSize;

CONST BBSIZE = 8192;
CONST SBSIZE = 8192;
CONST BBOFF = 0;
CONST SBOFF = BBOFF + BBSIZE;
CONST BBLOCK = 0;
CONST SBLOCK = BBLOCK + BBSIZE DIV FileDefs.DEV_BSIZE;

(*
  Addresses stored in inodes are capable of addressing fragments
  of `blocks'. File system blocks of at most size MAXBSIZE can
  be optionally broken into 2, 4, or 8 pieces, each of which is
  addressible; these pieces may be DEV_BSIZE, or some multiple of
  a DEV_BSIZE unit.
 
  Large files consist of exclusively large data blocks.  To avoid
  undue wasted disk space, the last data block of a small file may be
  allocated as only as many fragments of a large block as are
  necessary.  The file system format retains only a single pointer
  to such a fragment, which is a piece of a single large block that
  has been divided.  The size of such a fragment is determinable from
  information in the inode, using the ``blksize(fs, ip, lbn)'' macro.
 
  The file system records space availability at the fragment level;
  to determine block availability, aligned fragments are examined.
 
  The root inode is the root of the file system.
  Inode 0 can't be used for normal purposes and
  historically bad blocks were linked to inode 1,
  thus the root inode is 2. (inode 1 is no longer used for
  this purpose, however numerous dump tapes make this
  assumption, so we are stuck with it)
 *)
CONST ROOTINO = 2;               (* i number of all roots *)

CONST MINBSIZE = 4096;
(*
  MINBSIZE is the smallest allowable block size.
  In order to insure that it is possible to create files of size
  2^32 with only two levels of indirection, MINBSIZE is set to 4096.
  MINBSIZE must be big enough to hold a cylinder group block,
  thus changes to (struct cg) must keep its size within MINBSIZE.
  Note that super blocks are always of size SBSIZE,
  and that both SBSIZE and MAXBSIZE must be >= MINBSIZE.
 *)

CONST MAXMNTLEN = 512;
(*
  The path name on which the file system is mounted is maintained
  in fs_fsmnt. MAXMNTLEN defines the amount of space allocated in
  the super block for this name.
  The limit on the amount of summary information per file system
  is defined by MAXCSBUFS. It is currently parameterized for a
  maximum of two million cylinders.
 *)

CONST MAXCSBUFS = 32;

(*
  "csumT" defines Per cylinder group information; summarized in blocks allocated
  from first cylinder group data blocks.  These blocks have to be
  read in from fs_csaddr (size fs_cssize) in addition to the
  super block.
 
 N.B. sizeof(csumT) must be a power of two in order for
 the ``fs_cs'' macro to work (see below).
 *)
TYPE
  csumT = RECORD
    cs_ndir  : Ctypes.int;  (* number of directories *)
    cs_nbfree: Ctypes.int;  (* number of free blocks *)
    cs_nifree: Ctypes.int;  (* number of free inodes *)
    cs_nffree: Ctypes.int;  (* number of free frags *)
  END;

(*
  "T" is the super block for a file system. 
  Originally "fs" in C code, but renamed
to "T" to avoid name conflict.
 *)
CONST FS_MAGIC = 16_011954;
TYPE
  T = RECORD
      xxx1       : Ctypes.int;        (* struct fs *fs_link;*)
      xxx2       : Ctypes.int;        (* struct fs *fs_rlink;*)
      fs_sblkno  : FileDefs.daddr_t;  (* addr of super-block in filesys *)
      fs_cblkno  : FileDefs.daddr_t;  (* offset of cyl-block in filesys *)
      fs_iblkno  : FileDefs.daddr_t;  (* offset of inode-blocks in filesys *)
      fs_dblkno  : FileDefs.daddr_t;  (* offset of first data after cg *)
      fs_cgoffset: Ctypes.int;        (* cylinder group offset in cylinder *)
      fs_cgmask  : Ctypes.int;        (* used to calc mod fs_ntrak *)
      fs_time    : FileDefs.time_t;     (* last time written *)
      fs_size    : Ctypes.int;        (* number of blocks in fs *)
      fs_dsize   : Ctypes.int;        (* number of data blocks in fs *)
      fs_ncg     : Ctypes.int;        (* number of cylinder groups *)
      fs_bsize   : Ctypes.int;        (* size of basic blocks in fs *)
      fs_fsize   : Ctypes.int;        (* size of frag blocks in fs *)
      fs_frag    : Ctypes.int;        (* number of frags in a block in fs *)
      (* these are configuration parameters *)
      fs_minfree : Ctypes.int;   (* minimum percentage of free blocks *)
      fs_rotdelay: Ctypes.int;   (* num of ms for optimal next block *)
      fs_rps     : Ctypes.int;   (* disk revolutions per second *)
      (* these fields can be computed from the others *)
      fs_bmask : Ctypes.int;     (* ``blkoff'' calc of blk offsets *)
      fs_fmask : Ctypes.int;     (* ``fragoff'' calc of frag offsets *)
      fs_bshift: Ctypes.int;     (* ``lblkno'' calc of logical blkno *)
      fs_fshift: Ctypes.int;     (* ``numfrags'' calc number of frags *)
      (* these are configuration parameters *)
      fs_maxcontig: Ctypes.int;  (* max number of contiguous blks *)
      fs_maxbpg   : Ctypes.int;  (* max number of blks per cyl group *)
      (* these fields can be computed from the others *)
      fs_fragshift: Ctypes.int;  (* block to frag shift *)
      fs_fsbtodb  : Ctypes.int;  (* fsbtodb and dbtofsb shift constant *)
      fs_sbsize   : Ctypes.int;  (* actual size of super block *)
      fs_csmask   : Ctypes.int;  (* csum block offset *)
      fs_csshift  : Ctypes.int;  (* csum block number *)
      fs_nindir   : Ctypes.int;  (* value of NINDIR *)
      fs_inopb    : Ctypes.int;  (* value of INOPB *)
      fs_nspf     : Ctypes.int;  (* value of NSPF *)
      (* yet another configuration parameter *)
      fs_optim: Ctypes.int;      (* optimization preference, see below *)
      (* these fields are derived from the hardware *)
      fs_npsect    : Ctypes.int;  (* # sectors/track including spares *)
      fs_interleave: Ctypes.int;  (* hardware sector interleave *)
      fs_trackskew : Ctypes.int;  (* sector 0 skew, per track *)
      fs_headswitch: Ctypes.int;  (* head switch time, usec *)
      fs_trkseek   : Ctypes.int;  (* track-to-track seek, usec *)
      (* sizes determined by number of cylinder groups and their sizes *)
      fs_csaddr: FileDefs.daddr_t;  (* blk addr of cyl grp summary area *)
      fs_cssize: Ctypes.int;        (* size of cyl grp summary area *)
      fs_cgsize: Ctypes.int;        (* cylinder group size *)
      (* these fields are derived from the hardware *)
      fs_ntrak: Ctypes.int;      (* tracks per cylinder *)
      fs_nsect: Ctypes.int;      (* sectors per track *)
      fs_spc  : Ctypes.int;      (* sectors per cylinder *)
      (* this comes from the disk driver partitioning *)
      fs_ncyl: Ctypes.int;       (* cylinders in file system *)
      (* these fields can be computed from the others *)
      fs_cpg: Ctypes.int;        (* cylinders per group *)
      fs_ipg: Ctypes.int;        (* inodes per group *)
      fs_fpg: Ctypes.int;        (* blocks per group * fs_frag *)
      (* this data must be re-computed after crashes *)
      fs_cstotal: csumT;          (* cylinder summary information *)
      (* these fields are cleared at mount time *)
      fs_fmod : Ctypes.char;     (* super block modified flag *)
      fs_clean: Ctypes.char;     (* file system is clean flag *)
      fs_ronly: Ctypes.char;     (* mounted read-only flag *)
      fs_flags: Ctypes.char;     (* currently unused flag *)
      fs_fsmnt: ARRAY [1 .. MAXMNTLEN] OF Ctypes.char;  (* name mounted on *)
      (* these fields retain the current block allocation info *)
      fs_cgrotor: Ctypes.int;                     (* last cg searched *)
      was_fs_csp: ARRAY [1 .. MAXCSBUFS] OF Ctypes.int;
      fs_cpc: Ctypes.int;        (* cyl per cycle in postbl *)
      fs_opostbl: ARRAY [1 .. 16], [1 .. 8] OF Ctypes.short;  (* old rotation
                                                                 block list
                                                                 head *)
      fs_sparecon: ARRAY [1 .. 56] OF Ctypes.int;  (* reserved for future
                                                      constants *)
      fs_qbmask: FileDefs.quad;  (* ~fs_bmask - for use with quad size *)
      fs_qfmask: FileDefs.quad;  (* ~fs_fmask - for use with quad size *)
      fs_postblformat: Ctypes.int;  (* format of positional layout tables *)
      fs_nrpos       : Ctypes.int;  (* number of rotaional positions *)
      fs_postbloff   : Ctypes.int;  (* (short) rotation block list head *)
      fs_rotbloff: Ctypes.int;   (* (Ctypes.unsigned_char) blocks for each
                                    rotation *)
      fs_magic: Ctypes.int;      (* magic number *)
      fs_space: ARRAY [1 .. 1] OF Ctypes.unsigned_char;  (* list of blocks for
                                                            each rotation *)
      (* actually longer *)
    END;

(*
  Preference for optimization.
 *)
CONST FS_OPTTIME = 0;            (* minimize allocation time *)
CONST FS_OPTSPACE = 1;           (* minimize disk fragmentation *)

(*
  Rotational layout table format types
 *)
CONST FS_42POSTBLFMT = -1;       (* 4.2BSD rotational table format *)
CONST FS_DYNAMICPOSTBLFMT = 1;   (* dynamic rotational table format *)

(*
  Cylinder group block for a file system.
 *)
CONST CG_MAGIC = 16_090255;

TYPE
  cg =
    RECORD
      xxx1     : Ctypes.int;     (* struct cg *cg_link;*)
      cg_magic : Ctypes.int;     (* magic number *)
      cg_time  : FileDefs.time_t;  (* time last written *)
      cg_cgx   : Ctypes.int;     (* we are the cgx'th cylinder group *)
      cg_ncyl  : Ctypes.short;   (* number of cyl's this cg *)
      cg_niblk : Ctypes.short;   (* number of inode blocks this cg *)
      cg_ndblk : Ctypes.int;     (* number of data blocks this cg *)
      cg_cs    : csumT;           (* cylinder summary information *)
      cg_rotor : Ctypes.int;     (* position of last used block *)
      cg_frotor: Ctypes.int;     (* position of last used frag *)
      cg_irotor: Ctypes.int;     (* position of last used inode *)
      (* counts of available frags *)
      cg_frsum: ARRAY [1 .. FileDefs.MAXFRAG] OF Ctypes.int;
      cg_btotoff : Ctypes.int;   (* (long) block totals per cylinder *)
      cg_boff    : Ctypes.int;   (* (short) free block positions *)
      cg_iusedoff: Ctypes.int;   (* (char) used inode map *)
      cg_freeoff : Ctypes.int;   (* (Ctypes.unsigned_char) free block map *)
      cg_nextfreeoff: Ctypes.int;  (* (Ctypes.unsigned_char) next available
                                      space *)
      cg_sparecon: ARRAY [1 .. 16] OF Ctypes.int;  (* reserved for future
                                                      use *)
      cg_space: ARRAY [1 .. 1] OF Ctypes.unsigned_char;  (* space for cylinder
                                                            group maps *)
      (* actually longer *)
    END;

PROCEDURE fsbtodb(READONLY fs : T; b:INTEGER) : INTEGER;
(*
 Turn file system block numbers into disk block addresses.
 This maps file system blocks to device size blocks.
 *)

PROCEDURE dbtofsb(READONLY fs : T; b:INTEGER) : INTEGER;


(*
  Macros for handling inode numbers:
 
      "itoo" : inode number to file system block offset.
 
      "itog" : inode number to cylinder group number.
 
      "itod" : inode number to file system block address.
 *)
PROCEDURE itoo(READONLY fs: T; x: INTEGER): INTEGER;
PROCEDURE itog(READONLY fs: T; x: INTEGER): INTEGER;
PROCEDURE itod(READONLY fs: T; x: INTEGER): FileDefs.daddr_t;

(*
  The following macros optimize certain frequently calculated
  quantities by using shifts and masks in place of divisions
  modulos and multiplications.
 *)
PROCEDURE blkoff(READONLY fs : T; loc : INTEGER) : INTEGER;
(* calculates (loc % fs->fs_bsize) *)
PROCEDURE lblkno(READONLY fs : T; loc : INTEGER) : INTEGER;
(* calculates (loc / fs->fs_bsize) *)
PROCEDURE fragoff(READONLY fs: T; loc : INTEGER) : INTEGER;
(* calculates (loc % fs->fs_fsize) *)
PROCEDURE numfrags(READONLY fs: T; loc : INTEGER) : INTEGER;
(* calculates (loc / fs->fs_fsize) *)
PROCEDURE fragroundup(READONLY fs: T; size : INTEGER) : INTEGER;
(* calculates roundup(size, fs->fs_fsize) *)

(*
  "INOPB" is the number of inodes in a secondary storage block.
 *)
PROCEDURE INOPB(READONLY fs : T) : Ctypes.int;
PROCEDURE INOPF(READONLY fs : T) : INTEGER;


(*
 The I node is the focus of all file activity in the BSD Fast File System.
 There is a unique inode allocated for each active file,
 each current directory, each mounted-on file, text file, and the root.
 An inode is 'named' by its dev/inumber pair. (iget/iget.c)
 Data in icommon is read in from permanent inode on volume.
 *)

CONST NDADDR = 12;		(* direct addresses in inode *)
CONST NIADDR = 3;		(* indirect addresses in inode *)

CONST MAX_FASTLINK_SIZE = ((NDADDR + NIADDR) * BYTESIZE(FileDefs.daddr_t));

(*
   Inode structures
 *)

  
TYPE
  IndirectBlocks = RECORD  
    ic_db : ARRAY [0..NDADDR-1] OF FileDefs.daddr_t; (* 40:dblock addresses*)
    ic_ib : ARRAY [0..NIADDR-1] OF FileDefs.daddr_t; (* 88:indirect blocks*)
  END;
  
  icommon = RECORD
    ic_mode                         : Ctypes.unsigned_short;
    (* 0: mode and type of file *)
    ic_nlink                        : Ctypes.short;
    (* 2: number of links to file *)
    ic_uid                          : Ctypes.unsigned_short;
    ic_gid                          : Ctypes.unsigned_short;
    (* 8: number of bytes in file. *)
    
    ic_size                         : UfsFsSize.T;
    (* BUG time_t should be defined in Ctypes *)
    
    (* 16: time last accessed *)
    ic_atime                        : FileDefs.time_t;
    ic_atspare                      : Ctypes.int;
    (* 24: time last modified *)
    ic_mtime                        : FileDefs.time_t; 
    ic_mtspare                      : Ctypes.int;
    (* 32: last time inode changed *)
    ic_ctime                        : FileDefs.time_t; 
    ic_ctspare                      : Ctypes.int;
    
    (* ic_db and ic_ib together are used as a store for link name when
     the inode is symlink type and its linkname is shorter than
     BYTESIZE(b_db)+BYTESIZE(b_ib).
     The procedure symlink_direct_name gets the link value.
     *)
    ic_indirect : ARRAY [0 .. BYTESIZE(IndirectBlocks)-1] OF CHAR;
    
    (* 100: status *)
    ic_flags                        : Ctypes.int;
    (* 104: blocks actually held *)
    ic_blocks                       : Ctypes.int;
    (* 108: generation number *)
    ic_gen                          : Ctypes.int;
    (* 112: reserved, currently unused *)
    ic_spare                        : ARRAY [0..3] OF Ctypes.int;
  END;

PROCEDURE GetLinkNameFromInode(READONLY d:icommon; VAR buf : ARRAY OF CHAR);
(* Get the symlink name that is stored directly in the inode.
 *)

(* Bits in ic_flas *)
CONST IC_FASTLINK = 16_0001;		(* Symbolic link in inode *)

(* Mode bits for ic_mode *)
CONST IFMT   = 16_f000;          (* type of file *)
CONST IFCHR  = 16_2000;          (* character special *)
CONST IFDIR  = 16_4000;          (* directory *)
CONST IFBLK  = 16_6000;          (* block special *)
CONST IFREG  = 16_8000;          (* regular *)
CONST IFLNK  = 16_a000;          (* symbolic link *)
CONST IFSOCK = 16_c000;          (* socket *)

CONST ISUID  = 16_0800;          (* set user id on execution *)
CONST ISGID  = 16_0400;          (* set group id on execution *)
CONST ISVTX  = 16_0200;          (* save swapped text even after use *)
CONST IREAD  = 16_0100;          (* read, write, execute permissions *)
CONST IWRITE = 16_0080;
CONST IEXEC  = 16_0040;

(*
 *	Same structure, but on disk.
 *)
TYPE dinode = RECORD
  di_un: ARRAY [0..127] OF CHAR
  (* 
     union {
     struct icommon      di_com;
     char                di_char[128];
     } di_un;
  *)
END;
  

(*
   Directory structures
 *)

(*
  A directory consists of some number of blocks of DIRBLKSIZ
  bytes, where DIRBLKSIZ is chosen such that it can be transferred
  to disk in a single atomic operation (e.g. 512 bytes on most machines).
 
  Each DIRBLKSIZ byte block contains some number of directory entry
  structures, which are of variable length.  Each directory entry has
  a struct direct at the front of it, containing its inode number,
  the length of the entry, and the length of the name contained in
  the entry.  These are followed by the name padded to a 4 byte boundary
  with null bytes.  All names are guaranteed null terminated.
  The maximum length of a name in a directory is MAXNAMLEN.
 
  The macro DIRSIZ(dp) gives the amount of space required to represent
  a directory entry.  Free space in a directory is represented by
  entries which have dp->d_reclen > DIRSIZ(dp).  All DIRBLKSIZ bytes
  in a directory block are claimed by the directory entries.  This
  usually results in the last entry in a directory having a large
  dp->d_reclen.  When entries are deleted from a directory, the
  space is returned to the previous entry in the same directory
  block by increasing its dp->d_reclen.  If the first entry of
  a directory block is free, then its dp->d_ino is set to 0.
  Entries other than the first in a directory do not normally have
  dp->d_ino set to 0.
 *)
CONST DIRBLKSIZ = FileDefs.DEV_BSIZE;
CONST MAXNAMLEN = 255;

TYPE DirEntry = RECORD
  d_ino     : Ctypes.unsigned_int;   (* inode number of entry *)
  d_reclen  : Ctypes.unsigned_short; (* length of this record *)
  d_namelen : Ctypes.unsigned_short; (* length of d_name  *)
  (* char d_name[d_namelen] follows. *)
END;

END UfsFs.
