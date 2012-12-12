(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Specific to DEC OSF/1 uio.h.
 *
 * 15-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	This interface defines the types from sys/uio.h.
 *
 *)

INTERFACE UioDep;
IMPORT Ctypes;

TYPE caddr_t = Ctypes.char_star;
     daddr_t = Ctypes.int;
     off_t   = Ctypes.unsigned_long;
     dev_t   = Ctypes.int;

(* XXX: NOTICE, if we change the iov_len to be a Ctypes.long, then we
   could make the iovecT an M3 open array header.  But this might be
   difficult, since user-level programs also use iovecs.
*)

TYPE iovecT = RECORD 
    iov_base : caddr_t; 
    iov_len  : Ctypes.int; 
END;


TYPE enum_uio_rw = Ctypes.int;
CONST UIO_READ  : enum_uio_rw = 0;
      UIO_WRITE : enum_uio_rw = 1;
      UIO_AIORW : enum_uio_rw = 2;

TYPE enum_uio_seg = Ctypes.int;
CONST UIO_USERSPACE  : enum_uio_seg = 0; (* from user data space        *)
      UIO_SYSSPACE   : enum_uio_seg = 1; (* from system space           *)
      UIO_USERISPACE : enum_uio_seg = 2; (* from user I space           *)
      UIO_PHYSSPACE  : enum_uio_seg = 3; (* from physical address space *)

TYPE uioT = RECORD
  uio_iov    : UNTRACED REF iovecT; (* struct  iovec *uio_iov;                                                    *)
  uio_offset : off_t;      (* off_t   uio_offset;                                                        *)
  uio_iovcnt : Ctypes.int;          (* int     uio_iovcnt;                                                        *)
  uio_resid  : Ctypes.int;          (* int     uio_resid;                                                         *)
  uio_segflg : enum_uio_seg;        (* enum    uio_seg uio_segflg;                                                *)
  uio_rw     : enum_uio_rw;         (* enum    uio_rw uio_rw;                                                     *)
                                    (* The next two fields were added to support DDI/DKI interfaces.              *)
  uio_fmode  : Ctypes.int;          (* int     uio_fmode; /* File mode flags. Drivers can't set this */           *)
  uio_limit  : daddr_t;    (* daddr_t uio_limit; /* Max. blk. offset for file. Drivers can't set this */ *)
END;


END UioDep.
