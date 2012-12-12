(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be arch independent.
 *
 * 15-Sep-95  Marc Fiuczynski (mef) at the University of Washington
 *	This interface defines the types from sys/uio.h.
 *
 *)

INTERFACE Uio;
IMPORT UioDep;

CONST
  UIO_READ       = UioDep.UIO_READ;
  UIO_WRITE      = UioDep.UIO_WRITE;
  UIO_AIORW      = UioDep.UIO_AIORW;
  UIO_USERSPACE  = UioDep.UIO_USERSPACE;
  UIO_SYSSPACE   = UioDep.UIO_SYSSPACE;
  UIO_USERISPACE = UioDep.UIO_USERISPACE;
  UIO_PHYSSPACE  = UioDep.UIO_PHYSSPACE;
TYPE
  caddr_t        = UioDep.caddr_t;
  daddr_t        = UioDep.daddr_t;
  off_t          = UioDep.off_t;
  dev_t          = UioDep.dev_t;
  iovecT         = UioDep.iovecT;
  enum_uio_rw    = UioDep.enum_uio_rw;
  enum_uio_seg   = UioDep.enum_uio_seg;
  uioT           = UioDep.uioT;
END Uio.
