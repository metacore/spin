(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src.
 *
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Split into new interface for cross SAL platform support.
 *)

INTERFACE StcpSocketAddr;
IMPORT Ctypes;
CONST SALEN = 14;
TYPE T = RECORD (* sys/socket.h sockaddr *)
  sa_len    : BITS 8 FOR Ctypes.unsigned_char; (* unsigned char total length
 *)
  sa_family : BITS 8 FOR Ctypes.unsigned_char; (* unsigned char address family
 *)
  sa_data   : ARRAY [1..SALEN] OF CHAR;        (* actually longer; address value
 *)
END;
TYPE OldT = RECORD
  sa_family: BITS 16 FOR Ctypes.unsigned_short;
  sa_data  : ARRAY [1..SALEN] OF CHAR;
END;
END StcpSocketAddr.



