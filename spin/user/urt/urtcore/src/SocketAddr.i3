(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Split into new interface for cross SAL platform support.
 *)

INTERFACE SocketAddr;
IMPORT Ctypes;
CONST SALEN = 14;
TYPE T = RECORD (* sys/socket.h sockaddr *)
  sa_len    : BITS 8 FOR Ctypes.unsigned_char; (* unsigned char total length     *)
  sa_family : BITS 8 FOR Ctypes.unsigned_char; (* unsigned char address family   *)
  sa_data   : ARRAY [1..SALEN] OF CHAR;        (* actually longer; address value *)
END;
TYPE OldT = RECORD
  sa_family: BITS 16 FOR Ctypes.unsigned_short;
  sa_data  : ARRAY [1..SALEN] OF CHAR;
END;
END SocketAddr.
