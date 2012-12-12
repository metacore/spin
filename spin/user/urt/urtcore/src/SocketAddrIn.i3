(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 07-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Split into new interface for cross SAL platform support.
 *
 * 22-Jul-96  Frederick Gray (fgray) at the University of Washington
 *	Corrected representation of sin_zero in sockaddr_in from
 *	INTEGER to ARRAY [1..8] OF CHAR.
 *
 *)

INTERFACE SocketAddrIn;
IMPORT Ctypes;
TYPE in_addrT = BITS 32 FOR Ctypes.unsigned_int;
TYPE in_portT = BITS 16 FOR Ctypes.unsigned_short; 
TYPE T = RECORD
  sin_len    : BITS 8 FOR Ctypes.unsigned_char; (* u_char  sin_len; *)
  sin_family : BITS 8 FOR Ctypes.unsigned_char; (* u_char  sin_family; *)
  sin_port   : in_portT;                        (* u_short sin_port; *)
  sin_addr   : in_addrT;                        (* struct  in_addr sin_addr; *)
  sin_zero   : ARRAY [1..8] OF CHAR;            (* char sin_zero[8]; *)
END;
CONST SIN_ZERO = ARRAY [1..8] OF CHAR {'\000',..};
END SocketAddrIn.
