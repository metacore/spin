(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
 * HISTORY
 * 18-Feb-96  Yasushi Saito (yasushi) at the University of Washington
 *	added Get{UserSpaceThread,Space}.
 * 17-Nov-95  Yasushi Saito (yasushi) at the University of Washington
 *	created.
 *)

(*
 *  Run-time utilities for Sieg generated codes.
 *)

INTERFACE Sieg;
IMPORT Space, VirtAddr, SpinException;
IMPORT UserSpaceThread;
IMPORT VMError;
IMPORT NameServer;
IMPORT Translation;

CONST	Brand = "Sieg";
(* Define some standard types here. They should be moved somewhere
 *  in the future
 *)
TYPE
  Word8 = BITS 8 FOR [0..16_FF];
  Word16 = BITS 16 FOR [0..16_FFFF];
  Word32 = BITS 32 FOR [0..16_FFFFFFFF];
  Word64 = INTEGER;

TYPE Context = RECORD
  space: Space.T;
  caller: UserSpaceThread.T;
END;
  
PROCEDURE UnpackTEXT(s: Translation.T; addr: VirtAddr.Address): TEXT
  RAISES {VMError.E};
(*
  Get the Text instance from the user space <SPACE, ADDR>
 *)

PROCEDURE UnpackCTEXT(s: Translation.T; addr: VirtAddr.Address): TEXT
  RAISES {VMError.E};
(* Get the C-style string from the user space <SPACE, ADDR> and
   ocnvert it into M3 TEXT. *)
  
PROCEDURE UnpackNSName(s: Translation.T; addr: VirtAddr.Address;
		       VAR n: NameServer.Name) RAISES {VMError.E};

PROCEDURE UnpackCArrayOfChar(s: Translation.T; addr: VirtAddr.Address;
			     (*OUT*)VAR buf: ARRAY OF CHAR;
			     (*OUT*)VAR len: INTEGER)
  RAISES {VMError.E};
  
PROCEDURE SpinExceptionToErrno(READONLY e: SpinException.ExceptionInfo)
 : INTEGER;
  
(* Convert spin exception code into UNIX errno.
 XXX errno is unix specific. shall we put it in Sieg code? *)
END Sieg.
