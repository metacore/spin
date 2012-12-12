(*
 * Copyright 1995-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Copied from user/urt/urtcore/src and removed debug code.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to be SAL independent.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	made routines FUNCTIONAL
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support to bypass indirect function calls.
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	Added subfree which frees things allocated by subarray.
 *
 * 01-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Dabbled with checksum function.  I might have broken it for odd
 *	length packets.  
 *
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	This module contains some unsafe operations to compute the
 *	address of buffers and efficient queue operations.  It will be
 *	made safer in the next release when buffers are going to resemble
 *	Unix mbufs.
 *
 * 27-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *      Defines the interface for network/external number representation
 *      conversion, checksum, queue operations, and semaphore operations.
 *      The latter two are going to be replaced by async support from the
 *      SPIN dispatcher and the standard semaphore operators.
 *)

(* Trusted *) 
UNSAFE (* imports unsafe interface *)
MODULE StcpNet;
IMPORT StcpNetExtern, Ctypes, IO, StcpNet;
IMPORT DispatcherPrivate, Dispatcher;

EPHEMERAL FUNCTIONAL
PROCEDURE htons(x:Ctypes.unsigned_short):Ctypes.unsigned_short =
  BEGIN
    RETURN StcpNetExtern.htons(x);
  END htons;

EPHEMERAL FUNCTIONAL
PROCEDURE nstoh(x:Ctypes.unsigned_short):Ctypes.unsigned_short =
  BEGIN
    RETURN StcpNetExtern.nstoh(x);
  END nstoh;

EPHEMERAL FUNCTIONAL
PROCEDURE htonl(x:Ctypes.unsigned_int):Ctypes.unsigned_int =
  BEGIN
    RETURN StcpNetExtern.htonl(x);
  END htonl;

EPHEMERAL FUNCTIONAL
PROCEDURE nltoh(x:Ctypes.unsigned_int):Ctypes.unsigned_int =
  BEGIN
    RETURN StcpNetExtern.nltoh(x);
  END nltoh;

PROCEDURE Init() = 
  BEGIN
    TRY
      DispatcherPrivate.Bypass(StcpNet.htons,StcpNetExtern.htons);
      DispatcherPrivate.Bypass(StcpNet.nstoh,StcpNetExtern.nstoh);
      DispatcherPrivate.Bypass(StcpNet.htonl,StcpNetExtern.htonl);
      DispatcherPrivate.Bypass(StcpNet.nltoh,StcpNetExtern.nltoh);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("Dispatcher error during initialization of StcpNet\n");
    END;
  END Init; 

BEGIN
END StcpNet.
