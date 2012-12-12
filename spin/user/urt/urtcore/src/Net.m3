(*
 * Copyright 1995-1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
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
MODULE Net;
IMPORT NetExtern, Ctypes, IO, Net;
IMPORT DispatcherPrivate, Dispatcher;

PROCEDURE MapDebug(i:INTEGER):Level = 
  BEGIN
    IF i >= ORD(FIRST(oLevel)) AND i <= ORD(LAST(oLevel)) THEN
      RETURN VAL(i,oLevel);
    ELSE
      RETURN FIRST(oLevel);
    END;
  END MapDebug;

(* Debugging information *)
PROCEDURE Debug(debug_level, priority : Level; READONLY x:TEXT) = 
  BEGIN
    (* might want to do something more special per log message *)
    IF debug_level <= priority THEN IO.Put(x); END;
  END Debug;

EPHEMERAL FUNCTIONAL
PROCEDURE htons(x:Ctypes.unsigned_short):Ctypes.unsigned_short =
  BEGIN
    RETURN NetExtern.htons(x);
  END htons;

EPHEMERAL FUNCTIONAL
PROCEDURE nstoh(x:Ctypes.unsigned_short):Ctypes.unsigned_short =
  BEGIN
    RETURN NetExtern.nstoh(x);
  END nstoh;

EPHEMERAL FUNCTIONAL
PROCEDURE htonl(x:Ctypes.unsigned_int):Ctypes.unsigned_int =
  BEGIN
    RETURN NetExtern.htonl(x);
  END htonl;

EPHEMERAL FUNCTIONAL
PROCEDURE nltoh(x:Ctypes.unsigned_int):Ctypes.unsigned_int =
  BEGIN
    RETURN NetExtern.nltoh(x);
  END nltoh;

PROCEDURE Init() = 
  BEGIN
    TRY
      DispatcherPrivate.Bypass(Net.htons,NetExtern.htons);
      DispatcherPrivate.Bypass(Net.nstoh,NetExtern.nstoh);
      DispatcherPrivate.Bypass(Net.htonl,NetExtern.htonl);
      DispatcherPrivate.Bypass(Net.nltoh,NetExtern.nltoh);
    EXCEPT
    | Dispatcher.Error =>
      IO.PutError("Dispatcher error during initialization of StcpNet\n");
    END;
  END Init; 

BEGIN
END Net.
