(*
  Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 01-Nov-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *
 *)
INTERFACE LFSLock;

TYPE
  T <: Public;

  Public = BRANDED OBJECT
  METHODS
    init(): T;

    (* used to read/write files to/from LFS. blocked while lockwhole(). *)
    lock();
    unlock();

    (* used to lock entire LFS for sync. *)
    lockwhole();
    unlockwhole();
  END;

END LFSLock.
