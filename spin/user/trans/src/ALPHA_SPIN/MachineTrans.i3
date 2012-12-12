(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE MachineTrans;
IMPORT Ctypes;

TYPE
  DiffEntry = RECORD
    from, len: BITS 32 FOR Ctypes.unsigned_int;
  END;

END MachineTrans.
