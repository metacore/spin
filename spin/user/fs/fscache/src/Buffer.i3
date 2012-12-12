(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Jun-97  David Dion (ddion) at the University of Washington
 *	Created.
 *
 *)
INTERFACE Buffer;

IMPORT DoubleList;
IMPORT PhysAddr, MachineMem;

TYPE
  State = { Free, Occupied };
  T = DoubleList.EltT OBJECT
    data: PhysAddr.T;
    owner: REFANY;  (* FileMObjCache.T, but need to avoid circular import *)
    index: INTEGER; (* location in owner *)
  END;

CONST
  BlockSize = MachineMem.PAGESIZE;

END Buffer.
