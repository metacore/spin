(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 24-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE PagerObject;
IMPORT PhysAddr;

TYPE T = OBJECT
  METHODS
    pageOut(off: PageNumber; frame: PhysAddr.T; dirty: BOOLEAN): ResultCode;
  END;
  PageNumber = INTEGER;
  ResultCode = {Success, AlreadyPagedOut};

END PagerObject.
