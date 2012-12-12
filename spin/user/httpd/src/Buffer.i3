(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Buffer manager and recycler.
 *)
INTERFACE Buffer;
IMPORT FastList;

TYPE T = FastList.T OBJECT
  data: REF ARRAY OF CHAR;
END;

CONST
  DefaultSize = 8000;

PROCEDURE Allocate(size: CARDINAL := DefaultSize) : T;

PROCEDURE Deallocate(buffer: T);

END Buffer.
