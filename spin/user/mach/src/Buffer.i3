(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  David Dion (ddion) at the University of Washington
 *	Borrowed from httpd code for copyin/copyout buffer management
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Buffer manager and recycler.
 *)
INTERFACE Buffer;
IMPORT FastList;

TYPE T = FastList.T OBJECT
  data: REF ARRAY OF CHAR;
END;

CONST
  DefaultSize = 8192;

PROCEDURE Allocate(size: CARDINAL := DefaultSize) : T;

PROCEDURE Deallocate(buffer: T);

PROCEDURE FreeBuffer(buffer: REF ARRAY OF CHAR; len: CARDINAL; arg: REFANY);

END Buffer.
