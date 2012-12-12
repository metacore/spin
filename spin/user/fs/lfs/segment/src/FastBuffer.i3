(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Borrowed from Gun's httpd/src.  Changed Interface name as
 *      SegBuffer.m3 defines Buffer type.
 *
 * 02-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Buffer manager and recycler.
 *)
INTERFACE FastBuffer;
IMPORT FastList;

TYPE T = FastList.T OBJECT
  data: REF ARRAY OF CHAR;
END;

CONST
  DefaultSize = 8192;

PROCEDURE Allocate(size: CARDINAL := DefaultSize) : T;

PROCEDURE Deallocate(buffer: T);

END FastBuffer.
