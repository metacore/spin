(*
 *
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 08-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to not perform any hidden allocation.
 *
 * 29-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Mime related routines.
 *
 *)
INTERFACE Mime;

PROCEDURE FindMimeType(READONLY url: ARRAY OF CHAR; urllen: CARDINAL) : TEXT;

END Mime.
