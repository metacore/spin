(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 09-Sep-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed name to FilePager.
 *
 *)

INTERFACE FilePager;
IMPORT PagerObject;
PROCEDURE Create(size: CARDINAL) : PagerObject.T;
END FilePager.
