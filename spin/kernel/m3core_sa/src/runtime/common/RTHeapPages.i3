(*
 * Copyright 1994, 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)

(*
 * HISTORY
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

UNSAFE INTERFACE RTHeapPages;

IMPORT RTIO, RTHeapRep;

PROCEDURE DumpPageStatus (putter: RTIO.SimplePutter; full: BOOLEAN);
(* Prints the summary of status of all pages in the system.  If full is TRUE 
   prints each contiguous range of pages with the same status separately. *)

PROCEDURE PutPageStatus (putter: RTIO.SimplePutter; page: RTHeapRep.Page);
(* Print status of a single page *)

PROCEDURE Init();
(* Initialization *)

END RTHeapPages.
