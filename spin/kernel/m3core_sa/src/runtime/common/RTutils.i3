(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Fri Nov 18 17:32:30 PST 1994 by kalsow     *)
(*|      modified on Fri May  6 13:28:25 PDT 1994 by detlefs    *)
(*|      modified on Sun Feb 21 14:18:42 PST 1993 by jdd        *)
(*|      modified on Tue Jun 16 10:41:17 PDT 1992 by muller     *)
(*|      modified on Mon Jun  8 11:25:23 PDT 1992 by meehan     *)

(*
 * HISTORY
 * 03-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added reinitailiaztion after the runtime changes.
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Differentiate between calls from inside the GC and from outside.
 *	Made unsafe.
 *
 * 24-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Removed HeapText. Pass in RTIO.SimplePutter to Heap.
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Paramaterized HeapText to take pad and int fmt functions so we
 *	 don't build with dependency.  HeapText DOES NOT BELONG in here.
 *
 * 01-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Added HeapText to return TEXT with HTML commands.
 *)

(* "RTutils" provides information on the heap.  This interface is subject
   to change without notice. *)

(* unsafe because its invocation freezes the whole system for a very
   long time *)
UNSAFE INTERFACE RTutils;

IMPORT RTIO;

TYPE
  HeapPresentation = { ByTypecode, ByNumber, ByByteCount };

PROCEDURE Heap (suppressZeros := FALSE;
                presentation  := HeapPresentation.ByTypecode;
                window        := LAST(INTEGER);
                putter: RTIO.SimplePutter := NIL;
                fromGC : BOOLEAN := FALSE);
(* This prints a table of all the known types and some simple statistics
   about them (count, total size in bytes for all such objects, average
   size).  If "suppressZeros" is true, then nothing will be printed for a
   type that has no instances in the heap.  The "presentation"
   argument controls the order in which types appear: "ByTypeCode"
   indicates that types are printed in ascending typecode order,
   "ByNumber" indicates they are presented in order of decreasing
   count, and "ByByteCount" indicates that they are printed in order
   of decreasing space usage.  The "window" arguments limits how
   many types are printed.  The table is printed on stderr using RTIO. *)

PROCEDURE NewHeap (suppressZeros := TRUE;
                   presentation  := HeapPresentation.ByTypecode;
                   window        := LAST(INTEGER);
                   putter: RTIO.SimplePutter := NIL;
                   fromGC : BOOLEAN := FALSE);
(* This prints only the incremental information since the last call to Heap
   or NewHeap.  The arguments have the same meaning as in "Heap." *)

PROCEDURE Init ();
PROCEDURE Reset ();

END RTutils.
