(*| Copyright (C) 1992, Digital Equipment Corporation        *)
(*| All rights reserved.                                     *)
(*| See the file COPYRIGHT for a full description.           *)

(*| Last modified on Sun Feb 21 14:16:05 PST 1993 by jdd     *)
(*|      modified on Fri Mar 20 14:02:59 PST 1992 by muller  *)

(* "RTHeapEvent" is a private interface. *)

(*
 * HISTORY
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Removed padding while bringing the code closer to SRC distribution.
 *
 * 13-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Added padding to descriptor to please 32-bit machines.
 *
 *)

UNSAFE INTERFACE RTHeapEvent;

IMPORT RTHeapRep;

TYPE
  Kind = {
         (* program -> tool *)
         Begin, Flip, Roots, End, Change, Grow, Off, Bye,

         (* tool -> program *)
         CollectNow, GCOff, GCOn};

  T = RECORD
        kind : Kind;
        first: RTHeapRep.Page := RTHeapRep.Nil;
        nb   : CARDINAL       := 1;
        desc := RTHeapRep.Desc{
                  space := RTHeapRep.Space.Unallocated, generation :=
                  RTHeapRep.Generation.Younger, pure := FALSE, note :=
                  RTHeapRep.Note.Allocated, gray := FALSE,
		  protected := FALSE, continued := FALSE};
      END;

END RTHeapEvent.
