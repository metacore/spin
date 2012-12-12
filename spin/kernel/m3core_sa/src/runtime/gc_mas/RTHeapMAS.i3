(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Sep  4 17:29:36 PDT 1992 by rustan     *)
(*      modified on Thu Jan 30 14:47:00 PST 1992 by kalsow     *)
(*      modified on Wed Jul  3 04:15:39 1991 by muller         *)

UNSAFE INTERFACE RTHeapMAS;
IMPORT RTHeapRep;

(* This interface provides safe access to the storage allocator
   and garbage collector. *)

PROCEDURE InitHeap();
(* initializes the heap; should be called as the first thing from
   RTMain.Run, and from nowhere else *)

END RTHeapMAS.
