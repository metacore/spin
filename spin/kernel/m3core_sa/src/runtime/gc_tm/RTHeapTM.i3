(* Copyright (C) 1990, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* Last modified on Fri Sep  4 17:29:36 PDT 1992 by rustan     *)
(*      modified on Thu Jan 30 14:47:00 PST 1992 by kalsow     *)
(*      modified on Wed Jul  3 04:15:39 1991 by muller         *)

UNSAFE INTERFACE RTHeapTM;
IMPORT RTHeapRep, RTCollectorSRC;
IMPORT RTRefCount;
FROM RT0 IMPORT Typecode;

(* doubly linked lists for page lists. should switch to singly linked
lists *)

TYPE
  PageLink = RECORD 
    next, prev : RTHeapRep.Page;
  END;


(* computed during VisitHeapRefs *)
VAR
  Fragmentation : INTEGER;
  LiveBytes  : INTEGER;
  AllocatedBytes : INTEGER;
  ReallyLiveObjects : INTEGER;

PROCEDURE InstallSanity(tf :BOOLEAN);
PROCEDURE TMAlloc (dataSize : CARDINAL;
                   tc       : Typecode) : ADDRESS;

PROCEDURE ResetStats ();
PROCEDURE GetStats (VAR s : RTCollectorSRC.Statistics);
PROCEDURE CollectInternal();
PROCEDURE VisitHeapRefs(v:RTHeapRep.RefVisitor);
PROCEDURE InitHeap();
(* initializes the heap; should be called as the first thing from
   RTMain.Run, and from nowhere else *)


(* WRITE BARRIER *)
(* XXX IF THE POSITION OF THESE PROCEDURES CHANGES, UPDATE
   ALPHA_SPIN/RTWriteBarrier MACROS *)
PROCEDURE WriteBarrier(dest : ADDRESS; val : ADDRESS; ra : ADDRESS);
PROCEDURE WriteBarrierCAS(dest : ADDRESS; val : ADDRESS);
PROCEDURE WriteBarrierENQ(dest : ADDRESS; val : ADDRESS);
PROCEDURE WriteBarrierDEQ(dest : ADDRESS; val : ADDRESS);

(* incremental collection *)
PROCEDURE CollectFinish();

(* for fragmentation reporting *)
PROCEDURE ForceFragSample();

<* EXTERNAL *>
VAR
  COLLECTING : INTEGER; 

CONST 
  Implicit =TRUE;      (* Turn off for sanity checking etc. if FALSE, some extra
                            checks are automatically enabled *)
  Incremental = RTRefCount.WRITEBARRIER; 
  Remapping = TRUE;     (* VM remapping to/within reservepool.  Also enables moving
                           pages from reservepool to free lists *)


END RTHeapTM.






