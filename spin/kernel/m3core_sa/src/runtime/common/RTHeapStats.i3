(* Copyright (C) 1994, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)
(*                                                             *)
(* Last modified on Thu May  5 08:10:43 PDT 1994 by kalsow     *)


(*
 * HISTORY
 * 23-Feb-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added some stats for reference counting
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of typeidx.
 *
 * 21-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added support for summary for a single reference.
 *
 * 12-Mar-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed to support three modes of operation (see RTHeapStats.m3
 *	for details).
 *
 * 19-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Introduce SimplePutter.
 *
 * 30-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Added ReportReachableText to return TEXT string with HTML
 *      annotations.
 *)

INTERFACE RTHeapStats;

IMPORT RTIO, RT0, Word;

PROCEDURE ReportReachable (p: RTIO.SimplePutter; 
                           tc: RT0.Typecode := 0;
                           ref: INTEGER := 0;
                           detailed: BOOLEAN := FALSE;
                           types: BOOLEAN := FALSE;
                           pointers_only: BOOLEAN := FALSE;
                           all_pointers: BOOLEAN := FALSE);

(* reports the number of reachable objects and bytes from
   each compilation unit, thread stack, and the individual roots
   that reach the most bytes.  The report is written to
   p.  The Modula-3 process is frozen during the scanning
   and reporting.  *)

CONST
  DoTracing = FALSE;

VAR
  objects     : UNTRACED REF ARRAY OF ADDRESS;
  obj_cnt     : INTEGER;

(* gc summary info *)

(* pointer summary info *)
PROCEDURE Ptrs (addr, old, new: ADDRESS);

(* typecode summary info *)
PROCEDURE CountTC (addr: ADDRESS; tc: RT0.Typecode);
PROCEDURE ResetTC ();
PROCEDURE PrintTC (p: RTIO.SimplePutter; number: CARDINAL);

(* addr-specific info *)
PROCEDURE AllocateAddr (addr: ADDRESS; tc: RT0.Typecode; size: CARDINAL);
PROCEDURE DeallocateAddr (addr: ADDRESS; tc: RT0.Typecode; size: CARDINAL);
PROCEDURE MoveAddr (old, new: ADDRESS; tc: RT0.Typecode);
PROCEDURE ImplicitMoveAddr (addr: ADDRESS; tc: RT0.Typecode);
PROCEDURE TenureAddr (addr: ADDRESS; tc: RT0.Typecode);
PROCEDURE Collection ();
PROCEDURE PrintAddr (p: RTIO.SimplePutter; number: CARDINAL);
PROCEDURE ResetAddr ();

(* pc-specific info *)
PROCEDURE AllocatePC (pc: ADDRESS; size: Word.T);
PROCEDURE PrintPC (p: RTIO.SimplePutter; number: CARDINAL);
PROCEDURE ResetPC ();

PROCEDURE ResetDeallocation ();
PROCEDURE DeallocatedBytes () : Word.T;
PROCEDURE AllocatedBytes () : Word.T;

PROCEDURE Init ();
PROCEDURE PrintTraceInfo (p: RTIO.SimplePutter);
PROCEDURE UpdateClock (size: Word.T);


(* -------- Fragmentation reporting *)
CONST
  ReportFragmentation = FALSE;

(* allows frag sampling to be turned on or off at run time *)
VAR
  FragOn := FALSE;

PROCEDURE InitSampleArray (heapSize: INTEGER);
PROCEDURE SampleAlloc (allocSize : INTEGER) : BOOLEAN;
PROCEDURE SampleFrag(activeBytes : INTEGER;
                     allocBytes : INTEGER;
                     start : BOOLEAN := FALSE;
                     end   : BOOLEAN := FALSE);

PROCEDURE ResetSamples ();
PROCEDURE DumpSamples ();
PROCEDURE DumpHisto();


END RTHeapStats.
