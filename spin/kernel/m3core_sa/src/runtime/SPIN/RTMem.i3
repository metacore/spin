(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(* HISTORY
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Added GetTraced* and SetTraced* back for the Treadmill collector.
 *
 * 31-May-97  David Becker at the University of Washington
 *	Removed GetTraced* and SetTraced*
 *
 * 22-Nov-96  Marc Fiuczynski (mef) at the University of Washington
 *	Removed reference to "spin_up" external symbol.
 *
 * 28-Dec-94  Stefan Savage (savage) at the University of Washington
 *      Created
 *)

INTERFACE RTMem;
IMPORT Word;

PROCEDURE Alloc(size: INTEGER): ADDRESS;

PROCEDURE Align(number: Word.T): Word.T;

PROCEDURE AllocGC(VAR size: INTEGER): ADDRESS;

PROCEDURE GetMaxGCSize(): INTEGER;

PROCEDURE GetTracedStart(): ADDRESS;

PROCEDURE GetTracedEnd(): ADDRESS;

PROCEDURE SetTracedStart(a: ADDRESS);

PROCEDURE SetTracedEnd(a: ADDRESS);

END RTMem.
