(*| Copyright (C) 1994, Digital Equipment Corporation           *)
(*| All rights reserved.                                        *)
(*| See the file COPYRIGHT for a full description.              *)
(*|                                                             *)
(*| Last modified on Mon May  1 16:30:49 PDT 1995 by kalsow     *)
(*|      modified on Wed May 25 14:41:19 PDT 1994 by detlefs    *)

(*
 * HISTORY							
 * 14-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added printing of generation and protection in DumpPageStatus.
 *	Do not try to print if the module not initialized.
 *
 * 03-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Made the putter optional argument and added creation if not 
 *	passed in.
 *
 * 22-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added DumpPageStatus.
 *
 * 24-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Support RTIO.SimplePutter.
 *
 *)

UNSAFE MODULE RTHeapDebug;

IMPORT RTIO;

PROCEDURE Free(<* UNUSED *>r: REFANY) =
  BEGIN
    RTIO.PutText("RTHeapDebug\n");
    <* ASSERT FALSE *> (* MAS *)
  END Free;

PROCEDURE CheckHeap(<* UNUSED *>p: RTIO.SimplePutter := NIL) =
  BEGIN
    RTIO.PutText("RTHeapDebug\n");
    <* ASSERT FALSE *> (* MAS *)
  END CheckHeap;

PROCEDURE Init() =
  BEGIN
  END Init;

BEGIN
  <*ASSERT BYTESIZE (REFANY) = BYTESIZE (INTEGER)*>
END RTHeapDebug.
