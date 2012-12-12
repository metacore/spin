(*
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added GetEnd
 *
 * 16-Jul-97  Tian Fung Lim (tian) at the University of Washington
 *	Removed reliance on "current"
 *
 * 20-Apr-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.
 *
 *)

(* This module encapsulates virtual memory operations for remapping pages
   from freelists to reservepool and vice versa 
*)

UNSAFE MODULE RTHeapVM;

IMPORT RTMem, RTHeapRep, RTHeapDep;
FROM RTIO IMPORT PutText, PutInt, PutAddr, Flush;
IMPORT Spy;


VAR 
  End  : ADDRESS;
  Timer : Spy.T;

 (* assumes all elements on the page have been unlinked *)
 (* return the address of the page *)
PROCEDURE MovePageToReservePool(page : ADDRESS) : ADDRESS =
  VAR
    oldCurr := End;
  BEGIN
    Spy.Enter(Timer);

    RTHeapDep.Remap(page,End);
    
    (* move end back *)
    INC(End, RTHeapRep.BytesPerPage);
    RTMem.SetTracedEnd(End);

    Spy.Exit(Timer);
    RETURN oldCurr;
  END MovePageToReservePool;

PROCEDURE GetEnd () : ADDRESS =
  BEGIN
    RETURN End;
  END GetEnd;

PROCEDURE Init () =
  VAR
  BEGIN
    End := RTMem.GetTracedEnd();
    PutText("RTHeapVM initialized.  end = ");
    PutAddr(End);
    PutText("\n");
    
    Timer := Spy.Create("VM: RemapPage", TRUE);
    Spy.Activate(Timer);
  END Init;

BEGIN
END RTHeapVM.
