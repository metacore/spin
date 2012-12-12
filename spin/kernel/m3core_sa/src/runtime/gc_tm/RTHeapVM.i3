(*
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 17-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	Added GetEnd
 *
 * 20-Apr-97  Tian Fung Lim (tian) at the University of Washington
 *	Created.
 *
 *)


INTERFACE RTHeapVM;

PROCEDURE Init();

(* moves a page to the end of the reservepool, returning the new address 
   that the page lives at
*)
PROCEDURE MovePageToReservePool(page : ADDRESS) : ADDRESS;

(* get curr end *)
PROCEDURE GetEnd() : ADDRESS;

END RTHeapVM.
