(*
 *
 * Copyright 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)

(*
 * HISTORY
 * 27-Jan-97  Wilson Hsieh (whsieh) at the University of Washington
 *	created
 *
 *)

UNSAFE MODULE RTRefCount;

PROCEDURE ProcessOutstanding (free: BOOLEAN) =
  BEGIN
  END ProcessOutstanding;

PROCEDURE Decrement (lptr: ADDRESS) =
  BEGIN
  END Decrement;

PROCEDURE Increment (rptr: ADDRESS) =
  BEGIN
  END Increment;

PROCEDURE FreeZeroObjects () =
  BEGIN
  END FreeZeroObjects;

PROCEDURE Check () =
  BEGIN
  END Check;

PROCEDURE Init () =
  BEGIN
  END Init;

BEGIN
END RTRefCount.
