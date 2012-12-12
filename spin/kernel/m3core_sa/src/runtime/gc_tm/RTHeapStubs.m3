(* This is for general crud that just is not implemented *)

UNSAFE MODULE RTHeapStubs EXPORTS RTHeapRep;

PROCEDURE AllocForNewBody (<*UNUSED*>size : CARDINAL;
                       <*UNUSED*>alignment: CARDINAL): ADDRESS=
  BEGIN
  END AllocForNewBody;

PROCEDURE ReturnMem(a: ADDRESS) =
  BEGIN
  END ReturnMem;

BEGIN
END RTHeapStubs.
