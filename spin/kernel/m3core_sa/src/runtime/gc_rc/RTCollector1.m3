UNSAFE MODULE RTCollector1 EXPORTS RTHeapRep;
IMPORT RTIO, RTOS, RTRefCount;

(* this is used only by reference counting and should never be called
   in this collector *)
PROCEDURE MutatorFault (addr: ADDRESS; pc: ADDRESS) =
  BEGIN
    RTRefCount.Mutator(addr, pc);
  END MutatorFault;

(* this is used only by reference counting, just ignore *)
PROCEDURE EnableTraps () =
  BEGIN
    RTRefCount.EnableTraps();
  END EnableTraps;

(* this is used only by reference counting, just ignore *)
PROCEDURE Assignement (loc, lptr, rptr, pc: ADDRESS) =
  BEGIN
    RTRefCount.FS(loc, lptr, rptr, pc);
  END Assignement;

BEGIN
END RTCollector1.
