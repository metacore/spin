UNSAFE MODULE RTCollector1 EXPORTS RTHeapRep;
IMPORT RTIO, RTOS;

(* this is used only by reference counting and should never be called
   in this collector *)
PROCEDURE MutatorFault (addr: ADDRESS; pc: ADDRESS) =
  BEGIN
    RTIO.PutText("GC ERROR >> MutatorFault called, addr: ");
    RTIO.PutAddr(addr); RTIO.PutText(", pc: ");
    RTIO.PutAddr(pc); RTIO.PutText("\n");
    RTOS.Crash();
  END MutatorFault;

(* this is used only by reference counting, just ignore *)
PROCEDURE EnableTraps () =
  BEGIN
  END EnableTraps;

(* this is used only by reference counting, just ignore *)
PROCEDURE Assignement (loc, lptr, rptr, pc: ADDRESS) =
  BEGIN
  END Assignement;

BEGIN
END RTCollector1.

