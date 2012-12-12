(*
 * HISTORY
 * 23-Oct-97  Tian Fung Lim (tian) at the University of Washington
 *	added RemoveDeadThreads to better support real-time collection.
 *
 * 08-Mar-97  Wilson Hsieh (whsieh) at the University of Washington
 *	added IsOnCurrentStack, written by Przemek
 *
 * 14-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added SweepClosure and ProcessStacksCl.
 *
 *)

INTERFACE ThreadF;
IMPORT Thread;

(*--------------------------------------------- garbage collector support ---*)

PROCEDURE ProcessStacks (p: PROCEDURE (start, stop: ADDRESS (*; th: REFANY*)));
(* This procedure apply p to each each region of memory assigned to a thread
   that might contain pointers, start and stop being the limits
   of the regions. It is there mainly for the benefit of the garbage collector.
   For KThreads the regions are stack and the saved machine state
   *)

TYPE 
  StackClosure = OBJECT
  METHODS
    apply(start, stop: ADDRESS; thread: Thread.T);
  END;

PROCEDURE RemoveDeadThreads (n : INTEGER) : BOOLEAN;
(* Clean out dead threads from the activeThreads list.  In order to
   support real-time collection, an integer n indicates the maximum
   number of threads that can be processed.  TRUE is returned if
   all dead threads were dequeued *)



(*--------------------------------------------- exception handling support --*)

PROCEDURE GetCurrentHandlers(): ADDRESS;
(* == RETURN RTThread.handlerStack *)

PROCEDURE SetCurrentHandlers(h: ADDRESS);
(* == RTThread.handlerStack := h *)

PROCEDURE SuspendOthers();

PROCEDURE ResumeOthers();

PROCEDURE IsOnCurrentStack (addr: ADDRESS): BOOLEAN;


END ThreadF.


