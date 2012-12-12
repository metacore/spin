(* Copyright (C) 1992, Digital Equipment Corporation                         *)
(* All rights reserved.                                                      *)
(* See the file COPYRIGHT for a full description.                            *)
(*                                                                           *)
(* Last modified on Wed Nov 23 13:00:57 PST 1994 by kalsow                   *)
(*      modified on Tue Apr 20 16:19:54 PDT 1993 by muller                   *)

UNSAFE MODULE RTThread;

IMPORT RTOS;
IMPORT RTThreadExtern;

PROCEDURE SP (<*UNUSED*> READONLY s: State): ADDRESS =
  BEGIN
    RTOS.Error("RTThread.SP");
    RETURN LOOPHOLE(0,ADDRESS);
  END SP;




(*--------------------------------------------------------- thread stacks ---*)


PROCEDURE NewStack (<* UNUSED *>size: INTEGER;  <* UNUSED *>VAR(*OUT*)s: Stack) =
  BEGIN
    RTOS.Error("RTThread.NewStack");
  END NewStack;

PROCEDURE DisposeStack (<* UNUSED *>VAR s: Stack) =
  BEGIN
    RTOS.Error("RTThread.DisposeStack");
END DisposeStack;



PROCEDURE FlushStackCache () =
  BEGIN
    RTOS.Error("RTThread.FlushStackCache");
  END FlushStackCache;

(*-------------------------------------------------- modifying the models ---*)

PROCEDURE UpdateStateForNewSP (<* UNUSED *>VAR s: State; <*UNUSED *>offset: INTEGER) =
  BEGIN
    RTOS.Error("RTThread.UpdateStateforNewSP");
  END UpdateStateForNewSP;

PROCEDURE UpdateFrameForNewSP (<*UNUSED*> a: ADDRESS; 
                               <*UNUSED*> offset: INTEGER) =
  BEGIN
    RTOS.Error("RTThread.UpdateFrameForNewSP");
  END UpdateFrameForNewSP;






PROCEDURE Transfer (VAR from, to: State) =
  BEGIN
    RTThreadExtern.RTThread__Transfer(from, to);
  END Transfer;

PROCEDURE GetHandlerStack(): ADDRESS =
  BEGIN
    RETURN RTThreadExtern.RTThread__handlerStack;
  END GetHandlerStack;

PROCEDURE SetHandlerStack(a: ADDRESS) =
  BEGIN
    RTThreadExtern.RTThread__handlerStack := a;
  END SetHandlerStack;



BEGIN
END RTThread.

