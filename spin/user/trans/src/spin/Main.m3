(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 21-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Main;
IMPORT IO, TransCommands;
IMPORT Thread;

PROCEDURE ExecServer(<*UNUSED*>t : Thread.Closure) : REFANY =
  BEGIN
    TransServer.Loop();
    RETURN NIL;
  END ExecServer;

PROCEDURE Run (pp : ParseParams.T) : BOOLEAN =
  VAR
    cmd := pp.getNext();
  BEGIN
    IF Text.Equal(cmd, "-s") THEN
      EVAL Thread.Fork(NEW(Thread.Closure, apply := ExecServer));
    END;
    IO.Put("Stand alone transaction manager.\n");
    TransCommands.Loop();
    RETURN TRUE;
  END Run;

BEGIN
  IO.Put("Transaction manager loaded.\n");
  
END Main.
