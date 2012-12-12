(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Set up the priority for kernel shell execed threads.
 *)
MODULE Nice;

IMPORT IO, Fmt, Strand, ParseParams;

VAR  pri: Strand.PriorityT := Strand.defPriority;

PROCEDURE Priority(): Strand.PriorityT =
  BEGIN
    RETURN pri;
  END Priority;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
    IO.Put("Old pri was " & Fmt.Int(pri) & "\n");
    pri := pp.getNextInt();
    EXCEPT ParseParams.Error =>
      IO.Put("Usage: " & CommandName & CommandHelp & "\n");
      RETURN FALSE;
    END;
    IO.Put("New pri is " & Fmt.Int(pri) & "\n");
    RETURN TRUE;
  END Run;

BEGIN
END Nice.

