(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE SalDep;
IMPORT Word;

CONST
  (* They are originally defined in sys/fcntl.h *)
  FREAD = 1;
  FWRITE = 2;
  
TYPE
  SelectProc = PROCEDURE(dev: Word.T;
			 rw: [FREAD .. FWRITE];
			 curThread: REFANY) : INTEGER;

END SalDep.
