(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Moved from Exec.i3
 *)
INTERFACE ExecX86;

CONST
  UserCodeSel = 16_1f;
  UserDataSel = 16_27;
  UPCB = 16_40000000;
  
END ExecX86.
