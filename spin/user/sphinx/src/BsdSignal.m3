(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 11-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *	
 *	
 *)

MODULE BsdSignal;
IMPORT Word;

<*INLINE*>
PROCEDURE Mask (sig: INTEGER): Set =
  BEGIN
    RETURN Word.LeftShift(1, sig-1);
  END Mask;
  
PROCEDURE AddSet (VAR set: Set; sig: Number) =
  BEGIN
    set := Word.Or(set, Mask(sig));
  END AddSet;
  
PROCEDURE DelSet (VAR set: Set; sig: Number) =
  BEGIN
    set := Word.And(set, Word.Not(Mask(sig)));
  END DelSet;
  
PROCEDURE IsMember (set: Set; sig: Number): BOOLEAN =
  BEGIN
    RETURN Word.And(set, Mask(sig)) # 0;
  END IsMember;

CONST Names = ARRAY Number OF TEXT
  {
   "HUP",
   "INT",
   "QUIT",
   "ILL",
   "TRAP",
   "ABRT",
   "EMT",
   "FPE",
   "KILL",
   "BUS",
   "SEGV",
   "SYS",
   "PIPE",
   "ALRM",
   "TERM",
   "URG",
   "STOP",
   "TSTP",
   "CONT",
   "CHLD",
   "TTIN",
   "TTOU",
   "IO",
   "XCPU",
   "XFSZ",
   "VTALRM",
   "PROF",
   "WINCH",
   "INFO",
   "USR",
   "USR"
   };
				
PROCEDURE Name (signo: Number): TEXT =
  BEGIN
    RETURN Names[signo];
  END Name;
  
BEGIN
END BsdSignal.
