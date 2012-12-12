(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 27-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created. Regression test.
 *
 *)

MODULE Sirpa;

PROCEDURE Qpa1 (<* UNUSED *> a: RRT1; <* UNUSED *> b: RRT2): RRT1 =
  BEGIN
    RETURN NIL;
  END Qpa1;
  
PROCEDURE Qpa2 (<* UNUSED *> a: RRT1; <* UNUSED *> b: REF RRT2): RRT1 =
  BEGIN
    RETURN NIL;
  END Qpa2;
  
PROCEDURE Qpa3 (<* UNUSED *> a: RRT1; <* UNUSED *>VAR b: RRT2): RRT1 =
  BEGIN
    RETURN NIL;
  END Qpa3; 
  
PROCEDURE Qpa4 (<* UNUSED *> a: RRT1; <* UNUSED *> READONLY b: RRT2): RRT1 =
  BEGIN
    RETURN NIL;
  END Qpa4;
  
PROCEDURE Qpa5 (<* UNUSED *> a: RRT1; <* UNUSED *> b: RRT2): RRT2 =
  BEGIN
    RETURN NIL;
  END Qpa5;
  
PROCEDURE Qpa6 (<* UNUSED *> a: RRT1; <* UNUSED *> b: RRT2) =
  BEGIN
  END Qpa6;
  
BEGIN
END Sirpa.
