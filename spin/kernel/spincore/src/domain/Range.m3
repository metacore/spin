(* HISTORY 
 * 14-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	Created. A type to represent address ranges. It nearly duplicates 
 *      Gun's kernel regions, but I had to use a different compare function.
 *      We rely on the ranges never overlapping.
 *)

UNSAFE MODULE Range;

(* Return -1 if region a is completely less than region b,
   +1 if it is completely greater than b and 0 if they
   overlap. *)
PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1] =
  BEGIN
    IF a.stop <= b.start THEN RETURN -1;
    ELSIF a.start >= b.stop THEN RETURN 1;
    ELSE RETURN 0;
    END;
  END Compare;

BEGIN
END Range. 


