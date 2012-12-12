(* HISTORY 
 * 14-Feb-97  Charles Garrett (garrett) at the University of Washington
 *	Created. A type to represent address ranges. It nearly duplicates 
 *      Gun's kernel regions, but I had to use a different compare function.
 *      We rely on the ranges never overlapping.
 *)

INTERFACE Range;

TYPE T = RECORD
  start : ADDRESS;
  stop : ADDRESS;
END;

PROCEDURE Compare(READONLY a, b: T) : [-1 .. 1];

END Range. 


