(* 
 * HISTORY
 * 
 * 15-May-96 Przemek Pardyak (pardy) at the University of Washington
 *	Changed Lookup to IsThere.
 *
 * 6-Nov-95 Przemek Pardyak (pardy) at the University of Washington
 *      Simplified interface.
 *
 * 12-Jun-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

(* 
 *  Create an intentional strong reference to an object in order to
 *  prevent it from being collected or moved by garbage collection 
 *)

INTERFACE StrongRef;

PROCEDURE Add(ref: REFANY);

PROCEDURE Remove(ref: REFANY);

PROCEDURE Exists(ref: REFANY) : BOOLEAN; 

END StrongRef.

