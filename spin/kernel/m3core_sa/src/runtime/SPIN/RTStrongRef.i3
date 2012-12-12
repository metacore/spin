(* 
 * HISTORY
 * 
 * 15-May-96 Przemek Pardyak (pardy) at the University of Washington
 *	Moved IsThere to StrongRef.i3.
 *
 * 6-Nov-95 Przemek Pardyak (pardy) at the University of Washington
 *      Simplified interface.
 *
 * 27-Mar-95  Przemek Pardyak (pardy) at the University of Washington
 *	Created.
 *)

INTERFACE RTStrongRef;
IMPORT RTIO;

PROCEDURE ProcessRefs(p: PROCEDURE (start, stop: ADDRESS (*; th: REFANY*)));
(* This procedure applies p to the range of memory that contains
   references that should not be moved during garbage collection *)

PROCEDURE Dump(p: RTIO.SimplePutter := NIL);

PROCEDURE Init ();

END RTStrongRef.
