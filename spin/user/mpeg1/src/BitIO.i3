(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE BitIO;

IMPORT Ctypes;

EXCEPTION
  EOF;

TYPE
  T <: REFANY;

<*INLINE*>PROCEDURE ShowBits(self: T; length: CARDINAL): Ctypes.unsigned_int;
<*INLINE*>PROCEDURE GetBits(self: T; length: CARDINAL): Ctypes.unsigned_int RAISES {EOF};
<*INLINE*>PROCEDURE SkipBits(self: T; length: CARDINAL) RAISES {EOF};
<*INLINE*>PROCEDURE ByteAligned(self: T): BOOLEAN;

PROCEDURE OpenRead(name: TEXT): T RAISES {EOF};
PROCEDURE Length(self: T): INTEGER;
PROCEDURE Close(self: T);

END BitIO.