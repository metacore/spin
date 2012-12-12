(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
MODULE CharDevice;
IMPORT IO;

REVEAL T = Public BRANDED OBJECT
OVERRIDES
  read := Read;
  write := Write;
  open := Nop;
  close := Nop;
END;

PROCEDURE Nop (<*UNUSED*>self: T) =
  BEGIN
  END Nop;
PROCEDURE Read (self:T;
		<*UNUSED*>VAR data: ARRAY OF CHAR;
		<*UNUSED*>off: CARDINAL): CARDINAL =
  BEGIN
    IO.PutError(self.name() & ": read unimplemented.\n");
    RETURN 0;
  END Read;
PROCEDURE Write (self: T;
		 <*UNUSED*>READONLY data: ARRAY OF CHAR;
		 <*UNUSED*>off: CARDINAL): CARDINAL =
  BEGIN
    IO.PutError(self.name() & ": write unimplemented.\n");
    RETURN 0;
  END Write;
BEGIN
END CharDevice.
