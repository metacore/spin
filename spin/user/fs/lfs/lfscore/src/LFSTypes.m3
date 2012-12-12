(*
 * Copyright 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * May-29-96  Eric Christoffersen (ericc) at the University of Washington
 *	Whisted.
 *
 *)
MODULE LFSTypes;

TYPE
	
	REVEAL LFSErrorT = LFSErrorTPublic BRANDED
	OBJECT
	OVERRIDES
		message := Message;
	END;


PROCEDURE Message(self : LFSErrorT) : TEXT =
BEGIN
	RETURN self.messageE;
END Message;

BEGIN
END LFSTypes.
