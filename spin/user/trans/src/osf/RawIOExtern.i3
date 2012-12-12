(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Dec-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE RawIOExtern;
<*EXTERNAL*>
PROCEDURE get_file_size(fd: INTEGER): INTEGER;
END RawIOExtern.
