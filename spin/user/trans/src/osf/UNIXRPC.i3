(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

INTERFACE UNIXRPC;
IMPORT TransRPC;

TYPE UNIXRecvBuf <: TPublic;
  TPublic = TransRPC.RecvBuf OBJECT
    buf: REF ARRAY OF CHAR;
    idx: INTEGER;
    maxSize: CARDINAL;
  METHODS
    reset(size: INTEGER);
    (* Prepare reading from the buffer. "buf^" has to be filled in with
       the content beforehand(XXX). "size" specifies the size of valid portion
       INTEGER "buf^". Assumes that
       the buffer doesn't contain length header. *)
    
    stretchIfNecessary(size: INTEGER);
    (* Stretch the size of buffer so that it can hold at least "size" bytes. *)
  END;

END UNIXRPC.
