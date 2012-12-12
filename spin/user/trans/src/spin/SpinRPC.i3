(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
INTERFACE SpinRPC;
IMPORT TransRPC;
IMPORT Mbuf;
IMPORT Socket;

TYPE SpinRecvBuf <: TPublic;
  TPublic = TransRPC.RecvBuf OBJECT
    buf: UNTRACED REF ARRAY OF CHAR; (* = Mbuf.Array(mbuf) *)
    bufSize: CARDINAL; (* = NUMBER(buf^) *)
    mbuf: Mbuf.T;
    cumulativeIdx: CARDINAL; (* total bytes read so far. *)
    curIdx: CARDINAL; (* bytes read in current mbuf. *)
    sock: Socket.T; (* the channel to read in more mbufs *)
    maxSize: CARDINAL; (* total # of bytes supposed to be in the buffer. *)
  METHODS
    reset(mbuf: Mbuf.T; maxSize: CARDINAL; sock: Socket.T);
  END;
END SpinRPC.
