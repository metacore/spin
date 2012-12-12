(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Took out EnqueueHead.  The implementation was wrong and it wasn't
 *	used anywhere.
 *
 * 19-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	added EnqueueHead
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

INTERFACE Ifq;
IMPORT Mbuf;

(* datastructure used for network packet queueing *)
TYPE ifqueue = RECORD
  ifq_head          : Mbuf.T;
  ifq_tail          : Mbuf.T;
  ifq_len           : CARDINAL;
  ifq_bytes         : CARDINAL;
  ifq_maxlen        : CARDINAL;
  ifq_drops         : CARDINAL;
  ifq_mutex         : MUTEX;
  ifq_sendUnaOffset : CARDINAL;
END;

PROCEDURE Enqueue(VAR ifq:ifqueue; m: Mbuf.T);
PROCEDURE Dequeue(VAR ifq:ifqueue):Mbuf.T;
PROCEDURE NotEmpty(VAR ifq:ifqueue) : BOOLEAN;
PROCEDURE GetHead(VAR ifq:ifqueue):Mbuf.T;

(*
  Note this does NOT return the number of bytes total in the queue.
  It returns the number of bytes of unacknowledged data plus the 
  number of bytes of data that hasn't been sent.  There might be
  some number of bytes that have been acknowldeged but not removed
  from the queue, because they share an mbuf with unacked data.*)

PROCEDURE NumBytes(VAR ifq:ifqueue):CARDINAL;
PROCEDURE Init(VAR ifq:ifqueue);
END Ifq.
