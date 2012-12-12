(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 08-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Add SetNetOutput function which permits the unsafe interface
 *	 IfExtern to be hidden rather than visible. This module becomes
 *       unsafe as a result.
 *
 * 13-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements interface packet enqueue and dequeue operations.
 *
 *)

MODULE If;
IMPORT Mbuf;

(*
  ifqueue operations.
*)
PROCEDURE Enqueue(VAR ifq:ifqueue; m: Mbuf.T) = 
  BEGIN
    (* IF_ENQUEUE_NOLOCK net/if.h *)
    m.mh_hdr.mh_nextpkt := NIL;
    IF ifq.ifq_tail = NIL THEN
      ifq.ifq_head := m;
    ELSE
      ifq.ifq_tail.mh_hdr.mh_nextpkt := m;
    END;
    ifq.ifq_tail := m;
    INC(ifq.ifq_len);
  END Enqueue;

PROCEDURE Dequeue(VAR ifq:ifqueue):Mbuf.T = 
  VAR m:Mbuf.T;
  BEGIN
    m := ifq.ifq_head;
    IF m # NIL THEN
      ifq.ifq_head := m.mh_hdr.mh_nextpkt;
      IF ifq.ifq_head = NIL THEN
        ifq.ifq_tail := NIL
      END;
      m.mh_hdr.mh_nextpkt := NIL;
      DEC(ifq.ifq_len);
    END;
    RETURN m;
  END Dequeue;

BEGIN
END If.
