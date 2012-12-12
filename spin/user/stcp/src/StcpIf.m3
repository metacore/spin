(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 18-Mar-97  Tsutomu Owa (owa) at the University of Washington
 *	Borrowed from urt/urtcore/src.
 *
 * 08-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Add SetNetOutput function which permits the unsafe interface
 *	 StcpIfExtern to be hidden rather than visible. This module becomes
 *       unsafe as a result.
 *
 * 13-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Implements interface packet enqueue and dequeue operations.
 *
 *)

MODULE StcpIf;
IMPORT StcpMbuf;

(*
  ifqueue operations.
*)
PROCEDURE Enqueue(VAR ifq:ifqueue; m: StcpMbuf.T) = 
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

PROCEDURE Dequeue(VAR ifq:ifqueue):StcpMbuf.T = 
  VAR m:StcpMbuf.T;
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
END StcpIf.
