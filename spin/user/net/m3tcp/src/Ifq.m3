(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Took out EnqueueHead.  Fixed a bug Enqueue which was due to a
 *	misunderstanding of how mbufs work.  Both Rick and I agree that
 *	mbufs suck!
 *
 * 22-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	added bytes field to keep how many bytes there are in the queue.
 *
 * 22-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	added GetHead
 *
 * 19-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	added EnqueueHead
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

MODULE Ifq;
IMPORT Mbuf;

PROCEDURE NotEmpty(VAR ifq:ifqueue): BOOLEAN =
  BEGIN
    RETURN ifq.ifq_head # NIL;
  END NotEmpty;
    
PROCEDURE Enqueue(VAR ifq:ifqueue; m: Mbuf.T) = 
  VAR
    tail : Mbuf.T;
  BEGIN
    (* IF_ENQUEUE_NOLOCK net/if.h *)

    LOCK ifq.ifq_mutex DO

      tail := m;
      WHILE tail.mh_hdr.mh_next # NIL DO
        tail := tail.mh_hdr.mh_next;
      END;
      
      IF ifq.ifq_tail = NIL THEN
        ifq.ifq_head := m;
      ELSE
        ifq.ifq_tail.mh_hdr.mh_next := m;
      END;

      ifq.ifq_tail := tail;
      INC(ifq.ifq_len);
      INC(ifq.ifq_bytes, Mbuf.m_length(m));
    END;

  END Enqueue;

PROCEDURE Dequeue(VAR ifq:ifqueue):Mbuf.T = 
  VAR m:Mbuf.T;
  BEGIN

    LOCK ifq.ifq_mutex DO

      m := ifq.ifq_head;

      IF m # NIL THEN

        ifq.ifq_head := m.mh_hdr.mh_next;

        IF ifq.ifq_head = NIL THEN
          ifq.ifq_tail := NIL
        END;

        m.mh_hdr.mh_next := NIL;
        DEC(ifq.ifq_len);
        DEC(ifq.ifq_bytes, Mbuf.m_length(m));

      END;
    END;

    RETURN m;
  END Dequeue;

PROCEDURE GetHead(VAR ifq:ifqueue) : Mbuf.T = 

  BEGIN

    LOCK ifq.ifq_mutex DO
      RETURN ifq.ifq_head;
    END;

  END GetHead;

PROCEDURE NumBytes(VAR ifq:ifqueue):CARDINAL =

  BEGIN

    LOCK ifq.ifq_mutex DO
      RETURN ifq.ifq_bytes - ifq.ifq_sendUnaOffset;
    END;

  END NumBytes; 



PROCEDURE Init(VAR ifq:ifqueue) = 

  BEGIN
  ifq.ifq_mutex     := NEW (MUTEX);
  ifq.ifq_head          := NIL;
  ifq.ifq_tail          := NIL;
  ifq.ifq_len           := 0;
  ifq.ifq_bytes         := 0; 
  ifq.ifq_maxlen        := 0; 
  ifq.ifq_drops         := 0;
  ifq.ifq_sendUnaOffset := 0;
  END Init;


BEGIN
END Ifq.
