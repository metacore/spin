(*
 *
 * Copyright 1994, 1995, 1996 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *)
(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed NewMclGetOa to MclGetOa.  No need to pass it a dummy
 *	"free" function anymore.
 *
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	These Send and Recv procedures hide the use of Mbufs.
 *      Only use it if you are willing to take the performance hit 
 *      of an extra copy.
 *)

MODULE EasySocket;
IMPORT Socket, Errno, Mbuf;

PROCEDURE Recv(s: Socket.T; VAR arr: ARRAY OF CHAR): INTEGER RAISES {Errno.E} =
  VAR
    recvBuf : Mbuf.T := NIL;
    nRecvd := NUMBER(arr);
  BEGIN
    nRecvd := Socket.Recv(s, recvBuf, nRecvd);

    IF recvBuf = NIL THEN RETURN 0; END;

    VAR
      buf := recvBuf;
      offset := 0;       (* The index into array "arr" *)
    BEGIN
      WHILE buf # NIL DO
        (* Copy bytes out of the Mbuf *)
        WITH recvArray = Mbuf.Array(buf)^ DO
          SUBARRAY(arr, offset, NUMBER(recvArray)) := recvArray;
          INC(offset, NUMBER(recvArray));
        END;
        buf := buf.mh_hdr.mh_next;
      END;
    END;

    (* We checked for NIL Mbufs already *)
    Mbuf.m_freem(recvBuf);

    RETURN nRecvd;
  END Recv;

(*
 * Function that is called by the device when a buffer being sent
 * can be reused by the application.
 *)
<* UNUSED *>
PROCEDURE UnStrongRef(<*UNUSED*>buffer: REF ARRAY OF CHAR;
                      <*UNUSED*>len: CARDINAL;
                      <*UNUSED*>arg: REFANY) =
  BEGIN
  END UnStrongRef; 

PROCEDURE Send(s: Socket.T; READONLY arr: ARRAY OF CHAR) RAISES {Errno.E} =
  VAR
    sendBuf : Mbuf.T;
    length := NUMBER(arr);
  BEGIN
    IF length = 0 THEN
      RETURN;
    END;

    WITH sendArray = NEW(REF ARRAY OF CHAR, length) DO
      sendArray^ := arr;

      TRY
        sendBuf := Mbuf.MclGetOa(sendArray, NUMBER(sendArray^));

        Socket.Send(s, sendBuf);
      EXCEPT
      | Mbuf.LengthMismatch => (* ignore it *)
      END;
    END; 
  END Send;

BEGIN
END EasySocket.

