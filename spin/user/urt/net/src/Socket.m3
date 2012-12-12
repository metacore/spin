(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 30-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE Socket;
IMPORT Sal;
IMPORT Errno;
IMPORT Clock;
IMPORT Word;

PROCEDURE Select( 
    VAR sockets : ARRAY [1..1] OF readyDes;
    sec: CARDINAL;
    usec: CARDINAL): CARDINAL
  RAISES {Errno.E} = 
  VAR
    nready    : INTEGER;
    pollfiles : ARRAY [1..1] OF Sal.Pollfd;
    error     : Errno.T;
    timo      := Clock.TimeVal{sec,usec};
    pfds      := NUMBER(sockets);
  BEGIN
    (* setup the pollfd stuff *)
    FOR i := FIRST(sockets) TO LAST(sockets) DO

      pollfiles[i].selectproc := sockets[i].socket.selectproc();
      pollfiles[i].descriptor := sockets[i].socket.descriptor();
      pollfiles[i].revents := 0;
      pollfiles[i].events := 0;
      IF selectReadyState.read IN sockets[i].ready THEN 
        pollfiles[i].events := Word.Or(pollfiles[i].events, Sal.POLLNORM);
      END;
      IF selectReadyState.write IN sockets[i].ready THEN 
        pollfiles[i].events := Word.Or(pollfiles[i].events, Sal.POLLOUT);
      END;
      IF selectReadyState.except IN sockets[i].ready THEN 
        (* XXX pollfiles[i].events := Word.Or(pollfiles[i].events, POLLOUT); *)
      END;
    END;

    (* Knock on the door of any eligible files and let them know we are
       interested.  Note that it's legit to have no elgible files, in
       which case we are only here for the timeout.  
    *)

    error := Sal.DoScan(SUBARRAY(pollfiles, 0, pfds), nready, timo, TRUE);
    IF error # 0 THEN RAISE  Errno.E(error); END;

    nready := 0;
    IF Word.And(pollfiles[pfds].revents, Sal.POLLNORM) # 0 THEN 
      INC(nready);
      sockets[pfds].ready := sockets[pfds].ready + selectReadyStates{selectReadyState.read}; (* XXX *)
    END;

    IF Word.And(pollfiles[pfds].revents, Sal.POLLOUT) # 0 THEN 
      INC(nready);
      sockets[pfds].ready := sockets[pfds].ready + selectReadyStates{selectReadyState.write}; (* XXX *)
    END;
    
    IF Word.And(pollfiles[pfds].revents, Sal.POLLPRI) # 0 THEN 
      INC(nready);
      sockets[pfds].ready := sockets[pfds].ready + selectReadyStates{selectReadyState.except};
    END;

    RETURN nready;
  END Select;

BEGIN
END Socket.