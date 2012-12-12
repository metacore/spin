(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 3-Mar-98  David Becker at the University of Washington
 *	Gun rewrote the synchronization code to be Sema based.
 *	This code now prevents the alarm from resurrecting dead threads.
 *
 * 29-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
UNSAFE MODULE Select EXPORTS Sal, Select; (* it imports CPUPrivate *)
IMPORT SalDep;
IMPORT Thread, ThreadRep, Strand, SchedPrivate, Sema;
IMPORT Clock;
IMPORT CPU, CPUPrivate;
IMPORT Word;
IMPORT StrandSemaTbl;

VAR
  nTicksPerSecond := Clock.InterruptRate();
  sematable : StrandSemaTbl.Default;

PROCEDURE DoScan (VAR pollfiles : ARRAY OF Pollfd;
		  VAR n: INTEGER;
		  READONLY atv: Clock.TimeVal;
		  hasTimeout: BOOLEAN): INTEGER =
  VAR
    curThread: Thread.T := Strand.GetCurrent();
    timeout: CARDINAL;
    spl: CPU.InterruptLevel;
    sema: Sema.T;
  BEGIN
    IF hasTimeout THEN
      WITH delta = atv.tv_sec * nTicksPerSecond +
	           atv.tv_usec * nTicksPerSecond DIV 1000000 DO 
	timeout := SchedPrivate.gticks + delta;
      END;
    ELSE
      timeout := LAST(CARDINAL);
    END;
    
    sema := Sema.Alloc(0);
    EVAL sematable.put(curThread, sema);

    LOOP
      n := 0;  (* n is count of descriptors ready to be used *)
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      FOR i := 0 TO LAST(pollfiles) DO
        (*
          call the selectproc for all the file descriptors 
	  for either input or output
        *)
	pollfiles[i].revents := 0;
	IF Word.And(pollfiles[i].events, POLLNORM) # 0
	   AND pollfiles[i].selectproc(pollfiles[i].descriptor,
				       SalDep.FREAD, curThread) # 0 THEN
	  INC(n);
	  pollfiles[i].revents := Word.Or(pollfiles[i].revents, POLLNORM);
	END;
	IF Word.And(pollfiles[i].events, POLLOUT) # 0
	   AND pollfiles[i].selectproc(pollfiles[i].descriptor,
				       SalDep.FWRITE, curThread) # 0 THEN
	  INC(n);
	  pollfiles[i].revents := Word.Or(pollfiles[i].revents, POLLOUT);
	END;
      END;
      CPUPrivate.RestoreInterruptMask(spl);

      IF n > 0 OR SchedPrivate.gticks > timeout THEN
	EXIT;
      END;
      Clock.SetAlarm(timeout, Timeout, sema, TRUE);
      Sema.P(sema);
    END;

    EVAL sematable.delete(curThread, sema);
    RETURN 0;
  END DoScan;

PROCEDURE Timeout (r: REFANY) =
  VAR sema: Sema.T := r;
  BEGIN
    Sema.V(sema);
  END Timeout;

PROCEDURE Record (curThread: Thread.T; VAR selector: Selinfo) =
  BEGIN 
    IF selector.sema=NIL THEN RETURN END;
    EVAL sematable.get(curThread,selector.sema);
  END Record;

PROCEDURE Wakeup (VAR selector: Selinfo) =
  BEGIN
    IF selector.sema=NIL THEN RETURN END;
    Sema.V(selector.sema);
  END Wakeup;
		  
BEGIN
   sematable := NEW(StrandSemaTbl.Default).init();
END Select.
