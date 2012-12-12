(*
 * HISTORY
 * 25-Jul-97  Tsutomu Owa at the University of Washington
 *	Fixed Select().
 *
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 *	Unified select code in Kernel.DoScan
 *	No longer UNSAFE
 *      Replace SAL with Kernel and CPU interfaces
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 18-May-96 oystr at the University of Washington
 *	Created
 *
 *)
MODULE SphinxSelect EXPORTS Sphinx;
IMPORT StreamFile;
IMPORT VMError;
IMPORT Proc, ProcRep;
IMPORT IO, Translation, Fmt;
IMPORT Errno, ErrnoDep, Clock;
IMPORT Word;
IMPORT Sal;
IMPORT SphinxUtils, ProfileSupport;<*NOWARN*>
IMPORT Spy;<*NOWARN*>

(*
 * Following numerics come from OSF.
 *)
CONST MAX_NOFILE = 64;
      SELDEBUG = FALSE;
      FD_SET_SIZE = (MAX_NOFILE+BITSIZE(Word.T)-1) DIV BITSIZE(Word.T);

VAR
  selectSpy: Spy.T;
  
(*
 * returns: -1 on an error, error param contains error code.
 *	     0 on timeout, no files ready.
 *	    >0 number of files ready.
 *	       and inputs, outputs, excepts contain mask of ready files.
 *)
PROCEDURE Select( nfds    : INTEGER;
		  inputs  : INTEGER;
		  outputs : INTEGER;
		  excepts : INTEGER;
		  tv	  : INTEGER) : INTEGER 
	RAISES { Errno.E, VMError.E } =
  VAR
    proc: Proc.T := Translation.GetCurrent();
    charsize: CARDINAL; (* byte # of in/ou/ex bits that hold validcontents *)
    wordsize: CARDINAL; (* word # of in/ou/ex bits that hold validcontents *)
    j, k : CARDINAL;
    pfds : CARDINAL; (* # of fds to be watched *)
    nready : INTEGER := 0;
    timo : Clock.TimeVal;
    haveTimo : BOOLEAN;
    error : INTEGER;
    ibits, obits, ebits, tbits, allbits : Word.T;
    sfh: StreamFile.T;
    (* For now we poll a max of 32 active files. *)
    pollfiles : ARRAY [0..31] OF Sal.Pollfd;
    fds : ARRAY [0..31] OF INTEGER;
    (* And the max fd value is 64. *)
    inbits, oubits, exbits : ARRAY [0..FD_SET_SIZE] OF Word.T;
  BEGIN
    IF SphinxUtils.MicroBench THEN Spy.Enter(selectSpy); END;
    (*
       We have to be a little bit careful here.
       People have been known to cheat on the
       the sizes of fd_set arrays.
       First figure out how much space we need to hold the bit arrays.
    *)
    IF nfds <= 0 THEN
      error := 0;
      RETURN 0;
    END;
    
    (*  IF SphinxUtils.profCount = 10 THEN
       EVAL ProfileSupport.On();
       ELSIF SphinxUtils.profCount = 300 THEN
       EVAL ProfileSupport.Off();
       END;
    INC(SphinxUtils.profCount);  *)
    IF nfds > MAX_NOFILE THEN
      (*
	 N.B. - OSF/BSD checks nfds against the largest open fd of
	 a process and sets nfds according -- "forgiving if slightly
	 wrong.  Since we don't have a processes # of open files,
	 we punt.
      *)
      RAISE Errno.E(ErrnoDep.EBADF);
    END;


    (* Calculate number of bytes to read in. *)
    charsize := (nfds+7) DIV 8;
    wordsize := (nfds+BITSIZE(Word.T)-1) DIV BITSIZE(Word.T);

    (* Read in/out/ex bit vectors from the user space. *)
    IF inputs # 0 THEN
      Translation.Read(proc, inputs,
		       SUBARRAY(VIEW(inbits, ARRAY OF CHAR), 0, charsize));
    ELSE
      FOR i := 0 TO wordsize-1 DO inbits[i] := 0; END;
    END;
    IF outputs # 0 THEN
      Translation.Read(proc, outputs,
		       SUBARRAY(VIEW(oubits,ARRAY OF CHAR), 0, charsize));
    ELSE
      FOR i := 0 TO wordsize-1 DO oubits[i] := 0; END;
    END;
    IF excepts # 0 THEN
      Translation.Read(proc, excepts,
		       SUBARRAY(VIEW(exbits,ARRAY OF CHAR), 0, charsize));
    ELSE
      FOR i := 0 TO wordsize-1 DO exbits[i] := 0; END;
    END;
    
    haveTimo := FALSE;
    IF tv # 0 THEN
      Translation.Read(proc, tv, VIEW(timo, ARRAY OF CHAR));
      haveTimo := TRUE;
    END;

    (*
       Having read everything in, scan the bitstrings to determined
       which files we want to look at.
    *)

    pfds := 0;
    FOR i := 0 TO wordsize-1 DO 
      k := i * BITSIZE(Word.T);
      ibits := inbits[i];
      obits := oubits[i];
      ebits := exbits[i];
      allbits := Word.Or(ebits,Word.Or(ibits,obits));
      (* j := ffs(allbits) *)
      j := 0;
      IF allbits # 0 THEN
	FOR n := 0 TO BITSIZE(Word.T)-1 DO
	  IF Word.And(allbits,Word.LeftShift(1,n)) # 0 THEN
	    j := n + 1;
	    EXIT;
	  END;
	END;
      END;
      IF j > 0 THEN
	DEC(j);
	tbits := Word.LeftShift(1,j);
	WHILE (j < BITSIZE(Word.T)) AND (k + j < nfds) DO
	  IF Word.And(allbits,tbits) > 0 THEN
	    WITH pd = pollfiles[pfds] DO
	      fds[pfds] := k + j;
	      sfh := NARROW(Proc.FindFH(proc, fds[pfds]).h,StreamFile.T);
	      IF Word.And(pollfiles[pfds].revents, Sal.POLLNVAL) # 0 THEN
		RETURN ErrnoDep.EBADF;
	      END;
	      IF sfh.dev # NIL THEN 
		pd.selectproc := sfh.dev.selectproc();
		pd.descriptor := sfh.dev.descriptor();
	      ELSE
		IO.Put("Sphinx: Select.  sfh.dev looks incorrect\n");
	      END;
	      
	      pd.events := 0;
	      pd.revents := 0;
	      
	      IF Word.And(ibits,tbits) # 0 THEN
		pd.events := Sal.POLLNORM;
	      END;
	      IF Word.And(obits,tbits) # 0 THEN
		pd.events := Word.Or(pd.events, Sal.POLLOUT);
	      END;
	      IF Word.And(ebits,tbits) # 0 THEN
		pd.events := Word.Or(pd.events, Sal.POLLPRI);
	      END;
	    END (* with pd *);
	    INC(pfds);
	    allbits := Word.Xor(allbits, tbits);
	    IF allbits = 0 THEN
	      EXIT (* while j < *);
	    END;
	  END (* if word.and *);
	  tbits := Word.LeftShift(tbits,1);
	  INC(j);
	END (* while j ...*) ;
      END (* if j > 0.. scan one bit array entry*);
    END (* for i... scan bit arrays *);

    IF SELDEBUG THEN
    IO.Put("Pfds is " & Fmt.Int(pfds) & "\n");
      FOR i := 0 TO pfds-1 DO
	IO.Put("Poll file " & Fmt.Int(i) & ": fd " & Fmt.Int(fds[i]) &
	       " events " & Fmt.Int(pollfiles[i].events) & "\n" );
      END;
    END;

    (*
       * Knock on the door of any eligible files
       * and let them know we are interested.  Note that it's legit
       * to have no elgible files, in which case we are only here
       * for the timeout.
    *)
    error := Sal.DoScan(SUBARRAY(pollfiles, 0, pfds), nready, timo, haveTimo);
    IF error # 0 THEN
      RAISE Errno.E(error);
    END;

    FOR i := 0 TO wordsize-1 DO
      inbits[i] := 0;
      oubits[i] := 0;
      exbits[i] := 0;
    END;

    (*
       * Construct result bit vectors.
    *)
    nready := 0;
    WHILE pfds > 0 DO
      DEC(pfds);
      IF pollfiles[pfds].revents # 0 THEN
	tbits := Word.LeftShift(1,fds[pfds] MOD BITSIZE(Word.T));
	j := fds[pfds] DIV BITSIZE(Word.T);
	IF Word.And(pollfiles[pfds].revents, Sal.POLLNORM) # 0 THEN
	  INC(nready);
	  inbits[j] := Word.Or(inbits[j],tbits);
	END;
	IF Word.And(pollfiles[pfds].revents, Sal.POLLOUT) # 0 THEN
	  INC(nready);
	  oubits[j] := Word.Or(oubits[j],tbits);
	END;
	IF Word.And(pollfiles[pfds].revents, Sal.POLLPRI) # 0 THEN
	  INC(nready);
	  exbits[j] := Word.Or(exbits[j],tbits);
	END;
      END;
    END;

    (* 
       * Copy out the result bit masks.  There should be no
       * faults on this unless someone got tricky and gave
       * us read-only memory.
    *)
    
    IF inputs # 0 THEN
      Translation.Write(proc, 
			SUBARRAY(VIEW(inbits,ARRAY OF CHAR), 0, charsize),
			inputs);
    END;
    IF outputs # 0 THEN
      Translation.Write(proc, 
			SUBARRAY(VIEW(oubits,ARRAY OF CHAR), 0, charsize),
			outputs);
    END;
    IF excepts # 0 THEN
      Translation.Write(proc,
			SUBARRAY(VIEW(exbits,ARRAY OF CHAR), 0, charsize),
			excepts);
    END;
    IF SphinxUtils.MicroBench THEN Spy.Exit(selectSpy); END;
    RETURN nready;
  END Select;

BEGIN
  selectSpy := Spy.Create("select",FALSE,400);
END SphinxSelect.
