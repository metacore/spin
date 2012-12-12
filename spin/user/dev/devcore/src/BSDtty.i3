(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Added new select support.  BSDtty devices provide proc pointers
 *      for the shared selscan code in kernel/select.c Kernel.DoScan.
 *
 * 26-Oct-96  Yasushi Saito (yasushi) at the University of Washington
 *	Whisted. Changed semantics of "arg" when iocparmlen=0.
 *)

(* "BSDtty" device is an interface to the BSD4.3 termios tty
   driver. *)

INTERFACE BSDtty;
IMPORT CharDevice, Error, Word, SalDep;

(* a "signal handler" in this sytesm is called straight from
	the ttyinput interrupt handler via pgsignal  *)
TYPE
  SignalHandler = PROCEDURE(sig: Word.T);

  T <: Public;
  Public = CharDevice.T OBJECT 
   METHODS
     charsAvail(): CARDINAL RAISES {Error.E};
     sigHandler(handler: SignalHandler);
    
     ioctl(cmd: INTEGER; 
	   VAR arg: ARRAY OF CHAR;
	   flags: INTEGER)
       RAISES {Error.E};
     (* "ioctl" method is a kludge interface leftover from Unix.
        This interface happens to have a lot of legacy code written
        against it, so we provide it here but do not endorse its use.
        "arg" is the argument to the ioctl. It's contents vary depending
	on value of IOCPARM_LEN(cmd). If it is > 0, then "arg" holds the
	ioctl argument of that length(ie, content of the user space
	starting from 3rd arg to ioctl). If it is 0, then
	NUMBER(arg) is always equal to BYTESIZE(Word.T), and the content
	of "arg" is the value of 3rd arg to ioctl itself.

	"flags" is the current open mode(O_RDONLY, O_NONBLOCK, etc).
	
        The structure of ioctl is parallel to BSD device drivers.
        See also bsd/{sys_generic,tty}.c  *)

      selectproc(): SalDep.SelectProc;
      descriptor(): Word.T;
   END;

END BSDtty.
