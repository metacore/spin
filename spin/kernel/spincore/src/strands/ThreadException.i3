(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * ThreadException --
 *	Client interface to exceptions for in-kernel threads.
 *
 *
 * HISTORY
 * 07-Jul-96  Brian Bershad (bershad) at the University of Washington
 *	Created.
 *
 *
 *)


INTERFACE ThreadException;



IMPORT Thread, SpinException;


  (*
   * Event raised when an exception is generated in the named thread but which was not
   * itself handled by the named thread. Information about the exception can be found
   * in the thread descriptor.
   *
   * A client may override the exception event, but at what granularity??
   * If we override based on "owner" then all exceptions are overridden.
   * If we override at the level of individual thread, then we retain a 
   * reference to the thread forever. This is BAD BAD BAD.
   *
   * This function SHOULD NOT RETURN.
   *)

PROCEDURE RaiseException(th: Thread.T);


   (*
    * Return TRUE if the named thread is "caught" in an exceptional condition. "msg" is 
    * what went wrong
    *)

PROCEDURE InException(th: Thread.T; VAR ei: SpinException.ExceptionInfo) : BOOLEAN;

     

PROCEDURE TerminateInException(th: Thread.T) : BOOLEAN;
   (* 
    * mark a thread which is in an exceptional state as "done."
    *	 Returns FALSE if thread is not in an exception state.
    *    else, does not return at all if th = Thread.Self(),
    *    else returns TRUE.
    *)

END ThreadException.
