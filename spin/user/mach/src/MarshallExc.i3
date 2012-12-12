(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description.
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 28-Dec-95  Emin Gun Sirer (egs) at the University of Washington
 *	Changed to conform to the new syscall interface.
 *
 * 14-Dec-95  David Dion (ddion) at the University of Washington
 *	Created
 *
 *)

(* "MarshallExc" interface -- dynamically linked extension *)

(* The "MarshallExc" interface exports the procedures which provide
   argument marshalling and synchronization for redirecting system
   calls to the OSF/1 server.

   Arguments are marshalled via a "MarshallExc.Handle".  A client, 
   which is an application program which has dropped into the kernel 
   with a syscall request, obtains a handle.  It associates the parameters
   of its syscall request with this handle, kicks the server, and waits 
   for the result.

   A server thread waits on a request from the task which it was created 
   to handle syscalls for.  When a request arrives, it finds the 
   associated handle, grabs the arguments, and returns to the server.
   After performing the system call, it returns, places the result back
   upon the handle, and kicks the client thread. *)

INTERFACE MarshallExc;

IMPORT UserSpaceThread, Word, Space;

TYPE 
  Handle = CARDINAL;
  (* AlphaThreadState corresponds to the in/out state in trap.c in the
     server.  It is close to CPU.SavedState, but ordered slightly
     differently, since Alpha PAL-saved registers are placed at the end
     of CPU.SavedState. *)
  AlphaThreadState = RECORD (* see mach/machine/thread_status.h *)
    v0,
    t0, t1, t2, t3, t4, t5, t6, t7,
    s0, s1, s2, s3, s4, s5, s6,
    a0, a1, a2, a3, a4, a5,
    t8, t9, t10, t11,
    ra, pv, at, gp, sp, pc : Word.T;
  END;


PROCEDURE ClientGetHandle(VAR statePtr: REF AlphaThreadState): Handle;
(* Client get a new marshalling handle. *)

PROCEDURE ClientReturnHandle(h: Handle);
(* Client return marshalling handle to the pool. *)

PROCEDURE ClientSetSyscallData(         h:          Handle;
                                        uth:        UserSpaceThread.T;
                                        type:       Word.T;
                                        code0:      Word.T;
                                        code1:      Word.T;
                                        stateCount: CARDINAL);
(* Client set marshalled data. *)

PROCEDURE ServerGetNextHandle(serverUthread: UserSpaceThread.T;
                              appSpace: Space.T): Handle;
(* Server get a handle from the pool.  First wait until a client enters
   a request.  This handle will then allow access to the data from the 
   requesting client.  It will also register this server thread as holding
   this handle. *)

PROCEDURE ServerGetHandleFromThread(VAR h:             Handle;
                                    VAR statePtr:      REF AlphaThreadState;
                                        serverUthread: UserSpaceThread.T) :
  BOOLEAN;
(* Server get the handle held by its thread.  If this server thread just
   finished a system call, it will have a handle registered to it, and
   TRUE will be returned.  Otherwise, it is looking to handle its first
   system call and FALSE will be returned. *)

PROCEDURE ServerGetSyscallData(    h:          Handle;
                               VAR uth:        UserSpaceThread.T;
                               VAR type:       Word.T;
                               VAR code0:      Word.T;
                               VAR code1:      Word.T;
                               VAR statePtr:   REF AlphaThreadState;
                               VAR stateCount: CARDINAL);
(* Server get marshalled data entered by client. *)

PROCEDURE ClientKickServerAndWaitForState(h: Handle; space: Space.T);
(* Client kick a server thread.  The thread to kick is determined by 
   the client space, since server threads are bound to handle syscalls
   from one task.  Then wait for the this request to be completed. *)

PROCEDURE ServerRequestCompleted(h: Handle);
(* Server indicate that client's request has been serviced. *)

PROCEDURE Init();
(* Initialize the marshalling mechanism. *)

END MarshallExc.
