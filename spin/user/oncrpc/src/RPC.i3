(*
   RPC.i3
   Interface to generic RPC system.
   David Nichols, Xerox PARC
   July, 1991

   $Id: RPC.i3,v 1.1 1996/02/09 18:08:16 mef Exp $
*)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE RPC;

IMPORT ExceptionArg;

(* This is a minimal interface to a "generic" RPC facility.  The code for
   binding to remote servers is generally localized in applications, while
   the code that invokes remote procedures via stubs is often distributed
   over much of the application's code.  This interface suggests a
   convention for the latter.

   A remote interface should be defined as an object with methods and no
   data fields.  A client to the interface uses some runtime-specific
   mechanism to bind to the server and obtain an object of some subclass of
   this type.  The object contains any connection state required by the
   runtime.  A server of the interface creates a subclass of the type and
   provides an object of this subtype to the runtime.  Incoming RPC
   requests cause the corresponding methods of the object to be invoked.

   The RAISES list for each of the methods of the interface type includes
   RPC.Failed and Thread.Alerted.  RPC.Failed is defined below and uses the
   ExceptionArg.T mechanism for its arguement.  One subclass of Failure
   defined here guarantees that the call being attempted did not execute on
   the server.  A particular runtime may or may not be able to make this
   guarantee for certain failures.  If a runtime supports it,
   Thread.Alerted is raised due to application of Thread.Alert to a thread
   attempting an RPC. *)

EXCEPTION Failed(Failure);
TYPE
  (* Something went wrong. *)
  Failure = ExceptionArg.T BRANDED OBJECT END;
  (* Guarantees that the call did not execute remotely. *)
  ZeroTimesFailure = Failure BRANDED OBJECT END;

END RPC.
