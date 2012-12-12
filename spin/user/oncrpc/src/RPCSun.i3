(*
   RPCSun.i3
   Application interface to SunRPC.
   David Nichols, Xerox PARC
   July, 1991

   Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)
(*
 * HISTORY 
 *)
 

INTERFACE RPCSun;
IMPORT RPCOS;
IMPORT ExceptionArg, RPC, RPCAuth, XDR;
IMPORT Thread;

(* This module provides the routines for making and handling remote
   procedure calls using Sun's RPC protocol.  In general, applications that
   use m3rpcgen will not need to invoke any of the methods described in the
   objects below. *)

(*
 * Exceptions
 *)

(* Subypes of RPC.Failure. *)
TYPE
  RejectFailure =               (* Remote side rejected this invocation.
                                   Client object is still usable. *)
    RPC.ZeroTimesFailure BRANDED OBJECT END;
  TimeoutFailure =              (* Call timed out. *)
    RPC.Failure BRANDED OBJECT END;

(* In addition to RPC.Failed, various routines can raise RPCSun.Erred to
   indicate misuse (called with bad parameters, or in the wrong order,
   etc.).

   In general, a Client object will be unusable after Erred or
   Thread.Alerted is raised.  It is usable after RPC.Failed if it due to a
   RejectFailure. *)
EXCEPTION Erred(Error);
TYPE
  (* Calling program messed up *)
  Error = ExceptionArg.T BRANDED OBJECT END;

(* 
 * Authentication 
 *)

TYPE Credentials = RPCAuth.Credentials;

(*
 * Addressing.
 *)

TYPE
  Protocol = {UDP, TCP};

  (* Describes the "address" of something that listens for RPCs. *)
  BindingInfo <: REFANY;

(* Encapsulate some binding information.  If the port is 0, the runtime
   either discovers it (for clients, by calling the remote port mapper) or
   allocates one (for servers).

   The host address and port numbers must be in *host* byte order, not the
   network byte order that the Unix interfaces prefer.  This makes it
   easier to read them from users, encode them on the net (since they
   aren't different from other integers), etc. *)
PROCEDURE CreateBindingInfo (
    hostAddr    : RPCOS.InaddrT;
    progNum     : RPCOS.UINT32;
    progVersion : RPCOS.UINT32;
    port        : RPCOS.UINT16 := 0;
    protocol    := Protocol.UDP): BindingInfo;

(* Break one up. *)
PROCEDURE DecodeBindingInfo (    
    b               : BindingInfo;
    VAR hostAddr    : RPCOS.InaddrT;
    VAR progNum     : RPCOS.UINT32;
    VAR progVersion : RPCOS.UINT32;
    VAR port        : RPCOS.UINT16;
    VAR protocol    : Protocol     );

(*
 * Client side stuff.
 *)

CONST
  MaxClientConcurrency = 4;
  
TYPE
  Transaction = REF RECORD
    inUse: BOOLEAN; (* TRUE if a transaction is being carried out *)
    socket: RPCOS.SocketT;
    source: XDR.Source;
    sink: XDR.Sink;
    xid: RPCOS.UINT32;
    sendBuffer: REF ARRAY OF CHAR;
    recvBuffer: REF ARRAY OF CHAR;
  END;
    
  (* Client programs use these to make calls to servers.  The program calls
     StartCall(), uses a the sink to encode the parameters (using the Put*
     methods), calls SendCall (which actually does the RPC), decodes the
     results with the Get*() methods, and calls EndCall().  At most one
     call can be in progress at a time using a single Client object. *)
  Client = OBJECT
    tr: ARRAY [0 .. MaxClientConcurrency-1] OF Transaction;
    cred: Credentials;
    verf: Credentials;
    xid: RPCOS.UINT32 := 0;
  METHODS
    StartCall(proc: RPCOS.UINT32): Transaction
      RAISES {Erred, RPC.Failed, Thread.Alerted};
    SendCall(tr: Transaction): XDR.Source
      RAISES {Erred, RPC.Failed, Thread.Alerted};
    EndCall(tr: Transaction) RAISES {Erred, RPC.Failed, Thread.Alerted};
    GetRemote (): BindingInfo; (* server we're bound to *)
    Destroy ();
    (* Called when program is done with this handle. *)
  END;

(* Get a Client handle to make calls with. *)
PROCEDURE ImportService (bi: BindingInfo): Client
  RAISES {RPC.Failed, Thread.Alerted};


(*
 * Server side stuff.
 *)

CONST
  TransientProgram = 16_40000000; (* says we want a transient program number *)

TYPE
  (* One of these is passed to the HandleCall method (see below) when an
     incoming call arrives. *)
  Server =
    OBJECT
    socket     : RPCOS.SocketT;
    sendBuffer : REF ARRAY OF CHAR;
    recvBuffer : REF ARRAY OF CHAR;
    source     : XDR.Source;
    sink       : XDR.Sink;
    cred       : Credentials;
    verf       : Credentials;
    xid        : RPCOS.UINT32 := 0;
    METHODS
      (* Called before server proc switches from reading args to sending
         reply values. *)
      StartReply (): XDR.Sink RAISES {Erred, RPC.Failed, Thread.Alerted};
    END;

  (* The object invoked with incoming RPC calls.  A stub will provide one
     when given the object to export, and the ServerProc can then be passed
     to the various Export* routines.  Whenever a call is received for this
     service, the HandleCall method is invoked.  It should call
     s.StartReply() when it's ready to start sending reply data. *)
  ServerProc = BRANDED OBJECT
               METHODS
                 HandleCall (s: Server; proc: RPCOS.UINT32; so: XDR.Source)
                             RAISES {Erred, RPC.Failed, Thread.Alerted};
               END;

(* Standard export.  Caller provides program number, version number,
   protocol and optional port number, and runtime chooses port number (if
   necessary) and allocates a socket.

   If the program number is TransientProgram, then a new transient program
   number is picked and used.  The new value can be obtained from the
   BindingInfo returned from the function value.  ExportUDP and
   ExportTCPListener will also do this. *)
PROCEDURE Export (
    sp       : ServerProc;
    prog     : RPCOS.UINT32;
    vers     : RPCOS.UINT32;
    protocol : Protocol;
    port     : RPCOS.UINT16 := 0): BindingInfo
  RAISES {Erred, RPC.Failed, Thread.Alerted};

(* Caller has bound socket. *)
PROCEDURE ExportUDP (
    sp     : ServerProc; 
    prog   : RPCOS.UINT32; 
    vers   : RPCOS.UINT32; 
    socket : RPCOS.SocketT): BindingInfo
  RAISES {Erred, RPC.Failed, Thread.Alerted};

(* Caller has connected socket (e.g.  via inetd) to client.  Will not pick
   a transient program number since it doesn't register with the port
   mapper. *)
(* XXX not yet
PROCEDURE ExportTCP (
    sp     : ServerProc; 
    prog   : RPCOS.UINT32; 
    vers   : RPCOS.UINT32; 
    socket : RPCOS.SocketT): BindingInfo
  RAISES {Erred, RPC.Failed};
*)

(* Caller has a bound listening socket.  The runtime will accept new
   connections on this socket. *)
(* XXX not yet
PROCEDURE ExportTCPListener (
    sp     : ServerProc;
    prog   : RPCOS.UINT32;
    vers   : RPCOS.UINT32;
    socket : RPCOS.SocketT     ): BindingInfo
  RAISES {Erred, RPC.Failed, Thread.Alerted};
*)

END RPCSun.


