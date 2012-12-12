(*
   RPCSunPriv.i3
   SunRPC internal stuff.
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
 * 05-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	heavily hacked by me.
 *
 *)

INTERFACE RPCSunPriv;
IMPORT RPC, RPCOS, RPCSun, XDR;
IMPORT Thread;

CONST
  (* Current RPC version. *)
  RPCVERS              : RPCOS.UINT32 = 2;

  (* Messages types. *)
  CALLMSG              : RPCOS.UINT32 = 0;
  REPLYMSG             : RPCOS.UINT32 = 1;

  (* Reply types. *)
  MSG_ACCEPTED         : RPCOS.UINT32 = 0;
  MSG_DENIED           : RPCOS.UINT32 = 1;

  (* Accepted return codes. *)
  ACCEPT_SUCCESS       : RPCOS.UINT32 = 0; (* RPC executed successfully *)
  ACCEPT_PROG_UNAVAIL  : RPCOS.UINT32 = 1; (* remote hasn't exported program *)
  ACCEPT_PROG_MISMATCH : RPCOS.UINT32 = 2; (* remote can't support version # *)
  ACCEPT_PROC_UNAVAIL  : RPCOS.UINT32 = 3; (* program can't support procedure *)
  ACCEPT_GARBAGE_ARGS  : RPCOS.UINT32 = 4; (* procedure can't decode params *)

  (* Rejection return codes *)
  REJECT_RPC_MISMATCH  : RPCOS.UINT32 = 0; (* RPC version number # 2 *)
  REJECT_AUTH_ERROR    : RPCOS.UINT32 = 1; (* remote can't authenticate caller *)

  (* Auth failure reasons. *)
  AUTH_BADCRED         : RPCOS.UINT32 = 1; (* bad credentials *)
  AUTH_REJECTEDCRED    : RPCOS.UINT32 = 2; (* client must begin new session *)
  AUTH_BADVERF         : RPCOS.UINT32 = 3; (* bad verifier *)
  AUTH_REJECTED_VERF   : RPCOS.UINT32 = 4; (* verifier expired or replayed *)
  AUTH_TOOWEAK         : RPCOS.UINT32 = 5; (* rejected for security reasons *)

  (* Authentication types. *)
  AUTH_NULL            : RPCOS.UINT32 = 0;
  AUTH_UNIX            : RPCOS.UINT32 = 1;
  AUTH_SHORT           : RPCOS.UINT32 = 2;
  AUTH_DES             : RPCOS.UINT32 = 3;

  (* Port mapper constants. *)
  PMProg               : RPCOS.UINT32 = 100000;
  PMVers               : RPCOS.UINT32 = 2;

REVEAL
  RPCSun.BindingInfo = BRANDED REF RECORD
                                     hostAddr   : RPCOS.InaddrT;
                                     port       : RPCOS.UINT16;
                                     progNum    : RPCOS.UINT32;
                                     progVersion: RPCOS.UINT32;
                                     proto      : RPCSun.Protocol;
                                   END;

(* Header marshalling routines. *)
PROCEDURE PutCallHeader (s                    : XDR.Sink;
                         xid, prog, vers, proc: RPCOS.UINT32;
                         cred, verf           : RPCSun.Credentials)
  RAISES {XDR.Failed, Thread.Alerted};
PROCEDURE GetCallHeader (    s                    : XDR.Source;
                         VAR xid, prog, vers, proc: RPCOS.UINT32;
                         VAR cred, verf           : RPCSun.Credentials )
  RAISES {RPC.Failed, XDR.Failed, Thread.Alerted, HeaderError};
EXCEPTION HeaderError;

PROCEDURE PutReplyHeader (s                 : XDR.Sink;
                          xid, accept, code : RPCOS.UINT32;
                          authWhy, low, high: RPCOS.UINT32 := 0;
                          verf              : RPCSun.Credentials)
  RAISES {XDR.Failed, Thread.Alerted};

PROCEDURE GetReplyHeader (    s                    : XDR.Source;
                          VAR xid                  : RPCOS.UINT32;
                          VAR accept, code, authWhy: RPCOS.UINT32;
                          VAR low, high            : RPCOS.UINT32;
                          VAR verf                 : RPCSun.Credentials )
  RAISES {RPC.Failed, XDR.Failed, Thread.Alerted};

(* Import a service. *)
(* XXX not yet
PROCEDURE TCPImportService (bi: RPCSun.BindingInfo): RPCSun.Client
  RAISES {RPC.Failed};
*)
PROCEDURE UDPImportService (bi: RPCSun.BindingInfo): RPCSun.Client
  RAISES {RPC.Failed};

(* Initialize a struct_sockaddr_in. *)
PROCEDURE SetAddr (
    VAR addr : RPCOS.SockaddrInT;
    host     : RPCOS.InaddrT; 
    port     : RPCOS.UINT16);

(* Deduce port number from a socket. *)
PROCEDURE GetHostPortFromSocket (
    s        : RPCOS.SocketT; 
    VAR host : RPCOS.InaddrT; 
    port     : RPCOS.UINT16)
  RAISES {RPCSun.Erred, RPC.Failed};

(* Get a listing TCP socket at a particular port. *)
(* XXX not yet
PROCEDURE GetListeningSocket (port: RPCOS.UINT16): RPCOS.SocketT RAISES {RPC.Failed};
*)

(* Get a generic UDP socket. *)
PROCEDURE GetUDPSocket (port: RPCOS.UINT16 := 0): RPCOS.SocketT RAISES {RPC.Failed};

(* Register with the port mapper.  If prog is RPCSun.TransientProgram, then
   pick a transient program number and return its value in prog. *)
PROCEDURE PortMapperRegister (VAR (*in/out*) prog      : RPCOS.UINT32;
                                             vers      : RPCOS.UINT32;
                                             port      : RPCOS.UINT16;
                                             protocol  : RPCSun.Protocol)
  RAISES {RPCSun.Erred, RPC.Failed, Thread.Alerted};

END RPCSunPriv.
