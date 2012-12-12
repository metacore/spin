(*
   RPCSun.m3
   Sun RPC
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
 * 07-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Switched GetLocalInetAddr to use INADDR_ANY.
 *
 * 06-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed PutAuth and GetAuth to called RPCAuth_x functions.  
 *	Probably should inline the RPCAuth_x functions for performance.
 *
 * 05-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added support for rpc authentication.  Before only AUTH_NULL was
 *	support. 
 *
 * 04-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Using URT socket interface directly.  This should be abstracted
 *	out so that we can use this RPC package on different operating
 *	systems.
 *	
 *	Changed all INTEGER types to use proper 32bit or 16bit types.
 *	Should abstract this out and place in a RPCTypes.i3 file.
 *
 *)

MODULE RPCSun EXPORTS RPCSun, RPCSunPriv;

(* ONC RPC *)
IMPORT PortMapper;
IMPORT RPC, RPCOS;
IMPORT RPCAuth_x;
IMPORT XDR;

(* LIBS *)
(* IMPORT Random; <* NOWARN *>  *)
IMPORT Thread;

(*
 * Binding info
 *)


(* Encapsulate some binding information.  If the
   port is 0, the runtime either discovers it (by
   calling the remote port mapper) or allocates
   one (for servers). *)

PROCEDURE CreateBindingInfo (
    hostAddr    : RPCOS.InaddrT;
    progNum     : RPCOS.UINT32;
    progVersion : RPCOS.UINT32;
    port        : RPCOS.UINT16 := 0;
    protocol    := Protocol.UDP): BindingInfo =
  VAR bi: BindingInfo;
  BEGIN
    bi := NEW(BindingInfo);
    bi.hostAddr    := hostAddr;
    bi.port        := port;
    bi.progNum     := progNum;
    bi.progVersion := progVersion;
    bi.proto       := protocol;
    RETURN bi;
  END CreateBindingInfo;

PROCEDURE DecodeBindingInfo (    
    b               : BindingInfo;
    VAR hostAddr    : RPCOS.InaddrT;
    VAR progNum     : RPCOS.UINT32;
    VAR progVersion : RPCOS.UINT32;
    VAR port        : RPCOS.UINT16;
    VAR protocol    : Protocol     ) =
  BEGIN
    hostAddr    := b.hostAddr;
    progNum     := b.progNum;
    progVersion := b.progVersion;
    port        := b.port;
    protocol    := b.proto;
  END DecodeBindingInfo;

(*
 * Client services
 *)

(* Create a client from an address. *)
PROCEDURE ImportService (bi: BindingInfo): Client
  RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    IF bi.port = 0 THEN
      bi.port := LookupPort(bi);
      (* If still zero, then the program isn't registered. *)
      IF bi.port = 0 THEN
        RAISE
          RPC.Failed(NEW(RPC.Failure, info := "program not registered"));
      END;
    END;
    CASE bi.proto OF
      Protocol.UDP => RETURN UDPImportService(bi);
    | Protocol.TCP => 
      (* XXX not yet RETURN TCPImportService(bi); *)
      <*ASSERT FALSE *>
    END;
  END ImportService;

(*
 * Server services
 *)

(* Standard export (from RPCSun.i3).  Caller provides program number,
   version number, protocol and optional port number, and runtime chooses
   port number and allocates a socket. *)
PROCEDURE Export (sp        : ServerProc;
                  prog, vers: RPCOS.UINT32;
                  protocol  : Protocol;
                  port      : RPCOS.UINT16 := 0): BindingInfo
  RAISES {Erred, RPC.Failed, Thread.Alerted} =
  VAR s: RPCOS.SocketT;
  BEGIN
    CASE protocol OF
      Protocol.UDP =>
        s := GetUDPSocket(port);
        RETURN ExportUDP(sp, prog, vers, s);
    | Protocol.TCP =>
      (* XXX NOT YET
      
        s := GetListeningSocket(port);
        RETURN ExportTCPListener(sp, prog, vers, s);
      *)
      <*ASSERT FALSE*>
    END;
  END Export;


(*
 * Misc. marshalling routines.
 *)

(* Encode a call header. *)
PROCEDURE PutCallHeader (s                    : XDR.Sink;
                         xid, prog, vers, proc: RPCOS.UINT32;
                         cred, verf           : Credentials)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(s, xid);
    XDR.PutInteger(s, CALLMSG);
    XDR.PutInteger(s, RPCVERS);
    XDR.PutInteger(s, prog);
    XDR.PutInteger(s, vers);
    XDR.PutInteger(s, proc);
    PutAuth(s, cred);
    PutAuth(s, verf);
  END PutCallHeader;

PROCEDURE GetCallHeader (    s                    : XDR.Source;
                         VAR xid, prog, vers, proc: RPCOS.UINT32;
                         VAR cred, verf           : Credentials )
  RAISES {RPC.Failed, XDR.Failed, Thread.Alerted, HeaderError} =
  BEGIN
    xid := XDR.GetInteger(s);
    IF XDR.GetInteger(s) # CALLMSG THEN
      RAISE RPC.Failed(NEW(RPC.Failure, info := "not a call msg"));
    END;
    IF XDR.GetInteger(s) # RPCVERS THEN RAISE HeaderError; END;
    prog := XDR.GetInteger(s);
    vers := XDR.GetInteger(s);
    proc := XDR.GetInteger(s);
    cred := GetAuth(s);
    verf := GetAuth(s);
  END GetCallHeader;

PROCEDURE PutReplyHeader (
    s         : XDR.Sink;
    xid       : RPCOS.UINT32;
    accept    : RPCOS.UINT32;
    code      : RPCOS.UINT32;
    authWhy   : RPCOS.UINT32;
    low, high : RPCOS.UINT32 := 0;
    verf      : Credentials)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    XDR.PutInteger(s, xid);
    XDR.PutInteger(s, REPLYMSG);
    XDR.PutInteger(s, accept);
    CASE accept OF              <* NOWARN *>
      MSG_ACCEPTED =>
        PutAuth(s, verf);
        XDR.PutInteger(s, code);
        CASE code OF            <* NOWARN *>
          ACCEPT_SUCCESS, ACCEPT_PROG_UNAVAIL, ACCEPT_PROC_UNAVAIL,
            ACCEPT_GARBAGE_ARGS => (* no data *)
        | ACCEPT_PROG_MISMATCH =>
            XDR.PutInteger(s, low);
            XDR.PutInteger(s, high);
        END;
    | MSG_DENIED =>
        XDR.PutInteger(s, code);
        CASE code OF            <* NOWARN *>
          REJECT_RPC_MISMATCH =>
            XDR.PutInteger(s, low);
            XDR.PutInteger(s, high);
        | REJECT_AUTH_ERROR => XDR.PutInteger(s, authWhy);
        END;
    END;
  END PutReplyHeader;

PROCEDURE GetReplyHeader (    s                    : XDR.Source;
                          VAR xid                  : RPCOS.UINT32;
                          VAR accept, code, authWhy: RPCOS.UINT32;
                          VAR low, high            : RPCOS.UINT32;
                          VAR verf                 : Credentials )
  RAISES {RPC.Failed, XDR.Failed, Thread.Alerted} =
  VAR
    reply: RPCOS.UINT32;
  BEGIN
    xid := XDR.GetInteger(s);
    reply := XDR.GetInteger(s);
    IF reply # REPLYMSG THEN
      RAISE RPC.Failed(NEW(RPC.Failure, info := "protocol error: msg type"));
    END;
    accept := XDR.GetInteger(s);
    CASE accept OF
      MSG_ACCEPTED =>
        verf := GetAuth(s);
        code := XDR.GetInteger(s);
        CASE code OF
          ACCEPT_SUCCESS, ACCEPT_PROG_UNAVAIL, ACCEPT_PROC_UNAVAIL,
            ACCEPT_GARBAGE_ARGS => (* no data *)
        | ACCEPT_PROG_MISMATCH =>
            low := XDR.GetInteger(s);
            high := XDR.GetInteger(s);
        ELSE
          RAISE RPC.Failed(NEW(RPC.Failure,
                               info := "protocol error: bad acceptance"));
        END;
    | MSG_DENIED =>
        code := XDR.GetInteger(s);
        CASE code OF
          REJECT_RPC_MISMATCH =>
            low := XDR.GetInteger(s);
            high := XDR.GetInteger(s);
        | REJECT_AUTH_ERROR => authWhy := XDR.GetInteger(s);
        ELSE
          RAISE RPC.Failed(
                  NEW(RPC.Failure, info := "protocol error: bad denial"));
        END;
    ELSE
      RAISE RPC.Failed(
              NEW(RPC.Failure, info := "protocol error: bad reply type"));
    END;
  END GetReplyHeader;

(* Encode credentials.  *)
PROCEDURE PutAuth (s: XDR.Sink; cred: Credentials)
  RAISES {XDR.Failed, Thread.Alerted} =
  BEGIN
    RPCAuth_x.Put_Credentials(s,cred);
  END PutAuth;

(* Decode credentials.  *)
PROCEDURE GetAuth (s: XDR.Source): Credentials
  RAISES {XDR.Failed, Thread.Alerted} =
  VAR
    v: Credentials;
  BEGIN
    RPCAuth_x.Get_Credentials(s, v);
    RETURN v;
  END GetAuth;

(*
 * Port mapper.
 *)

PROCEDURE LookupPort (b: BindingInfo): RPCOS.UINT16
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR
    proto: RPCOS.UINT32;
    port : RPCOS.UINT16;
    pbi  : BindingInfo;         (* port mapper binding info *)
    pm: PortMapper.PMAP_VERSClient; (* port mapper client *)
  BEGIN
    pbi := CreateBindingInfo(b.hostAddr, PMProg, PMVers,
                             PortMapper.PMAP_PORT, Protocol.UDP);
    pm := PortMapper.ImportPMAP_VERS(pbi);
    CASE b.proto OF
      Protocol.UDP => proto := PortMapper.IPPROTO_UDP;
    | Protocol.TCP => proto := PortMapper.IPPROTO_TCP;
    END;
    TRY
      VAR map : PortMapper.mapping;
      BEGIN
        map := PortMapper.mapping{b.progNum,b.progVersion,proto,b.port};
        port := pm.GetPort(map);
      END;
    EXCEPT
      RPC.Failed (v) =>
        RAISE
          RPC.Failed(NEW(RPC.Failure, info := "can't contact portmapper",
                         subArg := v))
    END;
    pm.GetClient().Destroy();
    RETURN port;
  END LookupPort;

CONST
  TransientProgNumBase = 16_40000000; (* Start of SunRPC transient program
                                         numbers. *)
  TransientProgNumMax = 16_5FFFFFFF; (* First number past legal SunRPC
                                        transient program number. *)
  TransientStartMax = 16_48000000; (* max value we'll pick randomly *)
TYPE
  Registration = REF RECORD
                       next      : Registration;
                       prog, vers: RPCOS.UINT32;
                     END;
VAR registrations: Registration := NIL;

PROCEDURE PortMapperRegister (VAR (*in/out*) prog      : RPCOS.UINT32;
                                             vers      : RPCOS.UINT32;
                                             port      : RPCOS.UINT16;
                                             protocol  : Protocol )
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR
    proto: RPCOS.UINT32;
    pbi  : BindingInfo;         (* port mapper binding info *)
    pm: PortMapper.PMAP_VERSClient; (* port mapper client *)
    r : Registration;
  BEGIN
    pbi := CreateBindingInfo(
               RPCOS.INADDR_ANY, 
               PMProg, 
               PMVers, 
               PortMapper.PMAP_PORT, 
               Protocol.UDP);
    pm := PortMapper.ImportPMAP_VERS(pbi);
    CASE protocol OF
      Protocol.UDP => proto := PortMapper.IPPROTO_UDP;
    | Protocol.TCP => proto := PortMapper.IPPROTO_TCP;
    END;
    IF prog = TransientProgram THEN
      (* Find a transient program number to use and register it.  We do
         this by successively picking transient program numbers and trying
         to register them until one succeeds.  NOTE: Unlike the
         non-transient case, we should NOT try to unregister anything. *)
      prog := PickTransientProgramNumber();
      VAR map : PortMapper.mapping;
      BEGIN
        map := PortMapper.mapping{prog,vers,proto,port};
        WHILE NOT pm.Set(map) DO
          INC(prog);              (* Try the next one up. *)
          IF prog >= TransientProgNumMax THEN
            RAISE
              RPC.Failed(
                  NEW(RPC.Failure,
                      info := "can't find a free transient program number"));
          END;
        END;
      END;
    ELSE
      (* If we've never registered this prog/vers for any protocol before,
         then we need to unregister it at the portmapper.  We're assuming
         that we'll never have one process export for UDP and another for
         TCP for the same prog/vers pair, but the semantics of
         pmapproc_unset sort of guarantee that, anyway. *)
      r := registrations;
      WHILE r # NIL AND (r.prog # prog OR r.vers # vers) DO
        r := r.next;
      END;
      IF r = NIL THEN
        r := NEW(Registration, next := registrations, prog := prog,
                 vers := vers);
        registrations := r;
        VAR map : PortMapper.mapping;
        BEGIN
          map := PortMapper.mapping{prog, vers, proto, 0};
          EVAL pm.Unset(map);
        END;
      END;
      (* Now register ourselves. *)
      VAR map : PortMapper.mapping;
      BEGIN
          map := PortMapper.mapping{prog, vers, proto, port};
          IF NOT pm.Set(map) THEN
            RAISE RPC.Failed(NEW(RPC.Failure,
                                 info := "can't register with port mapper"));
          END;
      END;
    END;
    pm.GetClient().Destroy();
  END PortMapperRegister;

(* XXX mef;
   VAR rand: Random.T := NIL;
*)
VAR rand: CARDINAL := TransientProgNumBase;
PROCEDURE PickTransientProgramNumber (): RPCOS.UINT32 =
  (* Return a "randomly" picked transient program number. *)
  BEGIN
    (*
    IF rand = NIL THEN rand := Random.New(-1); END;
    RETURN Random.Subrange(rand, TransientProgNumBase, TransientStartMax);
    *)
    INC(rand); 
    IF rand > TransientStartMax THEN rand := TransientProgNumBase END;
    RETURN rand;
  END PickTransientProgramNumber;

BEGIN
END RPCSun.
