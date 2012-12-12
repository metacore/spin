(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 22-Jul-96  Frederick Gray (fgray) at the University of Washington
 *	Changed a '0' to SIN_ZERO.
 *
 * 23-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *)

MODULE PortMapper;
IMPORT Socket, SocketRep, Net;
IMPORT Errno;
IMPORT RPC, RPCOS, RPCSun, RPCSunPriv, XDR, XDRMem;
IMPORT ThreadExtra, Thread;
IMPORT PortMapperTbl, PortMapperKey, PortMapperVal;
IMPORT IO, Fmt;
IMPORT Ctypes;
IMPORT PortMapper_x;  <*NOWARN*>

CONST
  BufSize   = 8800;             (* Magic number from Sun code. *)

CONST
  debug = TRUE;

TYPE PMAP_VERSServerProc = RPCSun.ServerProc
	OBJECT
		serviceRoutines: PMAP_VERS;
	OVERRIDES
		HandleCall := PMAP_VERS_proc;
	END;

TYPE
  UDPServer = RPCSun.Server OBJECT
    sp                    : PMAP_VERSServerProc;
    prog, vers            : RPCOS.UINT32;
  OVERRIDES
    StartReply := StartReply;
  END;


PROCEDURE ServerLoop (arg: REFANY): REFANY RAISES {} =
  VAR
    server           := NARROW(arg,UDPServer);
    prog, vers, proc : RPCOS.UINT32;
    ok               : BOOLEAN;
    silent           : BOOLEAN;
    fromAddr         : RPCOS.SockaddrInT;
    nBytes           : CARDINAL;
  BEGIN
    TRY                         (* global error handler *)
      LOOP
        TRY                     (* one call *)
          ok := TRUE;
          silent := FALSE;
          nBytes := BufSize;
          IF NOT RPCOS.RecvFrom(
                   server.socket, server.recvBuffer, nBytes, fromAddr) THEN
            RETURN NIL;
          END;
          XDRMem.SetSourcePos(server.source, 0);
          XDRMem.SetSourceLen(server.source, nBytes);
          TRY
            RPCSunPriv.GetCallHeader(server.source, server.xid, prog, vers, proc,
                          server.cred, server.verf);
          EXCEPT
            RPCSunPriv.HeaderError =>
              SendRejection(server, RejectReason.RPCVersion);
              ok := FALSE;
          END;
          IF ok THEN
            IF prog # server.prog THEN
              SendRejection(server, RejectReason.ProgUnavail);
            ELSIF vers # server.vers THEN
              SendRejection(server, RejectReason.ProgMismatch);
            ELSE
              (* Do auth check here someday. *)
              (* server.sp.HandleCall(server, proc, server.source); *)
              (* PMAP_VERS_proc(server.sp, server, proc, server.source); *)
              TRY
                CASE proc OF
                  0 => PMAP_VERSNullProc(server.sp.serviceRoutines, server, server.source);
                | 1 => Set2(server.sp.serviceRoutines, server, server.source);
                | 2 => Unset2(server.sp.serviceRoutines, server, server.source);
                | 3 => GetPort2(server.sp.serviceRoutines, server, server.source);
                | 4 => Dump2(server.sp.serviceRoutines, server, server.source);
                | 5 => silent := TRUE; (* CallIt2(server.sp.serviceRoutines, server, server.source); *)
                ELSE RAISE RPC.Failed(NEW(RPC.Failure,
                                          info := "Bad PROCEDURE number"));
                END;
              EXCEPT
                XDR.Failed (e) =>
                RAISE RPC.Failed(NEW(RPC.Failure,
                                     info := "Marshalling failure", subArg := e));
              END;

            END;
          END;
        EXCEPT
          XDR.Failed =>
            SendRejection(server, RejectReason.BadArgs);
            ok := FALSE;
        END;
        (* We've either sent the reply or a rejection, so send the
           packet. *)
        IF NOT silent THEN 
          nBytes := XDRMem.GetSinkPos(server.sink);
          RPCOS.SendTo(server.socket, 
                       server.sendBuffer, 
                       nBytes,
                       fromAddr);
        END;
      END;
    EXCEPT
      (* If anything goes wrong, shut down the socket and give up. *)
      XDR.Failed, RPC.Failed, RPCSun.Erred, Thread.Alerted =>
        RPCOS.Close(server.socket);
      RETURN NIL;
    END;
  END ServerLoop;

TYPE
  RejectReason = {RPCVersion, ProgUnavail, ProgMismatch, BadProc, BadArgs};
PROCEDURE SendRejection (s: UDPServer; why: RejectReason)
  RAISES {RPC.Failed} =
  <* FATAL XDR.Failed, Thread.Alerted *>
  BEGIN
    CASE why OF
    | RejectReason.RPCVersion =>
        RPCSunPriv.PutReplyHeader(
            s      := s.sink, 
            xid    := s.xid, 
            accept := RPCSunPriv.MSG_DENIED,
            code   := RPCSunPriv.REJECT_RPC_MISMATCH, 
            low    := RPCSunPriv. RPCVERS, 
            high   := RPCSunPriv. RPCVERS, 
            verf   := s.verf);

    | RejectReason.ProgUnavail =>
        RPCSunPriv.PutReplyHeader(
            s      := s.sink, 
            xid    := s.xid, 
            accept := RPCSunPriv.MSG_ACCEPTED,
            code   := RPCSunPriv.ACCEPT_PROG_UNAVAIL, 
            verf   := s.verf);

    | RejectReason.ProgMismatch =>
        RPCSunPriv.PutReplyHeader(
            s      := s.sink, 
            xid    := s.xid, 
            accept := RPCSunPriv.MSG_ACCEPTED,
            code   := RPCSunPriv.ACCEPT_PROG_MISMATCH, 
            low    := s.vers,
            high   := s.vers, 
            verf   := s.verf);

    | RejectReason.BadProc =>
        RPCSunPriv.PutReplyHeader(
            s      := s.sink, 
            xid    := s.xid, 
            accept := RPCSunPriv.MSG_ACCEPTED,
            code   := RPCSunPriv.ACCEPT_PROC_UNAVAIL, 
            verf   := s.verf);

    | RejectReason.BadArgs =>
        RPCSunPriv.PutReplyHeader(
            s      := s.sink, 
            xid    := s.xid, 
            accept := RPCSunPriv.MSG_ACCEPTED,
            code   := RPCSunPriv.ACCEPT_GARBAGE_ARGS, 
            verf   := s.verf);
    END;
  END SendRejection;

PROCEDURE StartReply (s: UDPServer): XDR.Sink RAISES {RPCSun.Erred, RPC.Failed, Thread.Alerted} =
  <* FATAL XDR.Failed, Thread.Alerted *>
  BEGIN
    XDRMem.SetSinkPos(s.sink, 0);
    RPCSunPriv.PutReplyHeader(
        s      := s.sink, 
        xid    := s.xid, 
        accept := RPCSunPriv.MSG_ACCEPTED,
        code   := RPCSunPriv.ACCEPT_SUCCESS, 
        verf   := s.verf);
    RETURN s.sink;
  END StartReply;

PROCEDURE GetPMAP_VERSServerProc(o: PMAP_VERS): RPCSun.ServerProc =
  BEGIN
    RETURN NEW(PMAP_VERSServerProc, serviceRoutines := o);
  END GetPMAP_VERSServerProc;

PROCEDURE PMAP_VERS_proc(<*UNUSED*>self: PMAP_VERSServerProc; 
                         <*UNUSED*>s: RPCSun.Server; 
                         <*UNUSED*>proc: RPCOS.UINT32; 
                         <*UNUSED*>so: XDR.Source)
		RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
  END PMAP_VERS_proc;

PROCEDURE PMAP_VERSNullProc(<*UNUSED*>serviceRoutines: PMAP_VERS;
    s: RPCSun.Server; <*UNUSED*>so: XDR.Source)
  RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    EVAL s.StartReply();
  END PMAP_VERSNullProc;

PROCEDURE Set2(serviceRoutines: PMAP_VERS; s: RPCSun.Server; so: XDR.Source)
  RAISES {XDR.Failed, RPC.Failed, Thread.Alerted} =
  VAR
    inParm: mapping;
    outParm: BOOLEAN;
    si: XDR.Sink;
  BEGIN
    PortMapper_x.Get_mapping(so, inParm);
    outParm := serviceRoutines.Set(inParm);
    si := s.StartReply();
    XDR.PutBoolean(si, outParm);
  END Set2;

PROCEDURE Unset2(serviceRoutines: PMAP_VERS; s: RPCSun.Server; so: XDR.Source)
  RAISES {XDR.Failed, RPC.Failed, Thread.Alerted} =
  VAR
    inParm: mapping;
    outParm: BOOLEAN;
    si: XDR.Sink;
  BEGIN
    PortMapper_x.Get_mapping(so, inParm);
    outParm := serviceRoutines.Unset(inParm);
    si := s.StartReply();
    XDR.PutBoolean(si, outParm);
  END Unset2;

PROCEDURE GetPort2(serviceRoutines: PMAP_VERS; s: RPCSun.Server; so: XDR.Source)
  RAISES {XDR.Failed, RPC.Failed, Thread.Alerted} =
  VAR
    inParm: mapping;
    outParm: Ctypes.unsigned_int;
    si: XDR.Sink;
  BEGIN
    PortMapper_x.Get_mapping(so, inParm);
    outParm := serviceRoutines.GetPort(inParm);
    si := s.StartReply();
    XDR.PutInteger(si, outParm);
  END GetPort2;

PROCEDURE Dump2(serviceRoutines: PMAP_VERS; s: RPCSun.Server; <*UNUSED*>so: XDR.Source)
  RAISES {XDR.Failed, RPC.Failed, Thread.Alerted} =
  VAR
    outParm: pmaplist;
    si: XDR.Sink;
  BEGIN
    outParm := serviceRoutines.Dump();
    si := s.StartReply();
    PortMapper_x.Put_pmaplist(si, outParm);
  END Dump2;

(*
PROCEDURE CallIt2(serviceRoutines: PMAP_VERS; s: RPCSun.Server; so: XDR.Source)
  RAISES {XDR.Failed, RPC.Failed, Thread.Alerted} =
  VAR
    inParm: call_args;
    outParm: call_result;
    si: XDR.Sink;
  BEGIN
    PortMapper_x.Get_call_args(so, inParm);
    outParm := serviceRoutines.CallIt(inParm);
    si := s.StartReply();
    PortMapper_x.Put_call_result(si, outParm);
  END CallIt2;
*)


TYPE pmapServer = PMAP_VERS OBJECT OVERRIDES
  Set := Set;
  Unset := Unset;
  GetPort := GetPort;
  Dump := Dump;
  CallIt := CallIt;
END;

(* PMAPPROC_SET When a program first becomes available on a machine,
   it registers itself with the port mapper program on the same
   machine. The program passes its program number "prog", version
   number "vers", transport protocol number "prot", and the port
   "port" on which it awaits service request. The procedure returns a
   boolean reply whose value is "TRUE" if the procedure successfully
   established the mapping and "FALSE" otherwise. The procedure
   refuses to establish a mapping if one already exists for the tuple
   "(prog, vers, prot)". *)
PROCEDURE Set(<* UNUSED *> o: pmapServer; READONLY inParm: mapping): BOOLEAN
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR key: PortMapperKey.T;
      cur, val: PortMapperVal.T;
  BEGIN
    IF debug THEN
      IO.Put("PortMapper.Set ");
      IO.Putx(inParm.prog);
      IO.Putx(inParm.vers);
      IO.Putx(inParm.prot);
      IO.Put("\n");
    END;

    key := PortMapperKey.T{inParm.prog,inParm.prot};
    IF pmapTbl.get(key,val) THEN 
      cur := val;
      WHILE cur # NIL DO
        IF cur.map.vers = inParm.vers THEN
          RETURN FALSE;
        END;
        cur := cur.next;
      END;
      cur := val;
      val := NEW(PortMapperVal.T);
      val.map := inParm;
      (* Did not find pmap in linked list.  Insert a new one.  *)

      IF cur.map.vers < val.map.vers THEN
        (* Its at the head of the list.  update the table with val and
           append cur to it. *)
        EVAL pmapTbl.put(key,val);
        val.next := cur;
      ELSE
        (* search for it in the list *)
        WHILE cur.next # NIL DO
          IF cur.next.map.vers < val.map.vers THEN EXIT; END;
          cur := cur.next;
        END;
        (* insert *)
        val.next := cur.next;
        cur.next := val;
        RETURN TRUE;
      END;      

    ELSE
      val := NEW(PortMapperVal.T);
      val.map := inParm;
      EVAL pmapTbl.put(key,val);
      RETURN TRUE;
    END;
  END Set;

(* PMAPPROC_UNSET When a program becomes unavailable, it should
   unregister itself with the port mapper program on the same
   machine. The parameters and results have meanings identical to
   those of "PMAPPROC_SET". The protocol and port number fields of the
   argument are ignored. *)
PROCEDURE Unset(<* UNUSED *> o: pmapServer; READONLY inParm: mapping): BOOLEAN
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR key: PortMapperKey.T;
      cur,val: PortMapperVal.T;
  BEGIN
    IF debug THEN
      IO.Put("PortMapper.Unset ");
      IO.Putx(inParm.prog);
      IO.Putx(inParm.vers);
      IO.Putx(inParm.prot);
      IO.Put("\n");
    END;

    key := PortMapperKey.T{inParm.prog,inParm.prot};
    IF pmapTbl.get(key,val) THEN
      IF val.next = NIL THEN
        (* delete head and from table *)
        EVAL pmapTbl.delete(key,val);
      ELSE
        cur := val;
        WHILE cur.next # NIL DO
          IF cur.next.map.vers = inParm.vers THEN
            (* delete from list *)
            cur.next := cur.next.next;
          END;
        END;
      END;
    END;
    RETURN TRUE; (* deleted *)
  END Unset;

(* PMAPPROC_GETPORT Given a program number "prog", version number
   "vers", and transport protocol number "prot", this procedure
   returns the port number on which the program is awaiting call
   requests. A port value of zeros means the program has not been
   registered. The "port" field of the argument is ignored. *)
PROCEDURE GetPort(<* UNUSED *> o: pmapServer; READONLY inParm: mapping): Ctypes.unsigned_int
  RAISES {RPC.Failed, Thread.Alerted} =
  VAR key: PortMapperKey.T;
      cur,val: PortMapperVal.T;
  BEGIN
    IF debug THEN
      IO.Put("PortMapper.GetPort "); 
      IO.Putx(inParm.prog);
      IO.Putx(inParm.vers);
      IO.Putx(inParm.prot);
    END;

    key := PortMapperKey.T{inParm.prog,inParm.prot};
    IF pmapTbl.get(key,val) THEN
      IF debug THEN 
        IO.Put(" found at port ");
        IO.PutInt(val.map.port);
        IO.Put("\n");
      END;

      CASE inParm.vers OF
        0 =>
        cur := val;
        WHILE cur.next # NIL DO
          cur := cur.next;
        END;
        RETURN cur.map.port;
        (* what's the smallest version number *)
      | LAST(Ctypes.unsigned_int) =>
        (* what's the largest version number *)
        RETURN val.map.port;
      ELSE
        (* check for a specific version number *)
        cur := val;
        WHILE cur # NIL DO
          IF cur.map.vers = inParm.vers THEN
            RETURN cur.map.port;
          END;
          cur := cur.next;
        END;
      END;
    END;
    IF debug THEN 
      IO.Put(" not found\n");
    END;
    RETURN 0;
  END GetPort;

(* PMAPPROC_DUMP This procedure enumerates all entries in the port
   mapper's database. The procedure takes no parameters and returns a
   list of program, version, protocol, and port values.  *)
PROCEDURE Dump(<* UNUSED *> o: pmapServer; ): pmaplist
  RAISES {RPC.Failed, Thread.Alerted} =
(*  
    VAR key: PortMapperKey.T;
      val: PortMapperVal.T;
      plist, new: pmaplist;
*)
  BEGIN
(*
    WITH iterate = pmapTbl.iterate() DO
      WHILE iterate.next(key,val) = TRUE DO
        new       := NEW(pmaplist);
        new.next  := plist;
        plist     := new;
        plist.map := mapping{key.prog,key.vers,key.prot,val};
      END;
    END;
    RETURN plist;
*)
    RETURN NIL;
  END Dump;

PROCEDURE CallIt(<* UNUSED *> o: pmapServer; <* UNUSED *> READONLY inParm: call_args): call_result
  RAISES {RPC.Failed, Thread.Alerted} =
  BEGIN
    IF debug THEN 
      IO.Put("WARNING: Portmapper.CallIt not implemented.\n");
    END;
    <* ASSERT FALSE *>
  END CallIt;

PROCEDURE ExportUDP (sp: RPCSun.ServerProc; prog, vers: RPCOS.UINT32; socket: RPCOS.SocketT) (*:BindingInfo *)
  RAISES {RPCSun.Erred, RPC.Failed, Thread.Alerted} =
  VAR
    (*
    host : RPCOS.InaddrT;
    port : RPCOS.UINT16;
    *)
    s    : UDPServer;
  BEGIN
    (* RPCSunPriv.GetHostPortFromSocket(socket, host, port);  *)
    s            := NEW(UDPServer);
    s.sp         := sp;
    s.socket     := socket;
    s.prog       := prog;
    s.vers       := vers;
    s.sendBuffer := NEW(REF ARRAY OF CHAR, BufSize);
    s.recvBuffer := NEW(REF ARRAY OF CHAR, BufSize);
    s.sink       := XDRMem.NewSink(s.sendBuffer);
    s.source     := XDRMem.NewSource(s.recvBuffer);
    s.cred       := NIL;
    s.verf       := NIL;
    EVAL   ThreadExtra.PFork(ServerLoop,s);
    (* RPCSunPriv.PortMapperRegister(prog, vers, port, RPCSun.Protocol.UDP); *)
  END ExportUDP;

VAR pmapTbl : PortMapperTbl.Default;
BEGIN
  VAR
    pmapUdp: Socket.T;
    sin: SocketRep.sockaddr_in;
    key: PortMapperKey.T;
    val: PortMapperVal.T;
  BEGIN
    TRY
      pmapUdp := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_DGRAM,0);
      sin.sin_port := Net.htons(PMAP_PORT);
      sin.sin_len  := BYTESIZE(sin);
      sin.sin_addr := 0;
      sin.sin_zero := RPCOS.SIN_ZERO;
      Socket.Bind(pmapUdp, sin);
      pmapTbl := NEW(PortMapperTbl.Default).init();
      ExportUDP(GetPMAP_VERSServerProc(NEW(pmapServer)),
                PMAP_PROG_prognum, 
                PMAP_VERS_versnum, 
                pmapUdp);
      
      key := PortMapperKey.T{PMAP_PROG_prognum,IPPROTO_UDP};
      val := NEW(PortMapperVal.T);
      val.map := mapping{PMAP_PROG_prognum,PMAP_VERS_versnum,
                         IPPROTO_UDP,PMAP_PORT};
      EVAL pmapTbl.put(key,val);
      IO.Put("PortMapper module initialize\n");
    EXCEPT
    | Errno.E(err) =>
      IO.Put("Portmapper: " &Errno.Fmt(err) &", errno=" &Fmt.Int(err) &"\n");
    END;
  END;
END PortMapper.
