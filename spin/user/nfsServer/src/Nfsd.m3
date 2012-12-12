MODULE Nfsd;
IMPORT Ctypes, RPC, XDR;
IMPORT Mbuf, IO, Udp, UdpPktFormat, UdpGen;
IMPORT IpPktFormat, Misc, Nfsd_x;

CONST
 udp_hdr_len = BYTESIZE(UdpPktFormat.NewT);
 ip_hdr_len = BYTESIZE(IpPktFormat.T);

PROCEDURE PROC_NULL2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    <*UNUSED*>
    input         : Mbuf.T;
    <*UNUSED*>
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    size : CARDINAL;
  BEGIN
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_NULL2;

PROCEDURE PROC_GETATTR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR 
    inParm  : nfs_fh;
    outParm : attrstat;  
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_nfs_fh(input,inputPos,inParm);
    GETATTR(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_attrstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size
  END PROC_GETATTR2;

PROCEDURE PROC_SETATTR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : sattrargs;
    outParm : attrstat;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_sattrargs(input,inputPos,inParm);

    SETATTR(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);

    size := size + Nfsd_x.Put_attrstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_SETATTR2;

PROCEDURE PROC_ROOT2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    <*UNUSED*>
    input         : Mbuf.T;
    <*UNUSED*>
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T;  
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 

  BEGIN
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    RETURN RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
  END PROC_ROOT2;

PROCEDURE PROC_LOOKUP2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : diropargs;
    outParm : diropres;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_diropargs(input,inputPos,inParm);
    LOOKUP(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_diropres(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_LOOKUP2;

PROCEDURE PROC_READLINK2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : nfs_fh;
    outParm : readlinkres;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_nfs_fh(input,inputPos,inParm);

    READLINK(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_readlinkres(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_READLINK2;

(* 
PROCEDURE PROC_READ2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : readargs;
    outParm : readres;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_readargs(input,inputPos,inParm);
    size := size + READ(inParm,output,outputPos);
    RETURN size;
  END PROC_READ2;
*)

PROCEDURE PROC_WRITECACHE2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    <*UNUSED*>
    input         : Mbuf.T;
    <*UNUSED*>
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 

  BEGIN
    (* XXX WRITECACHE(inParm,outParm); *)
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    RETURN RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
  END PROC_WRITECACHE2;

PROCEDURE PROC_WRITE2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : writeargs;
    outParm : attrstat;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_writeargs(input,inputPos,inParm);
    WRITE(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);

    size := size + Nfsd_x.Put_attrstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_WRITE2;

PROCEDURE PROC_CREATE2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR 
    inParm  : createargs;
    outParm : diropres;
    size    : CARDINAL;

  BEGIN
    Nfsd_x.Get_createargs(input,inputPos,inParm);

    CREATE(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_diropres(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_CREATE2;

PROCEDURE PROC_REMOVE2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : diropargs;
    outParm : nfsstat;
    size    : CARDINAL;

  BEGIN
    Nfsd_x.Get_diropargs(input,inputPos,inParm);
    REMOVE(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_nfsstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_REMOVE2;

PROCEDURE PROC_RENAME2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : renameargs;
    outParm : nfsstat;
    size    : CARDINAL;

  BEGIN
    Nfsd_x.Get_renameargs(input,inputPos,inParm);

    RENAME(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_nfsstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_RENAME2;

PROCEDURE PROC_LINK2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : linkargs;
    outParm : nfsstat;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_linkargs(input,inputPos,inParm);

    LINK(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);

    size := size + Nfsd_x.Put_nfsstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_LINK2;

PROCEDURE PROC_SYMLINK2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : symlinkargs;
    outParm : nfsstat;
    size    : CARDINAL;

  BEGIN
    Nfsd_x.Get_symlinkargs(input,inputPos,inParm);

    SYMLINK(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_nfsstat(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_SYMLINK2;

PROCEDURE PROC_MKDIR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : createargs;
    outParm : diropres;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_createargs(input,inputPos,inParm);
    MKDIR(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_diropres(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_MKDIR2;

PROCEDURE PROC_RMDIR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : diropargs;
    outParm : nfsstat;
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_diropargs(input,inputPos,inParm);

    RMDIR(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_nfsstat(output,outputPos,outParm);
    RETURN size;
  END PROC_RMDIR2;

(*
PROCEDURE PROC_READDIR2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : readdirargs;
    (* XXX UNUSED 
    outParm : readdirres;
    *)
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_readdirargs(input,inputPos,inParm);

    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    output.mh_hdr.mh_len := size;
    size := size + READDIR(inParm,output,outputPos);
    RETURN size;
  END PROC_READDIR2;
*)

PROCEDURE PROC_STATFS2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR 
    inParm  : nfs_fh;
    outParm : statfsres;  
    size    : CARDINAL;
  BEGIN
    Nfsd_x.Get_nfs_fh(input,inputPos,inParm);
    STATFS(inParm,outParm);
    outputPos := 0;
    output := Misc.GetMbuf(Mbuf.MLEN); (* XXX could request something much smaller *)
    (* start reply *)
    size := RPC.PutReplyHeader(
        m      := output,
        pos    := outputPos,
        xid    := xid, 
        accept := RPC.MSG_ACCEPTED,
        code   := RPC.ACCEPT_SUCCESS, 
        verf   := verf);
    size := size + Nfsd_x.Put_statfsres(output,outputPos,outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END PROC_STATFS2;

FUNCTIONAL
PROCEDURE NfsGuard(
    <*UNUSED*> packet: Mbuf.T;
    curr: Mbuf.T;
    offset: CARDINAL): BOOLEAN = 
  BEGIN
    WITH  udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
          udp_header = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
     DO
      RETURN udp_header.dport = netOrderPort;  (* 2049 in network order *)
    END;
  END NfsGuard;

PROCEDURE NfsHandler (
    <*UNUSED*>
    packet : Mbuf.T; 
    curr   : Mbuf.T; 
    offset : CARDINAL):BOOLEAN = 
  VAR
    output: Mbuf.T;
    outputPos: CARDINAL;
    xid, prog, vers, proc : Ctypes.unsigned_int;
    inputPos : CARDINAL := offset;
    cred, verf: RPC.Credentials;
    bytes : CARDINAL;
  BEGIN
    TRY
      INC(inputPos,udp_hdr_len); (* skip over the udp header *)
      RPC.GetCallHeader(curr,inputPos,xid,prog,vers,proc,cred,verf);
      IF prog # Prog THEN
        bytes := Misc.SendRejection(RPC.RejectReason.ProgUnavail,xid,vers,verf,output,outputPos);
      ELSIF vers # Vers THEN
        bytes := Misc.SendRejection(RPC.RejectReason.ProgMismatch,xid,vers,verf,output,outputPos);
      ELSE
        (* XXX Do auth check here someday. *)
        CASE proc OF
        | PROC_NULL =>
          bytes := PROC_NULL2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_GETATTR =>
          bytes := PROC_GETATTR2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_SETATTR =>
          bytes := PROC_SETATTR2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_ROOT =>
          bytes := PROC_ROOT2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_LOOKUP =>
          bytes := PROC_LOOKUP2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_READLINK =>
          bytes := PROC_READLINK2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_READ =>
          bytes := PROC_READ2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_WRITECACHE =>
          bytes := PROC_WRITECACHE2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_WRITE =>
          bytes := PROC_WRITE2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_CREATE =>
          bytes := PROC_CREATE2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_REMOVE =>
          bytes := PROC_REMOVE2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_RENAME =>
          bytes := PROC_RENAME2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_LINK =>
          bytes := PROC_LINK2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_SYMLINK =>
          bytes := PROC_SYMLINK2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_MKDIR =>
          bytes := PROC_MKDIR2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_RMDIR =>
          bytes := PROC_RMDIR2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_READDIR =>
          bytes := PROC_READDIR2(xid,verf,curr,inputPos,output,outputPos);
        | PROC_STATFS =>
          bytes := PROC_STATFS2(xid,verf,curr,inputPos,output,outputPos);
        ELSE
          bytes := Misc.SendRejection(RPC.RejectReason.BadProc,xid,vers,verf,output,outputPos);
        END;
      END;
    EXCEPT
      | RPC.Failed(text) =>
        IO.Put("RPC Failure ");
        IO.Put(text);
        IO.Put("\n");
        bytes := Misc.SendRejection(RPC.RejectReason.RPCVersion,xid,vers,verf,output,outputPos); <* NOWARN *>
      | XDR.Failed =>
        bytes := Misc.SendRejection(RPC.RejectReason.BadArgs,xid,vers,verf,output,outputPos); <* NOWARN *>
    END;
    (* send reply *)
    (* prepend udp/ip header here based on what we extract from packet *)
    IF debug THEN
      IO.Put("Send reply with ");
      IO.PutInt(bytes);
      IO.Put(" bytes of rpc data.\n");
    END;
    (* send reply as udp/ip packet *)
    VAR
      ip  : IpPktFormat.Header;
      udp : UdpPktFormat.Header;
    BEGIN
      (* Charlie: I combined the next two WITH statements into one.  
         I am assuming that since the UDP header follows the IP header,
         it must also be 32-bit aligned. If that is not true, then this
         VIEW may fail in cases where the original version would succeed. 
      *)
      WITH  HeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset-ip_hdr_len,ip_hdr_len + udp_hdr_len), 
            Header = VIEW(HeaderBuf,RECORD ip: IpPktFormat.T; udp: UdpPktFormat.NewT END)
       DO
        udp.sport := Header.udp.dport;
        udp.dport := Header.udp.sport;
        udp.len   := udp_hdr_len + bytes;

        ip.frag_off := 0;
        ip.saddr := Header.ip.daddr;
        ip.daddr := Header.ip.saddr;
        ip.protocol := IpPktFormat.IPPROTO_UDP;
        ip.tot_len := ip_hdr_len + udp.len;
      END;

      UdpGen.PacketSend(ip,udp,output,TRUE);
    END;

    RETURN FALSE;
  END NfsHandler;

VAR nfsd : REFANY;

PROCEDURE Uninit(verbose: BOOLEAN) = 
  BEGIN
    Udp.Uninstall(nfsd);
    nfsd := NIL; 
    IF verbose THEN 
      IO.Put("nfsd uninstalled\n");
    END;
  END Uninit;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    nfsd := Udp.Install(Udp.PacketArrived,NfsGuard,NfsHandler);
    IF verbose THEN
      IO.Put("nfsd installed\n");
    END;
  END Init;

BEGIN
END Nfsd. 
