MODULE Mountd;
IMPORT Ctypes, RPC, XDR;
IMPORT Mbuf, IO, Udp, UdpPktFormat, UdpGen;
IMPORT IpPktFormat, Misc;
IMPORT Text;
IMPORT NfsdImpl;

CONST
 udp_hdr_len = BYTESIZE(UdpPktFormat.NewT);
 ip_hdr_len = BYTESIZE(IpPktFormat.T);

PROCEDURE PROC_NULL1(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;
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
  END PROC_NULL1; 

PROCEDURE Mnt(READONLY inParm: dirpath; VAR outParm: fhstatus) = 
  VAR
    t : TEXT;
  BEGIN
    t := Text.FromChars(SUBARRAY(inParm.name,0,inParm.len));
    IF NfsdImpl.LookupMountPoint(SUBARRAY(inParm.name,0,inParm.len),outParm.fh) THEN 
      outParm.status := 0; (* success *)
    ELSE
      outParm.status := 2; (* no such file/directory *)
      IO.Put(t & " not found\n");
    END;
  END Mnt;

PROCEDURE PROC_MNT1(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL): CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : dirpath;
    outParm : fhstatus;
    size    : CARDINAL;
  BEGIN
    (* inparm = get dirpath *)
    XDR.GetTextAsChars(input,inputPos,inParm.len, inParm.name);

    Mnt(inParm,outParm);

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
    size := size + XDR.PutWord32(output, outputPos, outParm.status);
    IF outParm.status = 0 THEN 
      size := size + XDR.PutBytes(output, outputPos, outParm.fh);
    END;
    RETURN size;
  END PROC_MNT1;

PROCEDURE Umnt(READONLY inParm: dirpath) = 
  VAR
    t : TEXT;
  BEGIN
    t := Text.FromChars(SUBARRAY(inParm.name,0,inParm.len));
    IO.Put("\numount "); IO.Put(t); IO.Put("\n")
  END Umnt;

PROCEDURE PROC_UMNT1(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL): CARDINAL
  RAISES {RPC.Failed, XDR.Failed} <*NOWARN*> = 
  VAR
    inParm  : dirpath;
  BEGIN
    (* inparm = get dirpath *)
    XDR.GetTextAsChars(input,inputPos,inParm.len, inParm.name);
    Umnt(inParm);
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
  END PROC_UMNT1;

FUNCTIONAL
PROCEDURE MountdGuard(
    <*UNUSED*> packet: Mbuf.T;
    curr: Mbuf.T;
    offset: CARDINAL): BOOLEAN = 
  BEGIN
    WITH  udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
          udp_header = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
     DO
      RETURN udp_header.dport = netOrderPort;  (* 110 in network order *)
    END;
  END MountdGuard;

PROCEDURE MountdHandler (
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
          bytes := PROC_NULL1(xid,verf,output,outputPos);
        | PROC_MNT =>
          bytes := PROC_MNT1(xid,verf,curr,inputPos,output,outputPos);
        | PROC_UMNT =>
          bytes := PROC_UMNT1(xid,verf,curr,inputPos,output,outputPos);
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
      IO.PutInt(outputPos);
      IO.Put(" bytes of rpc data.\n");
    END;

    (* send reply as udp/ip packet *)
    VAR
      ip  : IpPktFormat.Header;
      udp : UdpPktFormat.Header;
    BEGIN
      WITH  udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
            udpHeader = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
       DO
        udp.sport := udpHeader.dport;
        udp.dport := udpHeader.sport;
        udp.len   := udp_hdr_len + bytes;
      END;

      (* XXX BAAAARRRRFFFFF I hate this mbuf crap -- need to redo net events (mef) *)
      (* the following WITH assumes that the ip header is in the same
         mbuf as the udp header. This will usually be the case, but we
         shouldn't rely on it. 
      *)
      WITH  ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset-ip_hdr_len,ip_hdr_len),
            ipHeader = VIEW(ipHeaderBuf,IpPktFormat.T) 
       DO
        ip.frag_off := 0;
        ip.saddr := ipHeader.daddr;
        ip.daddr := ipHeader.saddr;
        ip.protocol := IpPktFormat.IPPROTO_UDP;
        ip.tot_len := ip_hdr_len + udp.len;
      END;
      UdpGen.PacketSend(ip,udp,output,TRUE);
    END;
    RETURN FALSE;
  END MountdHandler;


VAR mountd : REFANY;
PROCEDURE Uninit(verbose: BOOLEAN) = 
  BEGIN
    Udp.Uninstall(mountd);
    mountd := NIL;
    IF verbose THEN
      IO.Put("mountd uninstalled\n");
    END;
  END Uninit;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    mountd := Udp.Install(Udp.PacketArrived,MountdGuard,MountdHandler);
    IF verbose THEN
      IO.Put("mountd installed\n");
    END;
  END Init;

BEGIN
END Mountd.
