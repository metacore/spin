MODULE PM;
IMPORT Ctypes, RPC, XDR, Mbuf, IO;
IMPORT Udp, UdpPktFormat, UdpGen;
IMPORT IpPktFormat;
IMPORT Mountd, Nfsd;
IMPORT Misc;

VAR portmap : REF ARRAY OF REF T;
VAR portmapper : REFANY;


PROCEDURE GetPort(READONLY inParm: T): Ctypes.unsigned_int
  RAISES {RPC.Failed} <*NOWARN*> =
  VAR 
    cur : REF T := NIL;
  BEGIN
    IF debug THEN
      IO.Put("PortMapper.GetPort "); 
      IO.PutInt(inParm.prog);
      IO.Put(" ");
      IO.PutInt(inParm.vers);
      IO.Put(" ");
      IO.PutInt(inParm.prot);
    END;

    FOR i := FIRST(portmap^) TO LAST(portmap^) DO
      IF portmap[i].prog = inParm.prog AND portmap[i].prot = inParm.prot THEN
        cur := portmap[i]; 
        IF debug THEN 
          IO.Put(" located at port ");
          IO.PutInt(cur.port);
        END;
        CASE inParm.vers OF
        | 0 =>
          (* what's the smallest version number *)
          IF debug THEN IO.Put("\n"); END;
          RETURN cur.port;
        | LAST(Ctypes.unsigned_int) =>
          (* what's the largest version number *)
          IF debug THEN IO.Put("\n"); END;
          RETURN cur.port;
        ELSE
          (* check for a specific version number *)
          IF cur.vers <= inParm.vers THEN
            IF debug THEN IO.Put("\n"); END;
            RETURN cur.port;
          END;
        END;
      END;
    END;
    IF debug THEN 
      IO.Put(" not found\n");
    END;
    RETURN 0;
  END GetPort;

PROCEDURE Null2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;
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
  END Null2;

PROCEDURE GetPort2(
    xid           : Ctypes.unsigned_int;
    READONLY verf : RPC.Credentials;

    input         : Mbuf.T;
    inputPos      : CARDINAL;
    VAR output    : Mbuf.T; 
    VAR outputPos : CARDINAL) : CARDINAL
  RAISES {RPC.Failed, XDR.Failed} = 
  VAR
    inParm  : T;
    outParm : Ctypes.unsigned_int;
    size :CARDINAL;
  BEGIN
    (* get mapping XDRs *)
    inParm.prog := XDR.GetWord32(input, inputPos);
    inParm.vers := XDR.GetWord32(input, inputPos);
    inParm.prot := XDR.GetWord32(input, inputPos);
    inParm.port := XDR.GetWord32(input, inputPos);

    (* make call to actual getport implementation *)
    outParm     := GetPort(inParm);

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
    size := size + XDR.PutWord32(output, outputPos, outParm);
    output.mh_hdr.mh_len := size;
    RETURN size;
  END GetPort2;

CONST
  udp_hdr_len = BYTESIZE(UdpPktFormat.NewT);
  ip_hdr_len =  BYTESIZE(IpPktFormat.T);

FUNCTIONAL
PROCEDURE PMGuard(
    <*UNUSED*> packet: Mbuf.T;
    curr: Mbuf.T;
    offset: CARDINAL): BOOLEAN = 
  BEGIN
    WITH  udpHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,udp_hdr_len),
          udp_header = VIEW(udpHeaderBuf,UdpPktFormat.NewT) 
     DO
      RETURN udp_header.dport = 16_6f00;  (* 111 in network order *)
    END;
  END PMGuard;

PROCEDURE PMHandler (
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
        | 0 => 
          bytes := Null2(xid,verf,output,outputPos);
        | 3 => 
          bytes := GetPort2(xid,verf,curr,inputPos,output,outputPos);
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
      (* Charlie: I think we could combine both of these WITH statements
         into one, saving on some alignment and size checks. *)
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
  END PMHandler;


PROCEDURE Uninit(verbose: BOOLEAN) = 
  BEGIN
    Udp.Uninstall(portmapper);
    portmapper := NIL;
    portmap := NIL;
    IF verbose THEN
      IO.Put("portmapper uninstalled\n");
    END;
  END Uninit;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN
    (* Charlie: Could we make portmap be a REF ARRAY [0..2] OF REF T? *)
  portmap := NEW(REF ARRAY OF REF T, 3);
  portmap[0] := NEW(REF T, 
                    prog := Mountd.Prog,
                    vers := Mountd.Vers,
                    prot := IPPROTO_UDP, 
                    port := Mountd.hostOrderPort); (* MOUNTD *)

  portmap[1] := NEW(REF T, 
                    prog := Nfsd.Prog,
                    vers := Nfsd.Vers,
                    prot := IPPROTO_UDP, 
                    port := Nfsd.hostOrderPort); (* NFS    *)

  portmap[2] := NEW(REF T, 
                    prog := Prog, 
                    vers := Vers, 
                    prot := IPPROTO_UDP, 
                    port := 111); (* PM *)


  portmapper := Udp.Install(Udp.PacketArrived,PMGuard,PMHandler);

  IF verbose THEN
    IO.Put("portmapper installed\n");
  END;
  END Init;

BEGIN
END PM.
