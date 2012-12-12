MODULE EtherArpDep EXPORTS EtherArp;

IMPORT If, IfUtil, Dispatcher, EtherPktFormat, SocketRep, InUtil,
       SocketAddr, SocketAddrIn, IO, Net, IoctlPosix;

CONST debug = TRUE;

PROCEDURE NewIfHandler(READONLY ifp: UNTRACED REF If.ifnet):BOOLEAN =
  VAR 
    entry:T;
    so: SocketRep.socketT;
    ifreq1 : REF If.ifreq := NEW(REF If.ifreq);
    ifreq2 := ARRAY [1..BYTESIZE(If.ifreq)] OF CHAR {'\000',..};
  BEGIN
    IF InUtil.Control(so,IoctlPosix.SIOCGIFADDR,ifreq1,ifp^) # 0 THEN
      IF debug THEN 
        IO.PutError("Could not get interface address.\n");
      END;
      RETURN TRUE;
    ELSIF IfUtil.ioctl(ifp,IoctlPosix.SIOCGIFADDR,ifreq2) = 0 THEN
      entry := NEW(T, 
                   status := StatusSet{Status.Permanent,Status.Resolved}, 
                   proType := Net.htons(EtherPktFormat.ETHERTYPE_IP));

      WITH sin = VIEW(ifreq1.ifr_ifru, SocketAddrIn.T),
           sockaddrBuf = SUBARRAY(ifreq2,16,BYTESIZE(SocketAddr.T)),
           sockaddr = VIEW(sockaddrBuf,SocketAddr.T),
           etheraddress = VIEW(sockaddr.sa_data, EtherPktFormat.Address)
       DO
        IO.Put("IX86_SPIN NewIfHandler ");
        FOR i := FIRST(etheraddress) TO LAST(etheraddress) DO
          IO.Putx(ORD(etheraddress[i])); IO.Put(" ");
        END;
        IO.Put("\n");

        myAddr.protocolAddress := VIEW(sin.sin_addr, ProtoAddrT);
        myAddr.hardwareAddress := etheraddress;
        entry.protocolAddress  := myAddr.protocolAddress;
        entry.hardwareAddress  := etheraddress;
      END;
      Add(entry);

      (* send arp request *)
      SendRequest(ifp^, entry^, entry^);
    ELSE
      IF debug THEN
        IO.Put("EtherArp could not get ethernet address!\n");
      END;
      RETURN FALSE;
    END;
    RETURN TRUE;
  END NewIfHandler;

PROCEDURE Init2() = 
  VAR
    binding : Dispatcher.Binding;
  BEGIN
    TRY
      binding := Dispatcher.InstallHandler(IfUtil.NewIf, NIL, NewIfHandler);
    EXCEPT
    | Dispatcher.Error(ec) => 
      CASE ec OF
      | Dispatcher.ErrorCode.InvalidProcedure =>
        IO.Put("IpRoute invalid procedure installed.\n");
      ELSE
        IO.Put("IpRoute dispatcher install error.\n");
      END;
    END;
  END Init2;
  
BEGIN
  Init2();
END EtherArpDep.

