MODULE EtherArpDep EXPORTS EtherArp;

IMPORT If, IfUtil, Dispatcher, EtherPktFormat, SocketRep,
       InUtil, SocketAddrIn, IO, Net, IoctlPosix;

CONST debug = TRUE;

PROCEDURE NewIfHandler(READONLY ifp: UNTRACED REF If.ifnet):BOOLEAN =
  VAR 
    entry:T;
    so: SocketRep.socketT;
    ifreq : REF If.ifreq := NEW(REF If.ifreq);
    ifdevea : ARRAY [1..BYTESIZE(If.ifdevea)] OF Net.BYTE;
  BEGIN
    IF InUtil.Control(so,IoctlPosix.SIOCGIFADDR,ifreq,ifp^) # 0 THEN
      IF debug THEN 
        IO.PutError("Could not get interface address.\n");
      END;
      RETURN TRUE;
    ELSIF IfUtil.ioctl(ifp,IoctlPosix.SIOCRPHYSADDR,ifdevea) = 0 THEN

      entry := NEW(T, status := StatusSet{Status.Permanent,Status.Resolved}, 
                   proType := Net.htons(EtherPktFormat.ETHERTYPE_IP));

      WITH SocketAddr_in = VIEW(ifreq.ifr_ifru, SocketAddrIn.T),
           devea = VIEW(ifdevea, If.ifdevea) 
       DO
        myAddr.protocolAddress := VIEW(SocketAddr_in.sin_addr, ProtoAddrT);
        myAddr.hardwareAddress := devea.current_pa;

        entry.hardwareAddress := devea.current_pa;
        entry.protocolAddress := myAddr.protocolAddress;
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

