(* HISTORY
 * 09-Dec-97  David Becker at the University of Washington
 *	Removed etherdev and etherhandler file globals
 #
 * 13-Jun-97  David Becker at the University of Washington
 *      Oops, need OsfNet not commented out to run on alphas. 
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replaced If interface with NetDev/EtherDev
 *	ifconfig now installs the EtherPacket.Arrived handler on the
 *	EtherDev.Receive event.  It also now calls OsfNet.RegisterInterface
 *	so the ip/udp/tcp code recognizes the new ifp.
 *
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-96  Wilson Hsieh (whsieh) at the University of Washington
 *	PROCANY change
 *
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated routing table datastructure.  Commands are now even more
 *	similar to the UNIX route command.
 *
 * 12-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted the walking of the interface list to use a "closure".
 *	The if list is a private datastructure which should not have been
 *	exported by the URT's IfPrivate module in the first place.
 *	
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to new spin shell command style.
 *      Converted from obsolete Clib interface to IO.
 *
 * 15-Aug-95  Marc Fiuczynski (mef) at the University of Washington
 *	Make shift routing table.
 *
 *)

MODULE IpRoute;

IMPORT Ctypes, IO, IpPktFormat, IpRouteTbl, IpRouteKey;
IMPORT Mbuf, Net, SocketAddr, SocketAddrIn, Thread, Mutex, Word;
IMPORT Dispatcher, Device, NameServer, NetDev;
IMPORT EtherDev, EtherPacket, EtherGen;
IMPORT OsfNet;
IMPORT Commands, ParseParams, NetText;


(* route command *)

PROCEDURE Update(pp: ParseParams.T) 
  RAISES{ParseParams.Error} = 
  VAR 
    src: IpRouteKey.T;
    dst: IpRouteKey.T;
    route : T;
    newroute: T;
  BEGIN
    IO.Put("Adding new route.\n");
      IF pp.testNext("default") THEN
        IO.Put("default route.\n");
	dst := NetText.TextToIp(pp.getNext());
        route := Lookup(dst);
        newroute      := NEW(T);
        newroute^     := route^;
        newroute.src  := route.src;
        newroute.dst  := dst;
        newroute.status := StatusSet{Status.Gateway};
        defaultroute    := newroute;
      ELSIF pp.testNext("host") THEN
	src := NetText.TextToIp(pp.getNext());
	dst := NetText.TextToIp(pp.getNext());
        route := Lookup(dst);
        newroute      := NEW(T);
        newroute^     := route^;
        newroute.src  := src;
        newroute.dst  := dst;
        newroute.status := StatusSet{Status.Gateway,Status.Host};
        Add(src,dst,newroute);
      ELSE
        IO.Put("route" & " no such command.\n");
      END;
  END Update;

PROCEDURE RouteCmd( <*UNUSED*> closure : REFANY; pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* route *)
      IF pp.testNext("debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("dump") THEN
        DumpRoutes();
      ELSIF pp.testNext("add") THEN
        Update(pp);
      ELSIF pp.testNext("del") OR pp.testNext("delete") THEN
        IO.Put("route" & " \"delete\" option not implemented.\n");
      ELSIF pp.testNext("flush") THEN
        IO.Put("route" & " \"flush\" option not implemented.\n");
      ELSE
        RouteUsage();
      END;
    EXCEPT
      ParseParams.Error => RouteUsage();
    END;
   RETURN TRUE;
 END RouteCmd;

PROCEDURE RouteUsage() = 
  BEGIN
    IO.Put("Usage: " & "route" & ":" & "-- debug level| dump | add default num num num num" & "\n");
  END RouteUsage;


PROCEDURE EtherGuard(dev: EtherDev.T; <*UNUSED*>packet: Mbuf.T): BOOLEAN =
  BEGIN
    TYPECASE dev OF
    | NULL => RETURN FALSE
    | EtherDev.T => RETURN TRUE
    END
  END EtherGuard;

(* ifconfig command *)

PROCEDURE ConfigSetInet (devname: TEXT; pp: ParseParams.T) =
  VAR
    srcaddr, bcastaddr, netmask : SocketAddrIn.in_addrT;
    dev: NetDev.T;
  BEGIN
    IO.Put("ifconfig\n");

    TRY
      srcaddr := NetText.TextToIp(pp.getNext());
      bcastaddr := NetText.TextToIp(pp.getNext());
      netmask := NetText.TextToIp(pp.getNext());
    EXCEPT
      ParseParams.Error => IO.Put("Bad address\n"); RETURN;
    END;

      TRY
	dev := Device.Lookup(devname);
      EXCEPT
      | NameServer.Error =>
	IO.Put("ifconfig: " &devname &" not found\n");
	RETURN;
      END;

      TYPECASE dev OF
      | NULL =>
        IO.Put("ifconfig: " &devname &" not found\n");
	RETURN;
      | EtherDev.T =>
	EVAL Dispatcher.InstallHandler(EtherDev.Receive, EtherGuard, EtherPacket.Arrived, options := Dispatcher.Options{Dispatcher.Opt.Cancel});
        EVAL NewIfHandler(dev, EtherGen.PacketSend, Word.And(netmask,srcaddr), srcaddr);
      | NetDev.T =>
        IO.Put("ifconfig: " &devname &" is only NetDev.T\n");
      END;

    OsfNet.RegisterInterface(dev,srcaddr);
  END ConfigSetInet;

PROCEDURE ConfigUpDown (<*UNUSED*>devname: TEXT) =
  BEGIN
     (* lookup dev
     if up, dev.listen(listenproc)
     if down,dev.listen(NIL)
     *)
  END ConfigUpDown;

PROCEDURE ConfigMulti (devname: TEXT; pp: ParseParams.T; <*UNUSED*>add: BOOLEAN) = 
  VAR
    multiaddr : ARRAY[0..5] OF Ctypes.unsigned_char;
    dev: NetDev.T;
  BEGIN
    IO.Put("ifconfig\n");

    TRY
      NetText.TextToEther(pp.getNext(), multiaddr);
    EXCEPT
      ParseParams.Error => IO.Put("Bad address\n"); RETURN;
    END;

    TRY
      dev := Device.Lookup(devname);
    EXCEPT
    | NameServer.Error =>
      IO.Put("ifconfig: " &devname &" not found\n");
      RETURN;
    END;
    
    TYPECASE dev OF
    | NULL =>
      IO.Put("ifconfig: " &devname &" not found\n");
      RETURN;
    | EtherDev.T =>
      IO.Put("etherdev type!\n");
(*
      IF etherdev#NIL THEN
        IO.Put("ifconfig: " &devname &" replacing current ether dev\n");
        Dispatcher.Uninstall(etherhandler);
      END;
      etherdev := dev;
      etherhandler:= Dispatcher.InstallHandler(EtherDev.Receive, EtherGuard, EtherPacket.Arrived, options := Dispatcher.Options{Dispatcher.Opt.Cancel});
      EVAL NewIfHandler(dev, EtherGen.PacketSend, Word.And(netmask,srcaddr), srcaddr);
*)
    | NetDev.T =>
      IO.Put("ifconfig: " &devname &" is only NetDev.T\n");
    END;

    OsfNet.AddMultiAddr(dev,multiaddr);

  END ConfigMulti;

PROCEDURE IfconfigUsage () =
  BEGIN
    IO.Put("    ifconfig  devname srcaddr dstaddr netmask\n");
    IO.Put(
      "    e.g. ifconfig ln0 128.95.2.94 255.255.255.255 255.255.255.0\n");
    IO.Put(
      "    e.g. ifconfig tt0 128.95.2.94 128.95.2.95 255.255.255.0\n");
    IO.Put(
      "    e.g. ifconfig tal0 128.95.2.94 255.255.255.255 255.255.255.0\n");
    IO.Put(
      "    e.g. ifconfig fa0 128.95.2.94 255.255.255.255 255.255.255.0\n");
    IO.Put("    To turn the if on do ifconfig devname up\n");
    IO.Put("    To turn the if on do ifconfig devname down\n");
  END IfconfigUsage;

PROCEDURE IfconfigCmd (<*UNUSED*> closure : REFANY;pp: ParseParams.T): BOOLEAN =
  VAR
    devname: TEXT;
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* ifconfig *)
      devname := pp.getNext();
      IF pp.testNext("up") OR pp.testNext("down") THEN
        ConfigUpDown(devname);
      ELSIF pp.testNext("multiadd") THEN
        ConfigMulti(devname, pp, TRUE);
      ELSIF pp.testNext("multidel") THEN
        ConfigMulti(devname, pp, FALSE);
      ELSE
        ConfigSetInet(devname, pp);
      END;
    EXCEPT
      ParseParams.Error =>
        IO.Put("ifconfig: no such command ");
        IfconfigUsage();
    END;
    RETURN TRUE;
  END IfconfigCmd;





VAR
  debug_level : Net.Level := Net.oLevel.NODEBUG;

VAR netmasks: ARRAY [0..1] OF SocketAddrIn.in_addrT;

PROCEDURE Lookup(ipdaddr:IpPktFormat.Address):T = 
  VAR
      entry: T; 
      dst: IpRouteKey.T;
      inRouteTable: BOOLEAN;
  BEGIN
    IF Mutex.TryLock(mutex) THEN 
      FOR i := FIRST(netmasks) TO LAST(netmasks) DO 
        dst := Word.And(ipdaddr,netmasks[i]);
        inRouteTable := routeTable.get(dst,entry);
        IF inRouteTable THEN EXIT; END;
      END;
      Thread.Release(mutex); (* Mutex.Unlock *)
      IF inRouteTable THEN RETURN entry; END;
(*
    ELSE
      RETURN bitbucket;
*)
    END;
    RETURN defaultroute;
  END Lookup;

PROCEDURE NewIfHandler(dev: NetDev.T; out: HardwareFmtProc; target,route: IpPktFormat.Address):BOOLEAN = 
      
  PROCEDURE AddIf (stat: StatusSet; READONLY src,dst: IpRouteKey.T) = 
    VAR
      entry        : T;
      inRouteTable : BOOLEAN;
      BEGIN

        IO.Put("\t Addif looking for route to ip addr = " &NetText.FmtIp(dst) &"\n");
        inRouteTable := routeTable.get(src,entry);

        IF NOT inRouteTable THEN
          (* get the ifaddrlist address and insert into routing table *)
          entry            := NEW(T);
          entry.dev := dev;
          entry.PacketSend := out;
          entry.status     := stat;
          entry.src        := src;
          entry.dst        := dst;
          Add(src,dst,entry);
        ELSE
          IO.Put("route in table... weird.");
        END;
      END AddIf;

    BEGIN
      IO.Put("\t registering if with ip addr = " &NetText.FmtIp(route) &"\n");
      AddIf(StatusSet{Status.Net},target,route);
    RETURN TRUE;
  END NewIfHandler;

PROCEDURE Add(src,dst: IpRouteKey.T; entry:T) = 
  BEGIN
    IO.Put("\t adding route " &NetText.FmtIp(src) &"\t" &NetText.FmtIp(dst) &"\n");
    LOCK mutex DO
      EVAL routeTable.put(src,entry);
    END;
  END Add;

VAR routeTable: IpRouteTbl.Default;
    mutex: MUTEX;
    defaultroute: T;
    bitbucket: T;

PROCEDURE DumpRoutes() = 
  VAR entry: T;
      key: IpRouteKey.T;

  PROCEDURE StatusPrint(stat: StatusSet) =
    BEGIN
      IO.Put("\tU");
      IF Status.Gateway IN stat THEN IO.Put("G"); END;
      IF Status.Host IN stat THEN IO.Put("H"); END;
    END StatusPrint;

  BEGIN
    (* walk through table and print all active routes *)

    LOCK mutex DO
      (* XXX need to call IP address print routine *)
      IO.Put("default\t\t" &NetText.FmtIp(defaultroute.dst));
      StatusPrint(defaultroute.status);
      IO.Put("\n");

      WITH iterate = routeTable.iterate() DO
        WHILE iterate.next(key,entry) = TRUE DO
          IO.Put(NetText.FmtIp(entry.src) &"\t" &NetText.FmtIp(entry.dst));
          StatusPrint(entry.status);
          IO.Put("\n");
        END;
      END;
    END;
  END DumpRoutes;

PROCEDURE PacketSend(
    <* UNUSED *> dev: NetDev.T;
    mbuf         : Mbuf.T; 
    <* UNUSED *> VAR s   : SocketAddr.T;
    <* UNUSED *> rte          : ADDRESS := NIL): Ctypes.int =
  BEGIN
    IO.Put("IpRoute.PacketSend() consuming mbuf.\n");
    Mbuf.m_freem(mbuf);
    RETURN 0;
  END PacketSend;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    netmasks[0] := Net.htonl(16_ffffffff);
    netmasks[1] := Net.htonl(16_ffffff00);
    mutex       := NEW(MUTEX);
    routeTable  := NEW(IpRouteTbl.Default).init();

    WITH bogusifp = NIL DO


      (* default route to be reset from shell *)
      defaultroute            := NEW(T);
      defaultroute.dev        := bogusifp;
      defaultroute.PacketSend := PacketSend;
      defaultroute.status     := StatusSet{};

      (* default route to be reset from shell *)
      bitbucket            := NEW(T);
      bitbucket.dev        := bogusifp;
      bitbucket.PacketSend := PacketSend;
      bitbucket.status     := StatusSet{};

    END;

    EVAL Commands.Install(RouteCmd, "route", "-- debug level| dump | add default num num num num");
    EVAL Commands.Install(IfconfigCmd, "ifconfig", "-- devname unitno [up|down]|srcaddr dstaddr netmask");

    IF verbose THEN IO.Put("IpRoute module initialized.\n"); END;
  END Init;

BEGIN
END IpRoute.
