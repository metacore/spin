UNSAFE (* to import externals *)
MODULE BootDev EXPORTS EtherDev (* no, no real EXPORTS *);
IMPORT EtherDev, EtherDevRep;
IMPORT Device, NameServer, Mbuf;
IMPORT EtherDevExtern, StrongRef;
IMPORT IO, Text;
IMPORT StcpEtherPacket;
IMPORT Dispatcher;


PROCEDURE StcpGuard(dev: EtherDev.T; packet: Mbuf.T): BOOLEAN =
  BEGIN
    RETURN StcpEtherPacket.ArrivedGuard(dev,packet);
  END StcpGuard;

PROCEDURE StcpReceive(dev: EtherDev.T; packet: Mbuf.T) =
  BEGIN
    StcpEtherPacket.Arrived(dev,packet);
  END StcpReceive;

PROCEDURE Init() =
  VAR
    dev:= NEW(EtherDev.T);
    name: ARRAY[0..63] OF CHAR;
    t:TEXT;
  BEGIN
    TRY
      dev.ifp := EtherDevExtern.bootifp();
      EtherDevExtern.bootether(dev.hwaddr);
      EtherDevExtern.bootname(name);

      t:= Text.FromChars( name);
      t:= Text.Sub(t, 0, Text.FindChar(t,'\000'));
      Device.Register(t, dev);

      StrongRef.Add(dev);
      EtherDevExtern.install_bootlisten(dev);
      Dispatcher.Uninstall(StcpEtherPacket.etherhandler);
      
      (* Re-install Receive so that it is called last. All the handlers
	 installed on this event MUST specify "Cancel" option to
	 keep the original handler from being called. Otherwise, mbuf will
	 be freed twice. *)
      Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(EtherDev.Receive));
      EVAL Dispatcher.InstallHandler(EtherDev.Receive, NIL, EtherDev.Receive,
		     options := Dispatcher.Options{Dispatcher.Opt.Last,
						   Dispatcher.Opt.Cancel});
      
      StcpEtherPacket.etherhandler :=
        Dispatcher.InstallHandler(EtherDev.Receive,StcpGuard,StcpReceive,
		     options := Dispatcher.Options{Dispatcher.Opt.First,
						   Dispatcher.Opt.Cancel});
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Console: cannot register " &"device name\n");
    | Dispatcher.Error =>
      IO.PutError("EtherDev.Init: dispatcher error.\n");
    END;
  END Init;

BEGIN
  Init();
END BootDev.
