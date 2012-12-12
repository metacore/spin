UNSAFE (* to import externals *)
MODULE StcpEtherDev;
IMPORT StcpMbuf;
IMPORT StcpEtherDevExtern;
IMPORT IO, Text;

VAR
  stcpIfp: ADDRESS;
  stcpDevName: TEXT;

PROCEDURE Send(<*UNUSED*> dev:T; packet:StcpMbuf.T) =
  BEGIN
    StcpEtherDevExtern.ifpsend(stcpIfp,packet);
  END Send;

PROCEDURE Receive(<*UNUSED*> dev:T; packet: StcpMbuf.T) =
  BEGIN
    (* Default handler for StcpEtherDev.Receive event.
       The handler of this event responsible for freeing the mbuf.
       
       Replace the default handler thusly:
       Dispatcher.Uninstall(Dispatcher.GetOriginalHandler(StcpEtherDev.Receive));
       EVAL Dispatcher.InstallHandler(StcpEtherDev.Receive, EtherGuard, EtherPacket.Arrived);
     *)

    (*
    IO.Put("Rcv ");
     *)

    StcpMbuf.m_freem(packet)
  END Receive;

PROCEDURE Init() =
  VAR
    name: ARRAY[0..63] OF CHAR;
  BEGIN
    StcpEtherDevExtern.bootname(name);
    stcpDevName:= Text.FromChars( name);
    stcpDevName:= Text.Sub(stcpDevName,0,Text.FindChar(stcpDevName,'\000'));

    stcpIfp := StcpEtherDevExtern.bootifp();
    StcpEtherDevExtern.install_bootlisten();
    IO.Put("Stcp: using " &stcpDevName &"\n");
  END Init;

BEGIN
END StcpEtherDev.
