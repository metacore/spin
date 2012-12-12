UNSAFE (* external *)
INTERFACE EtherDevExtern;
IMPORT EtherDev, Mbuf;

<* EXTERNAL *> PROCEDURE bootname((*OUT*) VAR name: ARRAY[0..63] OF CHAR );
<* EXTERNAL *> PROCEDURE bootether((*OUT*) VAR hwaddr: EtherDev.EtherAddr);
<* EXTERNAL *> PROCEDURE bootifp(): ADDRESS;
<* EXTERNAL *> PROCEDURE ipaddr(ifp: ADDRESS): INTEGER;
<* EXTERNAL *> PROCEDURE ifpsend(ifp: ADDRESS; packet: Mbuf.T);
<* EXTERNAL *> PROCEDURE install_bootlisten(dev: EtherDev.T);

END EtherDevExtern.
