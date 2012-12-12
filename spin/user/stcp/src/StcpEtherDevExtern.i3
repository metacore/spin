UNSAFE (* external *)
INTERFACE StcpEtherDevExtern;
IMPORT StcpMbuf;

<* EXTERNAL *> PROCEDURE bootname((*OUT*) VAR name: ARRAY[0..63] OF CHAR );
<* EXTERNAL *> PROCEDURE bootifp(): ADDRESS;
<* EXTERNAL *> PROCEDURE ifpsend(ifp: ADDRESS; packet: StcpMbuf.T);
<* EXTERNAL *> PROCEDURE install_bootlisten();

END StcpEtherDevExtern.
