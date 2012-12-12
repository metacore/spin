UNSAFE (* for externals *)
INTERFACE SalnetExtern;

IMPORT Salnet, Word;

<*EXTERNAL *> VAR salnet_debug: INTEGER;

<*EXTERNAL *> PROCEDURE salnet_getetheraddr((*OUT*) VAR ether: Salnet.EtherAddr);
<*EXTERNAL *> PROCEDURE salnet_getipaddr(): Salnet.IpAddr;
<*EXTERNAL *> PROCEDURE salnet_setipaddr(ip: Salnet.IpAddr);
<*EXTERNAL *> PROCEDURE salnet_getbootserver(): Salnet.IpAddr;
<*EXTERNAL *> PROCEDURE salnet_setbootserver(ip: Salnet.IpAddr);

<*EXTERNAL *> PROCEDURE salnet_arp(ip: Salnet.IpAddr; (*OUT*) VAR ether: Salnet.EtherAddr): INTEGER;
<*EXTERNAL *> PROCEDURE salnet_bootp((*OUT*) VAR localip: Salnet.IpAddr; VAR serverip: Salnet.IpAddr; VAR servername: Salnet.BootpServer; VAR bootfile: Salnet.BootpFile): INTEGER;
<*EXTERNAL *> PROCEDURE m3dnsquery(hostname: TEXT; (*OUT*) VAR ip: Salnet.IpAddr): INTEGER;

<*EXTERNAL *> PROCEDURE m3tftpfetch(server: Salnet.IpAddr; filename: TEXT; (*OUT*) VAR buf: ARRAY OF CHAR; VAR  len: Word.T):INTEGER;

<*EXTERNAL *> PROCEDURE m3nfsmount(server: Salnet.IpAddr; dir: TEXT; (*OUT*) VAR mount: Salnet.NfsFileHandle): INTEGER;
<*EXTERNAL *> PROCEDURE m3nfsfetch(server: Salnet.IpAddr; VAR mount: Salnet.NfsFileHandle; filename: TEXT; (*OUT*) VAR  buf: ARRAY OF CHAR; VAR  len: Word.T): INTEGER;

END SalnetExtern.
