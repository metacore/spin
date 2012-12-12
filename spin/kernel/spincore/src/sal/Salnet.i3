INTERFACE Salnet;

IMPORT Errno, Word, Ctypes;

TYPE
  IpAddr = Ctypes.unsigned_int;
  EtherAddr = ARRAY[0..5] OF Ctypes.unsigned_char;
  BootpServer = ARRAY[0..63] OF CHAR;
  BootpFile = ARRAY[0..127] OF CHAR;
  NfsFileHandle = ARRAY[0..31] OF CHAR;

EXCEPTION
  Error(INTEGER);

CONST
	SALNET_SUCCESS	= 0;
	SALNET_TIMEOUT	= 1;
	SALNET_NOMBUF	= 2;
	SALNET_BADREPLY	= 3;
	SALNET_BADARGS	= 4;
	SALNET_NOTMINE	= 5;
	SALNET_NFSERROR	= 6;

PROCEDURE GetLocalEther((*OUT*) VAR ether: EtherAddr);
PROCEDURE GetLocalIp(): IpAddr;
PROCEDURE SetLocalIp(ip: IpAddr);

PROCEDURE BootServer(): IpAddr;

PROCEDURE Arp(ip: IpAddr; (*OUT*) VAR ether: EtherAddr) RAISES {Error};
PROCEDURE Bootp((*OUT*) VAR localip: IpAddr; VAR serverip: IpAddr;
		VAR servername: BootpServer; VAR bootfile: BootpFile)
		RAISES {Error};
PROCEDURE DnsQuery(hostname: TEXT; (*OUT*) VAR ip: IpAddr) RAISES {Error};

PROCEDURE TftpFetch(server: IpAddr; filename: TEXT;
		(*OUT*) VAR buf: ARRAY OF CHAR; VAR  len: Word.T)
		RAISES{Error, Errno.E};

PROCEDURE NfsMount(server: IpAddr; dir: TEXT;
		(*OUT*) VAR mount: NfsFileHandle) RAISES{Error,Errno.E};
PROCEDURE NfsFetch(server: IpAddr; VAR mount: NfsFileHandle; filename: TEXT;
		(*OUT*) VAR  buf: ARRAY OF CHAR; VAR  len: Word.T)
		RAISES{Error, Errno.E};

PROCEDURE FmtError(err: INTEGER): TEXT ;

END Salnet.
