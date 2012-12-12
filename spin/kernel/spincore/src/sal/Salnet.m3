UNSAFE (* to import SalnetExtern *)
MODULE Salnet;

IMPORT Word;
IMPORT SalnetExtern;
IMPORT Errno, ErrnoDep, Fmt;


PROCEDURE GetLocalEther(VAR ether: EtherAddr)=
  BEGIN
    SalnetExtern.salnet_getetheraddr(ether);
  END GetLocalEther;

PROCEDURE GetLocalIp(): IpAddr =
  BEGIN
    RETURN SalnetExtern.salnet_getipaddr();
  END GetLocalIp;

PROCEDURE BootServer(): IpAddr =
  BEGIN
    RETURN SalnetExtern.salnet_getbootserver();
  END BootServer;

PROCEDURE SetLocalIp(ip: IpAddr)=
  BEGIN
    SalnetExtern.salnet_setipaddr(ip);
  END SetLocalIp;

PROCEDURE Arp(ip: IpAddr; VAR ether: EtherAddr) RAISES {Error}=
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.salnet_arp(ip, ether);
    IF err#SALNET_SUCCESS THEN
      RAISE Error(err);
    END;
  END Arp;

PROCEDURE Bootp(VAR localip: IpAddr; VAR serverip: IpAddr;
		VAR servername: BootpServer; VAR bootfile: BootpFile)
		RAISES {Error}=
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.salnet_bootp(localip, serverip, servername, bootfile);
    IF err#SALNET_SUCCESS THEN
      RAISE Error(err);
    END;
  END Bootp;

PROCEDURE DnsQuery(hostname: TEXT; VAR ip: IpAddr) RAISES {Error} =
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.m3dnsquery(hostname, ip);
    IF err#SALNET_SUCCESS THEN
      RAISE Error(err);
    END;
  END DnsQuery;

PROCEDURE TftpFetch(server: IpAddr; filename: TEXT;
		VAR buf: ARRAY OF CHAR; VAR  len: Word.T)
		RAISES{Error, Errno.E} =
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.m3tftpfetch(server, filename, buf, len);
    (* translate tftp errcodes in arpa/tftp.h to errnos *)
    CASE err OF
    | 0 => RETURN
    | 1 => RAISE Errno.E(ErrnoDep.ENOENT)
    | 2 => RAISE Errno.E(ErrnoDep.EACCES)
    | 3 => RAISE Errno.E(ErrnoDep.EFBIG)
    | 4 => RAISE Error(SALNET_BADREPLY)
    | 5 => RAISE Error(SALNET_BADREPLY)
    | 6 => RAISE Errno.E(ErrnoDep.EEXIST)
    | 7 => RAISE Error(SALNET_BADREPLY)
    ELSE RAISE Errno.E(ErrnoDep.EIO)
    END
  END TftpFetch;

PROCEDURE NfsFetch(server: IpAddr; VAR mount: NfsFileHandle; filename: TEXT;
		VAR buf: ARRAY OF CHAR; VAR len: Word.T)
		RAISES{Error, Errno.E} =
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.m3nfsfetch(server, mount, filename, buf, len);
    IF err=SALNET_NFSERROR THEN
      WITH nfserror = VIEW(SUBARRAY(buf,0,4), INTEGER) DO
        RAISE Errno.E(nfserror);
      END;
    ELSIF err#SALNET_SUCCESS THEN
      RAISE Error(err);
    END;
  END NfsFetch;

PROCEDURE NfsMount(server: IpAddr; dir: TEXT;
		(*OUT*) VAR mount: NfsFileHandle) RAISES{Error,
		Errno.E}=
  VAR
    err :INTEGER;
  BEGIN
    err := SalnetExtern.m3nfsmount(server,dir,mount);
    IF err=SALNET_NFSERROR THEN
      WITH nfserror = VIEW(SUBARRAY(mount,0,4), INTEGER) DO
        RAISE Errno.E(nfserror);
      END;
    ELSIF err#SALNET_SUCCESS THEN
      RAISE Error(err);
    END;
  END NfsMount;

CONST ErrMsg = ARRAY[0..6] OF TEXT {
	"Success", "TimeOut", "NoMbuf", "BadReply", "BadArgs",
	"NotMine", "NfsError"
	};

PROCEDURE FmtError(err: INTEGER): TEXT =
  BEGIN
    CASE err OF
    | 0..6 => RETURN ErrMsg[err];
    ELSE
      RETURN "Salnet Error " & Fmt.Int(err);
    END
  END FmtError;

BEGIN
END Salnet.
