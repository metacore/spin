(*
 * Copyright 1996, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *)
(*
	Fetch.m3

 	Loads the initial extensions via tftp or http or nfs.
 *)
(*
 * HISTORY
 * 13-Jun-97  David Becker at the University of Washington
 *      Speed up tftp and nfs with globalBuf to avoid garbage collection.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Added salnfs option
 *
 * 11-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	SimpleHttp.SetServer() is now private.
 *
 * 20-Mar-97  Tsutomu Owa (owa) at University of Washington
 *	Moved from ALPHA_SPIN as stcp now works on both PC and alpha.
 *)

MODULE Fetch; 

IMPORT Text, IO;
IMPORT Salnet, SimpleHttp;
IMPORT Errno;
IMPORT BuildInfo, NetText;

CONST
  MaxBytes = 1024 * 1024;
TYPE
  Transport = {tftp, http, nfs};
VAR
  fetcher := Transport.http;
  (* if error count goes to 0, use tftp anyway *)
  errorCount : CARDINAL := 5;
  server : Salnet.IpAddr;
  mount : Salnet.NfsFileHandle;
  mount_point : TEXT;
  mount_pad : TEXT;

VAR
  globalBuf := NEW(REF ARRAY OF CHAR, MaxBytes);

PROCEDURE FetchTftp(path:TEXT; VAR buf:REF ARRAY OF CHAR): INTEGER =
  VAR
    bytes : INTEGER;
  BEGIN
    IF fetcher#Transport.tftp THEN IO.Put("tftp fetching " & path & "\n"); END;
    Salnet.TftpFetch(Salnet.BootServer(), path, globalBuf^, bytes);
    buf := NEW(REF ARRAY OF CHAR, bytes);
    buf^ := SUBARRAY(globalBuf^, 0, bytes);
    RETURN bytes;
  END FetchTftp;
  
PROCEDURE FetchHttp(path:TEXT; VAR buf:REF ARRAY OF CHAR): INTEGER 
	RAISES {SimpleHttp.Error} =
  BEGIN
    buf := SimpleHttp.Get(path , server);
    (*
    IO.Put("http " & path & " returns " & Fmt.Int(NUMBER(buf^)) & "bytes\n");
    *)
    RETURN NUMBER(buf^);
  END FetchHttp;

PROCEDURE Fetch(path:TEXT; VAR buf:REF ARRAY OF CHAR): INTEGER =
  VAR
    bytes : INTEGER;
  BEGIN
    CASE fetcher OF
    | Transport.tftp =>
      RETURN FetchTftp(mount_point &"/" &mount_pad &path, buf);
    | Transport.http =>
      TRY 
	RETURN FetchHttp(mount_point & "/" &mount_pad &path, buf);
      EXCEPT
	| SimpleHttp.Error(msg) =>
	  IO.Put("http error: " & path & ": " & msg & "\n");
	  DEC(errorCount);
	  IF errorCount = 0 THEN
	    IO.Put("Init: too many http erros.  switching to tftp\n");
            fetcher := Transport.tftp;
	  END;
	  RETURN FetchTftp(path, buf);
      END;
    | Transport.nfs =>
      TRY
        Salnet.NfsFetch(Salnet.BootServer(), mount,
		mount_pad & path, globalBuf^, bytes);
        buf := NEW(REF ARRAY OF CHAR, bytes);
	buf^ := SUBARRAY(globalBuf^, 0, bytes);
      EXCEPT
      | Salnet.Error(err) =>
        IO.Put("Init.Script: " &path &" - " &Salnet.FmtError(err) &"\n");
        bytes:=0;
      | Errno.E(err) =>
        IO.Put("Init.Script: " &path &" - " &Errno.Fmt(err) &"\n");
        bytes:=0;
      END;
      RETURN bytes;
    END;
  END Fetch;

PROCEDURE Config(READONLY line : TEXT) =
  VAR
    cmd: TEXT;
    curPos, first, last : CARDINAL;
  BEGIN
     IF globalBuf=NIL THEN
       globalBuf := NEW(REF ARRAY OF CHAR, MaxBytes);
     END;

    cmd := "boot tftp";
    IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
      IO.Put("boot via tftp\n");
      fetcher := Transport.tftp;
    END;

    cmd := "boot nfs";
    IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
      IO.Put("boot via nfs\n");
      fetcher := Transport.nfs;
      TRY
        Salnet.NfsMount(Salnet.BootServer(), mount_point, mount);
      EXCEPT
      | Salnet.Error(err) =>
        IO.Put("Fetch.Config: nfsmoun " &mount_point &" - " &Salnet.FmtError(err) &"\n");
        RETURN;
      | Errno.E(err) =>
        IO.Put("Fetch.Config: nfsmount "&mount_point &" - " &Errno.Fmt(err) &"\n");
        RETURN;
      END;
    END;

    cmd := "boot http";
    IF Text.Equal(cmd,Text.Sub(line,0,Text.Length(cmd))) THEN
      fetcher := Transport.http;
      IO.Put("boot via stcp\n");
      curPos := Text.Length(cmd);
      WHILE (curPos <= Text.Length(line)-1) AND
	    (Text.GetChar(line, curPos) = ' ') DO
	INC(curPos);
      END;
      first := curPos;
      WHILE (curPos <= Text.Length(line)-1) AND
	    (Text.GetChar(line, curPos) # ' ') DO
	INC(curPos);
      END;
      IF curPos > Text.Length(line) THEN curPos := Text.Length(line)-1; END;
      last := curPos;
      IF first < last THEN
       (* XXX.  Now we need to specify a server AT Get().
	* SimpleHttp.SetServer(Text.Sub(line, first, last - first));
	*)
	server := NetText.TextToIp( Text.Sub(line, first, last - first) );
      END;
    END;
  END Config;

PROCEDURE Init() =
  VAR
    fetch_method := BuildInfo.GetFetchMethod();
  BEGIN 
    server := NetText.TextToIp( BuildInfo.GetHttpServAddr() );
    mount_point := BuildInfo.GetMountPoint();
    mount_pad := BuildInfo.GetMountPad();

    IF Text.Equal(fetch_method, "nfs") THEN
      fetcher := Transport.nfs;
    ELSIF Text.Equal(fetch_method, "http") THEN
      fetcher := Transport.http;
    ELSIF Text.Equal(fetch_method, "tftp") THEN
      fetcher := Transport.http;
    ELSE
      fetcher := Transport.nfs;
    END;

       globalBuf := NEW(REF ARRAY OF CHAR, MaxBytes);
    IF fetcher = Transport.nfs THEN

       Salnet.NfsMount(Salnet.BootServer(), mount_point, mount);
    END;

  END Init;
    
PROCEDURE FreeMemory() =
  BEGIN
    SimpleHttp.FreeMemory();
  END FreeMemory;

BEGIN
END Fetch. 
