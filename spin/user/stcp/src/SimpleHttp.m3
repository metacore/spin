(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 *HISTORY
 * 26-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Use Salnet.Arp() instead of Stcp's Arp module to get ether address.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 * 16-Jun-97  Tsutomu Owa (owa) at the University of Washington
 *	Added data size check by comparing "Content-length" field in
 *	http header and buffer size obtained by StcpTcpPacket.Recv().
 *
 * 10-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Added a Semaphore to prevent multiple clients call SimpleHttp at
 *	the same time.  Changed numRetry from 2 to 5.
 *
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Change interface names so that these don't conflict w/
 *	those used by user/net.
 *
 * 03-Feb-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

MODULE SimpleHttp;

IMPORT StcpTcpPacket, StcpIpPktFormat;
(*
IMPORT DefaultAddr, StcpEtherArp;
 *)
IMPORT Sema, IO, Fmt, Text;
IMPORT Debugger; <* NOWARN *>
IMPORT StcpEtherPacket;
IMPORT Dispatcher;
IMPORT BuildInfo, Salnet, NetText;

(* XXX hardcoded IP address list.  replace this with DNS?  *)
(*
TYPE HttpdInfo = RECORD
  server : Salnet.IpAddr;
  ipAddress : StcpIpPktFormat.AddressArray;
  port : CARDINAL;
  httpdType : [0..1];           (* 0 -- Netscape Commerce. 1 -- SPIN httpd *)
END;
*)

EXCEPTION
  SizeMismatch(TEXT);		(* Content-Length and NUMBER(buf) differ *)
  InvalidBuffer(TEXT);		(* Recv() returns NIL.  This happens when
				   www-spin hits unhandled exceptions *)

VAR
(*  myHttpdInfo : ARRAY [0..10] OF HttpdInfo; *)
  mySema : Sema.T;
  debug := FALSE;

(*
PROCEDURE SetHttpdInfo() =
  BEGIN
    myHttpdInfo := ARRAY OF HttpdInfo{
      HttpdInfo{"velvet", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(241, CHAR)}, 8080, 0},
      HttpdInfo{"www-spin", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(222, CHAR)}, 80, 1},
      HttpdInfo{"alpo", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(18, CHAR)}, 80, 1},
      HttpdInfo{"bell", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(199, CHAR)}, 80, 1},
      HttpdInfo{"bugs", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(10, CHAR)}, 80, 1},
      HttpdInfo{"calico", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(233, CHAR)}, 80, 1},
      HttpdInfo{"elmer", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(134, CHAR)}, 80, 1},
      HttpdInfo{"hozhed", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(6, CHAR)}, 80, 1},
      HttpdInfo{"spandex", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(237, CHAR)}, 80, 1},
      HttpdInfo{"spincycle", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(92, CHAR)}, 80, 1},
      HttpdInfo{"spinoff", 
      StcpIpPktFormat.AddressArray{
	VAL(128, CHAR), VAL(95, CHAR), VAL(2, CHAR), VAL(94, CHAR)}, 80, 1}
    }
  END SetHttpdInfo;
*)

VAR curServer : StcpIpPktFormat.AddressArray;	

(*
PROCEDURE SetServer(server : TEXT) =
  BEGIN 

    (* Sema.P(mySema); *)

    (* see if the current server is the "server" passed. *)
    IF Text.Equal(server, myHttpdInfo[curServer].server) THEN
     (*
      * IO.Put("Current server is " & server & ".  Continue to use it\n");
      *)
      (* Sema.V(mySema); *)
      RETURN;
    ELSE
      (* search "server" in myHttpdInfo *)
      FOR i := 0 TO NUMBER(myHttpdInfo)-1 DO
        IF Text.Equal(server, myHttpdInfo[i].server) THEN
	  curServer := i;
	  (* XXX.  Need to fix ether Salnet.DnsQuery() on ALPHAs or
		   keeping ip address hardcoded.
	  DefaultAddr.targetIpAddress := myHttpdInfo[i].ipAddress;
	  *)
	  DefaultAddr.defaultHttpdPort := myHttpdInfo[i].port;
	  DefaultAddr.SetTarget(server);
	  IO.Put("SimpleHttp: sever " & server & " port " & 
		  Fmt.Int(myHttpdInfo[i].port) & "\n");
          (* Sema.V(mySema); *)
	  RETURN;
        END;
      END;
    END;

    IO.Put("SimpleHttp: Unknown server (" & server & ")");
    IO.Put("  still using " & myHttpdInfo[curServer].server & "\n");
    (* Sema.V(mySema); *)
  END SetServer;
*)

(* error messages returned from servers appear to vary... *)
(* e.g. www-spin returns
	"HTTP/1.0 404 Not Found"
	"MIME-version: 1.0"
	"Content-type: text/html"
	""					<--- '\n' = 16_0a
	"<HTML><HEAD><TITLE>2</TITLE></HEAD>"
	"<BODY><H1>404 The requested item was ...",
   while www.cs.washington.edu returns
	"HTTP/1.0 200 OK"
	"Server: Netscape-Commerce/1.12"
	"Date: Thursday, 06-Feb-97 23:13:16 GMT"
	"Last-modified: Saturday, 11-Jan-97 00:29:50 GMT"
	"Content-length: 4392"
	"Content-type: text/plain"
	""					<-- 16_0d,16_0a
  	"<HEAD><TITLE>404 Not Found</TITLE></HEAD>"
  	"<BODY><H2>404 Not Found</H2>"
  	...
   hmm... how can I check those errors?

   Also note that www-spin returns "HTTP/1.0 .. " header and white line
   then body while www.cs.washington.edu returns its body w/o header.
   assume that we'are connecting only to www-spin.
*)

(* On successful return. the Netscape-Comerce server returns
  HTTP/1.0 200 OK
  Server: Netscape-Commerce/1.12
  Date: Monday, 16-Jun-97 16:21:15 GMT
  Last-modified: Saturday, 07-Jun-97 02:26:40 GMT
  Content-length: 379
  Content-type: text/plain
*)

CONST
  (* define http header field name.  see TCP/IP Illustrated, volume 3. pp166. *)

  HTTPHdr = ARRAY OF CHAR {'H', 'T', 'T', 'P', '/', '1', '.', '0', ' '};
  HTTPHdrLen = NUMBER(HTTPHdr);
  ContentLenghtHdr = ARRAY OF CHAR {'C', 'o', 'n', 't', 'e', 'n', 't', '-', 
					'l', 'e', 'n', 'g', 't', 'h', ':', ' '};
  ContentLenghtHdrLen = NUMBER(ContentLenghtHdr);

  (* Netscape does not return Content-length field when url ends with '/' *)
  DirEntry = ARRAY OF CHAR{'<', 'T', 'I', 'T', 'L', 'E', '>',
			   'I', 'n', 'd', 'e', 'x', ' ', 'o', 'f'};
  DirEntryLen = NUMBER(DirEntry);

(*
 * Strip off the header placed by the remote http server.
 *)
PROCEDURE RemoveHttpHdr(VAR buf : REF ARRAY OF CHAR) : CARDINAL
	RAISES {SizeMismatch, InvalidBuffer} =
  PROCEDURE ATOI(code : ARRAY OF CHAR) : CARDINAL =
    VAR len, val : CARDINAL;
    BEGIN
      (* sanity check *)
      len := NUMBER(code);
      IF len = 0 THEN RETURN 0; END;

      val := 0;
      FOR i:=0 TO len-1 DO			(* XXX ought to see if *)
	val := 10 * val + ORD(code[i])-16_30;	(* it's numeric!!!. *)
      END;
      RETURN val;
    END ATOI;
  VAR
    totallen : CARDINAL;
    oldBuf : REF ARRAY OF CHAR;
    start, end, fieldLen, dataLen, retCode: CARDINAL;
    field : ARRAY [0..127] OF CHAR;	  (* XXX assume each field < 128 *)
    typeKnown : BOOLEAN := FALSE;
    serverType : [0..1] := 1;		  (* 1 - CR+LF, 0 - CR *)
  BEGIN
    (* Search for "HTTP1/0 " and "Content-length: ", get the return code
     * and file length from http header fields.
     * XXX.  what!? www-spin does not return Content-length field!!!
     *)

    IF debug THEN Debugger.Enter(); END;
    totallen := NUMBER(buf^);
    start := 0;
    end := 0;

    (* sanity check *)
    IF totallen = 0 THEN
      RAISE InvalidBuffer("http error: Server returns Null buffer\n");
    END;

    WHILE end < totallen DO
      IF (buf^[end] # VAL(16_0d, CHAR)) AND (buf^[end] # VAL(16_0a, CHAR)) THEN
	INC(end);			(* advance pointer *)
      ELSE
	fieldLen := end-start;
	IF fieldLen = 0 THEN
	  (* we reached the end of http header. update end ptr and exit. *)
	  INC(end, 1+serverType);
	  EXIT;
	END;
	IF fieldLen >= NUMBER(field) THEN
          IO.Put("http: error.  header field length too long\n");
	  fieldLen := NUMBER(field);
	END;
	SUBARRAY(field, 0, fieldLen) := SUBARRAY(buf^, start, fieldLen);
        IF debug THEN
	  IO.Put(Text.FromChars(SUBARRAY(field, 0, fieldLen)) & "\n"); 
	END;

        (* XXX Is there any faster way to do this ? *)
        IF fieldLen >= ContentLenghtHdrLen AND
	  SUBARRAY(field, 0, ContentLenghtHdrLen) = ContentLenghtHdr THEN
	  dataLen := ATOI(SUBARRAY(field, ContentLenghtHdrLen,
					  fieldLen-ContentLenghtHdrLen));
	  IF debug THEN IO.Put("Got length " & Fmt.Int(dataLen)); END;
	ELSIF fieldLen >= HTTPHdrLen AND
	      SUBARRAY(field, 0, HTTPHdrLen) = HTTPHdr THEN
	  retCode := ATOI(SUBARRAY(field, HTTPHdrLen, 3));
	END;
	(* assume http header should contain at least two lines.
	 * So that we can determine server type.
	 *)
	IF NOT typeKnown THEN
	  IF end+1 < totallen AND buf^[end+1] = VAL(16_0a, CHAR) THEN 
	    serverType := 1;		(* Netscape Commerse server CR+LF *)
	  ELSE
	    serverType := 0;		(* Spin CR *)
	  END;
	  typeKnown := TRUE;
	END;
	INC(end, 1+serverType);		(* skip '\r' (and '\n') *)
        start := end;			(* update start pointer *)
      END;
    END;

    (* see if we're fetching from commerce server or our own *)
    (* XXX following code is redundant as I got the file size.
     *     Use 'dataLen' and 'end' which is the ptr to the data in future.
     *)
      IF serverType = 0 THEN
      (* the server is www-spin; httpd on SPIN *)
      FOR i := 2 TO totallen DO
        IF buf^[i-2] = '\n' AND buf^[i-1] = '\n' THEN
          IF i # end THEN IO.Put("Hmm...\n"); END; (* for debug *)
	  oldBuf := buf;
	  buf := NEW(REF ARRAY OF CHAR, totallen - i);
          buf^ := SUBARRAY(oldBuf^, i, totallen - i);
          EXIT;
        END;
      END;
    ELSE
      (* the server is velvet; Netscape Commerce Server *)
      FOR i := 4 TO totallen DO
        IF buf^[i-4] = VAL(16_0d, CHAR) AND buf^[i-3] = VAL(16_0a, CHAR) AND
	   buf^[i-2] = VAL(16_0d, CHAR) AND buf^[i-1] =  VAL(16_0a, CHAR) THEN
          IF i # end THEN IO.Put("Hmm...\n"); END; (* for debug *)
	  oldBuf := buf;
	  buf := NEW(REF ARRAY OF CHAR, totallen - i);
          buf^ := SUBARRAY(oldBuf^, i, totallen - i);
          EXIT;
        END;
      END;
    END;

    (* XXX ugly.  www-spin does not return Content-length field.
     * we can't do 'dataLen # NUMBER(buf^)'
     *)
    IF retCode = 200 AND serverType = 1 AND dataLen # NUMBER(buf^) THEN
      (* XXX hack.  Netscape does not return Content-length field!!!
       * Not sure this is enough to check the contents is directory index.
       *)
      IF NUMBER(buf^) > DirEntryLen THEN 
	IF SUBARRAY(buf^, 0, DirEntryLen) = DirEntry THEN
	  RETURN retCode;
	END;
      END;
      (* Debugger.Enter(); *)
      RAISE SizeMismatch("http error: Wrong data size\n" &
		"  header says " & Fmt.Int(dataLen) & 
		" but buffer size is " & Fmt.Int(NUMBER(buf^)) & "\n");
    END;
    RETURN retCode;
  END RemoveHttpHdr;

CONST numOfRetry = 5;
CONST authText = " HTTP/1.0\nAuthorization: Basic bWVmOmYwcmczdA==\n\n";

(* url should not contain "http://servername" prefix.
   it should be just a path name starting with "/spin/" *)

PROCEDURE GetRaw(request : TEXT) : REF ARRAY OF CHAR
		RAISES {StcpTcpPacket.NoPortAvailable,
			StcpTcpPacket.NoResponseFromServer} =
  VAR
    buf : REF ARRAY OF CHAR;
  BEGIN
    TRY
     (*
      IO.Put(request);
      Debugger.Enter();
      *)
      StcpTcpPacket.Connect();
  
      (* requests server to send the file *)
      StcpTcpPacket.Send(request);

      (* block until server sends whole data and FIN *)
      StcpTcpPacket.Recv(buf);
  
      (* block until server replies to the last FIN *)
      StcpTcpPacket.Close();
    EXCEPT
    END;
    RETURN buf;
  END GetRaw;

PROCEDURE Get(url : TEXT ;<*UNUSED*>server : Salnet.IpAddr) :
	REF ARRAY OF CHAR RAISES {Error} =
  PROCEDURE PrintURL() =
    BEGIN
      WITH len = Text.Length(url)-Text.Length(authText)-4,
	   filename = Text.Sub(url, 4, len) DO
        IO.Put("file: " & filename & "\n");
      END;
    END PrintURL;
  VAR
    buf : REF ARRAY OF CHAR;
    retCode : CARDINAL;
  BEGIN
    (* sanity check *)
    IF (url = NIL) OR (Text.Length(url) = 0) THEN
      RAISE Error("Not sending NIL request\n");
    END;

    Sema.P(mySema);
    (* 
       SetServer(server);

       IF myHttpdInfo[curServer].httpdType = 1 THEN
         (* httpd server on SPIN.  Remove first "/spin".  *)
         url := Text.Sub(url, 5, Text.Length(url)-5);
       END;
    *)

    (* add "GET " command and passwd. *)
    url := Text.CatMultiple(ARRAY OF TEXT {"GET ", url, authText});
    FOR i:= 1 TO numOfRetry DO
      TRY
        buf := GetRaw(url);
  
        (* remove http header from the received data *)
        retCode := RemoveHttpHdr(buf);
        CASE retCode OF
          |   0 => Sema.V(mySema); RETURN buf;
	  | 200 => Sema.V(mySema); RETURN buf;
	  | 301 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("301 Requested resource has been assigned a new permanent URL.");
	  | 302 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("302 Requested resource resides temporarily under a different URL.");
          | 400 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("400 Bad Request");
          | 500 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("500 Bad Request");
          | 403 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("403 Forbidden");
	  | 404 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("404 Not Found");
  (* (* why is 500 used twice? *)       
	  | 500 => Sema.V(mySema); RAISE Error("500 Internal Error");  
   *)
	  | 501 => Sema.V(mySema);
		   PrintURL();
		   RAISE Error("501 Not Implemented");
  	ELSE
  	  (* unsuccessfull.*)
	  IO.Put("Unknown error( " & Fmt.Int(retCode) & ")\n");
	  Sema.V(mySema);
	  PrintURL();
	  RAISE Error("Unkown error");
        END;
      EXCEPT
      | SizeMismatch(msg) =>
	WITH len = Text.Length(url)-Text.Length(authText)-4,
	     filename = Text.Sub(url, 4, len) DO
	  IO.Put(msg & "Size mismatch. rerequesting " & filename & " \n");
	END;
      | InvalidBuffer(msg) =>
	WITH len = Text.Length(url)-Text.Length(authText)-4,
	     filename = Text.Sub(url, 4, len) DO
	  IO.Put(msg & "Size mismatch. rerequesting " & filename & " \n");
	END;
      | StcpTcpPacket.NoPortAvailable(msg) =>
	WITH len = Text.Length(url)-Text.Length(authText)-4,
	     filename = Text.Sub(url, 4, len) DO
	  IO.Put("No port available. rerequesting " & filename & " \n");
	END;
      | StcpTcpPacket.NoResponseFromServer(msg) =>
	WITH len = Text.Length(url)-Text.Length(authText)-4,
	     filename = Text.Sub(url , 4, len) DO
          IO.Put("No Response from server. rerequesting " & filename & " \n");
	END;
      END;
    END;
    Sema.V(mySema);
    RAISE Error("502 http/tcp Internal error");
  END Get;

PROCEDURE FreeMemory() =
  BEGIN
    Sema.P(mySema);
    StcpTcpPacket.FreeMemory();
    Sema.V(mySema);
  END FreeMemory;

PROCEDURE Init() =
  VAR 
    tmpServer := NetText.TextToIp( BuildInfo.GetHttpServAddr() );
  BEGIN
    mySema := Sema.Alloc(1);
(*    
      SetHttpdInfo();
      SetServer("velvet");
*)
    curServer := VIEW( tmpServer, StcpIpPktFormat.AddressArray );
  END Init;

PROCEDURE Uninit () =
  BEGIN
    Dispatcher.Uninstall(StcpEtherPacket.etherhandler);
    StcpEtherPacket.etherhandler := NIL;
  END Uninit;
  
  
BEGIN
END SimpleHttp.
