(*
 *
 * Copyright 1994, 1995, 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Changed over to new security manager
 *
 * 13-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	remove EXPORT of Main
 *
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext
 *
 * 22-Nov-96  becker at the University of Washington
 *	Change to TrackStrand from Spy
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Replaced integer 0 with SocketRep.SIN_ZERO.
 *
 * 24-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Check if the recvdata field is NIL after Socket.Recv.  
 *
 * 17-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	The server called Socket.Recv was called with the non-blocking
 *	option. This option is now supported by Socket.Recv and the
 *	server was not prepared to handle an EWOULDBLOCK exception.
 *
 * 07-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed parserequest not to use TextRd and Lex.
 *
 * 03-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Cache the header along with the file to avoid recomputation and
 *      allocation.
 *	Place the file into the cache only if it is all successfully sent.
 *
 * 01-May-96  Emin Gun Sirer (egs) at the University of Washington
 *	Chain long blocks together if they are in memory before
 *	calling on the networking code.
 *
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 29-Feb-96  Brian Bershad (bershad) at the University of Washington
 *	Use new Auth interface.
 *
 * 23-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Moved all StrongRef operations to urt.
 *
 * 21-Feb-96  Przemek Pardyak (pardy) at the University of Washington
 *	Added verbose flag that controls whether messages about requests
 *	are printed to the output or not.
 *
 * 13-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Changed parserequest to handle bad requests.
 *
 * 03-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added swapping and pageout onto an extent based storage device.
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Got rid of SPL calls around Socket calls.
 *
 * 08-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Updated to use new file system interface.
 *
 * 03-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Updated to use new Socket interface.  
 *	
 * 02-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Add logging of referrers.
 *	Changed to use text readers instead of hand rolled code.
 *	Generalized the shell interface.
 *
 * 23-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Handle IO through Wr interface.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added return statement to Httpd() to avoid protection fault
 *	after an exception.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. HTTP server.
 *
 *)
MODULE Httpd;
IMPORT HttpdInterface, Mime;
IMPORT Auth, NameServer, SecurityError, SecurityManager;
IMPORT ThreadExtra, Thread, File, FileStat, Error, FileSystem;
IMPORT Mbuf, MbufPublic, Buffer, FileCache, BlockList, BufferDesc;
IMPORT Socket, SocketRep, SocketAddr, SocketAddrIn;
IMPORT Text, Fmt, Spy, TrackStrand, Wr, TextWr;
IMPORT IO, Shell;
IMPORT IpPktFormat;

(* IMPORT SAL; to get rpcc/timestamp *)
IMPORT Ctypes, OsfNet;

IMPORT Errno, Net;

(* XXX mef new tcp *)
IMPORT TcpCtrlBlock, M3Tcp;

IMPORT Httpd;

CONST
  BigBlockSize     = 8000;
  SmallBlockSize   = 1024;
  Kilo             = 1024;
  Meg              = 1024 * 1024;
  MaxListenLog     = 1;
  ConsistencyCheck = TRUE;
  UseMbufChains    = TRUE; (* chaining mbufs stresses net code *)

TYPE
  FileHandle = File.T;

  Exception = {BadRequest, Forbidden, NotFound, Internal, NotImplemented};

VAR
  acceptTimer: Spy.T;
  closeTimer: Spy.T;
  dataflow  : Spy.T;

CONST
  IndexName = "index.html";

CONST
   Header = ARRAY OF CHAR {
   'H','T','T','P','/','1','.','0',' ','2','0','0',' ','O','K','\n',
   'M','I','M','E','-','V','e','r','s','i','o','n',':',' ','1','.','0','\n',
   'S','e','r','v','e','r',':',' ','S','P','I','N','\n',
   'C','o','n','t','e','n','t','-','T','y','p','e',':',' '};

CONST
  Errstr = ARRAY Exception OF TEXT {
      "500 Bad Request",
      "403 Forbidden",
      "404 Not Found",
      "500 Internal Error",
      "501 Not Implemented" 
  };

  Errmessage = ARRAY Exception OF TEXT {
      "500 Bad Request",
      "403 Your client is not allowed to request this item",
      "404 The requested item was not found on this server",
      "500 An error occurred while trying to retrieve item",
      "501 This server does not support the given request type"
  };
  MaxRequestLen = 1024; (* we examine the first MaxReqLen bytes of request *)

(* ******************************************************************** *)
(* FileSystem related routines                                          *)
(* ******************************************************************** *)

(*
 * Open the file.
 *)
PROCEDURE OpenFile(filename: TEXT) : FileHandle RAISES {FileNotFound} =
  BEGIN
    (*
     * Open the file to see if it exists.
     *)
    TRY
      RETURN FileSystem.Open((* XXX 0, taken out by mef *) "/mnt" & filename);
    EXCEPT
    | Error.E(e) => 
      RAISE FileNotFound(filename & ": " & e.message());
    END;
  END OpenFile;

(*
 * Get a buffer from the disk containing the data
 *)
PROCEDURE ReadFile(fp: FileHandle;
                   offset: CARDINAL;
                   size: CARDINAL;
                   VAR data: REF ARRAY OF CHAR) : CARDINAL RAISES {Error.E} =
BEGIN
  (*
   * Read it in.
   *)
  RETURN fp.read(SUBARRAY(data^, 0, size), offset);
END ReadFile;

(*
 * Close the file.
 *)
PROCEDURE CloseFile(fp: FileHandle) RAISES {Error.E} =
  BEGIN
    fp.close();
  END CloseFile;

(* ******************************************************************** *)
(* URL and HTTP related routines                                        *)
(* ******************************************************************** *)

(*
 * Given a client request, fills in the request structure
 * describing what it is that the client wants.
 *)
PROCEDURE ParseRequest(READONLY req: ARRAY OF CHAR; VAR request: Request) 
  RAISES {BadRequest,
          NotDoneYet} =
  CONST
    MinReqLen = 4;
  VAR
    end: CARDINAL;
  BEGIN
    IF NUMBER(req) < MinReqLen THEN
      RAISE BadRequest("Short request");
    END;
    IF req[0] # 'G' OR req[1] # 'E' OR req[2] # 'T' OR req[3] # ' ' THEN
      RAISE NotDoneYet("non-GET methods not implemented.");
    END;
    end := 4;
    WHILE end < NUMBER(req) DO
      IF req[end] = ' ' OR req[end] = '\n' THEN
        EXIT;
      END;
      INC(end);
    END;
    WITH reqarray = SUBARRAY(req, 4, end - 4) DO
      request.url := Text.FromChars(reqarray);
      (* Append index.html if url ends in slash *)
      IF req[end-1] = '/' THEN 
        request.url := request.url & IndexName;
        request.urltype := "text/html";
      ELSE
        request.urltype := Mime.FindMimeType(reqarray, end - 4);
      END;
    END;
     
 (*
  * This code is a lot nicer, and it finds the referer, but it uses
  * readers and does too much allocation.
   VAR
      str: TEXT;
      trd: TextRd.T;
    BEGIN
  BEGIN
    trd := TextRd.New(reqtext);
    TRY
      (* See if request type is valid *)
      Lex.Match(trd, "GET");
      req.op := HttpCmd.Get;
      Lex.Skip(trd);
    EXCEPT
    ELSE
      RAISE BadRequest("Cannot parse request");
   END;
   TRY
      (* Get the url *)
      req.url := Lex.Scan(trd);
      (* Append index.html if url ends in slash *)
      IF Text.GetChar(req.url, Text.Length(req.url) - 1) = '/' THEN
        req.url := req.url & IndexName;
      END;
      req.urltype := Mime.FindMimeType(req.url);
      Lex.Skip(trd);
      (* See if protocol is valid; we only support 1.0 *)
      Lex.Match(trd, "HTTP/1.0");
      WHILE NOT Rd.EOF(trd) DO
        str := Lex.Scan(trd);
        Lex.Skip(trd);
        IF Text.Equal(str, "Referer:") THEN
          req.referer := Lex.Scan(trd);
          EXIT;
        END;
      END;
      Rd.Close(trd);
    EXCEPT
      Lex.Error =>
      RAISE NotDoneYet("Badly parsed request for url: " & req.url);
    ELSE
      RAISE InternalFault(req.url);
    END;
 *)
  END ParseRequest;

PROCEDURE GetFileCache() : FileCache.T =
  BEGIN
    RETURN filecache;
  END GetFileCache;

(*
 * Function that is called by the device when a buffer being sent
 * can be reused by the application.
 *)
PROCEDURE FreeBuffer(<*UNUSED*>buffer: REF ARRAY OF CHAR;
                     <*UNUSED*>len: CARDINAL;
                     arg: REFANY) =
  VAR
    bd: BufferDesc.T;
  BEGIN
    bd := NARROW(arg, BufferDesc.T);
    Buffer.Deallocate(bd.buffer);
    BufferDesc.Deallocate(bd);
  END FreeBuffer;

PROCEDURE CacheBuffer(buffer: REF ARRAY OF CHAR;
                      len: CARDINAL;
                      arg: REFANY) =
  VAR
    bd: BufferDesc.T;
  BEGIN
    bd := NARROW(arg, BufferDesc.T);
    IF ConsistencyCheck THEN
      IF bd.buffer.data # buffer THEN
        IO.Put("CacheBuffer buffer arg doesn't match bd.buffer!\n");
      END;
      IF NUMBER(bd.buffer.data^) < len THEN
        IO.Put("CacheBuffer len arg doesn't match bd.buffer length!\n");
      END;
    END;
    bd.blocklist.addBlock(bd.blockno, bd.buffer, len);
    BufferDesc.Deallocate(bd);
  END CacheBuffer; 

PROCEDURE CacheUnlockBuffer(buffer: REF ARRAY OF CHAR;
                            len: CARDINAL;
                            arg: REFANY) =
  VAR
    bd: BufferDesc.T;
  BEGIN
    bd := NARROW(arg, BufferDesc.T);
    IF ConsistencyCheck THEN
      IF bd.buffer.data # buffer THEN
        IO.Put("CacheUnlockBuffer buffer arg doesn't match bd.buffer!\n");
      END;
      IF NUMBER(bd.buffer.data^) < len THEN
        IO.Put("CacheUnlockBuffer len arg doesn't match bd.buffer length!\n");
      END;
    END;
    bd.blocklist.unlockBlock(bd.blockno);
    BufferDesc.Deallocate(bd);
  END CacheUnlockBuffer; 

PROCEDURE ResponseHeader (VAR data: ARRAY OF CHAR; req: Request):CARDINAL = 
  VAR
    length,pos: CARDINAL;
  BEGIN
    (* Put response header into the buffer *)
    pos := 0;
    SUBARRAY(data,pos,NUMBER(Header)) := Header;
    INC(pos,NUMBER(Header));
    
    (* now the urltype *)
    length := Text.Length(req.urltype);
    Text.SetChars(SUBARRAY(data,pos,length), req.urltype);      
    INC(pos,length);
    
    (* now the terminating newlines for HTTP 1.0 *)
    data[pos] := '\n'; INC(pos);
    data[pos] := '\n'; INC(pos);
    RETURN pos;
  END ResponseHeader;

(*
 * Construct an mbuf containing a buffer of data and send it on.
 *)
PROCEDURE Send(s       : REFANY;
               buffer  : Buffer.T;
               len     : CARDINAL;
               methods : Mbuf.Methods;
               desc    : BufferDesc.T) 
  RAISES {Errno.E} =
  VAR
    senddata: Mbuf.T;
  BEGIN
    IF len = 0 THEN
      RETURN;
    END;

    IF desc = NIL THEN 
      desc := BufferDesc.Allocate();
      desc.buffer := buffer;
    END;
    TRY
      senddata := Mbuf.MclGetOa(buffer.data, len, methods, desc);
    EXCEPT
    | Mbuf.LengthMismatch =>
      IO.Put("Internal mbuf length error\n");
    END;
    TYPECASE s OF 
    | NULL =>
      IO.Put("Send() s arg is screwed.\n");
      Mbuf.m_freem(senddata);
      RETURN;
    | Socket.T(sock) =>
      Socket.Send(sock, senddata);
    | TcpCtrlBlock.T(tcb) =>
      M3Tcp.Output(tcb,senddata);
    ELSE
      IO.Put("Send() s arg is really screwed.\n");
      Mbuf.m_freem(senddata);
      RETURN;
    END;
    IF debug THEN
      IO.Put("httpd: queued some data for sending\n");
    END;
  END Send;

(*
 * send error message to client
 *)
PROCEDURE HandleError(err: Exception; url: TEXT; s: Socket.T) =
  (* Send this message back saying what went wrong *)
  VAR
    errormessage := ARRAY [0..12] OF TEXT {
    "HTTP/1.0 ", Errstr[err], "\nMIME-version: 1.0\n", 
    "Content-type: text/html\n\n", "<HTML><HEAD><TITLE>", Fmt.Int(ORD(err)), 
    "</TITLE></HEAD>\n<BODY><H1>", Errmessage[err], "</H1>", Errmessage[err], 
    ": <PRE> ", url, 
    "</PRE>\n<HR><ADDRESS><A HREF=\"http://www-spin.cs.washington.edu/\">\n</A></ADDRESS></BODY></HTML>\n" };
    response: Buffer.T;
    idx, size : INTEGER;
  BEGIN
    IF verbose THEN
      IO.Put("ERROR: " & Errmessage[err] & " " & url & "\n");
    END;
    IF keeplog THEN
      WriteLog(ARRAY OF TEXT{"ERROR: ", Errmessage[err], " ", url, "\n"});
    END;

    response := Buffer.Allocate(SmallBlockSize);
    idx := 0;
    size := NUMBER(response.data^);
    FOR i := FIRST(errormessage) TO LAST(errormessage) DO
      Text.SetChars(SUBARRAY(response.data^, idx, size-idx), errormessage[i]);
      INC(idx, Text.Length(errormessage[i]));
    END;
    TRY
      Send(s, response, idx, freebufferMethods, NIL);
    EXCEPT
    | Errno.E(ec) => 
      IO.Put(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
    END;
  END HandleError;

PROCEDURE ServeFromDisk(s: REFANY; req: Request; placeIntoCache: BOOLEAN)
  RAISES {FileNotFound, Error.E, Errno.E} =
  VAR
    fp: FileHandle;
    blocklist: BlockList.T;
    desc: BufferDesc.T;
    length, nread, offset: CARDINAL;
    blockno: CARDINAL := 0;
    response: Buffer.T;
  BEGIN
    fp := OpenFile(req.url);
    IF useCaching AND placeIntoCache THEN
      blocklist := BlockList.New(1 * Meg, 1 * Meg);
    END;
    TRY
      (*
       * Place the header into the cache as well so we will not
       * waste memory recomputing it.
       *)
      response := Buffer.Allocate(SmallBlockSize);
      length := ResponseHeader(response.data^,req);
      IF useCaching AND placeIntoCache THEN
        desc := BufferDesc.Allocate();
        desc.blocklist := blocklist;
        desc.blockno := blockno;
        desc.buffer := response;
        INC(blockno);
        Send(s, response, length, cachebufferMethods, desc);
      ELSE
        Send(s, response, length, freebufferMethods, NIL);
      END;
      (*
       * Since Socket.Send doesn't fragment for us, we read from the
       * disk in block size chunks and write out to the net in similar
       * chunks.
       *)
      offset := 0;
      REPEAT
        (*
         * We place the buffers in the cache when CacheBuffer is called.
         *)
        response := Buffer.Allocate(BigBlockSize);
        nread := ReadFile(fp, offset, BigBlockSize, response.data);
        offset := offset + nread;
        
        IF useCaching AND placeIntoCache THEN
          desc := BufferDesc.Allocate();
          desc.blocklist := blocklist;
          desc.blockno := blockno;
          desc.buffer := response;
          INC(blockno);
          Send(s, response, nread, cachebufferMethods, desc);
        ELSE
          Send(s, response, nread, freebufferMethods, NIL);
        END;
      UNTIL nread # BigBlockSize; (* short read from disk signals the end *)
      IF useCaching AND placeIntoCache THEN
        EVAL filecache.put(req.url, blocklist);
      END;
    FINALLY
      CloseFile(fp);
    END;
  END ServeFromDisk;

PROCEDURE ServeFromCache(s: Socket.T; <*UNUSED*>req: Request; bl: BlockList.T) RAISES {Errno.E} =
  CONST
    InitialTransferSize = 24*1024; (* amount of data we'll try to retrieve from the cache non-blockingly. *)
  VAR
    length, totalLength: CARDINAL := 0;
    response: Buffer.T;
    blockno : CARDINAL := 0;
    desc: BufferDesc.T;
    mbufhead, mbuftail, mbuf: Mbuf.T;
  BEGIN
    (*
     * We now make a huge mbuf chain from as much data as is
     * available. Blocking is bad for latency, though. We'll send
     * whatever is in memory, and while that data is getting
     * sent, we'll go block for more blocks from the disk.
     *)
    WHILE UseMbufChains AND
          bl.tryRetrieveBlock(blockno,length, response) AND
          totalLength < InitialTransferSize DO
      <* ASSERT response # NIL *>
      desc := BufferDesc.Allocate();
      desc.blocklist := bl;
      desc.blockno := blockno;
      desc.buffer := response;
      TRY
        mbuf := Mbuf.MclGetOa(response.data, 
                              length, 
                              cacheunlockbufferMethods, 
                              desc);
        IF mbufhead = NIL THEN
          mbufhead := mbuf;
          mbuftail := mbuf;
        ELSE
          mbuftail.mh_hdr.mh_next := mbuf;
          mbuftail := mbuf;
        END;
        INC(totalLength, length);
      EXCEPT
      | Mbuf.LengthMismatch =>
        IO.Put("Internal mbuf length error\n");
      END;
      INC(blockno);
    END;
    IF mbufhead # NIL THEN 
      MbufPublic.SetPktHdrLen(mbufhead, totalLength);
      Socket.Send(s, mbufhead);
      IF debug THEN
        IO.Put("httpd: queued some linked mbufs for sending\n");
      END;
    END;
    REPEAT
      response := bl.retrieveBlock(blockno,length);
      IF response # NIL THEN
        desc := BufferDesc.Allocate();
        desc.blocklist := bl;
        desc.blockno := blockno;
        desc.buffer := response;
        Send(s, response, length, cacheunlockbufferMethods, desc);
      END;
      INC(blockno);
    UNTIL response = NIL;
  END ServeFromCache;

(* Special purpose routines which let you probe the state of the system
   through special URLs. *)
PROCEDURE IsShellCommand(url: TEXT): BOOLEAN =
  BEGIN
    RETURN IsPrefix(url, "/shell/");
  END IsShellCommand;

PROCEDURE IsPrefix(t1, t2: TEXT): BOOLEAN =
  VAR
    len1, len2: INTEGER;
  BEGIN
    IF t1 = NIL OR t2 = NIL THEN RETURN FALSE; END;
    len1 := Text.Length(t1);
    len2 := Text.Length(t2);
    IF len2 > len1 THEN RETURN FALSE; END;
    FOR i := 0 TO len2-1 DO
      IF Text.GetChar(t1, i) # Text.GetChar(t2, i) THEN
        RETURN FALSE;
      END;
    END;
    RETURN TRUE;
  END IsPrefix;

VAR
  shell_mu : MUTEX;

PROCEDURE ServeFromShell(s: Socket.T; req: Request) RAISES ANY = 
  VAR
    wr: TextWr.T;
    oldWr: Wr.T;
    arg : TEXT;
    htmlText: TEXT;
    nput: INTEGER := 0;
    buffer: Buffer.T;
  BEGIN
    (*
     * Only allow one thread at a time to call the GC routines which
     * turn GC on and off. Turning it on while another thread is
     * scanning the heap could spell disaster.
     *)
    arg := req.url;
    VAR
      k := Text.Length("/shell/");
      len2 := Text.Length(arg) - k;
      chars := NEW(REF ARRAY OF CHAR, len2);
      c: CHAR;
    BEGIN
      FOR i := 0 TO len2-1 DO
        c  := Text.GetChar(arg, k);
        IF c = '_' THEN c := ' '; END;
        chars[i] := c;
        INC(k);
      END;
      arg := Text.FromChars(chars^);
    END;

    LOCK shell_mu DO
      TRY
        wr := TextWr.New();
        oldWr := ThreadExtra.SetWrSelf(wr);
        Wr.PutText(wr, "<pre>\n");
        IF NOT Shell.OneShellCommand(arg) THEN
              Wr.PutText(wr,"Command failed\n");
        END;
        Wr.PutText(wr, "</pre>\n");
        EVAL ThreadExtra.SetWrSelf(oldWr);
        htmlText := TextWr.ToText(wr);
        Wr.Close(wr);
      EXCEPT
        Wr.Failure => RAISE InternalFault("Writer failure for command output");
      END;
    END;
    nput := 0;
    WHILE nput < Text.Length(htmlText) DO
      WITH buffLen = MIN(BigBlockSize, Text.Length(htmlText) - nput) DO
        buffer := Buffer.Allocate(buffLen);
        Text.SetChars(buffer.data^, Text.Sub(htmlText, nput, buffLen));
        Send(s, buffer, buffLen, freebufferMethods, NIL);
        INC(nput, buffLen); 
      END;
    END;
  END ServeFromShell;

<*UNUSED*>
PROCEDURE TimerUpcall(tp: UNTRACED REF OsfNet.tcpcbT): Ctypes.int = 
  CONST 
    FAST_TIME_OUT = 1; (* *)
  BEGIN
    IF verbose THEN IO.Put("TimerUpcall called"); END;
    tp.t_idle := LAST(Ctypes.short); (* expire the idle timer *)
    RETURN FAST_TIME_OUT;
  END TimerUpcall;

PROCEDURE ServeClient(arg: REFANY) : REFANY =
  VAR
    s: Socket.T;
    req: Request;
    recvdata: Mbuf.T;
    bl: BlockList.T;
    len: CARDINAL;
    databuf : Net.Payload;
    sin: SocketAddrIn.T;
  BEGIN
    IF debug THEN IO.Put("-- connected --\n"); END;

    s := NARROW(arg, Socket.T);
    arg := NIL; (* help GC. *)

    TRY 
      TRY
        len := BYTESIZE(SocketAddrIn.T);

        IF verbose OR keeplog THEN
          WITH sockaddr = VIEW(sin,SocketAddr.T) DO
            Socket.Getpeername(s,sockaddr,len);
          END;
        END;

        IF verbose THEN
          WITH ipaddr = VIEW(sin.sin_addr,IpPktFormat.AddressArray) DO
            IO.Put("remote "); 
            IO.PutInt(ORD(ipaddr[0]));
            IO.Put(".");
            IO.PutInt(ORD(ipaddr[1]));
            IO.Put(".");
            IO.PutInt(ORD(ipaddr[2]));
            IO.Put(".");
            IO.PutInt(ORD(ipaddr[3]));
            IO.Put(" ");
            IO.PutInt(Net.nstoh(sin.sin_port));
            IO.Put(" ");
          END;
        END;

        IF keeplog THEN
          WITH ipaddr = VIEW(sin.sin_addr,IpPktFormat.AddressArray) DO
            WriteLog(ARRAY OF TEXT{"remote ", Fmt.Int(ORD(ipaddr[0])), ".", 
                                   Fmt.Int(ORD(ipaddr[1])), ".",
                                   Fmt.Int(ORD(ipaddr[2])), ".",
                                   Fmt.Int(ORD(ipaddr[3])), " ",
                                   Fmt.Int(Net.nstoh(sin.sin_port)), " "});
          END;
        END;

        len := MaxRequestLen;
        EVAL Socket.Recv(s, recvdata, len);
        IF recvdata = NIL THEN RETURN NIL; (* XXX ugly hack *) END;
        databuf := Mbuf.Array(recvdata);
        ParseRequest(databuf^, req);
        Mbuf.m_freem(recvdata);
        (* call me when the connection goes away.
        WITH tp = Socket.ExportPCB(s) DO 
          tp.t_timer_upcall := TimerUpcall;
        END;
        *)
        IF verbose THEN
          IO.Put("[" & Fmt.Int(Text.Length(req.url)) & "] ");
        END;

        IF verbose THEN
          IO.Put("GET ");
          IO.Put(req.url);
          IF req.referer # NIL THEN
            IO.Put(" referred by: " & req.referer & "\n");
          ELSE
            IO.Put("\n");
          END;
        END;
        (*
         * XXX log the request.
         *)
        IF keeplog THEN
          IF req.referer # NIL THEN
          WriteLog(ARRAY OF TEXT
            {"[", 
             Fmt.Int(Text.Length(req.url)),
             "] GET ", 
             req.url,
             " referred by: ",
             req.referer,
             "\n"});
          ELSE
          WriteLog(ARRAY OF TEXT
            {"[", 
             Fmt.Int(Text.Length(req.url)),
             "] GET ", 
             req.url,
             "\n"});
          END;
        END;

        (*
         * See if we need to bring the file in from disk
         *)
        IF filecache.get(req.url, bl) THEN
          (*
           * Otherwise, read back from memory.
           * The cache layer will get rid of large files transparently.
           * XXX we may want to treat files differently based on their
           * XXX types, e.g. MRU for video files.
           *)
          IF debug THEN IO.Put("-- serving from cache --\n"); END;
          ServeFromCache(s, req, bl);
        ELSIF IsShellCommand(req.url) THEN
          IF debug THEN IO.Put("-- serving from shell --\n"); END;
          ServeFromShell(s, req);
        ELSE
          (*
           * If file is not cached, serve from disk and place it into
           * cache if it needs to be cached.
           *)
          IF debug THEN IO.Put("-- serving from disk --\n"); END;
          ServeFromDisk(s, req, TRUE);
        END;
      EXCEPT
      | UrlForbidden(url) => HandleError(Exception.Forbidden, url, s);
      | FileNotFound(file) => HandleError(Exception.NotFound, file, s);
      | BadRequest(msg) => HandleError(Exception.BadRequest, msg, s);
      | NotDoneYet(msg) => HandleError(Exception.NotImplemented, msg, s);
      | InternalFault(str) => HandleError(Exception.Internal, str, s);
      | Error.E(e) => HandleError(Exception.Internal, "File error " &
        e.message(), s);
      | Errno.E => (* ignore - remote is gone. *)
      ELSE
        (* ignore, unknown exception *)
      END;
    FINALLY
      TRY

        Socket.Close(s);
      EXCEPT
      | Errno.E => (* ignore - remote is gone. *)
      END;

      s := NIL; (* help GC. *)
      IF debug THEN IO.Put("-- closed --\n"); END;
      RETURN NIL;
    END; 
  END ServeClient; 

(*
 * Server loop, where the server catches request and spawns threads to
 * handle them.
 *)
PROCEDURE ServerLoop(server : Socket.T) =
  VAR
    s      : Socket.T;
    t      : Thread.T;
  BEGIN
    LOOP
      (* Endless server loops are dangerous in keeping dead data
         alive.  For good measure we will NIL references before
         blocking in Accept. *)
      s := NIL;
      t := NIL;
      TRY 
        (* XXX *)
        Socket.Listen(server, MaxListenLog);
        s := Socket.Accept(server);
      EXCEPT
      | Errno.E(ec) => 
        IO.Put(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
      END;
      t := ThreadExtra.PFork(ServeClient, s, 
                             ThreadExtra.defaultPriority + serverpriority);
      IF serverShouldQuit THEN
        RETURN;
      END;
    END;
  END ServerLoop;

VAR
  didRun : BOOLEAN := FALSE;

PROCEDURE Run(<*UNUSED*>arg: REFANY) : REFANY =
  VAR
    server : Socket.T;
    sin    : SocketAddrIn.T;
  BEGIN
    IF didRun THEN RETURN NIL; END;
    didRun := TRUE;
    TRY
      server       := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0);
      sin.sin_port := Net.htons(80);
      sin.sin_len  := BYTESIZE(sin);
      sin.sin_addr := 0;
      sin.sin_zero := SocketAddrIn.SIN_ZERO;
      Socket.Bind(server, sin);
    EXCEPT
    | Errno.E(ec) =>
      IO.Put(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
      RETURN NIL;
    END;

    acceptTimer := Spy.Create("http accept");
    closeTimer  := Spy.Create("http close");
    dataflow    := Spy.Create("http recv");

    TRY
      Socket.Listen(server, MaxListenLog);
    EXCEPT
    | Errno.E(ec) =>
      IO.Put(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
      RETURN NIL;
    END;
    ServerLoop(server);
    TRY
      Socket.Close(server);
    EXCEPT
    | Errno.E(ec) =>
      IO.Put(Errno.Fmt(ec) & ", errno=" & Fmt.Int(ec) & "\n");
      RETURN NIL;
    END;
    IO.Put("HTTP server is done.\n");
    RETURN NIL;
  END Run;

(* Procedure to write logging information. *)
VAR logmutex : MUTEX;
PROCEDURE WriteLog(READONLY msgs : ARRAY OF TEXT) =
  VAR
    entry : REFANY;
    stat : FileStat.T;
    offset,size,bytes : CARDINAL;
    buf : ARRAY [1..512] OF CHAR;
    msgIndex : CARDINAL;

  BEGIN
    TRY
      entry := FileSystem.Lookup(NIL, logfile);
    EXCEPT
    | NameServer.Error =>
      IO.PutError("Httpd.WriteLog couldn't open "); 
      IO.Put(logfile); 
      IO.Put("\n");
      RETURN;
    END;


    TYPECASE entry OF 
    | File.T(file) =>
        TRY
          file := file.open(0);
          (* LOCK logmutex DO *)
            file.stat(stat);
            offset := stat.size;
            IF FALSE THEN
              IO.Put("["); IO.PutInt(offset); IO.Put("]");
            END;
            msgIndex := FIRST(msgs);

            REPEAT
              size  := 0;
              bytes := Text.Length(msgs[msgIndex]);
              WHILE size + bytes < NUMBER(buf) DO
                WITH b = SUBARRAY(buf,size,bytes) DO
                  Text.SetChars(b,msgs[msgIndex]);
                END;
                INC(size,bytes);
                INC(msgIndex);
                IF msgIndex <= LAST(msgs) THEN
                  bytes := Text.Length(msgs[msgIndex]);
                ELSE
                  EXIT;
                END;
              END;

              WITH b = SUBARRAY(buf,0,size) DO
                IF FALSE THEN
                  IO.Put("{");
                  IO.PutInt(NUMBER(b));
                  IO.Put("}");                
                END;
                bytes := file.write(b,offset);
                INC(offset,bytes);
              END;
            UNTIL msgIndex > LAST(msgs);
          (* END; *)

          IF FALSE THEN 
            IO.Put("\n");
          END;
          file.close();
        EXCEPT
        | Error.E(e) =>
          IO.PutError(logfile & ":" & e.message() & "\n");
        END;
      ELSE
        IO.PutError("Httpd.WriteLog "); IO.Put(logfile); IO.Put(" is not a file.\n");
      END;
  END WriteLog;

TYPE
  Authorizer = Auth.T OBJECT
    all: BOOLEAN;
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (self: Authorizer; 
                     <* UNUSED *> key: Auth.Key;
                     <* UNUSED *> arg: REFANY): BOOLEAN =
  VAR b: BOOLEAN;
  BEGIN
    TRY
      IO.Put("Request to authorize " &
             SecurityManager.GetUserName(SecurityManager.GetCurrentUid()) &
             "\n");
    EXCEPT
    | SecurityError.T =>
      IO.Put("Request ot authorize " &
             Fmt.Int(SecurityManager.GetCurrentUid()) & "\n");
    END;
    IF self.all THEN
      IO.Put(" -- OK\n");
      b := TRUE;
    ELSE
      IO.PutError(" -- Failed\n");
      b := FALSE;
    END;
    RETURN b;
  END Authorize;

VAR t : Authorizer;

VAR
  cachebufferMethods       : Mbuf.Methods;
  cacheunlockbufferMethods : Mbuf.Methods;
  freebufferMethods        : Mbuf.Methods;

CONST CacheSize = 128*Kilo;
BEGIN
  t := NEW(Authorizer);
  t.all := TRUE;
  filecache := FileCache.New();
  filecache.setMaxSize(CacheSize);
  EVAL HttpdInterface.Export(t);
  shell_mu := NEW(Thread.Mutex);
  IO.Put("Http init done --- starting Httpd thread.\n");

  cachebufferMethods       := NEW(Mbuf.Methods, free := CacheBuffer);
  cacheunlockbufferMethods := NEW(Mbuf.Methods, free := CacheUnlockBuffer);
  freebufferMethods        := NEW(Mbuf.Methods, free := FreeBuffer);
  
  WITH t = ThreadExtra.PFork(Httpd.Run, NIL) DO
    TrackStrand.SetName(ThreadExtra.GetTracker(t),"Httpd");
  END;
  logmutex := NEW(MUTEX);
END Httpd.
