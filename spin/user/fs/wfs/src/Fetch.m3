(*
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed constant 0 to SocketRep.SIN_ZERO.
 *
 * 20-Apr-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use new MclGetOa function.  
 *
 * 12-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Eliminated conversion in Recv from an array of bytes to text
 *      back to array of bytes.
 *      MBuf chains are now being freed.
 *
 * 12-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added code to strip off the remote web server header.
 *      Close the socket when it is no longer used.
 *
 * 03-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed Connect() function to not bind to a particular local port.
 *	TCP will chose a free local port automatically.
 *	Took out the phase TEXT varible in Connect().  For some reason it
 *	was NIL when used in the exceptin handler.
 *	Fixed Send() functions to set the len argument used by
 *	Mbuf.NewMclGetOa.
 *	Changed len size in Recv() function from 1024 to 128 bytes.  I
 *	was concerned that the Recv was blocking until it received 1024
 *	bytes.  The goal was to just see some data coming back.
 *	
 * 03-Mar-96  Brian Bershad (mef) at the University of Washington
 *	Created.
 *)

MODULE Fetch;

IMPORT Net, NetDb, Socket, SocketRep, Mbuf, Ctypes, Errno;
IMPORT Error, IO, Fmt, Text, File;


CONST debug = TRUE;

(*
 * Socket to file services 
 *)
PROCEDURE Connect (hostname: TEXT; port: Ctypes.unsigned_short): Socket.T =
  VAR
    sock  : Socket.T;
    sin   : SocketRep.sockaddr_in;
  BEGIN
    IF debug THEN IO.Put("Connecting to " & hostname & "\n"); END;

    TRY
      sock := Socket.Create(Socket.AF_INET, Socket.SOCK_STREAM, 0);
      sin.sin_port := Net.htons(port);
      sin.sin_len := BYTESIZE(sin);
      sin.sin_family := Socket.AF_INET;
      TRY
      WITH ipaddr = NetDb.GetHostByName(hostname) DO
        sin.sin_addr := VIEW(ipaddr, Ctypes.unsigned_int);
      END;
      EXCEPT NetDb.HostNotFound => RAISE Socket.Error(Errno.ENXIO); END;
      sin.sin_zero := SocketRep.SIN_ZERO;

      Socket.Connect(sock, sin);
    EXCEPT
      Socket.Error (ec) =>
        IO.Put("Connect" & Errno.ErrorMessages[ec] & ", errno=" & Fmt.Int(ec) & "\n");
        RETURN NIL;
    END;

    RETURN sock;
  END Connect;

PROCEDURE FreeBuffer(<*UNUSED*>buffer: REF ARRAY OF Net.BYTE;
                     <*UNUSED*>len: CARDINAL;
                     arg: REFANY) =
  BEGIN
    IO.Put("Freeing buffer " & NARROW(arg, TEXT) & "\n");
  END FreeBuffer;



PROCEDURE SocketErr (t: TEXT; ec: Errno.T) =
  BEGIN
    IO.Put("Socket Error " & t & Errno.ErrorMessages[ec] & ", errno="
             & Fmt.Int(ec) & "\n");
  END SocketErr;

PROCEDURE TextToRefArrayOfBytes(READONLY t: TEXT): REF ARRAY OF Net.BYTE =
  VAR len: CARDINAL;
      data: REF ARRAY OF Net.BYTE;
  BEGIN
    IF t = NIL THEN RETURN NIL; END;
    len := Text.Length(t);
    IF len = 0 THEN RETURN NIL; END;
    data := NEW(REF ARRAY OF Net.BYTE, len);
    Text.SetChars(data^,t);
    RETURN data;
  END TextToRefArrayOfBytes;

PROCEDURE Send(s: Socket.T; msg: TEXT) =
  VAR sendData: Mbuf.T;
      data: REF ARRAY OF Net.BYTE;
      len: CARDINAL;
  BEGIN
    IF msg = NIL THEN IO.Put("Not sending NIL data\n"); RETURN; END;
    data := TextToRefArrayOfBytes(msg);

    TRY
      len := Text.Length(msg);
      (* Not passing "methods" to MclGetOa. *)
      sendData := Mbuf.MclGetOa( data, len, NIL, msg);
    EXCEPT
    | Mbuf.LengthMismatch =>
      IO.Put("Internal mbuf length error\n");
    END;
    TRY
    Socket.Send(s, sendData);
    EXCEPT
    | Socket.Error(ec) =>  SocketErr("Fetch.Send", ec);
    END;
    RETURN;
  END Send;


PROCEDURE Recv (s: Socket.T; VAR totallen: CARDINAL) : REF ARRAY OF CHAR =
  VAR
    recvData, recvMbufs: Mbuf.T;
    msg, oldmsg: REF ARRAY OF CHAR;
    len	       : CARDINAL;
  BEGIN
    IF debug THEN IO.Put("Entering Recv loop\n"); END;

    totallen := 0;
    (*
     * this loop should ultimately be changed to chain buffers instead 
     * of use traced heap. - egs.
     *)
    LOOP
      TRY
        recvData := NIL;
        IF Socket.Recv(s, recvData, 1024) = 0 THEN 
          EXIT;
        END;

        recvMbufs := recvData;
        WHILE recvData # NIL DO
          WITH recvBuf = Mbuf.Array(recvData) DO
            len := NUMBER(recvBuf^);
            IF debug THEN
              IO.Put("Got " & Fmt.Int(len) & " bytes in an mbuf\n");
            END;
            totallen := totallen + len;
            oldmsg := msg;
            msg := NEW(REF ARRAY OF CHAR, totallen);
            IF oldmsg # NIL THEN
              SUBARRAY(msg^, 0, totallen - len) := oldmsg^;
            END;
            SUBARRAY(msg^, totallen - len, len) := SUBARRAY(recvBuf^, 0, len);
          END;
          recvData := recvData.mh_hdr.mh_next;
        END;
        Mbuf.m_freem(recvMbufs);
      EXCEPT
      | Socket.Error (ec) => 
        SocketErr("Fetch.Recv", ec); 
        EXIT;
      END;
    END;

    (* 
     * Strip off the header placed by the remote http server.
     *)
    FOR i := 2 TO totallen-1 DO
      IF msg^[i-2] = '\n' AND msg^[i-1] = '\n' THEN
        oldmsg := msg;
        msg := NEW(REF ARRAY OF CHAR, totallen - i);
        msg^ := SUBARRAY(oldmsg^, i, totallen - i);
        EXIT;
      END;
    END;
    IF debug THEN IO.Put("Exiting Recv loop\n"); END;
    RETURN msg;
  END Recv;

PROCEDURE GetFile (hostname: TEXT; filename: TEXT): REF ARRAY OF CHAR
  RAISES {Error.E} =
  VAR
    s  : Socket.T;
    buf: REF ARRAY OF CHAR;
    len: CARDINAL;
  BEGIN
    s := Connect(hostname, 80);
    IF s = NIL THEN
      IF debug THEN
        IO.Put("Connect returns NIL w/no exception!\n");
      END;
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NO_ENTRY));
    END;
    Send(s, "GET /" & filename & " HTTP/0.9\n\n");
    buf := Recv(s, len);

    IF debug THEN 
      IO.Put("Fetch returning "); 
      IO.PutInt(len);
      IO.Put(" bytes.\n");
    END;
    TRY
      Socket.Close(s);
    EXCEPT
      Socket.Error => IF debug THEN IO.Put("Close error\n"); END;
    END;
    RETURN buf;
  END GetFile;

BEGIN
END Fetch.



