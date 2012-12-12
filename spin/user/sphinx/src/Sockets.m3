(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Set fh.h.dev to suport Select() when creating a socket.
 *
 * 13-Jun-97  David Becker at the University of Washington
 *      clean up imports
 *	Change SocketIoctl to only raise Error.E
 * 04-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Use Translation.Write
 * 31-May-97  David Becker at the University of Washington
 *      Unified Socket.Error and Error.E into Errno exception 
 *      Brought bsd unix interfaces into sphinx from urt
 * 08-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up interface to sockaddr and sockaddr_in.  Handling some
 *	exceptions properly.
 *
 * 17-Sep-96  becker at the University of Washington
 *	SocketIoctl now handles FIO NREAD	
 *	SocketRead now handles EWOULDBLOCK 
 *	
 * 10-Jun-96 oystr at the University of Washington
 *	Added getpeername, setsockopt, shutdown, socketioctl.
 *	And a little tidying up.
 *
 * 20-May-96 oystr at the University of Washington
 *	Added Select support.
 *
 * 07-May-96  Paul Kromann (pkromann) at the University of Washington
 *	Socket added.
 *)
MODULE Sockets EXPORTS Sphinx;
IMPORT Proc, ProcRep;
IMPORT File, Socket, SocketAddr, SocketAddrIn, SocketRep, Space,
       Mbuf, Error, Errno, ErrnoDep, Ctypes, StreamFile,
       IoctlPosix, Word, CharArray, VMError;
(* IMPORT If; *)
IMPORT OpenFile, OpenFileQ;
IMPORT Translation;
IMPORT Fcntl AS URTFcntl ;
IMPORT Ioctl AS URTIoctl;
IMPORT SphinxUtils, ProfileSupport;<*NOWARN*>
IMPORT Spy;<*NOWARN*>

TYPE SocketFile = StreamFile.T OBJECT
  socket: Socket.T;
END;
  
VAR
  (* The mbufMethods are a workaround (June '96)
     to some problems with checksuming.
     Eventually we'll want to use them for
     dealing with buffers (i.e. don't allocate
     a NEW buffer on every read/write. *)
  mbufMethods: Mbuf.Methods;
  swMethods: Mbuf.Methods;

  acceptSpy: Spy.T;

PROCEDURE swFreeProc(
	 ext_buf : REF ARRAY OF CHAR;
	<* UNUSED *> ext_size: CARDINAL;
	<* UNUSED *> ext_arg : REFANY) =
  BEGIN
    CharArray.Free(ext_buf);
  END swFreeProc;

PROCEDURE CreateSocket(addrFamily, type, protocol: INTEGER): 
	INTEGER RAISES {Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    sock := Socket.Create(addrFamily, type, protocol);
  BEGIN
    fh := OpenFileQ.Allocate();
    fh.offset := 0;
    fh.refCount := 0;
    fh.flags := URTFcntl.O_RDWR;

    fh.h := NEW(SocketFile, 
		socket := sock,
		dev := sock,
		read := SocketRead,
		write := SocketWrite,
		writeRef := SocketWriteRef,
		close := SocketClose,
		ioctl := SocketIoctl);
    
    NARROW(fh.h, StreamFile.T).dev := NARROW(fh.h, SocketFile).socket;
    LOCK proc.mu DO 
      RETURN Proc.AllocateFD(proc, fh);
    END;
  END CreateSocket;

PROCEDURE AddrIn(VAR addr: SocketAddr.T) =
  BEGIN
    addr.sa_family := addr.sa_len;
    addr.sa_len := BYTESIZE(SocketAddr.T);
  END AddrIn;

PROCEDURE AddrOut(VAR addr: SocketAddr.T) =
  BEGIN
    addr.sa_len := addr.sa_family;
    addr.sa_family := 0;
  END AddrOut;

PROCEDURE SinIn(VAR sin: SocketAddrIn.T) =
  BEGIN
    sin.sin_family := sin.sin_len;
    sin.sin_len := BYTESIZE(SocketAddrIn.T);
  END SinIn;

PROCEDURE SinOut(VAR sin: SocketAddrIn.T) = 
  BEGIN
    sin.sin_len := sin.sin_family;
    sin.sin_family := 0;
  END SinOut;

PROCEDURE Connect (fd: INTEGER; address: INTEGER) 
  	: INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    (*
     * WARNING: We assume sizeof(sockaddr) == sizeof(sockaddr_in)
     *)
    fh: OpenFile.T;
    buf: ARRAY [1..BYTESIZE(SocketAddr.T)] OF CHAR;
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);

    Translation.Read(proc, address, buf);

    WITH sin = VIEW(buf,SocketAddrIn.T),
      s = NARROW(fh.h, SocketFile) DO
      SinIn(sin);
      Socket.Connect(s.socket, sin);
      SinOut(sin);
    END;
    
    Translation.Write(proc, buf, address);
    
    RETURN 0;
  END Connect;

PROCEDURE Listen(fd: INTEGER; backlog: INTEGER)
  	: INTEGER RAISES {Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);

    WITH s = NARROW(fh.h, SocketFile) DO
      Socket.Listen(s.socket, backlog);
    END;
    RETURN 0;
  END Listen;

PROCEDURE Accept (fd: INTEGER; address: INTEGER; VAR length: INTEGER)
 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    s: SocketFile;
    size: CARDINAL := BYTESIZE(SocketAddr.T);
    newfh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    buf: ARRAY [1..BYTESIZE(SocketAddr.T)] OF CHAR;
    socket: Socket.T;
    newFd: INTEGER;
  BEGIN
    IF SphinxUtils.MicroBench THEN Spy.Enter(acceptSpy); END;
    
    TYPECASE Proc.FindFH(proc, fd).h OF
    | SocketFile(sf) => s := sf;
    ELSE
      RAISE Errno.E(ErrnoDep.ENOTSOCK);
    END;

    socket := Socket.Accept(s.socket);
    newfh := OpenFileQ.Allocate();
    newfh.offset := 0;
    newfh.refCount := 0;
    newfh.flags := URTFcntl.O_RDWR;
    newfh.h := NEW(SocketFile,
		   socket := socket,
		   dev := socket,
		   read := SocketRead,
		   write := SocketWrite,
		   writeRef := SocketWriteRef,
		   close := SocketClose,
		   ioctl := SocketIoctl);
    WITH addr = VIEW(buf,SocketAddr.T) DO
      Socket.Getpeername(socket, addr, size);
    END;
    Translation.Write(proc, buf, address);
    length := size;

    LOCK proc.mu DO
      newFd := Proc.AllocateFD(proc, newfh);
    END;
    IF SphinxUtils.MicroBench THEN Spy.Exit(acceptSpy); END;
    RETURN newFd;
  END Accept;
    
PROCEDURE Bind(fd: INTEGER; address: INTEGER)
	 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    buf: ARRAY [1..BYTESIZE(SocketAddrIn.T)] OF CHAR;
    space := proc;
  BEGIN
    fh := Proc.FindFH(proc, fd);

    Translation.Read(space, address, buf);
    
    WITH sin = VIEW(buf,SocketAddrIn.T),
           s = NARROW(fh.h, SocketFile) DO
      SinIn(sin);
      Socket.Bind(s.socket, sin);
      SinOut(sin);
    END;
    Translation.Write(space, buf, address);

    RETURN 0;
  END Bind;

PROCEDURE Send(fd: INTEGER; message: INTEGER;
	       length: INTEGER;
	       <*UNUSED*>flags: INTEGER)
	 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    msgbuf := Proc.AllocateMemory(proc, length);
    mbuf: Mbuf.T;
    space := proc;
  BEGIN
    fh := Proc.FindFH(proc, fd);
    TRY
      Translation.Read(space, message, SUBARRAY(msgbuf^, 0, length));
      mbuf := Mbuf.MclGetOa(msgbuf, length, mbufMethods, NIL);
      WITH s = NARROW(fh.h, SocketFile) DO
        Socket.Send(s.socket, mbuf);
      END;
    EXCEPT
    | Mbuf.LengthMismatch => 
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    RETURN length;
  END Send;

PROCEDURE Sendto(fd: INTEGER; message: INTEGER; length: INTEGER;
		 flags: INTEGER; dest_addr: INTEGER;
		 <*UNUSED*>addrLen: INTEGER)
	 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    addrbuf: ARRAY [1..BYTESIZE(SocketAddr.T)] OF CHAR;
    msgbuf := NEW(REF ARRAY OF CHAR, length);
    mbuf: Mbuf.T;
    space := proc;
  BEGIN
    fh := Proc.FindFH(proc, fd);

    TRY
      Translation.Read(space, message, SUBARRAY(msgbuf^, 0, length));
      mbuf := Mbuf.MclGetOa(msgbuf, length, mbufMethods, NIL);
      Translation.Read(space, dest_addr, addrbuf);
      WITH s = NARROW(fh.h, SocketFile),
           addr = VIEW(addrbuf, SocketAddr.T) DO
        AddrIn(addr);
        Socket.Sendto(s.socket, mbuf, flags, addr);
      END;
    EXCEPT
    | Mbuf.LengthMismatch => 
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    
    RETURN length;
  END Sendto;


PROCEDURE Recv(fd: INTEGER; buffer: INTEGER; length, flags: INTEGER)
 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    mbuf: Mbuf.T;
    bytesRecvd: CARDINAL;
    space := proc;
  BEGIN
    fh := Proc.FindFH(proc, fd);

    WITH s = NARROW(fh.h, SocketFile) DO
      bytesRecvd := Socket.Recv(s.socket, mbuf, length, flags);
    END;
    WriteMbufToSpace(mbuf, space, buffer);
    RETURN bytesRecvd;
  END Recv;

  (*
    XXX Sphinx functions are limited to 6 parameters, so the length
    of the address field has been omitted for Recvfrom and Sendto.
  *)

PROCEDURE Recvfrom(fd: INTEGER; buffer, length: INTEGER;
		   flags: INTEGER; address:INTEGER;
                   <*UNUSED*>addrlen: INTEGER) 
	: INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
(*  addrbuf: ARRAY [1..BYTESIZE(SocketAddr.T)] OF CHAR; *)
    mbuf: Mbuf.T;
    bytesRecvd: CARDINAL;
    space := proc;
    addr: SocketAddr.T;
  BEGIN
    fh := Proc.FindFH(proc, fd);

    WITH s = NARROW(fh.h, SocketFile) DO
      bytesRecvd := Socket.Recvfrom(s.socket, mbuf, length, flags, addr);
    END;
    WriteMbufToSpace(mbuf, space, buffer);
    AddrOut(addr);
    Translation.Write(space, VIEW(addr, ARRAY OF CHAR), address);
    RETURN bytesRecvd;
  END Recvfrom;

(*
 * Frees mbuf, too.
 *)
PROCEDURE WriteMbufToSpace(VAR mbuf: Mbuf.T; space: Space.T; buffer: INTEGER)
  RAISES {VMError.E} =
  VAR
    pos: INTEGER := 0;
  BEGIN
    WHILE mbuf # NIL DO
      WITH recvBuf = Mbuf.Array(mbuf)^ DO
        IF BYTESIZE(recvBuf) # 0 THEN (* XXX Translation.Write should not allow *)
          Translation.Write(space, recvBuf, buffer + pos);
          INC(pos, BYTESIZE(recvBuf));
        END;
      END;
      mbuf := Mbuf.m_free(mbuf);
    END;
  END WriteMbufToSpace;

PROCEDURE WriteMbufToCharBuf(VAR mbuf: Mbuf.T; VAR charBuf: ARRAY OF CHAR) =
  VAR
    pos := 0;
  BEGIN
    (* Slow copy from mbuf chain to contiguous buffer -  ugh *)
    WHILE mbuf # NIL DO
      WITH recvBuf = Mbuf.Array(mbuf)^ DO
        SUBARRAY(charBuf,pos,BYTESIZE(recvBuf)) := recvBuf;
        INC(pos, BYTESIZE(recvBuf));
      END;
      mbuf := Mbuf.m_free(mbuf);
    END;
  END WriteMbufToCharBuf;

PROCEDURE SocketClose(s: SocketFile ) RAISES {Error.E} =
  BEGIN
    TRY
      Socket.Close(s.socket);
    EXCEPT
    | Errno.E(ec) => RAISE Error.E(NEW(File.ErrorT).init(ec));
    END;
  END SocketClose;

PROCEDURE SocketRead (s: SocketFile;
		      VAR data: ARRAY OF CHAR;
		      <*UNUSED*>offset: File.OffsetT
		      ):CARDINAL RAISES
		      {Error.E} =
  VAR
    mbuf: Mbuf.T;
    flags := 0;
    bytesRecvd: INTEGER;
  BEGIN
    IF s.async THEN
      flags := SocketRep.MSG_NONBLOCK;
    END;
    TRY
      bytesRecvd := Socket.Recv(s.socket, mbuf, NUMBER(data), flags);
      WriteMbufToCharBuf(mbuf, data);
      RETURN bytesRecvd;
    EXCEPT
    | Errno.E(ec) =>
      IF ec = ErrnoDep.EWOULDBLOCK THEN
        RAISE Error.E(NEW(File.ErrorT).init(File.FS_WOULD_BLOCK));
      END;
      RAISE Error.E(NEW(File.ErrorT).init(File.FS_NOT_SUPPORTED));
    END;
  END SocketRead;

PROCEDURE SocketWrite (s: SocketFile;
		       READONLY data: ARRAY OF CHAR;
		       <*UNUSED*>offset: File.OffsetT): CARDINAL RAISES
		       {Error.E} =
  VAR
    mbuf: Mbuf.T;
    buf := CharArray.Allocate(NUMBER(data));
  BEGIN
    TRY
      SUBARRAY(buf^, 0, NUMBER(data)) := data;
      mbuf := Mbuf.MclGetOa(buf, NUMBER(data), swMethods, NIL);
      Socket.Send(s.socket, mbuf);
    EXCEPT
    | Errno.E(ec) =>
      RAISE Error.E(NEW(File.ErrorT).init(ec));
    | Mbuf.LengthMismatch =>
    END;
    RETURN NUMBER(data);
  END SocketWrite;

PROCEDURE SocketWriteRef (s: SocketFile;
			  buf: REF ARRAY OF CHAR;
			  bytes: CARDINAL;
			  <*UNUSED*>offset: File.OffsetT;
			  from: CARDINAL;
			  VAR retain: BOOLEAN): CARDINAL RAISES {Error.E} =
  VAR
    mbuf: Mbuf.T;
  BEGIN
(*    IF SphinxUtils.profCount = 0 THEN
      EVAL ProfileSupport.On();
    ELSIF SphinxUtils.profCount = 1000 THEN
      EVAL ProfileSupport.Off();
    END; 
    INC(SphinxUtils.profCount); *)

    IF from # 0 THEN
      retain := FALSE;
      RETURN SocketWrite(s, SUBARRAY(buf^, from, bytes), 0);
    ELSE
      TRY
	retain := TRUE;
	mbuf := Mbuf.MclGetOa(buf, bytes, swMethods, NIL);
	Socket.Send(s.socket, mbuf);
      EXCEPT
      | Errno.E(ec) =>
	RAISE Error.E(NEW(File.ErrorT).init(ec));
      | Mbuf.LengthMismatch =>
      END;
    END;
    RETURN bytes;
  END SocketWriteRef;

PROCEDURE Getsockname(fd: INTEGER; address: INTEGER; VAR length: CARDINAL)
	 : INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    addrbuf: ARRAY [1..BYTESIZE(SocketAddr.T)] OF CHAR;
    space := proc;
  BEGIN
    fh := Proc.FindFH(proc, fd);
    
    Translation.Read(space, address, addrbuf);
    WITH s = NARROW(fh.h, SocketFile),
      addr = VIEW(addrbuf, SocketAddr.T) DO
      AddrIn(addr);
      Socket.Getsockname(s.socket, addr, length);
      AddrOut(addr);
    END;
    Translation.Write(space, addrbuf, address);
    RETURN 0;
  END Getsockname;

PROCEDURE SetSockOpt(fd: INTEGER; level, opt_name: INTEGER;
		      opt_val: INTEGER; opt_len: INTEGER)
	: INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    optbuf: ARRAY [1..16] OF CHAR;
  BEGIN
    fh := Proc.FindFH(proc, fd);
    IF NOT ISTYPE(fh.h,SocketFile) THEN
      RAISE Errno.E(ErrnoDep.ENOTSOCK);
    END;
    (*
     * C thinks it's a 4-byte int.
     *)
    opt_len := Word.And(opt_len,16_ffffffff);
    IF (opt_len <= 0) OR (opt_len > 16) THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    Translation.Read(proc, opt_val, SUBARRAY(optbuf, 0, opt_len));
    Socket.Setsockopt(NARROW(fh.h,SocketFile).socket, level, opt_name,
		      SUBARRAY(optbuf,0,opt_len));
    RETURN 0;
  END SetSockOpt;

PROCEDURE GetPeerName(fd: INTEGER; addr: INTEGER; addr_len: INTEGER)
	: INTEGER RAISES {VMError.E, Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
    caddr_len: CARDINAL;
    space := proc;
    sr: SocketAddr.T;
    intlen: Ctypes.int;
  BEGIN
    fh := Proc.FindFH(proc, fd);
    IF NOT ISTYPE(fh.h,SocketFile) THEN
      RAISE Errno.E(ErrnoDep.ENOTSOCK);
    END;
    Translation.Read(space, addr_len, VIEW(intlen,ARRAY OF CHAR));
    IF intlen <= 0 THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    
    Socket.Getpeername(NARROW(fh.h,SocketFile).socket, sr, caddr_len);

    Translation.Write(space, VIEW(sr, ARRAY OF CHAR), addr);
    Translation.Write(space, VIEW(intlen, ARRAY OF CHAR), addr_len);
    RETURN 0;
  END GetPeerName;

PROCEDURE ShutDown(fd: INTEGER; how: INTEGER)
	: INTEGER RAISES {Errno.E} =
  VAR
    fh: OpenFile.T;
    proc: Proc.T := Translation.GetCurrent();
  BEGIN
    fh := Proc.FindFH(proc, fd);
    
    IF NOT ISTYPE(fh.h,SocketFile) THEN
      RAISE Errno.E(ErrnoDep.ENOTSOCK);
    END;
    IF (how < 0) OR (how > 2) THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    
    Socket.Shutdown(NARROW(fh.h,SocketFile).socket, how);
    RETURN 0;
  END ShutDown;

(*
 * Socket ioctls are peculiar creatures since sockets
 * are not just file-type thingies.  They also front
 * for the network interfaces that sockets allow
 * access to.  So we pretend we are just like the
 * big boys and play it their way.  FIONBIO is the
 * only file-level ioctl on sockets, the rest get
 * shuffled off to Buffalo.
 *)

PROCEDURE SocketIoctl( self: SocketFile;
		      iocmd: INTEGER;
		      VAR iocdata: ARRAY OF CHAR;
		      <*UNUSED*>flags: INTEGER) RAISES { Error.E } =
(*
  VAR
    errno: INTEGER;
    so: SocketRep.socketT;
    ifreq: If.ifreq;
    ifconf: If.ifconf;
    n_ifs: INTEGER;
    outaddr: INTEGER;
    oldStyle: BOOLEAN;
*)
  BEGIN
    IF iocmd = IoctlPosix.FIONBIO THEN (* set/clear non-blocking i/o *)
      self.async := (VIEW(iocdata, Ctypes.int) # 0);
      RETURN;
    END;
    IF iocmd = IoctlPosix.FIONREAD THEN (* get number of bytes to read *)
      VIEW(iocdata,Ctypes.int) := Socket.Nread(self.socket);
      RETURN;
    END;
    
    (* Now, what I really *should* do here is call over to
       Socket.SocketIoctl(....) and let it do its thing (e.g., FIONREAD,
       FIOASYNC) perhaps calling further down into interface
       or route.  But what I'm currently interested in is getting
       X to work.  And I suspect I would get an earful about splattering
       krufty Unix stuff all over the pristine glory of SPIN.  So
       for we'll just take a shortcut over to IfUtils. *)
    IF URTIoctl.IOCGROUP(iocmd) # ORD('i') THEN
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EOPNOTSUPP));
    END;
    
    (*
       * Check the command, since we are only willing to do a few of
       * them.  Warning: there are huge differences in what
       * the ioctl commands look like, depending on the
       * compile time flags for a program.  The OSF code that we use
       * (e.g. netinet/in.c) implements only ``new'' style sockaddrs
       * with an explicit length field.  So if we 
       * stumble across one of the OSIO versions that some programs
       * use, translate from new to old style.  Since the Ioctl values
       * in M3 land are variables rather than constants, this can be
       * tedious.
    *)
    
    IF ( (iocmd # IoctlPosix.SIOCGIFFLAGS) AND
	(iocmd # IoctlPosix.SIOCGIFCONF) AND
	(iocmd # IoctlPosix.SIOCGIFBRDADDR) AND
	(iocmd # IoctlPosix.OSIOCGIFCONF) AND
	(iocmd # IoctlPosix.OSIOCGIFBRDADDR) ) THEN
      RAISE Error.E(NEW(Error.T).init(ErrnoDep.EOPNOTSUPP));
    END;
    
    RAISE Error.E(NEW(Error.T).init(ErrnoDep.EOPNOTSUPP));
    (* XXX IfUtils no long supported
       IF (iocmd # IoctlPosix.SIOCGIFCONF) AND
       (iocmd # IoctlPosix.OSIOCGIFCONF) THEN
       ifreq := VIEW(iocdata^,If.ifreq);
       errno := IfUtil.IfIoctl(so, iocmd, ifreq);
       VIEW(iocdata,If.ifreq) := ifreq;
       IF errno # 0 THEN
       RAISE Error.E(NEW(Error.T).init(errno));
       END;
       RETURN;
       END;
       
(*
   * They want a list of interfaces.  Since we are copying
   * stuff out we iterate by calling GetIfReq rather
   * that WalkIf where we would either have to modify the
   * closure signature or do some nasty stuff with global
   * variables.
*)
    oldStyle := (iocmd = IoctlPosix.OSIOCGIFCONF);
    ifconf := VIEW(iocdata,If.ifconf);
    IF ifconf.ifc_len < BYTESIZE(ifreq) THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    n_ifs := 0;
    outaddr := VIEW(ifconf.ifc_buf,INTEGER);
    WHILE IfUtil.GetIfReq(n_ifs, ifreq, oldStyle) DO
      TRY
	Translation.Write(s,VIEW(ifreq, ARRAY OF CHAR), outaddr);
      EXCEPT
      | VMError.E =>
	RAISE Errno.E(ErrnoDep.EFAULT);
      END;
      INC(outaddr,BYTESIZE(ifreq));
      INC(n_ifs);
      DEC(ifconf.ifc_len,BYTESIZE(ifreq));
      IF ifconf.ifc_len < BYTESIZE(ifreq) THEN
	EXIT;
      END;
    END;
    ifconf.ifc_len := n_ifs * BYTESIZE(ifreq);
    VIEW(iocdata,If.ifconf) := ifconf;
    *)
  END SocketIoctl;
  
BEGIN
  mbufMethods := NEW(Mbuf.Methods );
  mbufMethods.free := NIL;
  mbufMethods.csum := NIL;

  swMethods := NEW(Mbuf.Methods );
  swMethods.free := swFreeProc;
  swMethods.csum := NIL;
  IF SphinxUtils.MicroBench THEN
    acceptSpy := Spy.Create("accept",FALSE,400);
  END;
END Sockets.
