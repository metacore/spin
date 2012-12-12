(*
 * HISTORY
 * 30-Aug-97  Yasushi Saito (yasushi) at the University of Washington
 *	Separated common procs into Socket.m3
 *	
 * 31-May-97  David Becker at the University of Washington
 *      Unify errno exception with Errno.UnixError
 *      Replace SAL with Kernel and MachineCPU interfaces
 *	Redo select to use common Kernel.DoScan
 *
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *      Made T subtype of BSDtty.T to support sphinx's select.
 *
 * 10-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Ported to use FreeBSD socket interface.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 17-Sep-96  becker at the University of Washington
 *	Added Nread to support FIO NREAD ioctl
 *
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	fixed select support.
 *
 * 18-Jun-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up use of Spy timers and added support for socket select.
 *	Socket select needs to be unified with DlibSelect and probably
 *	exported as a generic service by urtcore.
 *
 * 11-Jun-96  Jan Sanislo (oystr) at the University of Washington
 *    Fix sizing glitch in Setsockopt.  Recv now uses flags argument
 *    for non-blocking IO.
 *
 * 29-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed up setsockopt to check the size of the option_value
 *	argument and set the mbuf len field to the size of the
 *	option_value.
 *
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Strongref'ing sockets when doing select().
 *	Moved MSG_XXX constants into interface.
 *
 * 27-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added a return value to CancelAlarm.
 *
 * 19-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added WeakRef support to sockets.
 *	Added checks for valid socket argument.
 *
 * 02-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed mbuf leak in Bind and Connect.
 *      Encapsulated all calls to C with TRY EXCEPTS to isolate
 *      severe failures in the C code.  
 *      Added locking to Accept and Connect.
 *
 * 01-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned out SPL variables and calls.  Removed paranoid calls to
 *	StrongRef.  Now allowing other protocols to be requested via
 *	Create().
 *
 * 14-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed Connect to take a sockaddr_in.
 *
 * 13-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added Getpeername, Getsockname and Setsockopt.
 *      Changed Close() to NIL socket argument.
 *
 * 03-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up socket implemetion to call C interface safely.
 *	Added Bind() and Shutdown() interfaces.  
 *      Removed the ServerSocket type.  
 *
 * 21-Dec-95  Charles Garrett (garrett) at the University of Washington
 *	There was an mbuf leak in internal_accept when passing sodequeue
 *      an mbuf as the third argument.
 *
 * 31-Oct-95  Marc Fiuczynski (mef) at the University of Washington
 *	Put sockaddr_in definition in SocketRep interface.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added exceptions.
 *
 * 21-Aug-95  David Becker (becker) at the University of Washington
 *	Created.
 *
 *)

UNSAFE (* because we are making calls to <* EXTERNALS *> *)
MODULE SocketDep EXPORTS Socket;
IMPORT Ctypes, Errno, Mbuf, MbufPublic, Net, Protosw, SocketExtern,
       SocketAddr, SocketAddrIn, SocketRep, Uio, Word, IO,
       SpinException, Thread;
IMPORT SalDep;
IMPORT WeakRef, StrongRef, Spy; <* NOWARN *>
IMPORT CPU, CPUPrivate;
IMPORT ErrnoDep;
IMPORT BSDtty;

CONST 
  debug  = FALSE;
  useWeakRef = FALSE;

CONST (* Priorities from sys/param.h *)
  PSOCK  = 24;
  PZERO  = 22;<*NOWARN*>
  PCATCH = 16_100;   (* /* implies interruptible sleep */ *)
  pmask : Ctypes.int = Word.Or(PSOCK,PCATCH);

CONST
  SO_DONTROUTE = 16_10; (* just use interface addresses *)

(* Bits for network events to sb_wakeup *)

<* UNUSED *> CONST SE_ERROR    = 16_0001;  (* so_error non-0                      *)
<* UNUSED *> CONST SE_HAVEDATA = 16_0002;  (* data in send or recv q              *)
<* UNUSED *> CONST SE_HAVEOOB  = 16_0004;  (* oob data in recv q                  *)
<* UNUSED *> CONST SE_DATAFULL = 16_0008;  (* send or recv q is full              *)
<* UNUSED *> CONST SE_CONNOUT  = 16_0010;  (* outgoing connect complete (connect) *)
<* UNUSED *> CONST SE_CONNIN   = 16_0020;  (* incoming connect complete (listen)  *)
<* UNUSED *> CONST SE_SENDCONN = 16_0040;  (* connected for send                  *)
<* UNUSED *> CONST SE_RECVCONN = 16_0080;  (* connected for recv                  *)
<* UNUSED *> CONST SE_POLL     = 16_4000;  (* wakeup is synchronous poll          *)
<* UNUSED *> CONST SE_STATUS   = 16_8000;  (* above status bits valid             *)

(* XXX Once the underlying socket code uses the traced allocator we
   can reveal the T directly as a BRANDED REF SocketExtern.Socket; *)
(*
REVEAL T = BRANDED REF RECORD 
  so : SocketExtern.T;
  mu : MUTEX;
  cv : Thread.Condition;
END;
*)

REVEAL T = BSDtty.T BRANDED OBJECT
  so : SocketExtern.T;
  mu : MUTEX;
  cv : Thread.Condition;
OVERRIDES
  selectproc := SelectProc;
  descriptor := Descriptor;
END;

(* internal utilities *)

PROCEDURE NewT(so: SocketExtern.T): T = 
  VAR
    socket : T;
  BEGIN
    socket    := NEW(T);
    socket.so := so;

    (* XXX hack to set the send buffer size to unlimited
       so.sb_snd.sb_lowat := 8192; *)
    so.sb_snd.sb_hiwat := 1024*1024;   (* XXX *)
    so.sb_snd.sb_mbmax := 2*1024*1024; (* XXX *)

    IF useWeakRef THEN
      EVAL WeakRef.FromRef(socket,SafeClose);
    END;

    RETURN socket;    
  END NewT;

PROCEDURE SafeClose(<* UNUSED *> READONLY w: WeakRef.T; r: REFANY) =
  VAR
    socket : T;
  BEGIN
    IO.Put("Socket.SafeClose called.\n");
    TRY
      socket := NARROW(r, T);
      Close(socket);
    EXCEPT
    | Errno.E(err) => IO.PutError("Close: " & Errno.Fmt(err) &"\n");
    | SpinException.Exception =>
      IO.PutError("Serious Failure In SafeClose function.\n"); 
    END;
  END SafeClose;

PROCEDURE SinToMbuf(READONLY sin: SocketAddr.T): Mbuf.T = 
  VAR 
    nam : Mbuf.T;
  BEGIN
    nam := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_SONAME);
    nam.mh_hdr.mh_len := BYTESIZE(sin);
    WITH databuf = Mbuf.Array(nam),
         sa = VIEW(databuf^,SocketAddr.T)
     DO
      sa := sin;
    END;
    RETURN nam;
  END SinToMbuf;

PROCEDURE MbufToSin(nam: Mbuf.T): SocketAddr.T = 
  VAR
    sin: SocketAddr.T;
  BEGIN
    WITH databuf = Mbuf.Array(nam),
         sa = VIEW(databuf^,SocketAddr.T)
     DO
      sin := sa;
    END;
    RETURN sin;
  END MbufToSin;

PROCEDURE Lock(VAR spl: CPU.InterruptLevel) =
  BEGIN
    spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.IO);
  END Lock;

PROCEDURE Unlock(spl: CPU.InterruptLevel) =
  BEGIN
    CPUPrivate.RestoreInterruptMask(spl);
  END Unlock;

PROCEDURE INset(val: Word.T; set: Word.T): BOOLEAN = 
  BEGIN
    RETURN Word.And(val,set) # 0;
  END INset;

(* socket ops *)

PROCEDURE Create(
    addrFamily : CARDINAL; 
    type       : CARDINAL; 
    protocol   : CARDINAL): T RAISES {Errno.E} =
  VAR
    error  : Ctypes.int;
    so     : SocketExtern.T;
  BEGIN
    TRY
      error := SocketExtern.socreate(addrFamily,so,type,protocol);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix socreate.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    IF error#0 THEN RAISE Errno.E(error); END;
    RETURN NewT(so);
  END Create;

PROCEDURE Bind(
    socket:T; 
    READONLY sin: SocketAddrIn.T) RAISES {Errno.E} =
  VAR
    nam    : Mbuf.T;
    error  : Errno.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    WITH sin2 = VIEW(sin,SocketAddr.T) DO
      nam := SinToMbuf(sin2);
    END;
    TRY
      TRY
        error := SocketExtern.sobind(socket.so,nam);
      EXCEPT
      | SpinException.Exception =>
        IO.PutError("Serious Failure In FreeBSD Unix sobind.\n"); 
        RAISE Errno.E(ErrnoDep.EFAULT);
      END;
    FINALLY
      Mbuf.m_freem(nam); (* deallocate the mbuf *)
    END;
    IF error#0 THEN RAISE Errno.E(error); END;
  END Bind;

PROCEDURE Listen(
    socket:T; 
    backlog: CARDINAL) RAISES {Errno.E} = 
  VAR
    error : Errno.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;

    TRY
      error := SocketExtern.solisten(socket.so,backlog);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix solisten.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    IF error#0 THEN RAISE Errno.E(error); END;
  END Listen;

PROCEDURE Accept(socket:T) : T RAISES {Errno.E} =
  VAR
    error : Errno.T;
    so    : SocketExtern.T;
    spl   : CPU.InterruptLevel;

  PROCEDURE internalAccept(internal_so : SocketExtern.T;
                            VAR new_so  : SocketExtern.T; 
                            <* UNUSED *> from        : Mbuf.T) RAISES {Errno.E} =
    VAR
      nam : Mbuf.T;
    BEGIN

      (* XXX still need to do this check.
	if ((so->so_options & SO_ACCEPTCONN) == 0) {
		splx(s);
		return (EINVAL);
	}
	if ((so->so_state & SS_NBIO) && so->so_qlen == 0) {
		splx(s);
		return (EWOULDBLOCK);
	} *)

      (* while (so->so_qlen == 0 && so->so_error == 0) { *)
      WHILE internal_so.so_qlen = 0 AND internal_so.so_error = 0 DO
        (*
          if (so->so_state & SS_CANTRCVMORE) {
             so->so_error = ECONNABORTED;
             break;
          } *)
        IF INset(SocketRep.SS_CANTRCVMORE,internal_so.so_state) THEN
          internal_so.so_error := ErrnoDep.ECONNABORTED;
          EXIT;
        END;

        TRY
          (* error = tsleep((caddr_t)&so->so_timeo, PSOCK | PCATCH,
            netcon, 0); *)
          error := SocketExtern.sosleep(internal_so,
                                        ADR(internal_so.so_timeo),
                                        pmask,0);
        EXCEPT
        | SpinException.Exception =>
          IO.PutError("Serious Failure In FreeBSD Unix sosleep.\n"); 
          RAISE Errno.E(ErrnoDep.EFAULT);
        END;

        (* if (error) {
             splx(s);
             return (error);
           } *)
        IF error # 0 THEN
          IO.PutError(ErrnoDep.ErrorMessages[error] & 
            " Socket.internalAccept failure #1.\n"); 
          RETURN;
        END;
      END;

      (* if (so->so_error) {
		error = so->so_error;
		so->so_error = 0;
		splx(s);
		return (error);
         } *)

      IF internal_so.so_error # 0 THEN
        error := internal_so.so_error;
        internal_so.so_error := 0;
        IO.PutError(ErrnoDep.ErrorMessages[error] & 
          " Socket.internalAccept failure #2.\n");        
        RETURN;
      END;

      TRY
        (* { struct socket *aso = so->so_q;
             if (soqremque(aso, 1) == 0)
                panic("accept");
	     so = aso;
           } *)
        (* XXX this code is wrong! *)
        new_so := internal_so.so_q;
        IF SocketExtern.soqremque(new_so, 1) = 0 THEN
          IO.Put("soqremque failed!\n");
          RAISE Errno.E(ErrnoDep.EINVAL);
        END;
        internal_so := new_so
      EXCEPT
      | SpinException.Exception =>
        IO.PutError("Serious Failure In FreeBSD soqremque.\n"); 
        RAISE Errno.E(ErrnoDep.EFAULT);
      | Errno.E(ec) =>
        RAISE Errno.E(ec);
      END;

      (* XXX I think that we can do w/o the mbuf, but lets be
         conservative here. *)
      nam := Mbuf.m_getclr(Mbuf.M_WAIT, Mbuf.MT_SONAME);
      IF nam = NIL THEN RAISE Errno.E(ErrnoDep.ENOMEM); END;
      TRY
        TRY
          (* XXX EVAL SocketExtern.soaccept(internal_so, nam); *)
          error := SocketExtern.soaccept(internal_so, nam);
        EXCEPT
        | SpinException.Exception =>
          IO.PutError("Serious Failure In FreeBSD Unix soconnect.\n"); 
          RAISE Errno.E(ErrnoDep.EFAULT);
        END
      FINALLY
        Mbuf.m_freem(nam); (* deallocate the mbuf *)
      END;

      IF error # 0 THEN
        IO.PutError(ErrnoDep.ErrorMessages[error] & 
          " Socket.internalAccept failure #3.\n");
      END;
    END internalAccept;

  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    (* error variable set in internalAccept. *)

    Lock(spl);
    TRY
      internalAccept(socket.so, so, NIL);
    FINALLY
      Unlock(spl);
    END;

    IF error#0 THEN RAISE Errno.E(error); END;
    RETURN NewT(so);
  END Accept;


(* Connect takes a sockaddr_in structure and frees the caller from
   having to deal with an Mbuf directly. *)

PROCEDURE Connect(
    socket:T; 
    READONLY sin: SocketAddrIn.T) 
  RAISES {Errno.E} =
  VAR
    nam   : Mbuf.T;
    error : Errno.T;
    spl   : CPU.InterruptLevel;

  PROCEDURE internalConnect(so: SocketExtern.T) 
    RAISES {Errno.E} =
    VAR (* implicit access to 

	   socket: T;
	   READONLY sin: SocketAddrIn.T;
	   error: Errno.T;
           nam: Mbuf.T
	 *)

    <* INLINE *>
    PROCEDURE exitOut() =
      VAR (* implicit access to 

  	     socket: T;
	     so: SocketExtern.T;
	     READONLY sin: SocketAddrIn.T;
	     error: Errno.T;
             nam: Mbuf.T
	   *)
      BEGIN
        (* socket unlock ? *)
        so.so_state := Word.And(so.so_state, Word.Not(SocketRep.SS_ISCONNECTING));
      END exitOut;

    BEGIN
      (* why are we setting the no route flag *)
      so.so_options := Word.Or(so.so_options,SO_DONTROUTE);
      TRY
        error := SocketExtern.soconnect(so,nam);
      EXCEPT
      | SpinException.Exception =>
        IO.PutError("Serious Failure in FreeBSD Unix soconnect.\n"); 
        RAISE Errno.E(ErrnoDep.EFAULT);
      END;
      IF error # 0 THEN
        IO.PutError(ErrnoDep.ErrorMessages[error] & 
          " Socket.internalConnect failure #1.\n");
        exitOut(); RETURN;
      END;

      Lock(spl);
      WHILE INset(SocketRep.SS_ISCONNECTING,so.so_state) AND
        so.so_error = 0 
       DO

        TRY
          error := SocketExtern.sosleep(so, ADR(so.so_timeo),pmask,0);
        EXCEPT
        | SpinException.Exception =>
          IO.PutError("Serious Failure in FreeBSD Unix sosleep.\n"); 
          RAISE Errno.E(ErrnoDep.EFAULT);
        END;
         IF error # 0 THEN EXIT; END;
         IF debug THEN
           IO.PutError("Socket.internalConnect wakeup ");
           IO.Putx(so.so_state);
         END;
       END;

      IF error = 0 THEN
        error := so.so_error;
        so.so_error := 0;
      ELSE
        IO.PutError(ErrnoDep.ErrorMessages[error] & 
          " Socket.internalConnect failure #2.\n");
      END;
      Unlock(spl);
      exitOut(); RETURN;
    END internalConnect;

  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;

    WITH sin2 = VIEW(sin,SocketAddr.T) DO
      nam := SinToMbuf(sin2);
    END;
    TRY
      (* error variable set in internalConnect. *)
      internalConnect(socket.so);
    FINALLY
      Mbuf.m_freem(nam); (* deallocate the mbuf *)
    END;
    IF error # 0 THEN RAISE Errno.E(error); END;
  END Connect;

PROCEDURE SendInternal(
    so: SocketExtern.T; 
    nam: Mbuf.T;
    data: Mbuf.T;
    flags: CARDINAL) RAISES {Errno.E} =
  VAR
    error     : Errno.T;
    resid     : CARDINAL;
    space     : CARDINAL;
    atomic    : BOOLEAN;
    dontroute : BOOLEAN;
    spl       : CPU.InterruptLevel;

  <* INLINE *>
  PROCEDURE internalSbspace(VAR sb: SocketRep.sockbufT): CARDINAL = 
    VAR space : INTEGER;
    BEGIN
      space := MIN(sb.sb_hiwat-sb.sb_cc, sb.sb_mbmax - sb.sb_mbcnt);
      IF space >= 0 THEN RETURN space; ELSE RETURN 0; END;
    END internalSbspace;

  <* INLINE *>
  PROCEDURE internalOut() RAISES {Errno.E} = 
    VAR (* from outer scope
           data  : Mbuf.T;
           error : Errno.T;
        *)
    BEGIN
      IF data # NIL THEN Mbuf.m_freem(data); END;
      IF error # 0 THEN RAISE Errno.E(error); END;
    END internalOut;

  <* INLINE *>
  PROCEDURE internalRelease() RAISES {Errno.E} = 
    VAR (* from outer scope
           resid : CARDINAL;
           so: SocketExtern.T;
        *)
    BEGIN
      IF INset(Protosw.PR_SEQPACKET,LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_flags) THEN
        IF resid = 0 THEN 
          so.sb_snd.sb_lowat := 1;
        END;
      END;
      TRY
        SocketExtern.sosbunlock(so.sb_snd);
      EXCEPT
      | SpinException.Exception =>
        IO.PutError("Serious Failure in FreeBSD Unix sbunlock function.\n"); 
        RAISE Errno.E(ErrnoDep.EFAULT);
      END;
      internalOut();
    END internalRelease;

  <* INLINE *>
  PROCEDURE internalSnderr(errno: Errno.T) RAISES {Errno.E} = 
    VAR (* from outer scope
           error : Errno.T;
        *)
    BEGIN
      error := errno; 
      internalRelease();
    END internalSnderr;

  BEGIN

    (* resid = top->m_pkthdr.len; *)
    resid := MbufPublic.GetPktHdrLen(data);

    TRY
      (* error = sblock(&so->so_snd, SBLOCKWAIT(flags)); *)
      (* XXX have to fix for nonblock support *)
      error := SocketExtern.sosblock(so.sb_snd,so);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure in FreeBSD Unix sosblock function.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    (*
      	if (error)
		goto out;
    *)
    IF error # 0 THEN internalOut(); END;

    Lock(spl);
    TRY
      IF INset(SocketRep.SS_CANTSENDMORE,so.so_state) THEN
        internalSnderr(ErrnoDep.EPIPE); 
      END;

      IF so.so_error # 0 THEN
        error := so.so_error;
        so.so_error := 0;
        internalRelease(); 
      END;

      IF NOT INset(SocketRep.SS_ISCONNECTED, so.so_state) THEN
        IF INset(Protosw.PR_CONNREQUIRED,LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_flags) THEN
          internalSnderr(ErrnoDep.ENOTCONN); RETURN;
        ELSIF nam = NIL THEN
          internalSnderr(ErrnoDep.EDESTADDRREQ); RETURN;
        END;
      END;

      IF INset(SocketRep.MSG_EOR,flags) THEN
        data.mh_hdr.mh_flags := data.mh_hdr.mh_flags + Mbuf.M_EOR; (* XXX *)
      END;

      space := internalSbspace(so.sb_snd);

      IF INset(SocketRep.MSG_OOB,flags) THEN
        INC(space,1024);
      END;

      (* do we have to send all at once on a socket? *)
      atomic := INset(Protosw.PR_ATOMIC,LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_flags);

      IF space < resid AND ( atomic OR space < so.sb_snd.sb_lowat ) THEN 
        IF atomic OR resid > so.sb_snd.sb_hiwat THEN
          internalSnderr(ErrnoDep.EMSGSIZE); RETURN;
        END;
      
        (*
          IF SS_NBIO IN so.so_state OR MSG_NONBLOCK IN flags THEN
          IF Protosw.PR_SEQPACKET IN LOOPHOLE(so.so_proto,Protosw.T).pr_flags THEN
          so.sb_snd.sb_lowat := resid;
          END;
          internalSnderr(ErrnoDep.EWOULDBLOCK); RETURN;
          END;
        *)
        TRY
        error := SocketExtern.sosbwait(so.sb_snd, so);
        EXCEPT 
        | SpinException.Exception => 
          IO.PutError("Serious Failure FreeBSD Unix sosbwait function.\n"); 
          RAISE Errno.E(ErrnoDep.EFAULT);
        END;
        IF error # 0 THEN internalOut(); END;
      END;

      (* XXX *)
      dontroute := TRUE; 
      (*
        (flags & MSG_DONTROUTE) && (so->so_options & SO_DONTROUTE) == 0 &&
        (so->so_proto->pr_flags & PR_ATOMIC);
      *)
      IF dontroute THEN so.so_options := so.so_options + SO_DONTROUTE; END;

      TRY
        error := LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_usrreq(so, 
                                     (* msg_oob? THEN PRU_SENDOOB ELSE PRU_SEND, *)
                                     (* XXX hack *) Protosw.PRU_SEND,
                                     data, nam, NIL);
        data := NIL;
      EXCEPT
      | SpinException.Exception => 
        IO.PutError("Serious Failure in FreeBSD Unix pr_usrreq function.\n"); 
        RAISE Errno.E(ErrnoDep.EFAULT);
      END;

      IF dontroute THEN so.so_options := so.so_options - SO_DONTROUTE; END;

      internalRelease(); (* calls internalOut(); *)
    FINALLY
      Unlock(spl);
    END;
  END SendInternal;

PROCEDURE Send(
    socket:T; 
    data: Mbuf.T) RAISES {Errno.E} =
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    SendInternal(socket.so, NIL, data, 0);
  END Send;

PROCEDURE Sendto(
    socket: T; 
    data: Mbuf.T; 
    flags: CARDINAL := 0; 
    READONLY dest: SocketAddr.T) RAISES {Errno.E} =
  VAR
    nam: Mbuf.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    nam := SinToMbuf(dest);
    TRY
      SendInternal(socket.so, nam, data, flags);
    FINALLY
      EVAL Mbuf.m_free(nam);
    END;
  END Sendto;

PROCEDURE Recv(
    socket   : T; 
    VAR data : Mbuf.T; 
    bytes    : CARDINAL;
    flags    : CARDINAL := 0): CARDINAL RAISES {Errno.E} =
  VAR
    auio   : Uio.uioT;
    error  : Errno.T;
    cflags : Ctypes.int;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    auio.uio_resid := VAL(bytes, Ctypes.int);
    cflags := VAL(flags,Ctypes.int);
    
    TRY
      error := SocketExtern.soreceive(socket.so, NIL, auio, data, 
		NIL, cflags);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure in FreeBSD Unix soreceive.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;

    IF error#0 THEN RAISE Errno.E(error); END;
    RETURN bytes - auio.uio_resid;
  END Recv;

PROCEDURE Recvfrom(
    socket: T;
    VAR data: Mbuf.T;
    bytes: CARDINAL;
    flags: CARDINAL := 0;
    VAR address: SocketAddr.T): CARDINAL RAISES {Errno.E} =
  VAR
    auio : Uio.uioT;
    error : Errno.T;
    from  : Mbuf.T;
    so : SocketExtern.T;
    cflags : Ctypes.int;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    so := socket.so;

    auio.uio_resid := VAL(bytes, Ctypes.int);
    from := NIL;

    (* debug  *)
    IF INset(Protosw.PR_ADDR,LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_flags) = FALSE THEN
      IO.PutError("PR_ADDR not set in socket.\n");
    END;
    (* debug  *)

    TRY
      cflags := VAL(flags,Ctypes.int);
      error := SocketExtern.soreceive(so, from, auio, data, NIL,cflags);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure in FreeBSD Unix soreceive.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    IF error # 0 THEN
      IF from # NIL THEN EVAL Mbuf.m_free(from); END;
      RAISE Errno.E(error); 
    END;
    address := MbufToSin(from);
    EVAL Mbuf.m_free(from);

    RETURN bytes - auio.uio_resid;
  END Recvfrom;

PROCEDURE Close(VAR socket:T) RAISES {Errno.E}=
  VAR 
    error : Errno.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RETURN; END;

    TRY
      error := SocketExtern.soclose(socket.so);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix soclose.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    socket.so := NIL; (* invalidate access to underlying bsd socket. *)
    socket := NIL; (* Don't want to give out reference to closed socket *)
    IF error#0 THEN RAISE Errno.E(error); END;
  END Close;

PROCEDURE Shutdown(socket:T; how: [0..2]) RAISES {Errno.E} =
  VAR
    error : Errno.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;

    TRY
      error := SocketExtern.soshutdown(socket.so, how);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix shutdown.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    IF error#0 THEN RAISE Errno.E(error); END;
  END Shutdown;

PROCEDURE GetName(
    socket: T; 
    VAR address: SocketAddr.T; 
    VAR len: CARDINAL; 
    pru: CARDINAL) RAISES {Errno.E} =
  VAR
    error: Errno.T;
    m: Mbuf.T;
    so : SocketExtern.T := socket.so;
  BEGIN
    m := Mbuf.m_getclr(Mbuf.M_WAIT, Mbuf.MT_SONAME);
    (* SOCKET_LOCK(socket.so) *)
    TRY
      error := LOOPHOLE(so.so_proto,UNTRACED REF Protosw.T).pr_usrreq(socket.so, pru, NIL, m, NIL);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix pr_usrreq getname.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    (* SOCKET_UNLOCK(socket.so) *)
    IF error = 0 THEN
      IF len > m.mh_hdr.mh_len THEN len := m.mh_hdr.mh_len END;      
      WITH buf = SUBARRAY(Mbuf.Array(m)^,0,BYTESIZE(address)) DO
        address := VIEW(buf,SocketAddr.T);
      END;
      EVAL Mbuf.m_free(m);
    ELSE
      EVAL Mbuf.m_free(m);
      RAISE Errno.E(error); 
    END;
  END GetName;

PROCEDURE Getsockname (
    socket: T; 
    VAR address: SocketAddr.T; 
    VAR len: CARDINAL) RAISES {Errno.E} =
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    GetName(socket,address,len,Protosw.PRU_SOCKADDR);
  END Getsockname;

PROCEDURE Getpeername (
    socket: T; 
    VAR address: SocketAddr.T; 
    VAR len: CARDINAL) RAISES {Errno.E} =
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    GetName(socket,address,len,Protosw.PRU_PEERADDR);
  END Getpeername;

PROCEDURE Setsockopt (
    socket: T; 
    level: CARDINAL; 
    option_name: CARDINAL; 
    VAR option_value: ARRAY OF Net.BYTE) RAISES {Errno.E} =
  VAR
    error: Errno.T;
    m: Mbuf.T;
  BEGIN
    IF socket = NIL OR socket.so = NIL THEN RAISE Errno.E(ErrnoDep.EINVAL); END;
    IF BYTESIZE(option_value) > Mbuf.MLEN THEN
      RAISE Errno.E(ErrnoDep.EINVAL);
    END;
    m := Mbuf.m_get(Mbuf.M_WAIT, Mbuf.MT_SOOPTS); (* freed in sosetopt *)
    m.mh_hdr.mh_len := BYTESIZE(option_value);
    WITH buf = Mbuf.Array(m) DO
      SUBARRAY(buf^,0,BYTESIZE(option_value)) := option_value;
    END;
      
    TRY
      error := SocketExtern.sosetopt(socket.so, level, option_name, m);
    EXCEPT
    | SpinException.Exception =>
      IO.PutError("Serious Failure In FreeBSD Unix sosetopt.\n"); 
      RAISE Errno.E(ErrnoDep.EFAULT);
    END;
    IF error # 0 THEN RAISE Errno.E(error); END;
  END Setsockopt;

PROCEDURE SelectProc (<*UNUSED*>t: T): SalDep.SelectProc =
  BEGIN
    RETURN SocketExtern.soo_select;
  END SelectProc;

PROCEDURE Descriptor (t:T): Word.T =
  BEGIN
    RETURN LOOPHOLE(t.so, Word.T);
  END Descriptor;
  
(* Nread is the FIONREAD soo_ioctl from bsd/sys_socket.c *)
PROCEDURE Nread(socket : T): INTEGER =
  BEGIN
    RETURN socket.so.sb_rcv.sb_cc;
  END Nread;

BEGIN
END SocketDep.
