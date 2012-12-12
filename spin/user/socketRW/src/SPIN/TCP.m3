(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Tue Feb  7 15:48:33 PST 1995 by kalsow *)
(*      modified on Wed Aug 31 15:57:17 PDT 1994 by wobber *)
(*      modified on Fri Jan  7 13:31:11 PST 1994 by msm    *)
(*      modified on Sun Jan 12 16:16:54 PST 1992 by meehan *)
(*      modified on Sat Jan 11 16:55:00 PST 1992 by gnelson *)

(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 09-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up types related to sockets.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Changed integer 0 to SocketRep.SIN_ZERO.
 *
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Made to use the Plexus spindle and Socket interfaces.
 *
 *)

MODULE TCP;

IMPORT Atom, AtomList, IP, Rd, Wr;
IMPORT Errno, ErrnoDep, TCPPosix, Fmt, Net;
IMPORT EasySocket, Socket, SocketRep, SocketAddr, SocketAddrIn;
IMPORT IO, Ctypes;

REVEAL
  Connector = MUTEX BRANDED "TCP.Connector" OBJECT
    s : Socket.T;       (*CONST*)
    ep: IP.Endpoint;   (*CONST*)
    closed: BOOLEAN := FALSE;
  END;

REVEAL
  T = TCPPosix.Public BRANDED "TCP.T" OBJECT
    ep: IP.Endpoint;
    error: AtomList.T := NIL;
  OVERRIDES
    get := GetBytesFD;
    put := PutBytesFD;
    shutdownIn := ShutdownIn;
    shutdownOut := ShutdownOut;
    close := Close;
  END;

VAR Unexpected: Atom.T;
    ClosedErr: AtomList.T;

PROCEDURE NewConnector (ep: IP.Endpoint): Connector RAISES {IP.Error} =
  VAR
    res                := NEW(Connector, ep := ep);
    name  : SocketAddrIn.T;
  BEGIN
    TRY
      res.s := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0 (* TCP*));
    EXCEPT
    | Errno.E(errno) => 
      IO.Put("Failed to Create\n");
      RAISE IP.Error(MapError(errno));
    END;

    name.sin_len := BYTESIZE(name);
    name.sin_family := SocketRep.AF_INET;
    name.sin_port := Net.htons(ep.port);
    name.sin_addr := VIEW(ep.addr,  BITS 32 FOR Ctypes.unsigned_int);
    name.sin_zero := SocketAddrIn.SIN_ZERO;
    TRY
      Socket.Bind(res.s, name);
    EXCEPT
    | Errno.E(errno) => 
      IO.Put("Failed to Bind " & Fmt.Int(errno) & "\n");
      RAISE IP.Error(MapError(errno));
    END;
    
    TRY
      Socket.Listen(res.s, 1);
    EXCEPT
    | Errno.E(errno) => 
      IO.Put("Failed to Listen " & Fmt.Int(errno) & "\n");
      RaiseUnexpected(); 
    END;
    
    RETURN res
  END NewConnector;

(* GetSockName is not implemented yet. *)
PROCEDURE GetEndPoint(c: Connector): IP.Endpoint =
  VAR
    name  : SocketAddr.T;
    len   : CARDINAL;
  BEGIN
    IF c.ep.addr = IP.NullAddress THEN
      c.ep.addr := IP.GetHostAddr();
    END;
    IF c.ep.port = IP.NullPort THEN
      TRY
        Socket.Getsockname(c.s, name, len);
      EXCEPT
        Errno.E => (* Ignore *)
      END; 
      c.ep.port := Net.nstoh(VIEW(name, SocketAddrIn.T).sin_port);
    END;
    RETURN c.ep
 END GetEndPoint; 

PROCEDURE Connect (ep: IP.Endpoint): T RAISES {IP.Error} =
  VAR
    s : Socket.T;
    thatEnd: SocketAddrIn.T;
  BEGIN
    TRY
      s := Socket.Create(SocketRep.AF_INET, SocketRep.SOCK_STREAM, 0 (* TCP*));
    EXCEPT
    | Errno.E(errno) => 
      IO.PutError("Failed to Create\n");
      RAISE IP.Error(MapError(errno));
    END;

    (* Fill in the sockaddr_in record and connect. *)
    thatEnd.sin_len := BYTESIZE(thatEnd);
    thatEnd.sin_family := SocketRep.AF_INET;
    thatEnd.sin_port := Net.htons(ep.port);
    thatEnd.sin_addr := VIEW(ep.addr, Ctypes.unsigned_int);
    thatEnd.sin_zero := SocketAddrIn.SIN_ZERO;
    
    TRY
      Socket.Connect(s, thatEnd);
    EXCEPT
    | Errno.E(errno) => 
      IO.PutError("Failed to Connect with error " & Fmt.Int(errno) & "\n");
      RAISE IP.Error(MapError(errno));
    END;

    RETURN NEW(T, s := s, ep := ep);
  END Connect;

PROCEDURE Accept (c: Connector): T RAISES {IP.Error} =
  VAR
    newS : Socket.T;
  BEGIN
    LOOP
      LOCK c DO
        IF c.closed THEN RaiseNoEC(Closed); END;
        TRY
          newS := Socket.Accept(c.s);
          RETURN NEW(T, s := newS, ep := IP.NullEndPoint);
        EXCEPT
        | Errno.E(errno) => 
          IO.Put("Failed to Accept " & Fmt.Int(errno) & "\n");
          RAISE IP.Error(MapError(errno));
        END;
      END;
    END;
  END Accept;

PROCEDURE CloseConnector(c: Connector) =
  BEGIN
    TRY
      Socket.Close(c.s);
    EXCEPT
    ELSE
    END;
    c.closed := TRUE;
  END CloseConnector;

(* methods of TCP.T *)

PROCEDURE Close(t: T) =
  BEGIN
    LOCK t DO
      IF NOT t.closed THEN
        TRY
          Socket.Close(t.s);
        EXCEPT
        ELSE
        END;
        t.closed := TRUE;
        t.error := ClosedErr;
      END;
    END;
  END Close;
  
(* Will use the Socket.Select interface in the future. *)

PROCEDURE GetBytesFD(
    t: T; VAR arr: ARRAY OF CHAR; <*UNUSED*> timeout: INTEGER) : CARDINAL
    RAISES {Rd.Failure} =
  VAR 
    len: INTEGER := 0;  
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
      TRY
        len := EasySocket.Recv(t.s, arr);
      EXCEPT
      | Errno.E(errno) =>
        (* CONNRESET error is handled differently between Get and Put *)
        (* other errors are the same. *)
        CASE errno OF
        | ErrnoDep.ECONNRESET => 
          RETURN 0;
        ELSE
          SetError(t, errno);
        END;
      END;
    END;
    RETURN len;
  END GetBytesFD;

PROCEDURE PutBytesFD(t: T; VAR arr: ARRAY OF CHAR) RAISES {Wr.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
      TRY 
        EasySocket.Send(t.s, arr);
      EXCEPT
      | Errno.E(errno) =>
        IO.PutError("PutBytes: " &Errno.Fmt(errno) & "\n");
        SetError(t, errno);
      END;
    END;
  END PutBytesFD;

VAR lastErrorMu := NEW(MUTEX);
    lastErrors: ARRAY [0..19] OF INTEGER;
    lastErrorPos: CARDINAL := 0;

PROCEDURE SetError(t: T; errno: Errno.T) =
  BEGIN
    LOCK t DO
      t.error := MapError(errno);

      LOCK lastErrorMu DO
        lastErrors[lastErrorPos] := errno;
        INC(lastErrorPos);
        IF lastErrorPos >= NUMBER(lastErrors) THEN lastErrorPos := 0; END;
      END;
    END;
  END SetError;

PROCEDURE MapError(errno: INTEGER): AtomList.T =
  BEGIN
    WITH eAtom = Atom.FromText(Fmt.Int(errno)) DO
      CASE errno OF
      | ErrnoDep.EPIPE, ErrnoDep.ECONNRESET, ErrnoDep.ENETRESET => 
        RETURN AtomList.List2(ConnLost, eAtom);
      | ErrnoDep.ETIMEDOUT => 
        RETURN AtomList.List2(Timeout, eAtom);
      | ErrnoDep.ENETUNREACH, ErrnoDep.EHOSTUNREACH, ErrnoDep.EHOSTDOWN, 
        ErrnoDep.ENETDOWN => 
        RETURN AtomList.List2(IP.Unreachable, eAtom);
      ELSE
        RETURN AtomList.List2(Unexpected, eAtom);
      END;
    END;
  END MapError;

PROCEDURE ShutdownIn(t: T) RAISES {Rd.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Rd.Failure(t.error); END;
      TRY
        Socket.Shutdown(t.s, 0);
      EXCEPT
      ELSE
      END;
    END;
  END ShutdownIn;

PROCEDURE ShutdownOut(t: T) RAISES {Wr.Failure} =
  BEGIN
    LOCK t DO
      IF t.error # NIL THEN RAISE Wr.Failure(t.error); END;
      TRY
        Socket.Shutdown(t.s, 1);
      EXCEPT
      ELSE
      END;
    END;
  END ShutdownOut;

PROCEDURE Raise(a: Atom.T; errno: Errno.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List2(a, Atom.FromText(Fmt.Int(errno))));
  END Raise;

PROCEDURE RaiseUnexpected() RAISES {IP.Error} =
  BEGIN
    Raise(Unexpected, 0);
  END RaiseUnexpected;

 PROCEDURE RaiseNoEC(a: Atom.T) RAISES {IP.Error} =
  BEGIN
    RAISE IP.Error(AtomList.List1(a));
  END RaiseNoEC;

  
BEGIN
  Refused := Atom.FromText("TCP.Refused");
  Closed := Atom.FromText("TCP.Closed");
  Timeout := Atom.FromText("TCP.Timeout");
  ConnLost := Atom.FromText("TCP.ConnLost");
  Unexpected := Atom.FromText("TCP.Unexpected");
  ClosedErr := AtomList.List1(Closed);
END TCP.

    
