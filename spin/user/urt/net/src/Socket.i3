(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 25-Jul-97  Tsutomu Owa (owa) at the University of Washington
 *	Made T subtype of BSDtty.T to support sphinx's select.
 *
 * 31-May-97  David Becker at the University of Washington
 *      Unify errno exception with Errno.UnixError
 *
 * 17-Sep-96  becker at the University of Washington
 *	Added Nread to support FIO NREAD ioctl
 #
 * 02-Jul-96  Marc Fiuczynski (mef) at the University of Washington
 *	Fixed select support.
 *
 * 12-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up Select support.
 *
 * 15-May-96  Marc Fiuczynski (mef) at the University of Washington
 *	Provided MSG_XXX constants for flags passed to the various socket
 *	functions.
 *
 * 02-Mar-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added Sendto and Recvfrom.
 *
 * 14-Feb-96  Charles Garrett (garrett) at the University of Washington
 *	Changed Connect to take a sockaddr_in.
 *
 * 08-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Added Getsockname, Setsockopt and Getpeername.
 *      Changed Close to take socket as VAR parameter.
 *
 * 03-Jan-96  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up interface to closely resemble the unix interface.
 *	Added Bind() and Shutdown() interfaces.  
 *      Removed the ServerSocket type.  
 *      Passing an invalid T will now raise an exception.
 *
 * 23-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added exceptions.
 *
 * 21-Aug-95  David Becker (becker) at the University of Washington
 *	Created.
 *)

INTERFACE Socket;
IMPORT Net, Mbuf, SocketAddr, SocketAddrIn, Errno;
IMPORT BSDtty;	(* XXX owa *)

(* XXX owa
TYPE T <: REFANY;
 *)
TYPE T <: BSDtty.T;

PROCEDURE Create(
    addrFamily: CARDINAL;
    type: CARDINAL;
    protocol: CARDINAL): T RAISES {Errno.E};
(* The Create() function is used to create a socket of the specified
   type in the specified addrFamily. It returns T that can be used in
   later system calls that operate on sockets.

   Upon successful completion, the Create() function returns a new
   Socket.T.  Otherwise, an exception is raised an the error condition
   is set to indicate the error.  
*)

PROCEDURE Bind(
    socket: T;
    READONLY sin: SocketAddrIn.T) RAISES {Errno.E};
(* The Bind() function assigns an address to an unnamed
   socket. Sockets created with the Create() function are unnamed;
   they are identified only by their address family.

   Upon successful completion, the Bind() function returns. Otherwise,
   an exception is raised an the error condition is set to indicate the
   error.  
*)

PROCEDURE Listen(
    socket:T;
    backlog: CARDINAL) RAISES {Errno.E};
(* The Listen() function identifies the socket that receives the
   connections, marks the socket as accepting connections, and limits
   the number (backlog) of outstanding connection requests in the
   system queue.

   Upon successful completion, the Listen() function returns. Otherwise,
   an exception is raised an the error condition is set to indicate the
   error.  
*)


PROCEDURE Accept(socket: T): T RAISES {Errno.E};
(* The Accept() function extracts the first connection on the queue of
   pending connections, creates a new socket with the same properties
   as the specified socket, and allocates a new file descriptor for
   that socket.

   Upon successful completion, the Listen() function returns a new
   Socket.T. Otherwise, an exception is raised an the error condition
   is set to indicate the error.
*)

PROCEDURE Connect(
    socket: T; 
    READONLY sin: SocketAddrIn.T) RAISES {Errno.E};
(* The Connect() function requests a connection between two sockets.
   The system sets up the communications links between the sockets;
   both sockets must use the same address format and protocol.

   Upon successful completion, the Connect() function returns.
   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)

PROCEDURE Send(
    socket: T;
    data: Mbuf.T) RAISES {Errno.E};
(* The Send() function sends a message on a connected socket. 

   Upon successful completion, the Send() function returns and the
   mbuf is freed by the callee.

   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)

PROCEDURE Sendto(
    socket : T;
    data   : Mbuf.T;
    flags  : CARDINAL := 0;
    READONLY dest: SocketAddr.T) RAISES {Errno.E};
(* The Sendto() function allows an applicaiton program to send
   messages though an unconnected socket by specifying a destination
   address and the mbuf is freed by the callee.

   Upon successful completion, the Send() function returns. Otherwise,
   an exception is raised an the error condition is set to indicate
   the error.  
*)

PROCEDURE Recv(
    socket: T;
    VAR data: Mbuf.T;
    bytes: CARDINAL;
    flags: CARDINAL := 0): CARDINAL RAISES {Errno.E};
(* The Recv() function receives messages from a connected socket. 

   Upon successful completion, the Recv() function returns the 
   message as an mbuf chain, and the total number of bytes in
   the mbuf chain is returned. Otherwise, an exception is 
   raised an the error condition is set to indicate the error.

   The caller is expected to free the mbuf.

*)

PROCEDURE Recvfrom(
    socket: T;
    VAR data: Mbuf.T;
    bytes: CARDINAL;
    flags: CARDINAL := 0;
    VAR address: SocketAddr.T): CARDINAL RAISES {Errno.E};
(* The Recvfrom() function permits an application program to receive
   messages from unconnected sockets. It is normally applied to
   unconnected sockets because it includes parameters that permit a
   calling program to retrieve the source endpoint of received data. 

   Upon successful completion, the  Recvfrom() function returns the 
   message as an mbuf chain, and the total number of bytes in
   the mbuf chain is returned.  Otherwise, an exception is 
   raised an the error condition is set to indicate the error.

   The caller is expected to free the mbuf.

*)

PROCEDURE Close(VAR socket: T) RAISES {Errno.E};
(* The Close() function closes the connection associated with the
   socket.

   Upon successful completion, the Close() function NILs out the
   socket.  Otherwise, an exception is raised an the error condition
   is set to indicate the error.
*)

PROCEDURE Shutdown(
    socket: T;
    how: [0..2]) RAISES {Errno.E};
(* The Shutdown() function disables receive and/or send operations on
   the specified socket.

   Upon successful completion, the Shutdown() function returnsq.
   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)

PROCEDURE Getsockname(
    socket: T;
    VAR address: SocketAddr.T;
    VAR len: CARDINAL) RAISES {Errno.E};
(* The Getsockname() function retrieves the locally bound address of
   the specified socket.

   Upon successful completion, the Getsockname() function returns.
   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)

PROCEDURE Getpeername(
    socket: T;
    VAR address: SocketAddr.T;
    VAR len: CARDINAL) RAISES {Errno.E};
(* The Getpeername() function retrieves the name of the peer socket
   connected to the specified socket.

   Upon successful completion, the Getpeername() function returns.
   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)


PROCEDURE Setsockopt(
    socket: T;
    level: CARDINAL;
    option_name: CARDINAL;
    VAR option_value: ARRAY OF Net.BYTE) RAISES {Errno.E};
(* The Setsockopt() function sets options associated with a socket. 

   Upon successful completion, the Setsockopt() function returns.
   Otherwise, an exception is raised an the error condition is set to
   indicate the error.  
*)

(* XXX clean up naming *)
TYPE selectReadyState = { read, write, except };
TYPE selectReadyStates = SET OF selectReadyState;
TYPE readyDes = RECORD
  socket : T;
  ready  : selectReadyStates;
END;

PROCEDURE Select( 
    VAR sockets : ARRAY [1..1] OF readyDes;
    sec: CARDINAL;
    usec: CARDINAL): CARDINAL RAISES {Errno.E};

PROCEDURE Nread(socket : T): INTEGER RAISES {Errno.E};
(*
 * Nread returns number of bytes available for non-block read.
 *)

END Socket.

