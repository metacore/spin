(* HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace Socket.Error with new Errno exception.
 *
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	An interface designed to hide the use of Mbuf's in the Socket
 *      code. This is only acceptable if you are willing to take the
 *      performance hit of an extra copy.
 *
 *)

INTERFACE EasySocket;
IMPORT Socket, Errno;

PROCEDURE Recv(s: Socket.T; VAR arr: ARRAY OF CHAR): INTEGER RAISES {Errno.E};

PROCEDURE Send(s: Socket.T; READONLY arr: ARRAY OF CHAR) RAISES {Errno.E};

END EasySocket.

