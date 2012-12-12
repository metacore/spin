(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by wobber *)

(* HISTORY
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Converted to a SPIN extension. A TCP.Public now contains a socket,
 *      rather than a file descriptor.
 *
 *)

INTERFACE TCPPosix;

IMPORT TCP, ConnFD, Socket;

REVEAL
  TCP.T <: Public;

TYPE
  Public = ConnFD.T OBJECT
             s     : Socket.T;
             closed: BOOLEAN   := FALSE;
           END;

(* The type "Public" reveals enough structure of the POSIX implementation
   of "TCP.T" to allow a client to perform operations directly upon the
   POSIX file descriptor in "fd".  If "closed" is "TRUE", then "fd" is no
   longer valid.  Any operations on "fd" must be performed with the
   object's mutex locked and the caller should assert that "closed" is
   "FALSE". *)

END TCPPosix.
