(* Copyright 1992 Digital Equipment Corporation. *)
(* Distributed only by permission. *)
(* Created on Sat Jan 11 15:49:00 PST 1992 by gnelson *)
(* Last modified on Wed Aug 31 15:59:31 PDT 1994 by wobber *)
(*      modified on Sun Jan 12 16:17:07 PST 1992 by meehan *)

(* HISTORY
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	The GetHostByName, GetCanonicalByName, GetCanonicalByAddr and
 *	 GetHostAddr procedures were removed from IP.i3 in converting it 
 *	 to SPIN.
 *
 *)

INTERFACE IP;

IMPORT Atom, AtomList, SocketAddrIn;

(* This interface defines the addresses used for communicating
   with the internet protocol family. 
   
   An IP ``endpoint'' identifies a running program in a way that allows
   other programs to communicate with it.

   An endpoint consists of an ``address'', which identifies the
   host machine on which the program is running, together with
   a ``port'', which distinguishes the program from other programs
   running on the same host.

   The host operating system guarantees that the same port is never
   in use by more than one program running on the host simultaneously.
   The same program may be identified by several ports.  Similarly, the
   internet police (try to) guarantee that the same address is never
   in use simultaneously by more than one machine in the world, but
   the same machine may be identified by several addresses.
   
   Port numbers and host addresses can be recycled: the operating system 
   can reuse a port number of a program that has exited or explicitly
   freed the port, and the internet police will reassign addresses 
   from old hosts to new ones. *)

TYPE EC = AtomList.T;

EXCEPTION Error(EC);

(* An IP implementation (or a layered IP protocolimplementation), can raise
   "Error" with error lists including, but not limited to, the following
   atoms: *)

VAR
  LookupFailure, Unreachable, PortBusy, NoResources: Atom.T;

(* "LookupFailure" indicates that a call to "GetHostByName" could
   not determine whether the argument name exists.

   The following errors codes can arise from implementations of protocols
   which are layered on IP:

   "Unreachable" indicates that the destination protocol address is
   not reachable from the local node.  This is typically occurs
   in layered protocols (e.g. TCP) during connection establishment.

   "PortBusy" indicates that the caller attempted to use a port
   which is already in use.

   "NoResources" indicates an OS-dependent resource shortage (such
   as "no more sockets").  The remainder of the error list may detail
   the failure.
*)


TYPE 
  Port = SocketAddrIn.in_portT;
  Address = SocketAddrIn.in_addrT;
  Endpoint = RECORD addr: Address; port: Port END;

(* The type "Address" is an IP address is network byte order.
   The type "Port" is an IP port number in host byte order.
*)

CONST 
  NullPort: Port = 0;
  NullAddress = 0;
  NullEndPoint = Endpoint{NullAddress, NullPort};

PROCEDURE NewEndpoint(name: TEXT; port: INTEGER): Endpoint RAISES {Error};

PROCEDURE GetHostAddr() : Address;

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN;

END IP.
