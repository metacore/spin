(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 13-Dec-95  Charlie Garrett (garrett) at the University of Washington
 *      Created.
 *)

(* A UdpWr.T is a writer which sends stuff over udp connections. *)

INTERFACE UdpWr;

IMPORT Wr, Ctypes, IpPktFormat, UdpPktFormat;

TYPE
  T <: Public;
  Public = Wr.T OBJECT METHODS init(dest: Destination): T END;

  Connection = RECORD
    ip: IpPktFormat.T;
    udp: UdpPktFormat.NewT;
  END;

  Destination = RECORD
    ipaddr: IpPktFormat.Address;
    port: Ctypes.unsigned_short;
  END;

PROCEDURE New(d: Destination): T;
(* Equivalent to "NEW(T).init(d)". *)

END UdpWr.  
