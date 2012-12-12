(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) INTERFACE T3PktFormat;
IMPORT Ctypes;

TYPE T3Type = Ctypes.unsigned_short;
CONST
  T3TYPE_PUP    : T3Type= 16_0200; (* PUP protocol *)
  T3TYPE_IP     : T3Type= 16_0800; (* IP protocol *)
  T3TYPE_ARP    : T3Type= 16_0806; (* Addr. resolution protocol *)
  T3TYPE_LAT    : T3Type= 16_6004; (* Local Area Transport (LAT) *)
  T3TYPE_DECNET : T3Type= 16_6003; (* Phase IV DECnet *)
  T3TYPE_MOPRC  : T3Type= 16_6002; (* MOP CCR protocol type *)
  T3TYPE_MOPDL  : T3Type= 16_6001; (* MOP Downline Load protocol type *)
  T3TYPE_LBACK  : T3Type= 16_9000; (* MOP loopback protocol type *)
  T3TYPE_AM     : T3Type= 16_08ff; (* ACTIVE MESSAGE OVER T3NET *)


(* 
   Harbison p258: packed types only need to be packed in records, objects, and arrays.
*)

TYPE Header = RECORD
  (* XXX M3 Compiler implementation problem
     TYPE T is untraced because M3 3.3 prepends type information.
     This only works with structures that are created by the M3 3.3
     run-time and we are dealing with a structure created by the
     network.  Two things need to get fixed:

     1. M3 3.3's assumption that type information is prepended to the
     structure that this REF points to needs to be fixed.  Type
     information should be allocated somewhere else so that M3 3.3 work
     with memory objects not created by its run-time.  I.e., a memory
     object CAST from another language or the network.

     2. TYPE Payload needs to be a special TRACED REF to an internal
     field of a some REF. I.e., the GC needs to know about these special
     REF to work correctly.
  *)
END;

TYPE T = UNTRACED REF Header;
END T3PktFormat.

