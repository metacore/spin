(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *)

(* Untrusted *) INTERFACE An1Default;
IMPORT Ctypes;

CONST
  AN1TYPE_PUP    : Ctypes.unsigned_short = 16_0200; (* PUP protocol *)
  AN1TYPE_IP     : Ctypes.unsigned_short = 16_0800; (* IP protocol *)
  AN1TYPE_ARP    : Ctypes.unsigned_short = 16_0806; (* Addr. resolution protocol *)
  AN1TYPE_LAT    : Ctypes.unsigned_short = 16_6004; (* Local Area Transport (LAT) *)
  AN1TYPE_DECNET : Ctypes.unsigned_short = 16_6003; (* Phase IV DECnet *)
  AN1TYPE_MOPRC  : Ctypes.unsigned_short = 16_6002; (* MOP CCR protocol type *)
  AN1TYPE_MOPDL  : Ctypes.unsigned_short = 16_6001; (* MOP Downline Load protocol type *)
  AN1TYPE_LBACK  : Ctypes.unsigned_short = 16_9000; (* MOP loopback protocol type *)
  AN1TYPE_AM     : Ctypes.unsigned_short = 16_08ff; (* ACTIVE MESSAGE OVER AN1NET *)


(* 
   Harbison p258: packed types only need to be packed in records, objects, and arrays.
*)

TYPE Address = ARRAY [1..6] OF Ctypes.unsigned_char;
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

  dhost   : Address;
  shost   : Address;
  type    : Ctypes.unsigned_short;
END;

TYPE Packet = RECORD
  header  : Header;
  data    : ADDRESS;
END;

TYPE T = UNTRACED REF Header;

PROCEDURE Init();
END An1Default.

