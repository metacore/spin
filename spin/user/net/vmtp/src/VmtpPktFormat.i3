(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)

(* Versatile Message Transaction Protocol. See also RFC 1045. *)

INTERFACE VmtpPktFormat;
IMPORT Ctypes;

TYPE
  (* Entity is an opaque 64 bit value in general. *)
  Entity = RECORD
    high, low: Ctypes.unsigned_int;
  END;

  (* When it's used in the IP world, entity has the following format. *)
  IPEntity = RECORD
    disc: Ctypes.unsigned_int; (* upper 4 bits: typeflag, lower 28 bits: disc*)
    ipAddr: Ctypes.unsigned_int;
  END;

  (* A bit of terminology lesson:
     Packet is a stuff that can be sent in network layer frame.
     Segment is a unit of request and response. It consists of sequence
     of packets.
     Packet group is a synonym of segment. 

     Segment size is limited to 16K, because the message delivery mask is
     32 bits, and each bit correspond to 512 bytes(32*512=16K). *)
  SegmentSize = [0 .. 65535];
  
CONST
  (* Defines the upper 4 bits of the entity id. *)
  RAE = 16_80000000; (* remote alias entry. *)
  GRP = 16_40000000; (* entity group. *)
  LEE = 16_20000000; (* little endian. Note that header is
			big endian regardless of this flag. *)
  (* The last bit is unused. *)


VAR
  (* Well known entities, none of which are supported :(. *)
  MANAGER_GROUP, DEFAULT_BECLIENT, DEFAULT_LECLIENT: Entity;
  
TYPE
  TID = Ctypes.unsigned_int; (* transaction ID. *)
  DeliveryMask = Ctypes.unsigned_int; (* 32 bit vector. *)
  MCB = RECORD  
    server: Entity;
    cmd, (* conditional message delivery *)
      dgm, (* datagram. No reply, no retransmit *)
      mdm, (* msg delivery mask. "msgDeliveryMask" used. *)
      sda, (* segment data appended *)
      res, (* notused *)
      cre, (* coresident used. *)
      mrd, (* multiple responses desired. *)
      pic (* public interface code. *): BITS 1 FOR BOOLEAN;
    req: BITS 24 FOR [0 .. 16_FFFFFF]; (* request code. *)
    coResident: Entity;
    userData: ARRAY [0 .. 2] OF Ctypes.unsigned_int; (* unused. *)
    msgDeliveryMask: DeliveryMask;
    segSize: Ctypes.unsigned_int; (* length of the segment. up to 16k *)
  END;
      
CONST
  (* "prio" field values. *)
  PrioUrgent = 2_1100;
  PrioImportant = 2_1000;
  PrioNormal = 0;
  PrioBackground = 2_0100;
  
TYPE
  Header = RECORD
    client: Entity;
    version: BITS 3 FOR [0 .. 7]; (* always 0 *)
    domain: BITS 13 FOR [0 .. 8191]; (* always 1 *)
    hco (* header checksum only *),
      epg (* encrypted *),
      mpg (* multicast *): BITS 1 FOR BOOLEAN;
    length: BITS 13 FOR [0 .. 8191]; (* len of this packet. *)
    nrs, (* next receive sequence. *)
      apg, (* acknowledge packet group *)
      nsr, (* not start run *)
      ner, (* not end run *)
      nrt, (* no retransmission *)
      mdg, (* member of destination group *)
      cmg, (* conitnued message. *)
      sti, (* skip transaction identifiers *)
      drt (* delay response transmission. *): BITS 1 FOR BOOLEAN;
    retransCount: BITS 3 FOR [0 .. 7];
    forwardCount: BITS 4 FOR [0 .. 15];
    gap: Ctypes.unsigned_char;
    prio: BITS 4 FOR [0 .. 15];
    reserved_: BITS 3 FOR [0 .. 7];
    isResponse: BITS 1 FOR BOOLEAN; (* TRUE for response, FALSE for request. *)
    tid: TID;
    deliveryMask: DeliveryMask;    
    mcb: MCB;
    checksum: Ctypes.unsigned_int;
  END;

CONST
  ManagerDisc = 1; (* node manager discriminator number. *)
  RequestRetries = 6; (* max # of request retries *)
  ResponseRetries = 6; (* max # of response retries. *)
  
END VmtpPktFormat.





