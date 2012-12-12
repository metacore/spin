(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 09-Apr-97  Tsutomu Owa (owa) at the University of Washington
 *	Created.
 *)

MODULE StcpEtherPacketPub;

IMPORT StcpMbuf;

PROCEDURE PacketArrived(packet : StcpMbuf.T) =
  VAR
  BEGIN
    (* sanity check *)
    IF packet # NIL THEN
      StcpMbuf.m_freem(packet);
    END;
  END PacketArrived;

PROCEDURE Init() =
  BEGIN
    Arrived := PacketArrived;
  END Init;

BEGIN
END StcpEtherPacketPub.
