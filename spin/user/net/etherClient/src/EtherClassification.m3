(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 10-Dec-97  Marc Fiuczynski (mef) at the University of Washington
 *	Cleaned up a bit.  Moved IPv6 functionality out.
 *
 * 19-Oct-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changes to make it work on IX86_SPIN platform.
 *
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 02-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to new IO interface.
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *)

(* Untrusted *)
MODULE EtherClassification;
IMPORT Ether, Ip, EtherPktFormat, Net, Mbuf, Fmt, IO;

VAR ip: REFANY;
CONST debug  = FALSE;
CONST eth_hdr_len = BYTESIZE(EtherPktFormat.T);

FUNCTIONAL
PROCEDURE WhenClause_IP(
    <*UNUSED*> 
    packet : Mbuf.T; 
    curr   : Mbuf.T; 
    offset : CARDINAL):BOOLEAN =
  BEGIN
    WITH etherHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset, eth_hdr_len),
         etherHeader = VIEW(etherHeaderBuf,T) 
     DO
      RETURN etherHeader.type = EtherPktFormat.ETHERTYPE_IP;
    END;
  END WhenClause_IP;

PROCEDURE PacketArrived_IP(
    <*UNUSED*> 
    packet : Mbuf.T; 
    curr   : Mbuf.T; 
    offset : CARDINAL):BOOLEAN =
  BEGIN
    IF debug AND debug_level # Net.oLevel.NODEBUG THEN
      Net.Debug(debug_level,Net.oLevel.INFO,Fmt.Unsigned(curr.mh_hdr.mh_len) & "EtherIp ");
    END;

    INC(offset,BYTESIZE(EtherPktFormat.T));
    IF offset >= curr.mh_hdr.mh_len THEN
      curr := curr.mh_hdr.mh_next;
      EVAL Ip.PacketArrived(curr, curr, 0);
    ELSE
      (* XXX illegal hack !!! DARN! *)
      INC(curr.mh_hdr.mh_data,BYTESIZE(EtherPktFormat.T));
      DEC(curr.mh_hdr.mh_len,BYTESIZE(EtherPktFormat.T));      
      EVAL Ip.PacketArrived(curr, curr, 0);
      DEC(curr.mh_hdr.mh_data,BYTESIZE(EtherPktFormat.T));
      INC(curr.mh_hdr.mh_len,BYTESIZE(EtherPktFormat.T));      
    END;
    RETURN FALSE;
  END PacketArrived_IP;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    ip := Ether.Install(Ether.PacketArrived,
                        WhenClause_IP,
                        PacketArrived_IP);
    IF verbose THEN IO.Put("EtherClassification initialized\n"); END;
  END Init;

PROCEDURE Uninit(verbose:BOOLEAN) =
  BEGIN
    IF ip # NIL THEN
      Ether.Uninstall(ip);
      ip := NIL;
    END;
    IF verbose THEN IO.Put("EtherClassification unloaded\n"); END;
  END Uninit;

BEGIN
END EtherClassification. 
