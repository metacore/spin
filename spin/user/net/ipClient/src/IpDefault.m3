(*
 * Copyright 1994, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Sep-96  Wilson Hsieh (whsieh) at the University of Washington
 *	make guards FUNCTIONAL
 *
 * 28-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Changed to use three argument PacketArrived handler.
 *
 * 07-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Converted to use new spin shell command support.
 *	Converted obsolete Clib calls to IO calls.
 *	Added command option to print out the number of packets received.
 *
 *)

MODULE IpDefault;

IMPORT Net;
IMPORT Mbuf;
IMPORT Ip; (* event we are handling *)
IMPORT IpPktFormat;
IMPORT IO;

IMPORT ParseParams;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* ipdefault *)
      IF pp.testNext("-debug") THEN
        debug_level := Net.MapDebug(pp.getNextInt());
      ELSIF pp.testNext("-packets") THEN
        IO.Put("ip stats: received ");
        IO.PutInt(ip_packet_counter);
        IO.Put(" packets.\n");
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
    END;
   RETURN TRUE;
 END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

VAR
  ip_packet_counter : INTEGER := 0;
  promiscuous: REFANY;
  debug_level : Net.Level := Net.oLevel.NODEBUG;

CONST
  ip_hdr_len = BYTESIZE(IpPktFormat.Header);

FUNCTIONAL
PROCEDURE WhenClause_PROMISCUOUS(
    <* UNUSED *> packet: Mbuf.T;
    curr: Mbuf.T; 
    offset: CARDINAL):BOOLEAN =
  BEGIN
    WITH ipHeaderBuf = SUBARRAY(Mbuf.Array(curr)^,offset,ip_hdr_len),
         ipHeader = VIEW(ipHeaderBuf,T)
     DO
      RETURN ipHeader.hlen = 5 AND (* No IP options ? *)
      ipHeader.vers = 4 AND (* IP version 4  ? *)
      TRUE;
    END;
  END WhenClause_PROMISCUOUS;

PROCEDURE PacketArrived_PROMISCUOUS(
    <* UNUSED *> packet: Mbuf.T;
    <* UNUSED *> curr: Mbuf.T; 
    <* UNUSED *> offset: CARDINAL):BOOLEAN =
  BEGIN
    INC(ip_packet_counter);
    RETURN FALSE; (* return true if consuming packet *)
  END PacketArrived_PROMISCUOUS;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    promiscuous := Ip.Install(Ip.PacketArrived,
                              WhenClause_PROMISCUOUS,
                              PacketArrived_PROMISCUOUS);

    IF verbose THEN IO.Put ("IpDefault module initialized\n"); END;
  END Init;

BEGIN
END IpDefault. 

