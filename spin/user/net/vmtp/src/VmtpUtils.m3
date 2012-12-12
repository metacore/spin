(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Salnet and MachineCPU interfaces
 *
 * 27-Feb-97  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE VmtpUtils;
IMPORT Vmtp;
IMPORT VmtpPktFormat;
IMPORT IpPktFormat;
IMPORT IO, Fmt;
IMPORT Word;
IMPORT CPU, Salnet;

PROCEDURE MyIpAddr(): IpPktFormat.Address =
  VAR
    hostid := Salnet.GetLocalIp();
    ipaddr: ARRAY [0..3] OF CHAR;
  BEGIN
    WITH addr = VIEW(hostid, ARRAY OF CHAR) DO
      (* XXX THIS IS ALPHA SPECIFIC. SOMEONE FIX THIS SHIT PLEASE *)
      ipaddr[0] := addr[7];
      ipaddr[1] := addr[6];
      ipaddr[2] := addr[5];
      ipaddr[3] := addr[4];
      RETURN VIEW(ipaddr, IpPktFormat.Address);
    END;
  END MyIpAddr;
  
PROCEDURE Random(): CARDINAL =
  BEGIN
    RETURN Word.And(CPU.CycleCounter(), 16_7FFFFFFF);
    (* pseudo random number *)
  END Random;

CONST Msgs = ARRAY [0 .. Vmtp.LAST_CODE] OF TEXT
  {
   "ok",
   "retry",
   "retry all",
   "port busy",
   "nonexistent entity",
   "error 5",
   "error 6",
   "error 7",
   "some random error",
   "error 9",
   "bad transaction ID",
   "streaming not supported",
   "error 12",
   "retransmission timeout",
   "user timeout",
   "response discarded",
   "security not supported",
   "bad reply segment"
   };
			    
PROCEDURE ErrorMsg (msg: TEXT; status: INTEGER) =
  VAR s: TEXT;
  BEGIN
    IF status >= FIRST(Msgs) AND status <= LAST(Msgs) THEN
      s := Msgs[status];
    ELSE
      s := "error " & Fmt.Int(status);
    END;
    IO.Put(msg&":"&s&".\n")
  END ErrorMsg;

PROCEDURE EntityToString (READONLY e: VmtpPktFormat.Entity): TEXT =
  BEGIN
    RETURN Fmt.Int(e.high, 16) & ":" & Fmt.Int(e.low, 16);
  END EntityToString;

BEGIN
END VmtpUtils.
