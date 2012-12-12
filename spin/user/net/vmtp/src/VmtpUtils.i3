(*
 * Copyright 1994-97 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 *)
INTERFACE VmtpUtils;
IMPORT IpPktFormat;
IMPORT VmtpPktFormat;

PROCEDURE MyIpAddr(): IpPktFormat.Address;
PROCEDURE Random(): CARDINAL;
PROCEDURE ErrorMsg(msg: TEXT; status: INTEGER);
PROCEDURE EntityToString(READONLY e: VmtpPktFormat.Entity): TEXT;
  
END VmtpUtils.
