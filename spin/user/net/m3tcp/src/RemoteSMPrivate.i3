(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Passing in the tcpHdr as READONLY to avoid uncessary copies.
 *
 * 04-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	created.
 *)

INTERFACE RemoteSMPrivate;
IMPORT TcpCtrlBlock, TcpPktFormat;

IMPORT Mbuf;

PROCEDURE ProcessPacket(tcp:TcpCtrlBlock.T; m:Mbuf.T; offset:CARDINAL);

PROCEDURE Closed     (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Listen     (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE SYN_Sent   (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE SYN_Rcvd   (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Estab      (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE FIN_Wait1  (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE FIN_Wait2  (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Close_Wait (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Closing    (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Last_Ack   (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
PROCEDURE Time_Wait  (tcb:TcpCtrlBlock.T; READONLY tcpHdr:TcpPktFormat.NewT);
END RemoteSMPrivate.
