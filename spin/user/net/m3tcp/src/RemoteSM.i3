(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Basic interface to the M3Tcp module 
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Pass the tcpHdr as READONLY to avoid by VALUE copies.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

INTERFACE RemoteSM;
IMPORT TcpCtrlBlock;
IMPORT TcpPktFormat;

PROCEDURE ChangeState(tcb: TcpCtrlBlock.T; READONLY tcpHdr : TcpPktFormat.NewT);

END RemoteSM.
