(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Jan-97  Marc Fiuczynski (mef) at the University of Washington
 *	created.
 *)

INTERFACE LocalSMPrivate;
IMPORT TcpCtrlBlock, TcpSM;

PROCEDURE Closed     (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Listen     (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE SYN_Sent   (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE SYN_Rcvd   (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Estab      (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE FIN_Wait1  (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE FIN_Wait2  (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Close_Wait (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Closing    (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Last_Ack   (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
PROCEDURE Time_Wait  (tcb: TcpCtrlBlock.T; expectedState: TcpSM.States);
END LocalSMPrivate.
