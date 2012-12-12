(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	added the timeout constants.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

INTERFACE TcpSM;

CONST

TYPE States = {
  Closed, 
  Listen, 
  SYN_Sent, 
  SYN_Rcvd, 
  Estab, 
  FIN_Wait1, 
  FIN_Wait2, 
  Close_Wait, 
  Closing, 
  Last_Ack, 
  Time_Wait};

StateSet = SET OF States;

PROCEDURE StateToText(s : States) : TEXT;


PROCEDURE SynTimeout(arg:REFANY);



END TcpSM.
