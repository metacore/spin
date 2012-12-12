(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * Basic interface to the M3Tcp module 
 *
 * HISTORY
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	XXX (Take this comment back out in the future).
 *	Temporarily took out the Input procedure.  We may want to put it
 *	back in the future.  For now, lets concentrate on HTTPD.
 *	
 *	XXX (Take this comment back out in the future).
 *	Temporarily exposing the incarnation table directly.  This should
 *	be done properly via the Incarnation interface.
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)


INTERFACE M3Tcp;
IMPORT Mbuf, TcpPktFormat,TcpCtrlBlock, IncarnationTbl;

(*SPIN SPECIFIC*)
IMPORT ParseParams;
CONST Brand = "M3Tcp";
CONST CommandName = "m3tcp";
      CommandHelp = " -zap|-reset|-pktlossrate|-state|-passiveopen|-activeopen|-passiveclose|-activeclose|-senddata|-debug";
PROCEDURE Run(pp: ParseParams.T) : BOOLEAN;

VAR (* XXX hack -- should not be exported directly *)
  incarnation : IncarnationTbl.Default;
  incMutex : MUTEX;

(* XXX take out temporarily
PROCEDURE Input(m        : Mbuf.T; 
                tcb      : TcpCtrlBlock.T; 
                ipOffset : CARDINAL);

*)
(* Input is a called when the ip layer has some
   data for a given connection. A connection is identified by a 
   TcpCtrlBlock. The data is stored in an Mbuf.  It expects that 
   the Mbuf has a tcp header in it and the header is offset ipOffset bytes 
   from it.  daddr is the ip address of the source of the data.  Source
   is my ip address.

   A user should never call this function.  
*)

PROCEDURE Output(tcb     : TcpCtrlBlock.T; 
                 dataBuf : Mbuf.T;
                 flags   : TcpPktFormat.Flags := TcpPktFormat.Flags{});

(* Output is called to send data to the ip layer.  It takes 
   a TcpCtrlBlock which identifies the connection, an Mbuf which 
   contains the data.  It also gets flags that specify what the packet is
   and how to treat it.  It is called internally by the Tcp State 
   machine as well as externally by the socket layer.  

   This function locks inout mutex. So if you have that mutex locked,
   unlock it before you call this function. (negrin)
*)

PROCEDURE OutputControl(tcb   : TcpCtrlBlock.T; 
                        flags : TcpPktFormat.Flags := TcpPktFormat.Flags{});

(* The user probably shouldn't call this function either.  It does not
lock the inout mutex, but it does alter the state of the pcb.  With the 
current locking scheme (a coarse grain locking on the procedure level) make
sure that the inout mutex is locked before calling this function. (negrin)*)


PROCEDURE CauseResend(tcb :  TcpCtrlBlock.T);
PROCEDURE CleanUp(tcb : TcpCtrlBlock.T);
PROCEDURE DataTimeout(arg:REFANY);

END M3Tcp.


