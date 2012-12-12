(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * LocalSM.i3  Interface for Local State Machine.
 *
 * HISTORY
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)


INTERFACE LocalSM;
IMPORT TcpSM;
IMPORT TcpCtrlBlock;

(**)

(* 
   The local state machine is a state machine for all the TCP states.  It changes
   state depending on some input.  This input is always from the local user. 
   An example of how a user might change the state machine would be a call to 
   the sockets API function Listen.  This would propagate down and ultimately result in 
   a call to change state.  
 *)

PROCEDURE ChangeState(tcb : TcpCtrlBlock.T; expectedState : TcpSM.States);

(*
  This function is the method to change the state machine.  The input is the 
  TcpCtrlBlock, which contains a state variable (t_state) which holds the current
  state.  The expectedState variable holds a state with which the user wants to 
  go to.  For example if the user wants to do a passive open the expected state
  would be Listen.  If the user wants to do an active open the expected state
  would be SYN_Sent. (see TcpSM.i3 for list of states). 

  If the state machine could not reach the expected state, there is a state machine
  error.

  Valid State changes are: <BR>

  Current   Expected       <BR>
  ----------------------   <BR>
  Closed     -> Listen     <BR>
  Closed     -> SYN_Sent   <BR>
  SYN_Sent   -> Closed     <BR>
  Close_Wait -> Last_Ack   <BR>
  Syn_Rcvd   -> Fin_Wait_1 <BR>
  Estab      -> Fin_Wait_1 <BR>
  Time_Wait  -> Closed     <BR>
 #Listen     -> SYN_Sent   <BR>



#Note: Not currently implemented.
*)

END LocalSM.
