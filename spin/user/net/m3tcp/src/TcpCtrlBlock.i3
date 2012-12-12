(*
 * Copyright 1997, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 23-Feb-97  Richard Negrin (negrin) at the University of Washington
 *	added calcRtt function and some variables (timed_seq, clockVal,
 *	and timing_rtt) to do the round trip time and timeout calculation.
 *
 * 06-Feb-97  Marc Fiuczynski (mef) at the University of Washington
 *	Made the TcpCtrlBlock.T an opaque object and exposed the state
 *	machine as methods.  Overrideing the statemachine methods with
 *	the procedures defined in the Local/Remote statemachine modules.
 *	
 *	Created an incarnation field that defines the tcb's current
 *	connection in terms of local/remote port and local/remote
 *	address.
 *	
 *	Added a msl (max seg life) field that can be changed by the
 *	application.
 *	
 *	Added a processPacket method that allows the application to
 *	receive incoming packets.
 *	
 *
 * 16-Jan-97  Richard Negrin (negrin) at the University of Washington
 *	created.
 *
 *)

INTERFACE TcpCtrlBlock;
(* standard M3 interfaces *)
FROM Ctypes IMPORT unsigned_short, unsigned_int, unsigned_char,
                   short, int, char;
IMPORT Thread;

(* URT *)
IMPORT Mbuf;

(* local *)
IMPORT TcpSM, TcpPktFormat, Ifq, Incarnation;

TYPE INCOMPLETE = ADDRESS;
TYPE tcp_seq = unsigned_int;
CONST TCPT_NTIMERS = 4;


TYPE T <: TPublic;
TYPE TPublic = 
(* XXX we need to start taking out stuff that we are not using. *)
OBJECT 
  seg_next: INCOMPLETE;     (* struct  ovtcpiphdr *seg_next;    sequencing queue  *)
  seg_prev: INCOMPLETE;     (* struct  ovtcpiphdr *seg_prev; *)
  t_state: TcpSM.States;    (* short   t_state;  state of this connection  *)
  t_softerror: short;       (* possible error not yet reported  *)
  t_timer: ARRAY [1..TCPT_NTIMERS] OF short; (*tcp timers  *)
  t_rxtshift: short;        (* log(2) of rexmt exp. backoff  *)
  t_rxtcur: short;          (* current retransmit value  *)
  t_dupacks: short;         (* consecutive dup acks recd  *)
  t_maxseg: unsigned_short; (* maximum segment size  *)
  t_force: char;            (* 1 if forcing out a byte  *)
  t_flags: unsigned_char;   

  (* out-of-band data *)
  t_oobflags: char;         (* have some  *)
  t_iobc: char;             (* input character  *)

  t_template : INCOMPLETE;  (* struct  tcpiphdr t_template;    
                               skeletal packet for transmit (used to be mbuf)  *)
  t_inpcb: INCOMPLETE;      (* struct  inpcb *t_inpcb; pointer to internet pcb  *)
  t_timestamp: tcp_seq;     (* used by slowtimo  *)

  (*
   * The following fields are used as in the protocol specification.
   * See RFC793, Sep. 1981, page 21.
   *)

  (* send sequence variables *)
  snd_una: tcp_seq; (* send unacknowledged  *)
  snd_nxt: tcp_seq; (* send next  *)
  snd_up: tcp_seq;  (* send urgent pointer  *)
  snd_wl1: tcp_seq; (* window update seg seq number  *)
  snd_wl2: tcp_seq; (* window update seg ack number  *)
  iss: tcp_seq;     (* initial send sequence number  *)
  snd_wnd: tcp_seq; (* send window  *)

  (* receive sequence variables *)
  rcv_wnd: tcp_seq; (* receive window  *)
  rcv_nxt: tcp_seq; (* receive next  *)
  rcv_up: tcp_seq;  (* receive urgent pointer  *)
  irs: tcp_seq;     (* initial receive sequence number  *)

  (*
   * Additional variables for this implementation.
   *)

  (* receive variables *)
  rcv_adv: tcp_seq;   (* advertised window  *)

  (* retransmit variables *)
  snd_max: tcp_seq;   (* highest seq number sent; used to recognize retransmits  *)
  t_rtseq: tcp_seq;                 (* sequence number being timed  *)

  (* congestion control (for slow start, source quench, retransmit after loss) *)
  snd_cwnd: tcp_seq;     (* congestion-controlled window  *)
  snd_ssthresh: tcp_seq; (* snd_cwnd size threshhold for slow
                            start exponential to linear switch  *)
  tcprexmtthresh: short; (*number of duplicates to start retxmt*)

  (*
   * transmit timing stuff.  See below for scale of srtt and rttvar.
   * "Variance" is actually smoothed difference.
   *)

  t_idle: short;                    (* inactivity time  *)
  t_rtt: short;                     (* round trip time  *)
  t_srtt: short;                    (* smoothed round-trip time  *)
  t_rttvar: short;                  (* variance in round-trip time  *)
  t_rttmin: unsigned_short;         (* minimum rtt allowed  *)
  t_rttmax: unsigned_short;         (* maximum rtt allowed  *)
  t_msslimit: unsigned_short;       (* user-specified max segment size*)
  t_clockVal: unsigned_int;         (* clock value when a timed segment is sent*)
  t_timed_seq: tcp_seq;             (* sequence number of packet being timed.*)
  timing_rtt: BOOLEAN;              (* flag to specify if we are already timing one*)

  max_rcvd: tcp_seq;                (* most peer has sent into window*)
  max_sndwnd: tcp_seq;              (* largest window peer has offered*)
  snd_scale: unsigned_char;         (* window scaling for send window*)
  rcv_scale: unsigned_char;         (* window scaling for recv window*)
  request_r_scale: unsigned_char;   (* pending window scaling  *)
  requested_s_scale: unsigned_char; (*  *)
  t_rptr2rxt: int;                  (* Repeat counter for R2 RXT timer*)
  t_rptr2cur: int;                  (* Current repeat counter for R2 timer*)


  (*Stuff I need for spin*)
  (* added by mef *)
  incarnation : Incarnation.T;
  msl: CARDINAL;
  (*
    sport : BITS 16 FOR unsigned_short;
    dport : BITS 16 FOR unsigned_short;
    saddr : BITS 32 FOR unsigned_int;
    daddr : BITS 32 FOR unsigned_int;
  *)
  inOutMutex : MUTEX;

  queueingStates : TcpSM.StateSet;
  sendingStates  : TcpSM.StateSet;
  recveingStates : TcpSM.StateSet;
  finSendStates  : TcpSM.StateSet;

  sendQ : REF Ifq.ifqueue;
  fin_seq: tcp_seq;    (*seq number of fin*)
  sent_fin : BOOLEAN;

  max_syns: CARDINAL;
  num_syns: CARDINAL;
  last_ack: tcp_seq;
  last_rxmt_seq : tcp_seq;
  num_rxmts : CARDINAL;
  max_rxmts : CARDINAL;

  maxNumSends : CARDINAL; (*most packets sent in one call to output*)

  sendReady: Thread.Condition; (* just refers to the recvCondition of another PCB *)
  name : TEXT;

  rcvdSyn : BOOLEAN  := FALSE;
METHODS
  init(name : TEXT): T;
  getWindow(): CARDINAL;
  calcRtt(numTicks : INTEGER);
  processAck(tcpHdr : TcpPktFormat.NewT) : BOOLEAN; 
  timeout();

  (* XXX temporary solution to process input while we figure out where we
     should be processing the incoming data. *)
  processPacket(m:Mbuf.T; offset:CARDINAL);

  (* remote state machine functions *)
  changeRemoteState (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteClosed      (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteListen      (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteSYN_Sent    (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteSYN_Rcvd    (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteEstab       (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteFIN_Wait1   (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteFIN_Wait2   (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteClose_Wait  (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteClosing     (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteLast_Ack    (READONLY tcpHdr : TcpPktFormat.NewT);
  remoteTime_Wait   (READONLY tcpHdr : TcpPktFormat.NewT);

  (* local state machine functions *)
  changeLocalState  (expectedState: TcpSM.States);
  localClosed       (expectedState: TcpSM.States);
  localListen       (expectedState: TcpSM.States);
  localSYN_Sent     (expectedState: TcpSM.States);
  localSYN_Rcvd     (expectedState: TcpSM.States);
  localEstab        (expectedState: TcpSM.States);
  localFIN_Wait1    (expectedState: TcpSM.States);
  localFIN_Wait2    (expectedState: TcpSM.States);
  localClose_Wait   (expectedState: TcpSM.States);
  localClosing      (expectedState: TcpSM.States);
  localLast_Ack     (expectedState: TcpSM.States);
  localTime_Wait    (expectedState: TcpSM.States);
END;

(* for hash table *)
CONST Brand = "TcpCtrlBlock";


END TcpCtrlBlock.
