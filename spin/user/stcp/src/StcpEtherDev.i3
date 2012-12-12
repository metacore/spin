INTERFACE StcpEtherDev;
IMPORT StcpMbuf;

TYPE T = REFANY;

VAR
  stcpDev: T;

PROCEDURE Send(dev: T; packet:StcpMbuf.T) ;

PROCEDURE Receive(dev: T; packet: StcpMbuf.T);
    (* Called from intr handler with interrupts off.
       Handlers of this event responsible for freeing mbuf.
     *)

PROCEDURE Init();

END StcpEtherDev.
