INTERFACE EtherDev;
IMPORT NetDev, Mbuf, Ctypes;

TYPE EtherAddr = ARRAY[0..5] OF Ctypes.unsigned_char;

TYPE
  T <: Public;

  Public = NetDev.T OBJECT METHODS
    etherAddr((*OUT*) VAR addr: EtherAddr);
  END;

PROCEDURE Receive(dev: T; packet: Mbuf.T);
    (* Called from intr handler with interrupts off.
       Handlers of this event responsible for freeing mbuf.
     *)

END EtherDev.
