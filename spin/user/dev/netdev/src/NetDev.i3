INTERFACE NetDev;
IMPORT Device, Error, Mbuf;


TYPE T <: Public;
     Public = Device.T OBJECT METHODS

	    send(packet:Mbuf.T) RAISES {Error.E};
	    (* send assumes packet is in a form the driver expects *)

	    mtu():INTEGER; (* max transmission unit, in bytes *)

	    bsdIfp():ADDRESS; (* NIL or a (struct ifp * ) to support BSD udp/tcp *)
	    ipAddr():INTEGER; (* get ipaddr, 0 if not set *)

	  END;

END NetDev.
