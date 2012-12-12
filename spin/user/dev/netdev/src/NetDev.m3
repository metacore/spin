MODULE NetDev;
IMPORT Auth, NetDevInterface;
IMPORT Mbuf;

REVEAL T = Public BRANDED OBJECT
		  OVERRIDES
		    bsdIfp := BsdIfp;
		    send := Send;
		    mtu := Mtu;
		  END;


PROCEDURE BsdIfp(<*UNUSED*>self: T): ADDRESS = BEGIN RETURN NIL END BsdIfp;
PROCEDURE Mtu(<*UNUSED*>self: T): INTEGER = BEGIN RETURN 0 END Mtu;

PROCEDURE Send(<*UNUSED*>self: T;<*UNUSED*> packet:Mbuf.T) =
  BEGIN
  END Send;

BEGIN
      EVAL NetDevInterface.Export(NEW (Auth.AuthAlways));
END NetDev.
