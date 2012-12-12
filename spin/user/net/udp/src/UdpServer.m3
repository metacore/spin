MODULE UdpServer;
IMPORT Udp;
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    Udp.Init();
    IF verbose THEN IO.Put("UdpServer module initialized.\n"); END;
  END Init;

CONST 
  verbose = FALSE;

BEGIN
  Init(verbose);
END UdpServer.
