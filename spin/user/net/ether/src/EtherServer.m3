MODULE EtherServer;
IMPORT EtherPacket;
IMPORT Ether;
IMPORT IO;

CONST 
  verbose = FALSE;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN 
    Ether.Init();
    EtherPacket.Init();
    IF verbose THEN IO.Put("EtherServer module initialized.\n"); END;
  END Init;

BEGIN
  Init(verbose);
END EtherServer.
