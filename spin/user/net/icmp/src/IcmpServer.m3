MODULE IcmpServer;
IMPORT Icmp;
IMPORT IO;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN 
    Icmp.Init();
    IF verbose THEN IO.Put("IcmpServer module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END IcmpServer.
