MODULE IpServer;
IMPORT Ip;
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN 
    Ip.Init();
    IF verbose THEN IO.Put("IpServer module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END IpServer.
