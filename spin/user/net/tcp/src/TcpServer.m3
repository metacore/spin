MODULE TcpServer;
IMPORT Tcp;
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    Tcp.Init();
    IF verbose THEN IO.Put("TcpServer module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END TcpServer.
