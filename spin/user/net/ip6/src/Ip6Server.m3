MODULE Ip6Server;
IMPORT Ip6;
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN 
    Ip6.Init();
    IF verbose THEN IO.Put("Ip6Server module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END Ip6Server.
