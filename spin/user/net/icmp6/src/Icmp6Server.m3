MODULE Icmp6Server;
IMPORT Icmp6;
IMPORT IO;

PROCEDURE Init(verbose: BOOLEAN) = 
  BEGIN 
    Icmp6.Init();
    IF verbose THEN IO.Put("Icmp6Server module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END Icmp6Server.
