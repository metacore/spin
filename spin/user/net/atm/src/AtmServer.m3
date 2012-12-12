MODULE AtmServer;
IMPORT AtmPacket;
IMPORT IO;

PROCEDURE Init() = 
  BEGIN 
    AtmPacket.Init();
    (* IO.Put("AtmServer module initialized.\n"); *)
  END Init;

BEGIN
END AtmServer.
