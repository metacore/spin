MODULE T3Server;
IMPORT T3Packet;
IMPORT IO;
PROCEDURE Init() =
  BEGIN 
    T3Packet.Init();
    (* IO.Put("T3Server module initialized.\n"); *)
  END Init;

BEGIN
END T3Server.
