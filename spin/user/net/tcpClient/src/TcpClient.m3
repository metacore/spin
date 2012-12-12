(* 
 * HISTORY
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE TcpClient;
IMPORT TcpDefault;
IMPORT TcpClassification;
IMPORT TcpClientLink;
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    TcpDefault.Init(verbose);
    TcpClassification.Init(verbose);
    TcpClientLink.Init();
    IF verbose THEN IO.Put("TcpClient module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;
BEGIN
  Init(verbose);
END TcpClient.
