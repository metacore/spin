(* 
 * HISTORY
 *
 * 18-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Help procedure to conform to the new shell interface.
 *
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE IpClient;
IMPORT IpClientLink; <* NOWARN *>
IMPORT IpDefault; <* NOWARN *>
IMPORT IpClassification; <* NOWARN *>
IMPORT IpFrag; <* NOWARN *>
IMPORT IpRoute; <* NOWARN *>
IMPORT IpGen; <* NOWARN *>
IMPORT IO;

PROCEDURE Init(verbose:BOOLEAN) = 
  BEGIN
    IpGen.Init(verbose);
    (* IpDefault.Init(verbose);*)
    IpClassification.Init(verbose);
    IpFrag.Init(verbose);
    IpRoute.Init(verbose);
    IpClientLink.Init();
    IF verbose THEN IO.Put("IpClient module initialized.\n"); END;
  END Init;

CONST verbose = TRUE;
BEGIN
  Init(verbose);
END IpClient.
