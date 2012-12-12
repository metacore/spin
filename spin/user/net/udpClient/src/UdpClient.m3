(* 
 * HISTORY
 * 13-Jun-96  Marc Fiuczynski (mef) at the University of Washington
 *	Only do RPC support.
 *
 *
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE UdpClient;
IMPORT IO;
IMPORT UdpDefault; <* NOWARN *>
(* UdpClassification, *) 
(*
IMPORT UdpRedirect;  <* NOWARN *>
IMPORT UdpRpc;  <* NOWARN *>
*)
IMPORT UdpClientLink;

PROCEDURE Init(verbose:BOOLEAN) =
  BEGIN
    (* UdpDefault.Init(verbose); *)
    (* UdpClassification.Init(verbose); *)
    (* UdpRedirect.Init(verbose); *)
    (*
    UdpRpc.Init(verbose);
    *)
    UdpClientLink.Init();
    IF verbose THEN IO.Put("UdpClient module initialized.\n"); END;
  END Init;

CONST verbose = FALSE;

BEGIN
  Init(verbose);
END UdpClient.
