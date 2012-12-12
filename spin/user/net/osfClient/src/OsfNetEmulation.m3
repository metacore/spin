(* 
 * HISTORY
 *
 * 28-Jun-95  Marc Fiuczynski (mef) at the University of Washington
 *	Initialization module called started by the dynamic linker after
 *	the M3 code has been initialized.
 *)

MODULE OsfNetEmulation;
IMPORT UdpOsf; <* NOWARN *>
IMPORT TcpOsf; <* NOWARN *>
IMPORT IcmpOsf; <* NOWARN *>
IMPORT OsfNetEmulationLink; <* NOWARN *>

PROCEDURE Init() =
  BEGIN
    UdpOsf.Init();
    TcpOsf.Init();
    IcmpOsf.Init(FALSE);
    OsfNetEmulationLink.Init();
  END Init;

BEGIN
  Init();
END OsfNetEmulation.
