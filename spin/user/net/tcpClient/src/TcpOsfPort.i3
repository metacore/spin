(*
 * HISTORY
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)
 
INTERFACE TcpOsfPort;
IMPORT Ctypes;
IMPORT Dispatcher;

TYPE T = REF RECORD
  port: Ctypes.unsigned_short;
  spindle: Dispatcher.Spindle;
  count : CARDINAL;
END;

CONST Brand = "TcpOsfPort";

END TcpOsfPort.
