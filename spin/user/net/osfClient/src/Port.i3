(*
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 29-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Created.
 *
 *)
 
INTERFACE Port;
IMPORT Ctypes;
IMPORT Dispatcher;
TYPE States = {Active, Dead, Reap};
     State =  SET OF States;

TYPE T = REF RECORD
  port    : Ctypes.unsigned_short;
  binding : Dispatcher.Binding;
  count   : CARDINAL;
  state   : State;
END;
CONST Brand = "Port";
END Port.
