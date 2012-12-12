(*
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Implements two different IPC calls, one is for control transfer, the 
 *		other for 128 byte data transfer.
 *)
INTERFACE RPC;
IMPORT Machine;

PROCEDURE RPCrequest(ss: REF Machine.SavedState);
PROCEDURE RPCgetevent(ss: REF Machine.SavedState);
PROCEDURE RPCreturn_getnext(ss: REF Machine.SavedState);

END RPC.
