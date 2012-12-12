(*
 * HISTORY
 * 16-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *	Implements two different IPC calls, one is for control transfer, the 
 *		other for 128 byte data transfer.
 *)
MODULE RPC;
IMPORT Machine, Sema, Dispatcher, Trap;
IMPORT Space, UserSpaceThread;
IMPORT Clib, Fmt, OS, CPUState, Word;

VAR
  serverkick: Sema.T;
  resultready: Sema.T;
  result: INTEGER;

  serverkick2: Sema.T;
  resultready2: Sema.T;
  requestaddr: Word.T;
  requestspace: Space.T;
  buffer: ARRAY[0..256] OF CHAR;

CONST size = 128;

PROCEDURE rpchndlr(ss: REF Machine.SavedState) =
  BEGIN
    CASE ss.v0 OF
    | 155 => RPCrequest(ss);
    | 156 => RPCgetevent(ss);
    | 157 => RPCreturn_getnext(ss);
    | 158 => RPCrequest2(ss);
    | 159 => RPCgetevent2(ss);
    | 160 => RPCreturn_getnext2(ss);
    ELSE
    END;
  END rpchndlr;

PROCEDURE rpchndlr_guard(ss: REF Machine.SavedState) : BOOLEAN =
  BEGIN
    RETURN ss.v0 >= 155 AND ss.v0 < 161;
  END rpchndlr_guard;

PROCEDURE RPCrequest(ss: REF Machine.SavedState) = 
  VAR start, end: Word.T;
  BEGIN
    Sema.V(serverkick);
    Sema.P(resultready);
    ss.v0 := result;
  END RPCrequest;

PROCEDURE RPCgetevent(ss: REF Machine.SavedState) =
  BEGIN
    Sema.P(serverkick);
  END RPCgetevent;

PROCEDURE RPCreturn_getnext(ss: REF Machine.SavedState) =
  BEGIN
    result := ss.a0;
    Sema.V(resultready);
    Sema.P(serverkick);
  END RPCreturn_getnext;

(* ************ This RPC transfers 128 bytes ************** *)
PROCEDURE RPCrequest2(ss: REF Machine.SavedState) = 
  BEGIN
    requestaddr := ss.a0;
    requestspace := Space.GetCurrent();
    Sema.V(serverkick2);
    Sema.P(resultready2);
  END RPCrequest2;

PROCEDURE RPCgetevent2(ss: REF Machine.SavedState) =
  BEGIN
    Sema.P(serverkick2);
  END RPCgetevent2;

PROCEDURE RPCreturn_getnext2(ss: REF Machine.SavedState) =
  BEGIN
    Space.Read(Space.GetCurrent(), ss.a0, buffer, size);
    Space.Write(requestspace, buffer, requestaddr, size);
    Sema.V(resultready2);
    Sema.P(serverkick2);
  END RPCreturn_getnext2;

BEGIN
  serverkick := Sema.Alloc(0);
  resultready := Sema.Alloc(0);
  serverkick2 := Sema.Alloc(0);
  resultready2 := Sema.Alloc(0);
(* EVAL Trap.InstallSyscallHandler(rpchndlr, rpchndlr_guard); *)
  EVAL Trap.InstallSyscallHandler(rpchndlr, NIL);
  Clib.Print("RPC installed...\n");
END RPC.
