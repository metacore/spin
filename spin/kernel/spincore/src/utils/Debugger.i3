(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Changed code (RegisterStrand, Task/TID conversion) to allow ttd
 *	to get the pmap associated with a thread (bershad).
 *
 * 25-Jan-96  Devid Becker (becker) at the University of Washington
 *      added GetDomain for domain sweep gdb command
 *
 * 22-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Changed ApplyTopLevelDomains to reflect how the application
 *	 terminated.
 *
 * 10-Oct-95  Stefan Savage (savage) at the University of Washington
 *	Changed Impl to Rep
 *
 * 09-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Moved machine dependent functions out to DebuggerImpl.
 *      Added Enter.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added "GetThreadName"
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. Interface to and for the remote teledebugger.
 *)
INTERFACE Debugger;
IMPORT Domain, Strand, Translation, CPU, Ctypes, Word;

(* This module is highly unsafe and for use only by priveleged clients *)

TYPE
  TID = Ctypes.unsigned_int;
  Task = Translation.T;
  Pmap = CPU.VirtAddress;

TYPE
  DomainIterator = Domain.Closure OBJECT
                      domain: Domain.T := NIL;
                    END;

(* Threads and tasks *)
PROCEDURE RegisterStrand(s: Strand.T; translation: Translation.T := NIL);
PROCEDURE DeregisterStrand(s: Strand.T);
PROCEDURE NextTID(VAR thread: TID) : BOOLEAN;
PROCEDURE CurrentTID() : TID;
PROCEDURE ValidTID(tid: TID) : BOOLEAN;
PROCEDURE TIDtoTask(tid: TID) : Task;
PROCEDURE TasktoPmap(t: Task): Pmap;

(* Domains *)
PROCEDURE RegisterDomain(d: Domain.T);  
PROCEDURE DeregisterDomain(d: Domain.T);
PROCEDURE GetDomain (i: INTEGER;VAR name:ARRAY OF CHAR; VAR textaddr: Word.T)
	: BOOLEAN ;

(*  Apply until iterator returns FALSE and return FALSE. 
 *  TRUE if all domains iterated upon.
 *)
PROCEDURE ApplyToTopLevelDomains(di: DomainIterator) : BOOLEAN;


(* Register State Manipulation *)
PROCEDURE GetRegs(thread: TID;
                  usermode: BOOLEAN; 
                  VAR state: CPU.MachineState);

PROCEDURE SetRegs(thread: TID;
                  READONLY state: CPU.MachineState);

PROCEDURE InUserMode(thread:TID) : BOOLEAN;

PROCEDURE DisablePreemption();
PROCEDURE EnablePreemption();

(* Name functions should be used carefully. Only GetCName should be called
 * when runtime services are suspect.  Others may call NEW. *)
PROCEDURE GetName(tid: TID): TEXT;		(* TID to name *)
PROCEDURE GetCName(tid: TID; buf: Ctypes.char_star; len: CARDINAL);  
PROCEDURE GetThreadName(s: Strand.T): TEXT;	(* Strand to name *)

PROCEDURE Enter();	        (* Wrapper for gimmeabreak *)

PROCEDURE Init(verbose: BOOLEAN);

END Debugger.
