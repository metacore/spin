(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 04-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Fixed calls into security runtime
 *
 * 31-May-97  David Becker at the University of Washington
 *	Replace SAL with Kernel and CPU interfaces
 *	Remove OKToAllocAtSPL
 *
 * 24-Jan-97  Yasushi Saito (yasushi) at the University of Washington
 *	Changed WordTranslationTbl to WordRefTbl
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext and added call to UsersGroups
 *
 * 16-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Use RTIO in Crash().
 *
 * 14-Nov-96  Przemek Pardyak (pardy) at the University of Washington
 *	Delete translation from the hash table when a strand is
 *	deregistered.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Cleanup for machine independence and new interface to interrupt
 *      handling.  In particular, changed LAST(TID) to a constant
 *      LastTID because LAST(TID) is machine-dependent.  Also moved some
 *      functions to MachineDebugger.
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Changed code (RegisterStrand, Task/TID conversion) to allow ttd
 *	to get the pmap associated with a thread (bershad).
 *
 * 27-May-96  Stefan Savage (savage) at the University of Washington
 *	Replaced all uses of Space with Translation (space is now an
 *	extension)
 *
 * 29-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Changed code (RegisterStrand, Task/TID conversion) to allow ttd
 *	to get the pmap associated with a thread (bershad).
 *
 * 25-Jan-96  Devid Becker (becker) at the University of Washington
 *      added GetDomain for domain sweep gdb command
 *
 * 12-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Handle thread = initialThreadID in SetRegs.
 *
 * 12-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Tolerate NIL return from TIDtoStrand in GetCName.
 *
 * 22-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	Support early termination of top level iterator.
 *
 * 10-Oct-95  Stefan Savage (savage) at the University of Washington
 *	Changed DebuggerImpl to DebuggerRep.  Moved machdep Debugger
 *	interface to SAL
 *
 * 09-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Moved machine dependent functions out to DebuggerImpl.
 *      Added Enter.
 *
 * 28-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Added GetThreadName
 *
 * 06-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Fixed unsafe used of M3toC in GetCName
 *
 * 06-Jul-95  Emin Gun Sirer (egs) at the University of Washington
 *	Created. The interface to and for the debugger.
 *
 *)
UNSAFE (* imports RTMisc *)
MODULE Debugger;
IMPORT Strand,StrandRep, CPU, CPUPrivate, Thread;
IMPORT ThreadRep, MachineThread;
IMPORT SchedPrivate, IntRefTbl;
IMPORT Domain, DomainPrivate, DomainList, RTMisc;
IMPORT Translation, TranslationPrivate, TranslationRep;
IMPORT Word;
IMPORT IO, Fmt, Ctypes, Text, Textify, SecurityManager;
IMPORT DebugOption;
IMPORT MachineDebugger;
IMPORT RTIO;

CONST InitialThreadID = 0;  (* The thread that starts when we boot *)
      LastTID = 16_ffffffff;
TYPE
  LeafIterator = DomainIterator OBJECT
                OVERRIDES
                  apply := DomainLeafList;
                END;


VAR
  statelock: MUTEX;
  topLevelDomains: DomainList.T := NIL; (* List of all top-level domains *)
  leafDomains: DomainList.T := NIL; (* List of all bottom-level domains *)
  leafIterator : LeafIterator;
  activestrands: Strand.T            := NIL;
  threadno     : INTEGER             := InitialThreadID + 1;
  transtable   : IntRefTbl.Default;

PROCEDURE DomainLeafList (<*UNUSED*> sc: LeafIterator; d: Domain.T): BOOLEAN =
  VAR
    nametext: TEXT;
    trusted: BOOLEAN;
    dynamic: BOOLEAN;
  BEGIN
    DomainPrivate.GetState(d, nametext, trusted, dynamic);
    IF NOT dynamic THEN RETURN TRUE END;
    leafDomains := DomainList.Cons(d, leafDomains);
    RETURN TRUE;
  END DomainLeafList;

PROCEDURE RegisterDomain(d: Domain.T) =
  BEGIN
    LOCK statelock DO
      topLevelDomains := DomainList.Cons(d, topLevelDomains);
      EVAL DomainPrivate.Apply(d, leafIterator);
    END;
  END RegisterDomain;

PROCEDURE DeregisterDomain(<*UNUSED*>d: Domain.T) =
  BEGIN
    (* XXX implement when domain.destroy works *)
  END DeregisterDomain;

PROCEDURE ApplyToTopLevelDomains(di: DomainIterator) : BOOLEAN =
  BEGIN
    FOR i := DomainList.Length(topLevelDomains) - 1 TO 0 BY -1 DO
      di.domain := DomainList.Nth(topLevelDomains, i);
      IF NOT DomainPrivate.Apply(di.domain, di) THEN RETURN FALSE; END;
    END;
    RETURN TRUE;
  END ApplyToTopLevelDomains;


PROCEDURE RegisterStrand (s: Strand.T; t: Translation.T := NIL) =
  VAR spl: CPU.InterruptLevel;
  BEGIN
    IF t = NIL THEN
      t := TranslationPrivate.GetKernel();
    END;

    TRY
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      s.tid := threadno;
      INC(threadno);
      IF activestrands = NIL THEN
        activestrands := s;
        s.dbgnext := s;
        s.dbgprev := s;
      ELSE
        s.dbgnext := activestrands;
        s.dbgprev := activestrands.dbgprev;
        activestrands.dbgprev.dbgnext := s;
        activestrands.dbgprev := s;
      END;
      EVAL transtable.put(s.tid, t);
    FINALLY
      CPUPrivate.RestoreInterruptMask(spl);
    END;
    IF DebugOption.DebuggerDebug THEN
      IO.Put("Register strand: tid=" & Fmt.Int(s.tid) & 
        " trans=" & Textify.Ref(t) & ".\n");
    END;
  END RegisterStrand;

PROCEDURE DeregisterStrand (s: Strand.T) =
  VAR spl: CPU.InterruptLevel;
      t : REFANY;
  BEGIN
    TRY
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
      IF s.dbgnext = NIL OR s.dbgprev = NIL THEN
        IO.Put("Deregistering twice\n");
        RETURN;
      END;
      s.dbgnext.dbgprev := s.dbgprev;
      s.dbgprev.dbgnext := s.dbgnext;
      IF activestrands = s THEN
        IF activestrands # s.dbgnext THEN
          activestrands := s.dbgnext;
        ELSE
          activestrands := NIL;
        END;
      END;
      s.dbgnext := NIL;
      s.dbgprev := NIL;
      EVAL transtable.delete(s.tid, t);
    FINALLY
      CPUPrivate.RestoreInterruptMask(spl);
   END;
  END DeregisterStrand;

PROCEDURE GetDomain (i: INTEGER;VAR name:ARRAY OF CHAR; VAR loadaddr: Word.T)
	: BOOLEAN =
  VAR
    d: Domain.T;
    nametext: TEXT;
    trusted: BOOLEAN;
    dynamic: BOOLEAN;
    size   : INTEGER;
  BEGIN
    IF i<0 OR i >=DomainList.Length(leafDomains) THEN
      Text.SetChars(name,"End of domain list");
      loadaddr := 0;
      RETURN FALSE;
    END;
    d := DomainList.Nth(leafDomains, i);
    DomainPrivate.GetState(d, nametext, trusted, dynamic);
    Text.SetChars(name, nametext);
    name[MIN(NUMBER(name)-1, Text.Length(nametext))] := '\000';
    EVAL DomainPrivate.TextInfo(d, loadaddr, size);
    RETURN TRUE;
  END GetDomain;

(*
 * Generator function to iterate over active threads.
 * It doesn't make much sense to let time elapse between calls to the
 * iterator, since the list can change. For the debugger, this should
 * not be an issue.
 *)
PROCEDURE NextTID (VAR t: TID): BOOLEAN =
  VAR
    spl: CPU.InterruptLevel;
    cur: Strand.T;
  BEGIN
    IF DebugOption.DebuggerDebug THEN
      IO.Put("NextTID called with tid " & Fmt.Int(t) & "\n");
    END;
    TRY
      spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);

      IF t = LastTID THEN
        IF activestrands # NIL THEN
          t := activestrands.tid;
        ELSE
          t := InitialThreadID;		(* you can't see this one normally *)
        END;
        RETURN TRUE;
      END;

      cur := TIDtoStrand(t);
      IF cur = NIL OR cur.dbgnext = activestrands THEN
        t := LastTID;
        RETURN FALSE;
      ELSE
        t := cur.dbgnext.tid;
        RETURN TRUE;
      END;
    FINALLY
      CPUPrivate.RestoreInterruptMask(spl);
    END;
  END NextTID;

PROCEDURE TIDtoStrand (tid: TID): Strand.T =
  VAR cur: Strand.T;
  BEGIN
    IF tid = InitialThreadID THEN RETURN NIL; END;
    IF activestrands # NIL THEN
      cur := activestrands;
      IF cur.tid = tid THEN RETURN cur; END;
      cur := cur.dbgnext;
      WHILE cur # activestrands DO
        IF cur.tid = tid THEN RETURN cur; END;
        cur := cur.dbgnext;
      END;
    END;
    IF DebugOption.DebuggerDebug THEN
      IO.Put("Warning: cannot find TID " & Fmt.Int(tid) & " in active list\n");
    END;
    RETURN NIL;
  END TIDtoStrand;

PROCEDURE ValidTID (tid: TID): BOOLEAN =
  BEGIN
    IF tid = InitialThreadID THEN
      RETURN TRUE;
    ELSE
      RETURN TIDtoStrand(tid) # NIL;
    END;
  END ValidTID;

PROCEDURE CurrentTID (): TID =
  VAR s: Strand.T;
  BEGIN
    s := Strand.GetCurrent();
    IF s = NIL THEN RETURN InitialThreadID; ELSE RETURN s.tid; END;
  END CurrentTID;

PROCEDURE GetName (<* UNUSED *> tid: TID): TEXT =
  BEGIN
    (*
    RETURN GetThreadName(TIDtoStrand(tid));
    *)
    RETURN "cannot alloc strand name";
  END GetName;



PROCEDURE GetThreadName (s: Strand.T): TEXT =
  VAR
    name  : TEXT;
    idname: TEXT;
  BEGIN
    IF s # NIL THEN
      TRY
        idname := SecurityManager.GetUserName(SecurityManager.GetCurrentUid());
      EXCEPT
      ELSE idname := NIL;
      END;
    END;
    IF idname = NIL THEN idname := "nascent"; END;
    IF TYPECODE(s) = TYPECODE(Thread.T) THEN
      name := idname & "[k]"
                & Fmt.Unsigned(Word.And(LOOPHOLE(s, Word.T), 16_ffffff));
(* UserSpaceThread are now an extension
    ELSIF TYPECODE(s) = TYPECODE(UserSpaceThread.T) THEN
      name := idname & "[u]"
                & Fmt.Unsigned(Word.And(LOOPHOLE(s, Word.T), 16_ffffff));
*)
    ELSE
      name := idname & "[user-space-thread?]"
                & Fmt.Unsigned(Word.And(LOOPHOLE(s, Word.T), 16_ffffff));
    END;
    RETURN name;
  END GetThreadName;

PROCEDURE GetCName (tid: TID; buf: Ctypes.char_star; len: CARDINAL) =
  VAR
    name: ARRAY [0 .. 15] OF CHAR;
    s   : Strand.T;
  BEGIN
    s := TIDtoStrand(tid);
    IF s = NIL THEN
      Text.SetChars(name,"BootThread");
    ELSE
      TRY
        Text.SetChars(name, SecurityManager.GetUserName(
                                SecurityManager.GetCurrentUid()));
      EXCEPT
      ELSE Text.SetChars(name,"*unknown*");
      END;
    END;
    name[MIN(NUMBER(name), len)] := '\000';
    RTMisc.Copy(ADR(name[0]), buf, MIN(NUMBER(name), len));
  END GetCName;

PROCEDURE InUserMode (thread: TID): BOOLEAN =
  VAR s: Strand.T;
  BEGIN
    s := TIDtoStrand(thread);
    IF DebugOption.DebuggerDebug THEN
      IO.Put("InUserMode:: ");
      IF TYPECODE(s) # TYPECODE(Thread.T) THEN
        IO.Put("returning true\n");
      ELSE
        IO.Put("returning false\n");
      END;
    END;
    RETURN TYPECODE(s) # TYPECODE(Thread.T);
  END InUserMode;

PROCEDURE CopySavedUserRegs (    s    : Strand.T;
                             VAR state: CPU.MachineState) =
  BEGIN
    MachineDebugger.CopySavedUserRegs(s, state);
  END CopySavedUserRegs;

PROCEDURE GetRegs (    thread  : TID;
                       usermode: BOOLEAN;
                   VAR state   : CPU.MachineState) =
  VAR
    s      : Strand.T;
    kthread: Thread.T;
  BEGIN
    RTMisc.Zero(LOOPHOLE(ADR(state), Ctypes.void_star), ADRSIZE(state));
    s := TIDtoStrand(thread);

    IF usermode THEN
      IF DebugOption.DebuggerDebug THEN
        IO.Put("user registers are getting queried for thread " & Fmt.Int(thread)  &"\n");
      END;
      IF thread = InitialThreadID THEN
        IO.Put("user registers for initial thread are getting queried.\n");
        RETURN;
      END;
      IF TYPECODE(s) = TYPECODE(Thread.T) THEN
        (*
         * We are trying to find the user mode registers of a kernel thread
         * This means the kernel thread is in the kernel on behalf of a user
         * thread. Look for that one instead.
         *)
        IF s.bound_to_user # NIL THEN
          s := s.bound_to_user;
        ELSE
          IO.Put("Trying to find the usermode regs of a kernel thread!\n");
        END;
      END;
      CopySavedUserRegs(s, state);
    ELSE (* KERNEL MODE *)
      IF DebugOption.DebuggerDebug THEN
        IO.Put("Get regs for TID " & Fmt.Int(thread) & "\n");
      END;
      IF thread = InitialThreadID OR s = Strand.GetCurrent() THEN
        IF DebugOption.DebuggerDebug THEN IO.Put("read the esp.\n"); END;
        (* EZ CASE *)
        CPUPrivate.GetDebuggerRegs(state);
      ELSE
        IF DebugOption.DebuggerDebug THEN IO.Put("read the saved state.\n"); END;
        IF TYPECODE(s) # TYPECODE(Thread.T) THEN
          IO.Put("Trying to get the kernel-mode registers of a user strand!\n");
          RETURN;
        END;
        kthread := NARROW(s, Thread.T);
        MachineThread.GetSavedRegs(kthread, state);
      END;
    END;
  END GetRegs;

PROCEDURE SetRegs (thread: TID; 
                   READONLY state: CPU.MachineState) =
  VAR
    s      : Strand.T;
    kthread: Thread.T;
  BEGIN
    (* IO.Put("SetRegs\n");*)
    s := TIDtoStrand(thread);
    IF DebugOption.DebuggerDebug THEN
      IO.Put("Set regs for TID " & Fmt.Int(thread) & "\n");
    END;
    IF thread = InitialThreadID OR s = Strand.GetCurrent() THEN
      IF DebugOption.DebuggerDebug THEN IO.Put("set the esp.\n"); END;
      CPUPrivate.SetDebuggerRegs(state);
    ELSIF TYPECODE(s) = TYPECODE(Thread.T) THEN
      IF DebugOption.DebuggerDebug THEN
        IO.Put("Setting the saved kernel-mode regs\n");
      END;
      kthread := NARROW(s, Thread.T);
      MachineThread.SetSavedRegs(kthread, state);
    ELSE
      IO.Put("Setting the registers of a user-mode strand.\n");
      IO.Put("NOT HANDLED - email egs if it happens.\n");
    END;
  END SetRegs;



(*
 * ASSUMPTION:
 * We do not expect a GC to occur between when we get a space and when we use
 * the space.
 *)

PROCEDURE TIDtoTask (tid: TID): Task =
  VAR r: REFANY;
  BEGIN
    (* IO.Put("TIDToTask\n");*)
    IF transtable = NIL THEN RETURN NIL END; (* we are not booted yet *)
    IF transtable.get(tid, r) THEN
      RETURN r;
    ELSE
        IO.Put("TIDToTask: failure to map from TID " & Fmt.Int(tid) & ".\n");
        RETURN NIL;
    END;
  END TIDtoTask;

PROCEDURE TasktoPmap(t: Task) : Pmap =
  BEGIN
     IF t = NIL THEN 
	IO.Put("Warning: TasktoMap called with nil task\n");
	RETURN 0;
     ELSE
        RETURN t.pagetableBase;
     END;
  END TasktoPmap;
  

(*
 * use RTIO in this procedure. IO.Put can cause allocation which
 * makes this procedure unusable if the collector crashes.
 *)
PROCEDURE Enter() = 
  VAR spl: CPU.InterruptLevel;  
  BEGIN
     RTIO.PutText("entering debugger\n");
     spl := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
     CPUPrivate.Breakpoint();
     CPUPrivate.RestoreInterruptMask(spl);
     RTIO.PutText("leaving debugger\n");
  END Enter;

PROCEDURE DisablePreemption () =
  BEGIN
    (* assume an atomic write *)
    SchedPrivate.kernelPreemptive := FALSE;
  END DisablePreemption;

PROCEDURE EnablePreemption () =
  BEGIN
    (* assume an atomic write *)
    SchedPrivate.kernelPreemptive := TRUE;
  END EnablePreemption;

PROCEDURE Init (verbose: BOOLEAN) =
  BEGIN
    statelock := NEW(MUTEX);
    transtable := NEW(IntRefTbl.Default).init();
    leafIterator := NEW(LeafIterator);
    IF verbose THEN RTIO.PutText("Debugger interface initialized...\n"); END;
  END Init;

BEGIN
END Debugger.
