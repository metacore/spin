(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 13-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransProc;
IMPORT Proc, ProcRep;
IMPORT OpenFile;
IMPORT File;
IMPORT Error;
IMPORT Strand;
IMPORT IO;
IMPORT LightMutex;

VAR
  (* "lastGroup" and "lastThread" act as one entry cache to make "Self"
     find the commonly used group fast. *)
  lastGroup: T;
  lastThread: Strand.T;
  mu := LightMutex.Create(); (* Guards both "lastGroup" and "lastThread". *)
  
TYPE GroupPseudoFile = File.T OBJECT
  group: T;
OVERRIDES
  close := CloseGroup;
END;

REVEAL T = TPublic BRANDED OBJECT END;
  
PROCEDURE CloseGroup (gf: GroupPseudoFile) =
  BEGIN
    gf.group.shutDown();
    LightMutex.Lock(mu);
    lastGroup := NIL;
    lastThread := NIL;
    LightMutex.Unlock(mu);
  END CloseGroup;

VAR
  atom := NEW(REF CHAR);

PROCEDURE Intern (proc: Proc.T): T RAISES {Error.E} =
  VAR
    r: REFANY;
    gf: GroupPseudoFile;
    of: OpenFile.T;
    group: T;
  BEGIN
    LOCK proc.mu DO 
      IF NOT Proc.FindAtom(proc, atom, r) THEN
	group := NEW(T, proc := proc, curTrans := NIL);
	EVAL group.init();
	gf := NEW(GroupPseudoFile, group := group);
	of := NEW(OpenFile.T, path := "*transaction*",
		  h := gf, refCount := 0);
	EVAL Proc.AllocateFD(proc, of);
	EVAL Proc.InternAtom(proc, atom, gf);
      ELSE
	gf := r;
      END;
    END;
    RETURN gf.group;
  END Intern;

PROCEDURE SetSelf (thread: Strand.T) =
  BEGIN
    TRY
      lastThread := thread;
      lastGroup := Intern(Proc.Self());
    EXCEPT
    | Error.E(e) =>
      IO.Put("transproc.self: " & e.message() & ".\n");
      lastThread := NIL;
      lastGroup := NIL;
    END;
  END SetSelf;
  
PROCEDURE Self (): T =
  VAR thread := Strand.GetCurrentUserStrand();
  BEGIN
    LightMutex.Lock(mu);
    IF thread # lastThread THEN SetSelf(thread); END;
    LightMutex.Unlock(mu);
    RETURN lastGroup;
  END Self;
  
PROCEDURE GetSphinxProc (t: T): Proc.T =
  BEGIN
    RETURN t.proc;
  END GetSphinxProc;

PROCEDURE AddObject (t: T; obj: REFANY): ExtRef =
  BEGIN
    FOR i := FIRST(ExtRef) TO LAST(ExtRef) DO
      IF t.obj[i] = NIL THEN
	t.obj[i] := obj;
	RETURN i;
      END;
    END;
    <*ASSERT FALSE*>
    RETURN 0;
  END AddObject;

PROCEDURE UnaddObject (t: T; e: ExtRef) =
  BEGIN
    <*ASSERT t.obj[e] # NIL*>
    t.obj[e] := NIL;
  END UnaddObject;
  
BEGIN
END TransProc.
