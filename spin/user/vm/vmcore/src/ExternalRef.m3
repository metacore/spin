(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 19-Jul-97  Yasushi Saito (yasushi) at the University of Washington
 *	Moved into vmcore.
 * 27-May-96  Stefan Savage (savage) at the University of Washington
 *	Deleted Map and Unmap
 *
 * 08-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Let user specify the external ref value.
 * 11-Mar-96  David Dion (ddion) at the University of Washington
 *	Added CopyTranslationTable() and reversetable field.
 *
 * 13-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	Use Textify.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Consolidate interfaces, move to m3 thread interface, cleanup.
 *
 * 07-Aug-95  Brian Bershad (bershad) at the University of Washington
 *	console device interface.
 *
 * 19-Jun-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added deletion code.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Externalized references.
 *)
MODULE ExternalRef;
IMPORT Word, IntRefTbl, RefIntTbl, Textify, Fmt;
IMPORT IO, Debugger;
IMPORT AddressSpace;
IMPORT Translation;

REVEAL T = Public BRANDED OBJECT
  lock: MUTEX;
  table: IntRefTbl.Default;
  reversetable: RefIntTbl.Default;
  maxno: Word.T := 0;
OVERRIDES
  init := Init;
  destroy := Destroy;
  internalize := Internalize;
  externalize := Externalize;
  delete := Delete;
  copy := Copy;
END;
CONST Debug = FALSE;
  
(* prevent race condition caused by needing locks on two boundary tables
   simultaneously for the copy operation *)
VAR
  copyMu: MUTEX;

PROCEDURE Init (t: T) : T =
  BEGIN
    t.lock := NEW(MUTEX);    
    t.table := NEW(IntRefTbl.Default).init();
    t.reversetable := NEW(RefIntTbl.Default).init();
    IF copyMu = NIL THEN
      copyMu := NEW(MUTEX);
    END;
    RETURN t;
  END Init;

PROCEDURE Externalize (b: T; intptr: REFANY; desiredSlot : Word.T)
  	: Word.T RAISES {Conflict} =
  VAR
    num: INTEGER;
    extptr: Word.T;
    r : REFANY;
  BEGIN
    IF intptr = NIL THEN RETURN NilRef; END;
    LOCK b.lock DO
      (* first make sure an externalization of this ref does not already
         exist *)
      IF b.reversetable.get(intptr, extptr) THEN
        num := extptr;
      ELSE
	IF desiredSlot # DontCare THEN
	  (* the user specified the external ref value explicitly. *)
	  num := desiredSlot;
	  IF b.table.get(num, r) THEN RAISE Conflict; END;
	ELSE
	  (* choose any free slot *)
	  REPEAT 
	    num := b.maxno;
	    INC(b.maxno);
            IF (b.maxno) = LAST(Word.T) THEN
              IO.Put("External ref table full. Find the truck that failed to hit you.\n");
              Debugger.Enter();
            END;
	  UNTIL NOT b.table.get(num, r);
	END;
	
        EVAL b.table.put(num, intptr);
        EVAL b.reversetable.put(intptr, num);
      END;
    END;
    IF Debug THEN
      IO.Put("Externalizing " & Textify.Ref(intptr) & 
                 " in table " & Textify.Ref(b) &
                 " -> " & Fmt.Unsigned(num) & "\n");
    END;
    RETURN num;
  END Externalize;

PROCEDURE Internalize (b: T; extref: Word.T) : REFANY =
  VAR
    ptr: REFANY;
  BEGIN
    IF extref = NilRef THEN RETURN NIL; END;
    LOCK b.lock DO
      IF NOT b.table.get(extref, ptr) THEN
        IF Debug THEN
          IO.Put("Internalize failed: extref = " & 
            Fmt.Unsigned(extref) & "\n");
        END;
        ptr := NIL;
      END;
    END;
    IF Debug THEN
      IO.Put("Internalizing " & Fmt.Unsigned(extref) & 
               " in table " & Textify.Ref(b) &
               " -> " & Textify.Ref(ptr) & "\n");
    END;
    RETURN ptr;
  END Internalize;

PROCEDURE Delete (b: T; extref: Word.T) =
  VAR
    ptr: REFANY;
  BEGIN
    IF extref = NilRef THEN RETURN; END;
    LOCK b.lock DO
      EVAL b.table.delete(extref, ptr);
      EVAL b.reversetable.delete(ptr, extref);
    END;
  END Delete;

PROCEDURE Destroy (b: T) =
  VAR 
    ptr: REFANY;
    junk: INTEGER;
  BEGIN
    LOCK b.lock DO
      FOR i := 1 TO b.maxno DO
	IF b.table.delete(i, ptr) THEN
	  EVAL b.reversetable.delete(ptr, junk);
	END;
      END;
      b.maxno := FIRST(Word.T);
    END;
  END Destroy;

(* If "inheriting" the dest space from the src space, this method should
   be called before dest has the chance to externalize anything.  Otherwise,
   dest.maxno will be incorrect, table entries will be overridden, and
   problems will abound. *)
PROCEDURE Copy (src: T; dest: T) =
  VAR
    it: IntRefTbl.Iterator;
    key: Word.T;
    value: REFANY;
  BEGIN
    (* make sure nobody else is copying - simple way to eliminate race *)
    LOCK copyMu DO
      LOCK src.lock DO
        LOCK dest.lock DO
          (* get the iterator for the source table *)
          it := src.table.iterate();

          (* iterate over all entries in source table, adding them to dest *)
          WHILE it.next(key, value) DO
            EVAL dest.table.put(key, value);
            EVAL dest.reversetable.put(value, key);
            IF dest.maxno < key THEN
              dest.maxno := key;
            END;
            IF Debug THEN
              IO.Put("CopyTranslationTable: Copying key = " & 
                Fmt.Unsigned(key) & " value = " & 
                Textify.Ref(value) & "\n");
            END;
          END;
          (* want the new maxno to be one more than biggest copied extref *)
          INC(dest.maxno);
        END;
      END;
    END;
  END Copy;

PROCEDURE GetCurrent (): T =
  BEGIN
    RETURN NARROW(Translation.GetCurrent(), AddressSpace.T).getXrefTbl();
  END GetCurrent;
  
BEGIN
END ExternalRef.
