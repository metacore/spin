(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Added support for passing arguments through stack.
 *
 *)

MODULE RegSchedule;
IMPORT Msg, Target, Type, Module, Word, Declaration;

PROCEDURE AssignRegsToParams (VAR m: Module.T; p: Type.Proc) =
  VAR
    inRegAddr: CARDINAL := 0;
    outRegUsed: ARRAY [0 .. 100] OF BOOLEAN;
  
  PROCEDURE AssignNextFreeReg (): CARDINAL =
  BEGIN
    FOR i := 4 TO 4 DO 
      (* XXXX 4 is alpha dependent! ask yasushi *)
      IF NOT outRegUsed[i] THEN
	IF i > (Target.maxSavedRegAddr DIV 8) THEN
	  Msg.Error(p.srcPos,
		    "Param can't be passed by call-by-value-result\n");
	  Msg.Error(p.srcPos, "You HAVE TO reduce the # of VAR params.\n");
	END;
	outRegUsed[i] := TRUE;
	RETURN i*8;
      END;
    END;
    Msg.Fatal(Msg.POS_VOID, "Sorry, too many VAR INTEGER parameters.\n",
	      "You should try adding <*NO_SMALL_VAR_OPTIMIZATION*> pragma.\n");
  END AssignNextFreeReg;

  PROCEDURE AddException (e : TEXT) =
    BEGIN
      EVAL p.raises.put(e, NIL);
      EVAL m.exceptionList.put(e, NIL);
    END AddException;
    
  PROCEDURE AddExceptions (e : REF ARRAY OF TEXT) =
    BEGIN
      IF e # NIL THEN
	FOR i := 0 TO NUMBER(e^)-1 DO
	  AddException(e[i]);
	END;
      END;
    END AddExceptions;
  BEGIN
    outRegUsed[3] := TRUE; (* a3 is used to hold errno *)
    
    p.needSpace := FALSE;
    p.needArgs := FALSE;

    FOR i := FIRST(outRegUsed) TO LAST(outRegUsed) DO
      outRegUsed[i] := FALSE;
    END;
  
    IF p.retType # NIL AND p.retType.needSpace() THEN 
      (* We have to in/externalize this one. *)
      p.needSpace := TRUE;
    END;
  
    FOR i := 0 TO LAST(p.params^) DO
      WITH t = p.params[i] DO
	t.inReg := inRegAddr;
	t.outReg := 99999;

	IF inRegAddr >= Target.maxRegAddr THEN
	  p.needArgs := TRUE;
	  p.needSpace := TRUE;
	END;
      
	IF t.type.extensionOnly THEN 
	  p.needSpace := TRUE;
	ELSE 
	  
	  IF t.mode # Type.Mode.Value THEN
	    IF NOT CalledByValueResult(m, t.type) THEN
	      p.needSpace := TRUE;
	    END;
	  
	    IF (t.mode = Type.Mode.Out OR t.mode = Type.Mode.InOut) THEN
	      IF NOT CalledByValueResult(m, t.type) THEN
		t.outReg := inRegAddr;
		outRegUsed[inRegAddr] := TRUE;
	      ELSE
		t.outReg := AssignNextFreeReg();
	      END;
	    END;

	    INC(inRegAddr, BYTESIZE(Word.T));

	    CASE t.mode OF
	    | Type.Mode.In =>
	      AddExceptions(t.type.exceptionOnUnpack());
	    | Type.Mode.Out =>
	      AddExceptions(t.type.exceptionOnPack());
	    | Type.Mode.InOut =>
	      AddExceptions(t.type.exceptionOnUnpack());
	      AddExceptions(t.type.exceptionOnPack());
	    | Type.Mode.Value =>
	      <*ASSERT FALSE*>
	    END;

	  
	  ELSE
	    (* VALUE parameter *)
	    IF ISTYPE(t.type, Type.Ref) THEN
	      TYPECASE t.type OF
	      | Type.Text =>
		(* text is special. it is ref, but is readonly *)
	      | Type.Ref(r) =>
		IF NOT r.capabilityP THEN
		  (* we have to write back ref, so we preserve reg *)
		  t.outReg := inRegAddr;
		END;
	      ELSE
	      END;
	    END;
	    
	    AddExceptions(t.type.exceptionOnUnpack());
	    
	    IF t.type.needSpace() THEN 
	      p.needSpace := TRUE;
	    END;
	    INC(inRegAddr, MAX(BYTESIZE(Word.T),
			       Type.BitsToBytes(t.type.bitsize)));
	  END;
	END;
      END;
    
      IF p.needSpace THEN
	(* Since we access memory, we may get Space exceptions *)
	AddException("VMError.E");
      END;
    
    END;
  END AssignRegsToParams;

PROCEDURE Scan (VAR m: Module.T) =
  BEGIN
    m.minProcID := LAST(INTEGER);
    m.maxProcID := FIRST(INTEGER);
    
    FOR i := 0 TO m.names.size()-1 DO
      WITH genericDecl = m.names.get(i) DO 
	TYPECASE genericDecl OF
	| Declaration.Proc(decl) =>
	  AssignRegsToParams(m, decl.proc);
	  m.maxProcID := MAX(decl.id, m.maxProcID);
	  m.minProcID := MIN(decl.id, m.minProcID);
	ELSE
	END;
      END;
    END;
  END Scan;

PROCEDURE CalledByValueResult (READONLY m: Module.T; t: Type.T): BOOLEAN =
  BEGIN
    IF m.noVarOptimization THEN
      RETURN FALSE;
    ELSE
      RETURN t.bitsize <= Target.regSize * 8;
    END;
  END CalledByValueResult;


BEGIN
END RegSchedule.
