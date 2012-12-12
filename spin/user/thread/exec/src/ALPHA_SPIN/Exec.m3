(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 04-Jun-97  Yasushi Saito (yasushi) at the University of Washington
 *	Use Translation.Write
 * 19-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed the loader path. Also allowed changing the loader path by
 *	setting an env var.
 * 13-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Added dynlink support.
 *
 * 09-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed the stack segment address.
 *	
 * 07-Jun-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed not to resume the newly created thread inside Execute.
 *	You have to call Resume explicitly.
 *	
 * 29-May-96  Stefan Savage (savage) at the University of Washington
 *	Made StackBegin and StackLen variables so they may be passed
 *	by VAR
 *
 * 20-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	Separated exec cmd and Exec itself.
 *
 * 08-May-96  Yasushi Saito (yasushi) at the University of Washington
 *	environ passing(environ is shell var list).
 *
 * 09-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Made safe.
 *
 * 26-Jan-96  Yasushi Saito (yasushi) at the University of Washington
 *	Argument passing.
 *  
 * 23-Jan-96  David Dion (ddion) at the University of Washington
 *	Added support for exec -noreturn in Run (skip -noreturn arg).
 *	Added FPUState to construction of UserSpaceThread.State
 *
 * 20-Dec-95  Brian Bershad (bershad) at the University of Washington
 *	ParseParams.
 *
 * 04-Apr-95  Emin Gun Sirer (egs) at the University of Washington
 *      Created. Start up a user level process.
 * 
 *)

MODULE Exec;
IMPORT Space, Translation;
IMPORT VirtAddr;
IMPORT Auxv;
IMPORT AoutParser;
IMPORT UserSpaceThread;
IMPORT CPU, IO, Fmt;
IMPORT Error, Text;
IMPORT VMError;
IMPORT File;
IMPORT FileSystem;
IMPORT Shell, Glob;

CONST
  Slack = 64;
  DynamicLoader = "/spin/bin/loader";
  LoaderVariable = "LOADER_PATH";
  
VAR
  StackLen  : VirtAddr.Size         := 64 * 2048;
  StackBegin : VirtAddr.Address     := 16_120000000 - StackLen;
  (* Stack bottom has to be just above the text segment.
   Gcc alloca emits a seemingly bogus code that reads a location beyond the
   stack bottom, so we have to make the stack contiguous to the text segment
   to retain bug-compatibility. -- yas *)

  
PROCEDURE LoadWithLoader (stp: File.T;(* Handle to statlinked loader. *)
			  <*UNUSED*>dynp: File.T;  (* Dynlinked binary.
						    dynp may be null. *)
			  READONLY argv : ARRAY OF TEXT;
			  READONLY environ : ARRAY OF TEXT;
			  READONLY auxv: ARRAY OF Auxv.T;
			  s : Space.T;
			  VAR state: UserSpaceThread.State;
			  VAR aoutInfo : Info) RAISES {Error.E, VMError.E} =
  VAR
    argvAddrs := NEW(REF ARRAY OF VirtAddr.Address, NUMBER(argv));
    envAddrs := NEW(REF ARRAY OF VirtAddr.Address, NUMBER(environ));
    buf := NEW(REF ARRAY OF CHAR, 128); (* tmp area *)
    
    stackPtr : VirtAddr.Address := StackBegin + StackLen - Slack;
    (* stack pointer. It is first at the bottom(ie, max. address) of the
       stack, and it moves upward. *)
    
    nAuxv := 99999; (* random big number for sentinel *)
    auxvAddrs := NEW(REF ARRAY OF VirtAddr.Address, NUMBER(auxv));
    
  (* Store the "string" just above the location "stackPtr".
     "stackPtr" points to the beginning of the stored string on return.
     *)
  PROCEDURE PushString (string: TEXT) RAISES {VMError.E} =
    VAR len := Text.Length(string);
    BEGIN
      (* Expand the buffer so that it can hold all the string contents. *)
      IF NUMBER(buf^) < len+1 THEN
	buf := NEW(REF ARRAY OF CHAR, len+1);
      END;
      
      Text.SetChars(buf^, string);
      buf[len] := '\000'; (* Null terminator to make it char*. *)
      DEC(stackPtr, len + 1);
      Translation.Write(s, SUBARRAY(buf^, 0, len+1), stackPtr);
    END PushString;

  (* Round down the stack pointer. *)
  PROCEDURE AlignStackPtrToWordBoundary () =
    BEGIN
      stackPtr := (stackPtr DIV BYTESIZE(stackPtr)) * BYTESIZE(stackPtr);
    END AlignStackPtrToWordBoundary;

  BEGIN
    (*
       Create a thread state.
     *)
    state := NEW(UserSpaceThread.State);
    state.cpustate := NEW(REF CPU.GeneralRegs);
    state.fpustate := NEW(REF CPU.FloatRegs);

    (*
       Open the image and read it in
     *)
    AoutParser.GetInfo(stp, aoutInfo);
    AoutParser.GetExecutable(s, stp);
    state.cpustate.pc := aoutInfo.pc;
    state.cpustate.gp := aoutInfo.gp;
    
    (*
       Allocate a stack. 
     *)
    Space.Allocate(s, StackBegin, StackLen);
    aoutInfo.stackTop := StackBegin;
    
    (* Calculate the # of auxv. *)
    FOR i := 0 TO LAST(auxv) DO
      IF auxv[i].type = Auxv.AT_NULL THEN
	nAuxv := i+1;
	EXIT;
      END;
    END;
      
    IF nAuxv > LAST(auxv) THEN
      IO.Put("auxv does't have AT_NULL!!");
      nAuxv := 0;
    END;

    (*
     * Push the arguments onto the stack
     *
     *  This is a bit tricky. First, push all the *argv's onto the
     *  stack, and remember the places where we put each of them.
     *
     *  Then, push argv itself, by pushing the values we remembered.
     *
     *)

    (* Push the body of strings *)
    FOR i := 0 TO nAuxv DO
      IF auxv[i].string # NIL THEN
	PushString(auxv[i].string);
	auxvAddrs[i] := stackPtr;
      ELSE
	auxvAddrs[i] := auxv[i].data;
      END;
    END;
      
    (* Write the argv bodies. *)
    FOR i := 0 TO LAST(argv) DO
      PushString(argv[i]);
      argvAddrs[i] := stackPtr;
    END;

    (* Write the environ bodies *)
    FOR i := 0 TO LAST(environ) DO
      PushString(environ[i]);
      envAddrs[i] := stackPtr;
    END;
	
    AlignStackPtrToWordBoundary();

    (* Write the auxp ptrs. *)
    FOR i := nAuxv-1 TO 0 BY -1 DO
      VIEW(SUBARRAY(buf^, 0, 8), INTEGER) :=  auxv[i].type;
      VIEW(SUBARRAY(buf^, 8, 8), INTEGER) := auxvAddrs[i];
      DEC(stackPtr, 16);
      Translation.Write(s, SUBARRAY(buf^, 0, 16), stackPtr);
    END;

    (* mark the end of environ *)
    VIEW(buf^, VirtAddr.Address) := 0;
    DEC(stackPtr, BYTESIZE(stackPtr));
    Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);

    (* push environ addresses *)
    FOR i := LAST(envAddrs^) TO 0 BY -1 DO
      VIEW(buf^, VirtAddr.Address) := envAddrs[i];
      DEC(stackPtr, BYTESIZE(stackPtr));
      Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);
    END;
      
    (* mark the end of argv *)
    VIEW(buf^, VirtAddr.Address) := 0;
    DEC(stackPtr, BYTESIZE(stackPtr));
    Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);

    (* push argv addresses *)
    FOR i := LAST(argv) TO 0 BY -1 DO
      VIEW(buf^, VirtAddr.Address) := argvAddrs[i];
      DEC(stackPtr, BYTESIZE(stackPtr));
      Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);
    END;
      
    (* Finally, push argc, and we're done. *)
    VIEW(buf^, VirtAddr.Address) := NUMBER(argv);
    DEC(stackPtr, BYTESIZE(stackPtr));
    Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);
          
    state.cpustate.usp := stackPtr;
    state.cpustate.a0 := stackPtr;
    state.cpustate.ra := 16_abbaabbaabbaabba;
    state.cpustate.pv := state.cpustate.pc;
    
  END LoadWithLoader;

PROCEDURE LoadFile (path: TEXT;
		    READONLY argv : ARRAY OF TEXT;
		    READONLY environ : ARRAY OF TEXT;
		    s : Space.T;
		    VAR state: UserSpaceThread.State;
		    VAR aoutInfo : Info) RAISES {Error.E, VMError.E} =
  VAR
    loaderPath: TEXT;
    stp, dynp: File.T;
    auxv: ARRAY [0..15] OF Auxv.T;
  BEGIN
    Auxv.Init(auxv);
    
    stp := FileSystem.Lookup(NIL, path);

    TRY
      (*
       * Open the image and read it in
       *)
      AoutParser.GetInfo(stp, aoutInfo);
      
      IF aoutInfo.dynamic THEN
	dynp := stp;
	loaderPath := Glob.GetVariable(Shell.Vars(), LoaderVariable);
	IF loaderPath = NIL THEN
	  loaderPath := DynamicLoader;
	END;
	
	stp := FileSystem.Lookup(NIL, DynamicLoader);
	AoutParser.GetInfo(stp, aoutInfo);
	IF aoutInfo.dynamic THEN
	  IO.Put("Exec.LoadFile: dynlinker can't be dynlinked binary.\n");
	  RAISE Error.E(NEW(Error.T).init(99)); (* XXX *)
	END;
	Auxv.AddText(auxv, Auxv.AT_EXEC_FILENAME, path);
	Auxv.AddText(auxv, Auxv.AT_EXEC_LOADER_FILENAME, DynamicLoader);
	Auxv.AddInt(auxv, Auxv.AT_EXEC_LOADER_FLAGS, 0);
      ELSE
	dynp := NIL;
	Auxv.AddText(auxv, Auxv.AT_EXEC_FILENAME, path);
      END;
    
      LoadWithLoader(stp, dynp, argv, environ, auxv, s, state, aoutInfo);
    FINALLY
      stp.close();
      IF dynp # NIL THEN dynp.close(); END; (* XXX don't work with longjmp
					     based exception.*)
    END;
  END LoadFile;

BEGIN
END Exec.
