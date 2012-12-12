(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 05-Jan-98  Tsutomu Owa (owa) at the University of Washington
 *	Increased StackLen to 64 * 2048 so that lmbench works.
 *
 * 24-Sep-96  Yasushi Saito (yasushi) at the University of Washington
 *	Merged sphinx exec and this exec.
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
IMPORT UserSpaceThread, File, VirtAddr, AoutParser;
IMPORT FileSystem, Log, CPU, IO, Fmt;
IMPORT Error, Text;
IMPORT VMError;
IMPORT ExecX86;
IMPORT SigTramp;

CONST
  Slack = 64;
  Verbose = FALSE;

VAR
  StackBegin : VirtAddr.Address     := 16_80000000;
  (* StackLen  : VirtAddr.Size         := 64 * 1024; *)
  StackLen  : VirtAddr.Size         := 64 * 2048;
  
PROCEDURE LoadFile (filename : TEXT;
		    READONLY argv : ARRAY OF TEXT;
		    READONLY environ : ARRAY OF TEXT;
		    s : Space.T;
		    VAR state: UserSpaceThread.State;
		    VAR aoutInfo : Info) RAISES {Error.E} =
  VAR
    fp        : File.T;
    
    argvAddrs := NEW(REF ARRAY OF VirtAddr.Address, NUMBER(argv));
    envAddrs := NEW(REF ARRAY OF VirtAddr.Address, NUMBER(environ));
    buf : REF ARRAY OF CHAR;
    maxArgLen := 0;
    stackPtr : VirtAddr.Address := StackBegin + StackLen - Slack;
  BEGIN

    (*
     * Create a user thread in this space
     *)
    state := NEW(UserSpaceThread.State);
    state.cpustate := NEW(REF CPU.GeneralRegs);
    state.fpustate := NEW(REF CPU.FloatRegs);

    (*
     * Open the image and read it in
     *)
    TRY
      fp := FileSystem.Open((* XXX 0, *) filename);
      AoutParser.GetExecutable(s, fp, aoutInfo);
      state.cpustate.pc := aoutInfo.pc;
    FINALLY
      TRY IF fp # NIL THEN fp.close(); END; EXCEPT ELSE END;
    END;

    (*
     * Allocate a stack
     *)
    TRY
      Space.Allocate(s, StackBegin, StackLen);
    EXCEPT
    | VMError.E(ec) => IO.Put("Cannot allocate stack(" & Fmt.Int(ec) & ".\n");
      RETURN;
    END;

    aoutInfo.stackTop := StackBegin;
    TRY
      Space.Zero(s, StackBegin, StackLen);
    
      (*
       * Push the arguments onto the stack
       *
       *  This is a bit tricky. First, push all the *argv's onto the
       *  stack, and remember the places where we put each of them.
       *
       *  Then, push argv itself, by pushing the values we remembered.
       *
       *)

      (* determine the max. argument length. *)
      FOR i := 0 TO LAST(environ) DO
	WITH len = Text.Length(environ[i]) DO
	  IF len > maxArgLen THEN maxArgLen := len; END;
	END;
      END;
      FOR i := 0 TO LAST(argv) DO 
	WITH len = Text.Length(argv[i]) DO
	  IF len > maxArgLen THEN maxArgLen := len; END;
	END;
      END;
      
      (* Then allocate the buffer. It's clumsy, but we have to convert
       *  text into array of char before passing it to Translation.Write.
       *  "+ BYTESIZE(stackPtr)" is for tmp work area to push ptrs.
       *)
      buf := NEW(REF ARRAY OF CHAR, maxArgLen+1 + BYTESIZE(stackPtr));
      
      (* Write the argv bodies. *)
      FOR i := 0 TO LAST(argv) DO
	WITH len = Text.Length(argv[i]) DO
	  Text.SetChars(buf^, argv[i]);
	  buf[len] := '\000'; (* Null terminator to make it char*. *)
	  DEC(stackPtr, len + 1);
	  Translation.Write(s, SUBARRAY(buf^, 0, len+1), stackPtr);
	  argvAddrs[i] := stackPtr;
	END;
      END;

      (* Write the environ bodies *)
      FOR i := 0 TO LAST(environ) DO
	WITH len = Text.Length(environ[i]) DO
	  Text.SetChars(buf^, environ[i]);
	  buf[len] := '\000'; (* Null terminator to make it char*. *)
	  DEC(stackPtr, len + 1);
	  Translation.Write(s, SUBARRAY(buf^, 0, len+1), stackPtr);
	  envAddrs[i] := stackPtr;
	END;
      END;
	
      (* Align the stackPtr to the word boundary *)
      stackPtr := (stackPtr DIV BYTESIZE(stackPtr)) * BYTESIZE(stackPtr);

      (* mark the end of environ *)
      VIEW(buf^, VirtAddr.Address) := 0;
      DEC(stackPtr, BYTESIZE(stackPtr));
      Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);

      (* push environ's *)
      FOR i := LAST(envAddrs^) TO 0 BY -1 DO
	VIEW(buf^, VirtAddr.Address) := envAddrs[i];
	DEC(stackPtr, BYTESIZE(stackPtr));
	Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);
      END;
      
      (* mark the end of argv *)
      VIEW(buf^, VirtAddr.Address) := 0;
      DEC(stackPtr, BYTESIZE(stackPtr));
      Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);

      (* push argv vectors *)
      FOR i := LAST(argv) TO 0 BY -1 DO
	VIEW(buf^, VirtAddr.Address) := argvAddrs[i];
	DEC(stackPtr, BYTESIZE(stackPtr));
	Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);
	IO.Put("stackptr = " & Fmt.Int(stackPtr, 16) & ":" & argv[i] &  "\n");
      END;
      IO.Put("final stackptr = " & Fmt.Int(stackPtr, 16) & "\n");
      
      (* Finally, push argc, and we're done. *)
      VIEW(buf^, VirtAddr.Address) := NUMBER(argv);
      DEC(stackPtr, BYTESIZE(stackPtr));
      Translation.Write(s, SUBARRAY(buf^, 0, BYTESIZE(stackPtr)), stackPtr);

      (* Write out the process control block. Currently, we only have
       sigtramp code *)
      VAR addr : VirtAddr.Address := ExecX86.UPCB;
      BEGIN
	Space.Allocate(s, addr, CPU.PAGESIZE);
	Translation.Write(s, SigTramp.Code, ExecX86.UPCB);
      END;
      
    EXCEPT
    | VMError.E(ec) =>
      IO.Put("Exec.Execute : vm error(" & Fmt.Int(ec) & "\n");
      RAISE Error.E(NEW(File.ErrorT).init(ec)); 
    END;

     
    buf := NIL;
    argvAddrs := NIL;
      
    (* set the registers up for user mode *)
    state.cpustate.usp := stackPtr;
    state.cpustate.uss := ExecX86.UserDataSel;
    state.cpustate.eax := stackPtr;
    state.cpustate.uss := ExecX86.UserDataSel; 
    state.cpustate.ds := ExecX86.UserDataSel;
    state.cpustate.es := ExecX86.UserDataSel;
    state.cpustate.cs := ExecX86.UserCodeSel;
    state.cpustate.eflags := 16_00000202; (* interrupts enabled + various *)
                                      (* reserved flags               *)

    state.fpustate.Control := 16_362; (* various exceptions enabled *)
    state.fpustate.Status := 16_0; (* no exceptions detected *)
    state.fpustate.Tag := 16_ffff; (* all FPU registers empty *)

    IF Verbose THEN 
      IO.Put("Execed with pc: " & Fmt.Unsigned(state.cpustate.pc) & " sp: "
	     & Fmt.Unsigned(state.cpustate.usp) & " eax: "
	     & Fmt.Unsigned(state.cpustate.eax) & "\n");
      Log.Log("Execed with pc: " & Fmt.Unsigned(state.cpustate.pc) & " sp: "
	      & Fmt.Unsigned(state.cpustate.usp) & " eax: "
	      & Fmt.Unsigned(state.cpustate.eax) & "\n");

    END;

  END LoadFile;

BEGIN
END Exec.
