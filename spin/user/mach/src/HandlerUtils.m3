(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 04-Apr-96  David Dion (ddion) at the University of Washington
 *	Updated for spin-20
 *
 * 10-Mar-96  David Dion (ddion) at the University of Washington
 *	Export Debug variable and Print routine to improve efficiency.
 *	Add SimpleWait and EndWait to implement very basic wait mechanism
 *	for user-level shell.
 *
 * 15-Dec-95  David Dion (ddion) at the University of Washington
 *	Updated to spin-8 for CVS.
 *
 * 17-Aug-95  David Dion (ddion) at the University of Washington
 *      Created to provide syscall handler utility functions to other 
 *           system calls.
 *      Modified extensively and repeatedly.
 *
 *)
MODULE HandlerUtils;

IMPORT Space, Translation, Word, Fmt;
IMPORT IO;
IMPORT VMHandlers; (* for VM constants *)
IMPORT Sema;
(* IMPORT MachHandlers; *)(* for ServerTask - Externalize, Internalize *)
IMPORT UserSpaceThread; (* for Internalize/Externalize *)
IMPORT Textify;
IMPORT VMError;

(*
 * There are more parameters to a system call than could be passed
 * through registers a0 - a5.  Get additional paramaters off the stack.
 *)
PROCEDURE GetNArgsOffStack(space: Space.T; 
                           sp: Word.T; 
                           N: CARDINAL;
                           VAR extraArgs: 
                           ARRAY OF Word.T) : BOOLEAN =
  VAR
    stackbuf: ARRAY[0..63] OF CHAR; (* currently big enough for eight words *)
    bytesToRead: CARDINAL := N * 8;
    stackPtrLow, stackPtrHigh: CARDINAL;
  BEGIN
    (*
     * Perform some quick checks.  First, is extraArgs the correct size 
     * (does it agree with n)?  Second, is stackbuf big enough?  This
     * check could be eliminated through dynamic allocation, but that 
     * would be slow.  There must be eight bytes in stackbuf for each
     * arg to be grabbed off the stack.
     *)
    IF NUMBER(extraArgs) # N THEN
      PrintError("Bad argument N to GetNArgsOffStack.\n");
      RETURN FALSE;
    END;

    IF NUMBER(stackbuf) < bytesToRead THEN
      PrintError("Buffer too small to get args off stack.\n");
      RETURN FALSE;
    END;
    
    (* 
     * Read enough of the stack over.  An exception here will be caught
     * by whoever calls this procedure.  
     *)
    TRY
      Translation.Read(space, sp, SUBARRAY(stackbuf, 0, bytesToRead));
    EXCEPT
    | VMError.E(ec) =>
      PrintError("GetNArgsOffStack: vm exception :" & VMError.Message(ec) &
        "reading args off stack.\n");
      RETURN FALSE;
    ELSE
      PrintError("GetNArgsOffStack: unknown exception " & 
        "retrieving arguments from stack.\n");
      RETURN FALSE;
    END;



    (*
     * Filter out the arguments by using ArrayToWord.
     *)
    stackPtrLow := 0; (* beginning of first arg in stackbuf array *)
    stackPtrHigh := 7; (* end of first arg in stackbuf array *)
    FOR i := FIRST(extraArgs) TO LAST(extraArgs) DO
      ArrayToWord(extraArgs[i], stackbuf, stackPtrLow, stackPtrHigh);
      stackPtrLow := stackPtrLow + 8;
      stackPtrHigh := stackPtrHigh + 8;
    END;
    
    RETURN TRUE;
  END GetNArgsOffStack;

(*
 * Starting with the least significant byte and moving to the most
 * significant, copy bytes from Word.T data into ARRAY OF CHAR array 
 * at indexes start through finish.
 *)
PROCEDURE WordToArray(data: Word.T; VAR array: ARRAY OF CHAR; 
  start: CARDINAL; finish: CARDINAL) = 
  VAR
    temp: Word.T;
  BEGIN
    temp := data;
    IF finish > start THEN
      FOR i := start TO finish DO
        array[i] := VAL(Word.And(data, 16_ff), CHAR);
        temp := data;
        data := Word.RightShift(temp, 8);
      END;
    ELSE  (* must step down through loop - reversing bytes of data *)
      FOR i := start TO finish BY -1 DO
        array[i] := VAL(Word.And(data, 16_ff), CHAR);
        temp := data;
        data := Word.RightShift(temp, 8);
      END;
    END;
  END WordToArray;

(*
 * Starting with the least significant byte and moving to the most
 * significant, copy into Word.T data the bytes from ARRAY OF CHAR array
 * from index start to index finish.
 *)
PROCEDURE ArrayToWord(VAR data: Word.T; READONLY array: ARRAY OF CHAR;
  start: CARDINAL; finish: CARDINAL) =
  VAR
    ret, temp: Word.T;
  BEGIN
    ret := 0;
    IF finish > start THEN
      IF finish - start > 7 THEN
        RETURN;
      END;
      FOR i := start TO finish DO
        temp := ORD(array[i]);
        ret := Word.Insert(ret, temp, (i - start) * 8, 8);
      END;
    ELSE  (* must step down through loop - reversing bytes of data *)
      IF start - finish > 7 THEN
        RETURN;
      END;
      FOR i := start TO finish BY -1 DO
        temp := ORD(array[i]);
        ret := Word.Insert(ret, temp, (start - i) * 8, 8);
      END;
    END;
    data := ret;
  END ArrayToWord;

(*
 * Round x up to page size.
 *)
PROCEDURE RoundUpToPage(VAR x: Word.T) =
  BEGIN
    IF Word.Mod(x, VMHandlers.VM_PAGE_SIZE) # 0 THEN
      x := x + VMHandlers.VM_PAGE_SIZE - Word.Mod(x, VMHandlers.VM_PAGE_SIZE);
    END;
  END RoundUpToPage;

(*
 * Round x down to page size.
 *)
PROCEDURE RoundDownToPage(VAR x: Word.T) =
  BEGIN
    x := x - Word.Mod(x, VMHandlers.VM_PAGE_SIZE);
  END RoundDownToPage;

PROCEDURE DebugPrint(mesg: TEXT) =
  BEGIN
    IF Debug THEN
      Print(" " & mesg);
    END;
  END DebugPrint;

(*
 * Try to funnel all printing to the console through the Print or PrintError
 * procedure.  That way, if we ever want to change how we do it, we change 
 * it here.
 *)
PROCEDURE Print(mesg: TEXT) =
  BEGIN
    (*
     * NOTE:  this procedure assumes that the handler thread reader/writer
     *        has already been set.
     *)
    IO.Put(mesg);
  END Print;

PROCEDURE PrintError(mesg: TEXT) =
  BEGIN
    IO.PutError(mesg);
  END PrintError;

PROCEDURE SetDebugSyscall(switch: Word.T) : Word.T =
  VAR 
    oldval: Word.T;
  BEGIN
    IF Debug THEN oldval := 1; ELSE oldval := 0; END;
    IF switch # 0 THEN
      Print("Setting debug switch to TRUE.\n");
      Debug := TRUE;
    ELSE
      Print("Setting debug switch to FALSE.\n");
      Debug := FALSE;
    END;
    RETURN oldval;
  END SetDebugSyscall;

VAR
  WaitSema: Sema.T;

PROCEDURE SimpleWait() =
  VAR
    semaIsReset: BOOLEAN;
  BEGIN
    REPEAT
      TRY
        Sema.Reset(WaitSema);
        semaIsReset := TRUE;
      EXCEPT
      ELSE
        semaIsReset := FALSE;
        Sema.V(WaitSema);
      END;
    UNTIL semaIsReset;
    Sema.P(WaitSema);
  END SimpleWait;

PROCEDURE EndWait() =
  BEGIN
    Sema.V(WaitSema);
  END EndWait;

PROCEDURE Externalize(uthread: UserSpaceThread.T;
                      intptr: REFANY) : Word.T =
  BEGIN
    RETURN UserSpaceThread.GetSpace(uthread).getXrefTbl().externalize(intptr);
  END Externalize;

PROCEDURE Internalize(uthread: UserSpaceThread.T;
                      extptr: Word.T) : REFANY =
  BEGIN
    RETURN UserSpaceThread.GetSpace(uthread).getXrefTbl().internalize(extptr);
  END Internalize;

PROCEDURE CopyIn(space: Space.T; 
                 source: Word.T; 
                 VAR dest: ARRAY OF CHAR;
                 len: CARDINAL) : BOOLEAN =
  BEGIN
    TRY
      Translation.Read(space, source, SUBARRAY(dest, 0, len));
    EXCEPT
    | VMError.E =>
      RETURN FALSE;
    ELSE
      PrintError("Unknown exception in CopyIn: " &
        "addr 0x" & Fmt.Unsigned(source) & " size 0x" &
        Fmt.Unsigned(len) & " bytes\n");
      RETURN FALSE;
    END;

    RETURN TRUE;
  END CopyIn;

PROCEDURE CopyOut(space: Space.T; 
                  READONLY source: ARRAY OF CHAR; 
                  dest: Word.T;
                  len: CARDINAL) : BOOLEAN =
  BEGIN
    TRY
      Translation.Write(space, SUBARRAY(source, 0, len), dest);
    EXCEPT
    | VMError.E =>
      RETURN FALSE;
    ELSE
      PrintError("Unknown exception in CopyOut: " &
        "space 0x" & Textify.Ref(space) & 
        " dest 0x" & Fmt.Unsigned(dest) & " size 0x" &
        Fmt.Unsigned(len) & " bytes\n");
    END;

    RETURN TRUE;
  END CopyOut;

BEGIN
  WaitSema := Sema.Alloc(0);
END HandlerUtils.
