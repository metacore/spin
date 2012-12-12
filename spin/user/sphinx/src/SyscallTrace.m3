(*
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 *)

MODULE SyscallTrace;
IMPORT Text;
IMPORT Trap;
IMPORT CPU;
IMPORT Strand;
IMPORT IO, Fmt;
IMPORT Dispatcher;
IMPORT SphinxInfo;
IMPORT SphinxUtils;
IMPORT Sieg;
IMPORT MachineSyscallTrace;
IMPORT Proc, ProcRep;
<*NOWARN*>
IMPORT Log;
IMPORT UserSpaceThread, Translation;

FROM SphinxUtils IMPORT Msg;

PROCEDURE Put(t: TEXT) =
  BEGIN
    CASE output OF
    | Output.Log =>
      Log.Log(t);
    | Output.Console =>
      IO.Put(t);
    END;
  END Put;

PROCEDURE ToHex (x: [0 .. 15]): CHAR =
  BEGIN
    IF x < 10 THEN
      RETURN VAL(x + ORD('0'), CHAR);
    ELSE
      RETURN VAL(x - 10 + ORD('A'), CHAR);
    END;
  END ToHex;
  
PROCEDURE GetTextFromUserSpace (c: Sieg.Context;
				addr, maxLen: INTEGER): TEXT =
  VAR
    t: TEXT;
    buf: ARRAY [0 .. 1] OF CHAR;
    buf2: ARRAY [0 .. 511] OF CHAR;
    j: CARDINAL;
  BEGIN
    maxLen := MIN(maxLen, LAST(buf2)-4);
    
    TRY
      LOOP
	Translation.Read(c.space, addr, buf);
	IF buf[0] = '\000' THEN
	  EXIT;
	END;
	IF buf[0] >= ' ' AND buf[0] < VAL(16_7F, CHAR) THEN
	  buf2[j] := buf[0];
	  INC(j);
	ELSE
	  buf2[j] := 'x';
	  buf2[j+1] := ToHex(ORD(buf[0]) DIV 16);
	  buf2[j+2] := ToHex(ORD(buf[0]) MOD 16);
	  INC(j, 3);
	END;
	IF j >= maxLen THEN
	  EXIT;
	END;
	INC(addr);
      END;
      t := Text.FromChars(SUBARRAY(buf2, 0, j));
    EXCEPT
    ELSE
      t := "inv-ptr(" & Fmt.Int(addr, 16) & ")";
    END;
    RETURN t;
  END GetTextFromUserSpace;

PROCEDURE SyscallName (num: INTEGER): TEXT =
  VAR name: TEXT;
  BEGIN
    name := SphinxInfo.ProcNames[num];
    IF name = NIL THEN
      name := "syscall(" & Fmt.Int(num) & ")";
    END;
    RETURN name;
  END SyscallName;

PROCEDURE NProcs (): CARDINAL =
  VAR
    itr := Proc.Iterate();
    proc: Proc.T;
    n := 0;
  BEGIN
    WHILE itr.next(proc) DO
      INC(n);
    END;
    RETURN n;
  END NProcs;
  
PROCEDURE Syscall (strand: Strand.T; VAR ss: CPU.SavedState) =
  VAR
    args: ARRAY [0 .. 7] OF TEXT;
    arg: MachineSyscallTrace.Args;
    context := Sieg.Context{caller:= UserSpaceThread.Self(),
			    space := Translation.GetCurrent()};
    proc := Proc.Self();
  BEGIN
    MachineSyscallTrace.GetArgs(strand, ss, arg);
    IF arg.syscallNum < SphinxInfo.MinProcID
      OR arg.syscallNum > SphinxInfo.MaxProcID THEN
      Msg("syscall no. out of range(" & Fmt.Int(arg.syscallNum) & "), args=");
      FOR i := 0 TO LAST(arg.args) DO
	IO.Put(" 0x"&Fmt.Int(arg.args[i], 16));
      END;
      IO.Put("\n");
      RETURN;
    END;
    
    CASE arg.syscallNum OF
    | SphinxInfo.ProcID_Write =>
      args[0] := Fmt.Int(arg.args[0], 16);
      args[1] := GetTextFromUserSpace(context, arg.args[1], arg.args[2]);
      args[2] := Fmt.Int(arg.args[2], 16);
    | SphinxInfo.ProcID_Execve =>
      args[1] := GetTextFromUserSpace(context, arg.args[0], 128);
      args[1] := Fmt.Int(arg.args[1], 16);
      args[2] := Fmt.Int(arg.args[2], 16);
    ELSE
      FOR i := 0 TO 5 DO
	CASE SphinxInfo.ProcArgs[arg.syscallNum][i] OF
	| 0 =>
	  (* end of arg *)
	  EXIT; 
	| 1 =>
	  args[i] := Fmt.Int(arg.args[i], 16);
	| 2 =>
	  (* ctext argument *)
	  args[i] := GetTextFromUserSpace(context, arg.args[i], 128);
	END;
      END;
    END;
    INC(proc.nthSyscall);
    IF NProcs() > 1 THEN
      Put(SphinxUtils.ProcName() & ":");
    END;
    Put(Fmt.Int(proc.nthSyscall) & ":");
    Put(SyscallName(arg.syscallNum));
    FOR i := 0 TO 5 DO
      IF args[i] = NIL THEN EXIT; END;
      Put(" " & args[i]);
    END;
    Put(".\n");
  END Syscall;

PROCEDURE Syscall2 (strand: Strand.T; VAR ss: CPU.SavedState) =
  VAR
    arg: MachineSyscallTrace.Args;
    proc := Proc.Self();
  BEGIN
    MachineSyscallTrace.GetArgs(strand, ss, arg);
    Put(Fmt.Int(proc.nthSyscall) & ":retval=" & 
	Fmt.Int(arg.retval, 16) & ".\n");
  END Syscall2;
  
VAR tracer: ARRAY [0..1] OF Dispatcher.Binding;

PROCEDURE Install (proc: Proc.T): Dispatcher.Binding =
  VAR
    strand: Strand.T := NIL;
    b: ARRAY [0..1] OF Dispatcher.Binding;
  BEGIN
    IF proc = NIL THEN
      IF tracer[0] # NIL THEN
	IO.PutError("SyscallTrace: already installed.\n");
	RETURN NIL;
      END;
    ELSE
      strand := proc.thread;
    END;
    
    TRY
      b[0] := Dispatcher.InstallHandler(Trap.Syscall, NIL, Syscall,
			  options:=Dispatcher.Options{Dispatcher.Opt.First},
			  key:=NEW(Trap.AuthKey,
				   strand := strand,
				   minProcID := -100000,
				   maxProcID := 10000));
      b[1] := Dispatcher.InstallHandler(Trap.Syscall, NIL, Syscall2,
			  options:=Dispatcher.Options{Dispatcher.Opt.Last},
			  key:=NEW(Trap.AuthKey,
				   strand := strand,
				   minProcID := -100000,
				   maxProcID := 100000)); 
    EXCEPT
    | Dispatcher.Error =>
      IO.Put("SyscallTrace.Install: dispatcher error.\n");
    END;
    IF proc = NIL THEN
      tracer := b;
    END;
    RETURN b[0];
  END Install;

PROCEDURE Uninstall() =
  BEGIN
    TRY
      FOR i := FIRST(tracer) TO LAST(tracer) DO
	IF tracer[i] # NIL THEN 
	  Dispatcher.Uninstall(tracer[i]);
	  tracer[i] := NIL;
	END;
      END;
    EXCEPT
    | Dispatcher.Error =>
      IO.Put("SyscallTrace.Uninstall: dispatcher error.\n");
    END;
  END Uninstall;

BEGIN
  output := Output.Console;
END SyscallTrace.
