MODULE StcpShell;

IMPORT Text, Word, IO, Fmt, ParseParams, StcpShellCmd;
IMPORT SimpleHttp,CPU;

CONST
  MaxBytes = 4 * 1024 * 1024;


PROCEDURE Fetch(path: TEXT) =
  VAR
    (* buf := NEW(REF ARRAY OF CHAR, MaxBytes); *)
    buf : REF ARRAY OF CHAR;
    bytes,start,stop,usecs : Word.T;
    server := BuildInfo.GetHttpServAddr();
  BEGIN
    TRY
      IO.Put("fetch "&path&"\n");
      start := CPU.CycleCounter();
      buf := SimpleHttp.Get(path, server);
      stop := CPU.CycleCounter();
      bytes := BYTESIZE(buf^);
      usecs := CPU.CycleToMicrosec(CPU.CycleMinus(stop,start));
      IO.Put(Fmt.Int(bytes)&" bytes, " &Fmt.Int(usecs DIV 1000)&" msecs, ");
      IO.Put(Fmt.Int((8*bytes) DIV (usecs DIV 1000))&" Kbps\n");
    EXCEPT
    END;
  END Fetch;

PROCEDURE Zap() =
  BEGIN
    StcpShellCmd.Uninstall();
    IO.Put("Stcp is uninstalled\n");
  END Zap;

PROCEDURE Run (pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();             (* skip arg0 *)
      IF pp.testNext("zap") THEN Zap();
      ELSIF pp.testNext("fetch") THEN Fetch(pp.getNext());
      END;
      RETURN TRUE;
    EXCEPT
    | ParseParams.Error =>
        IO.Put(CommandName & CommandHelp & "\n");
       RETURN FALSE;
    END;
  END Run;

BEGIN
END StcpShell.
