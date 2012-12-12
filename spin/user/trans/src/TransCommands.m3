(*
 * Copyright 1994-96 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 16-Nov-96  Yasushi Saito (yasushi) at the University of Washington
 *	Changed to table jump impl.
 * 05-Apr-96  Yasushi Saito (yasushi) at the University of Washington
 *	Created.
 *)
MODULE TransCommands;
IMPORT IO;
IMPORT Text;
IMPORT Scan;
IMPORT Lex;
IMPORT Fmt;
IMPORT Transaction;
IMPORT Error;
IMPORT Storage;
IMPORT FastIntRefTbl;
IMPORT TransOS;
IMPORT TransGroup, TransGroupPrivate;
IMPORT TransMode;
IMPORT TransUtils;
IMPORT TransDaemon;
IMPORT WAL, WALPrivate;
IMPORT TransRPC;
IMPORT RawIO;
IMPORT Buffer;

CONST Commands = ARRAY OF Command
  {
   Command{"abort", "TRANS", Abort},
   Command{"begin", " [undo|noundo]", Begin},
   Command{"checkpoint", ": take checkpoint of everything", CheckPoint},
   Command{"close", "ST", Close},
   Command{"commit", "TRANS", Commit},
   Command{"debug", "[on|off]: debug on/off.", Debug},
   Command{"end", "shutdown the trans system.", End},
   Command{"crash", "crash the trans system.", Crash},
   Command{"nukelog", ": clear the log", NukeLog},
   Command{"nukebuf", ": nuke the file buffer", NukeBuf},
   Command{"open", " FILE", Open},
   Command{"port", "[NUM]:get/set TPC port # for RPC.", Port},
   Command{"write", "FROM STRING", Write},
   Command{"server", "[PORT]", Server},
   Command{"stat", " : display some info.", Stat},
   Command{"stopserver", " : stop the server.", StopServer}
   };

VAR
  otherCommands: REF ARRAY OF Command;
  s: Storage.T; 
  t: Transaction.T;
  id: INTEGER;
  tableOfThings := NEW(FastIntRefTbl.Default).init();
  nThings: INTEGER := 0;
  group := TransGroup.default;

PROCEDURE AddCommands (READONLY com: ARRAY OF Command) =
  VAR n: REF ARRAY OF Command;
  BEGIN
    IF otherCommands = NIL THEN
      n := NEW(REF ARRAY OF Command, NUMBER(com));
      n^ := com;
    ELSE
      n := NEW(REF ARRAY OF Command, NUMBER(otherCommands^)+NUMBER(com));
      SUBARRAY(n^, 0, NUMBER(otherCommands^)) := otherCommands^;
      SUBARRAY(n^, NUMBER(otherCommands^), NUMBER(com)) := com;
    END;
    otherCommands := n;
  END AddCommands;

PROCEDURE Server (READONLY argv: ARRAY OF TEXT) =
  VAR port: CARDINAL := 0;
  BEGIN
    IF NUMBER(argv) > 1 THEN
      port := Scan.Int(argv[1]);
    END;
    TransDaemon.Loop(port);
  END Server;
  
PROCEDURE StopServer (<*UNUSED*>READONLY argv: ARRAY OF TEXT) =
  BEGIN
    TransDaemon.Stop();
  END StopServer;

PROCEDURE Stat (READONLY argv: ARRAY OF TEXT) =
  BEGIN
    IF NUMBER(argv) > 1 THEN
      IF Text.Equal(argv[1], "on") THEN
	TransUtils.GatherStats := TRUE;
      ELSE
	TransUtils.GatherStats := FALSE;
      END;
    ELSE
      TransGroupPrivate.PrintStat();
      WAL.PrintStat(Transaction.GetDefaultLogDevice());
      RawIO.PrintStat();
    END;
  END Stat;
  
PROCEDURE Debug (READONLY argv: ARRAY OF TEXT) =
  BEGIN
    IF NUMBER(argv) > 1 THEN
      IF Text.Equal(argv[1], "on") THEN
	(* *)
      ELSE
	(* *)
      END;
    END;
    IO.Put("New debug value: " & Fmt.Int(ORD(TransUtils.Debug)) & ".\n");
  END Debug;
  
PROCEDURE Port (READONLY argv: ARRAY OF TEXT) =
  VAR newPort: CARDINAL;
  BEGIN
    IF NUMBER(argv) > 1 THEN
      TRY
	newPort := Scan.Int(argv[1]);
      EXCEPT
      | Lex.Error =>
	IO.Put("port number " & argv[1] & " is not a number.\n");
	newPort := 8888;
      END;
      TransRPC.Port := newPort;
    END;
    IO.Put("New port number: " & Fmt.Int(TransRPC.Port) & ".\n");
  END Port;
  
PROCEDURE End (<*UNUSED*>READONLY argv: ARRAY OF TEXT) =
  BEGIN
    group.shutDown();
    Respond("ok\n");
    TransOS.Exit(0);
  END End;
  
PROCEDURE Crash (<*UNUSED*>READONLY argv: ARRAY OF TEXT) =
  VAR i := 10;
  BEGIN
    <*ASSERT i < 0*>
    Respond("ok\n");
    TransOS.Exit(0);
  END Crash;

PROCEDURE Open (READONLY argv: ARRAY OF TEXT) RAISES {Error.E} =
  BEGIN
    s := Storage.Open(argv[1], TransGroup.default);
    id := InternThing(s);
    Respond(Fmt.Int(id) & ".\n");
  END Open;
  
PROCEDURE Begin (READONLY argv: ARRAY OF TEXT) =
  VAR mode := TransMode.Default;
  BEGIN
    (* Parse mode flags *)
    IF NUMBER(argv) > 1 THEN
      mode := TransMode.Set{};
      FOR i := 1 TO LAST(argv) DO
	IF Text.Equal(argv[i], "nolog") THEN
	  mode := mode + TransMode.Set{TransMode.T.NoLogging};	  
	ELSIF Text.Equal(argv[i], "nolock") THEN
	  mode := mode + TransMode.Set{TransMode.T.NoLocks};
	ELSE
	  IO.Put("trans mode: " & argv[i] & " is not known.\n");
	END;
      END;
    END;

    t := Transaction.Begin(group, mode);
  
    IO.Put("transaction " & Fmt.Int(Transaction.GetTID(t))
	   & " started.\n");
    id := InternThing(t);
    Respond(Fmt.Int(id) & ".\n");
  END Begin;
  
PROCEDURE Write (READONLY argv: ARRAY OF TEXT) RAISES {Lex.Error} =
  VAR
    st := NARROW(GetThing(argv[1]), Storage.T);
    pos := Scan.Int(argv[2]);
    str := argv[3];
    newImage: REF ARRAY OF CHAR;
  PROCEDURE Callback (VAR buf: ARRAY OF CHAR; curPos: CARDINAL) =
    BEGIN
      buf := SUBARRAY(newImage^, curPos-pos, NUMBER(buf));
    END Callback;
  BEGIN
    newImage := NEW(REF ARRAY OF CHAR, Text.Length(str));
    Text.SetChars(newImage^, str);
    st.access(t, pos, NUMBER(newImage^), Callback);
    Respond("ok\n");
  END Write;
  
PROCEDURE Commit (READONLY argv: ARRAY OF TEXT) RAISES {Error.E} =
  BEGIN
    t := NARROW(GetThing(argv[1]), Transaction.T);
    EVAL Transaction.Commit(t);
    Respond("ok\n");
  END Commit;
  
PROCEDURE Abort (READONLY argv: ARRAY OF TEXT) RAISES {Error.E} =
  BEGIN
    t := NARROW(GetThing(argv[1]), Transaction.T);
    Transaction.Abort(t);
    Respond("ok\n");
  END Abort;

PROCEDURE Close (READONLY argv: ARRAY OF TEXT) =
  BEGIN
    s := NARROW(GetThing(argv[1]), Storage.T);
    s.close(group);
    Respond("ok\n");
  END Close;
  
PROCEDURE CheckPoint (<*UNUSED*>READONLY argv: ARRAY OF TEXT) RAISES {Error.E} =
  BEGIN
    Transaction.CheckPoint();
    Respond("ok\n");
  END CheckPoint;

PROCEDURE NukeLog (READONLY argv: ARRAY OF TEXT) =
  VAR fileName: TEXT;
  BEGIN
    IF NUMBER(argv) >= 2 THEN
      fileName := argv[1];
    ELSE
      fileName := TransOS.GetDefaultLogFileName();
    END;
    IO.Put("nuking " & fileName & ".\n");
    WALPrivate.Nuke(fileName);
  END NukeLog;

PROCEDURE NukeBuf (<*UNUSED*>READONLY argv: ARRAY OF TEXT) =
  BEGIN
    Buffer.Nuke();
  END NukeBuf;
  
PROCEDURE InternThing (r: REFANY): INTEGER =
  BEGIN
    INC(nThings);
    EVAL tableOfThings.put(nThings, r);
    RETURN nThings;
  END InternThing;

  
PROCEDURE GetThing (id: TEXT): REFANY =
  VAR r: REFANY;
  BEGIN
    TRY
      IF NOT tableOfThings.get(Scan.Int(id), r) THEN
	IO.Put("ID " & id & " not found.\n");
      END;
    EXCEPT
    | Lex.Error =>
      IO.Put("expecting integer, but got " & id & ".\n");
      r := NIL;
    END;
    RETURN r;
  END GetThing;

PROCEDURE Respond (s: TEXT) =
  BEGIN
    IO.Put("@>>" & s);
  END Respond;

PROCEDURE Execute (READONLY argv: ARRAY OF TEXT) =
  BEGIN
    TRY
      IF otherCommands # NIL THEN 
	FOR i := 0 TO LAST(otherCommands^) DO
	  IF Text.Equal(argv[0], otherCommands[i].name) THEN
	    otherCommands[i].proc(argv);
	    RETURN;
	  END;
	END;
      END;
      FOR i := 0 TO LAST(Commands) DO
	IF Text.Equal(argv[0], Commands[i].name) THEN
	  Commands[i].proc(argv);
	  RETURN;
	END;
      END;

      IF otherCommands # NIL THEN
	FOR i := 0 TO LAST(otherCommands^) DO
	  IO.Put(otherCommands[i].name & ": " & otherCommands[i].help & ".\n");
	END;
      END;
      FOR i := 0 TO LAST(Commands) DO
	IO.Put(Commands[i].name & ": " & Commands[i].help & ".\n");
      END;
    EXCEPT
    | IO.Error =>
      IO.Put("IO error\n");
      TransOS.Exit(1);
    | Error.E(e) =>
      IO.Put("Trans error" & e.message() & "\n");
    | Lex.Error=>
      IO.Put("lex error ...\n");
    END;
  END Execute;
  
BEGIN
END TransCommands.
