MODULE Commands;
IMPORT Dispatcher, ParseParams, Shell, IO, Fmt;
IMPORT Text;
REVEAL T = TPublic BRANDED OBJECT
  helpBinding : Dispatcher.Binding;
  runBinding  : Dispatcher.Binding;
END;

PROCEDURE ParseError(closure: TPublic) = 
  BEGIN
    TYPECASE closure OF
    | NULL =>
    | T(t) =>
      Help(t, NIL, TRUE);
    ELSE
    END;
  END ParseError;

FUNCTIONAL PROCEDURE Guard (closure: T; pp: ParseParams.T): BOOLEAN =
  BEGIN
    RETURN pp.ftestNext(closure.name);
  END Guard;

FUNCTIONAL PROCEDURE HelpGuard(closure: T;
			       name: TEXT;
			       <*UNUSED*>detailed: BOOLEAN):BOOLEAN =
  BEGIN
    RETURN name = NIL OR Text.Equal(name, closure.name);
  END HelpGuard;

PROCEDURE Help(closure: T; <*UNUSED*>name: TEXT; detailed: BOOLEAN) =
  BEGIN
    IO.Put(closure.name & " " & closure.synopsis);
    IO.Put("\n");
    IF detailed THEN
      IO.Put(": " & closure.help);
      IO.Put("\n");
    END;
  END Help;

PROCEDURE Error(closure: T; t: TEXT; ec: Dispatcher.ErrorCode) =
  BEGIN
    IO.Put( closure.name
	   & " Dispatcher failure  " & t & ":" & Fmt.Int(ORD(ec)));
  END Error;

PROCEDURE Install (Run : PROCANY;
		   command, synopsis: TEXT;
		   help1: TEXT := NIL;
		   help2: TEXT := NIL;
		   help3: TEXT := NIL;
		   help4: TEXT := NIL;
		   help5: TEXT := NIL): T =
  VAR
    closure: T;
    help := "";
  BEGIN
    IF help1 # NIL THEN help := help1; END;
    IF help2 # NIL THEN help := help & help2; END;
    IF help3 # NIL THEN help := help & help3; END;
    IF help4 # NIL THEN help := help & help4; END;
    IF help5 # NIL THEN help := help & help5; END;
    
    closure := NEW(T, Run := Run, name := command,
		   synopsis := synopsis, help := help);
    TRY
      closure.helpBinding := Dispatcher.InstallHandler(
                                 Shell.Help, HelpGuard, Help, 
                                 guardClosure   := closure,
                                 handlerClosure := closure,
                                 options := Dispatcher.DefaultOptions);

    EXCEPT
    | Dispatcher.Error (ec) => 
      Error(closure,"Install help handler ", ec);
      RETURN NIL;
    END;
    TRY
      closure.runBinding :=  Dispatcher.InstallHandler(
                                 Shell.Run, Guard, Run,
                                 guardClosure   := closure,
                                 handlerClosure := closure,
                                 options := Dispatcher.DefaultOptions);
    EXCEPT
    | Dispatcher.Error (ec) => 
      Error(closure,"Install run handler ", ec);
      RETURN NIL;
    END;
    RETURN closure;
  END Install;

PROCEDURE Uninstall (b: T) =
  BEGIN
    TRY
      IF b.helpBinding # NIL THEN Dispatcher.Uninstall(b.helpBinding); END;
      b.helpBinding := NIL;
      IF b.runBinding # NIL THEN Dispatcher.Uninstall(b.runBinding); END;
      b.runBinding := NIL;
    EXCEPT
    | Dispatcher.Error(ec) =>
      IO.Put(b.name & ": uninstallation failure : " & Fmt.Int(ORD(ec)))
    END;
  END Uninstall;
  
BEGIN
END Commands.
