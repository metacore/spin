MODULE DispPost;

IMPORT IO, Dispatcher, Fmt, DispPost;

PROCEDURE Event (a: INTEGER) =
  BEGIN
    IO.Put("Event: " & Fmt.Int(a) & "\n");
  END Event;

PROCEDURE Handler (a: INTEGER) =
  BEGIN
    IO.Put("Handler: " & Fmt.Int(a) & "\n");
  END Handler;

PROCEDURE Guard (a: INTEGER): BOOLEAN =
  BEGIN
    RETURN a = 13;
  END Guard;

VAR
  z : BOOLEAN;

PROCEDURE PreGuard (a: INTEGER): BOOLEAN =
  BEGIN
    IO.Put("Pre: " & Fmt.Int(a) & "\n");
    RETURN z;
  END PreGuard;

PROCEDURE PostGuard (a: INTEGER) =
  BEGIN
    IO.Put("Post: " & Fmt.Int(a) & "\n");
  END PostGuard;

PROCEDURE Test () =
  VAR
    binding: Dispatcher.Binding;
    imposed: Dispatcher.ImposedGuard;
  BEGIN
    IO.Put("Start...\n");

    IO.Put(">> E7 E13...\n");
    DispPost.Event(7);
    DispPost.Event(13);

    IO.Put(">> E7 E13 H13...\n");
    binding := Dispatcher.InstallHandler(Event, Guard, Handler);
    DispPost.Event(7);
    DispPost.Event(13);

    imposed := Dispatcher.ImposeGuard(binding, PreGuard, NIL, PostGuard, NIL);

    IO.Put(">> E7 E13 Pre13 H13 Post13...\n");
    z := TRUE;
    DispPost.Event(7);
    DispPost.Event(13);

    IO.Put(">> E7 E13 Pre13 Post13...\n");
    z := FALSE;
    DispPost.Event(7);
    DispPost.Event(13);

    IO.Put(">> E7 E13 H13...\n");
    Dispatcher.UninstallGuard(imposed);
    DispPost.Event(7);
    DispPost.Event(13);

    IO.Put(">> E7 E13...\n");
    Dispatcher.Uninstall(binding);
    DispPost.Event(7);
    DispPost.Event(13);

    IO.Put("End...\n");
  END Test;

BEGIN
  Test();
END DispPost.
