
MODULE Ephemeral;

IMPORT Scope, Module;

PROCEDURE Check () : Return =
  BEGIN
    IF Scope.InBounded (Scope.Top ()) THEN
      IF Module.IsSafe () THEN
        RETURN Return.Error;
      ELSE
        RETURN Return.Warn;
      END;
    ELSE
      RETURN Return.Ok;
    END;
  END Check;

BEGIN
END Ephemeral.
