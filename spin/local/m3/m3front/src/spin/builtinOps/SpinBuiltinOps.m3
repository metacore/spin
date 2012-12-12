MODULE SpinBuiltinOps;
IMPORT InterfaceUnit, ThisModule, View;

PROCEDURE Initialize () =
  BEGIN
    InterfaceUnit.Initialize();
    ThisModule.Initialize();
    View.Initialize ();
  END Initialize;

BEGIN
END SpinBuiltinOps.
