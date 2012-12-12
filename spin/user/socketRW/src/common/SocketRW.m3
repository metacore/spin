(* HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Switched over to new security manager
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Removed Identity
 *
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Converted to a SPIN extension.
 *
 *)

MODULE SocketRW;

IMPORT SocketRWInterface;
IMPORT SecurityError, SecurityManager;
IMPORT Auth, NameServer;
IMPORT IO, Fmt;

TYPE
  T = Auth.T OBJECT
    all: BOOLEAN;
  OVERRIDES
    authorize := Authorize;
  END;

PROCEDURE Authorize (self: T; 
                     <* UNUSED *> key: Auth.Key;
                     <* UNUSED *> arg: REFANY): BOOLEAN =
  VAR b: BOOLEAN;
  BEGIN
    TRY
      IO.Put("Request to authorize " &
             SecurityManager.GetUserName(SecurityManager.GetCurrentUid()) &
             "\n");
    EXCEPT
    | SecurityError.T =>
      IO.Put("Request ot authorize " &
             Fmt.Int(SecurityManager.GetCurrentUid()) & "\n");
    END;
    IF self.all THEN
      IO.Put(" -- OK\n");
      b := TRUE;
    ELSE
      IO.PutError(" -- Failed\n");
      b := FALSE;
    END;
    RETURN b;
  END Authorize;


VAR t : T;
BEGIN
  TRY
    t := NEW(T);
    t.all := TRUE;
    EVAL SocketRWInterface.Export(t);
  EXCEPT
  | NameServer.Error  => 
    IO.PutError("SocketRW init FAILED\n");
  END;

  IO.Put("SocketRW init done.\n");
END SocketRW.


