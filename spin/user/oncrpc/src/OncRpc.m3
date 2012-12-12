(*
 * Copyright 1994, 1995, 1996, 1997 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 *
 * 07-Aug-97  Robert Grimm (rgrimm) at the University of Washington
 *      Updated to new security management
 *
 * 13-Jun-97  Wilson Hsieh (whsieh) at the University of Washington
 *	remove EXPORT of Main
 *
 * 11-Feb-97  Robert Grimm (rgrimm) at the University of Washington
 *      Replaced Identity with SecurityContext
 *
 * 26-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added an argument to authorization. 
 *
 *)


MODULE OncRpc;
IMPORT OncRpcInterface;
IMPORT Auth, SecurityError, SecurityManager, NameServer;
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
             SecurityManager.GetUserName(SecurityManager.GetCurrentUid())
             & " \n" );
    EXCEPT
    | SecurityError.T =>
      IO.Put("Request to authorize user id " &
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
    EVAL OncRpcInterface.Export(t);
  EXCEPT
  | NameServer.Error(ec)  => 
    IO.PutError("OncRpc init FAILED\n");
  END;
  IO.Put("OncRpc init done.\n");
END OncRpc.

