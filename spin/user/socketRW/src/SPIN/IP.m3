(* HISTORY
 * 11-Mar-96  Charles Garrett (garrett) at the University of Washington
 *	Most of the SRC functionality for IP has been removed.
 *
 *)

MODULE IP;

IMPORT Atom, AtomList, NetDb;


PROCEDURE NewEndpoint(name: TEXT; port: INTEGER): Endpoint RAISES {Error} =
  BEGIN
    TRY
      RETURN Endpoint{addr := NetDb.GetHostByName(name), port := port};
    EXCEPT
    | NetDb.HostNotFound =>
      RAISE Error(AtomList.List1(LookupFailure));
    END;
  END NewEndpoint;

PROCEDURE GetHostAddr() : Address =
  BEGIN
    TRY
      RETURN NetDb.GetHostByName("localhost");
    EXCEPT
    | NetDb.HostNotFound =>
      RETURN NullAddress;
    END;
  END GetHostAddr;

PROCEDURE GetHostByName(nm: TEXT; VAR (*out*) res: Address): BOOLEAN =
  BEGIN
    TRY
      res := NetDb.GetHostByName(nm);
    EXCEPT
    | NetDb.HostNotFound =>
      RETURN FALSE;
    END;
    RETURN TRUE;
  END GetHostByName;

BEGIN
  LookupFailure := Atom.FromText("IP.LookupFailure");
END IP.
