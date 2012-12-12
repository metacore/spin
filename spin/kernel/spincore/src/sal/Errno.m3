MODULE Errno;
IMPORT ErrnoDep, Fmt AS FmtInterface;

PROCEDURE Fmt(err: T): TEXT =
  BEGIN
    CASE err OF
    | ErrnoDep.FirstError..ErrnoDep.LastError => RETURN ErrnoDep.ErrorMessages[err];
    ELSE
      RETURN "Errno " & FmtInterface.Int(err);
    END
  END Fmt;

BEGIN
END Errno.
