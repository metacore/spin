(*
 * HISTORY 
 * 4-oct-96  becker at the University of Washington
 *	Added /proc files
 *
 * 09-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	Updated to new version system, no longer based on SAL's vers.c.
 *
 * 19-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *	Print system version. Made to work again.
 *)
MODULE Version;
IMPORT IO, BuildInfo, ParseParams;
IMPORT InfoFile, Wr, Error;

PROCEDURE Run (<*UNUSED*>pp: ParseParams.T): BOOLEAN =
  VAR version       : TEXT;
      target        : TEXT;
      buildDate     : TEXT;
      builder       : TEXT;
      thisTree      : TEXT;
  BEGIN
    BuildInfo.GetInfo(version, target, buildDate, builder, thisTree);
    IO.Put(version & " built " &buildDate& " by " & builder & "\n");
    RETURN TRUE;
  END Run;

PROCEDURE VersionFile (wr: Wr.T) =
  VAR version       : TEXT;
      target        : TEXT;
      buildDate     : TEXT;
      builder       : TEXT;
      thisTree      : TEXT;
  BEGIN
    BuildInfo.GetInfo(version, target, buildDate, builder, thisTree);
    IO.Put(version & " built by " &builder& " at " &buildDate& "\n" & 
	   " in " & thisTree & "\n", wr);
  END VersionFile;

BEGIN
  TRY
     InfoFile.Create("/proc/version",VersionFile);
  EXCEPT
  | Error.E(e) =>
    IO.PutError("gc procfs files:" & e.message() & "\n");
  END;
END Version.
