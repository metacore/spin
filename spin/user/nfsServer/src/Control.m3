MODULE Control;
IMPORT ParseParams, ControlCmd;
IMPORT PM, Nfsd, Mountd, NfsdImpl;
IMPORT IO;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  PROCEDURE DebugOnOff() : BOOLEAN
    RAISES {ParseParams.Error} =
    BEGIN
      IF pp.testNext("on") THEN 
        RETURN TRUE;
      ELSIF pp.testNext("off") THEN 
        RETURN FALSE;
      END;
      RETURN FALSE;
    END DebugOnOff;

  BEGIN
    pp.reset();
    TRY
      pp.skipNext(); (* etherdefault *)
      IF pp.testNext("-zap") THEN
        Uninstall();
        ControlCmd.Uninstall();
      ELSIF pp.testNext("-install") THEN
        Install();
      ELSIF pp.testNext("-uninstall") THEN
        Uninstall();
      ELSIF pp.testNext("-debug") THEN
      IF pp.testNext("pm") THEN
        PM.debug := DebugOnOff();
      ELSIF pp.testNext("nfsd") THEN
        Nfsd.debug := DebugOnOff();
      ELSIF pp.testNext("mountd") THEN
        Mountd.debug := DebugOnOff();
      END;        
      ELSE
        Usage();
      END;
    EXCEPT
    | ParseParams.Error => Usage();
    END;
   RETURN TRUE;
 END Run;

PROCEDURE Usage() = 
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

PROCEDURE Install() = 
  BEGIN
    PM.Init(TRUE);
    Mountd.Init(TRUE);
    Nfsd.Init(TRUE);
    NfsdImpl.Init2(TRUE);
  END Install;

PROCEDURE Uninstall() = 
  BEGIN
    PM.Uninit(TRUE);
    Mountd.Uninit(TRUE);
    Nfsd.Uninit(TRUE);
    NfsdImpl.Uninit2(TRUE);
  END Uninstall;

BEGIN
END Control.
