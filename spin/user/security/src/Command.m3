MODULE Command;

IMPORT ParseParams, Fmt, IO;
IMPORT DebugOption;
IMPORT SecurityManager, SecurityManagerPrivate;
IMPORT Performance;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
  BEGIN
    pp.reset();
    TRY
      pp.skipNext();

      IF    pp.testNext("whoami") THEN
        WhoAmI();
      ELSIF pp.testNext("performance") THEN
        IF SecurityManager.CheckSecurity() THEN
          Performance.Run();
        ELSE
          IO.Put("Security is disabled!\n");
        END;
      ELSE
        Usage();
      END;
    EXCEPT
      ParseParams.Error => Usage();
      ELSE IO.Put("Random exception in " & CommandName & "\n");
    END;
    RETURN TRUE;
  END Run;

PROCEDURE Usage() =
  BEGIN
    IO.Put("Usage: " & CommandName & ":" & CommandHelp & "\n");
  END Usage;

PROCEDURE WhoAmI() =
  VAR
    uid    : SecurityManager.UID;
    sid    : SecurityManager.SID;
    oid    : SecurityManager.SID;
    gids   : REF ARRAY OF SecurityManager.GID;
    uname  : TEXT;
    sname  : TEXT;
    oname  : TEXT;
    gnames : REF ARRAY OF TEXT;
  BEGIN
    IO.Put("\n");
    uid  := SecurityManager.GetCurrentUid();
    sid  := SecurityManager.GetCurrentSubjectSid();
    oid  := SecurityManager.GetCurrentObjectSid();
    TRY
      uname := SecurityManager.GetUserName( uid );
      sname := SecurityManager.GetSidName ( sid );
      oname := SecurityManager.GetSidName ( oid );
    EXCEPT
    ELSE
      BEGIN
        IO.Put("*** Couldn't resolve uid and sid names ***\n");
        uname := "";
        sname := "";
        oname := "";
      END;
    END;
    TRY
      gids  := SecurityManager.GetGroups( uid );
    EXCEPT
    ELSE
      BEGIN
        IO.Put("*** Couldn't get group ids ***\n");
        gids := NEW( REF ARRAY OF SecurityManager.GID, 0 );
      END;
    END;
    gnames := NEW( REF ARRAY OF TEXT, LAST(gids^)-FIRST(gids^)+1 );
    FOR i := 0 TO LAST(gids^)-FIRST(gids^) DO
      TRY
        gnames[i] := SecurityManager.GetGroupName(gids[FIRST(gids^)+i]);
      EXCEPT
      ELSE
        BEGIN
          IO.Put("*** Couldn't resolve gid name ***\n");
          gnames[i] := "";
        END;
      END;
    END;

    IF DebugOption.Security THEN
      IO.Put("   Thread security is enabled!\n");
    ELSE
      IO.Put("   Thread security is disabled!\n");
    END;
    IF SecurityManagerPrivate.CheckTypeSecurity() THEN
      IO.Put("   Object security is enabled!\n");
    ELSE
      IO.Put("   Object security is disabled!\n");
    END;

    IO.Put("   User        : " & Fmt.Int(uid) & " \"" & uname & "\"\n");
    IO.Put("   Subject sid : " & Fmt.Int(sid) & " \"" & sname & "\"\n");
    IO.Put("   Object sid  : " & Fmt.Int(oid) & " \"" & oname & "\"\n");
    IO.Put("   Groups      : \n");
    FOR i := FIRST(gnames^) TO LAST(gnames^) DO
      IO.Put("                 " & Fmt.Int(gids[FIRST(gids^)+i]) & " \"" &
             gnames[i] &"\"\n");
    END;
  END WhoAmI;

BEGIN
END Command.
