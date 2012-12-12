(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

UNSAFE MODULE DrawPerfShell;

IMPORT ParseParams;
IMPORT DrawPerfShellCmd;
IMPORT DrawPerfTests, IO;

PROCEDURE Run(pp: ParseParams.T): BOOLEAN =
BEGIN
  pp.reset();
  TRY
    pp.skipNext();
    IF pp.testNext("-zap") THEN
      DrawPerfShellCmd.Uninstall();

    ELSIF pp.testNext("-DirectLine") THEN
      DrawPerfTests.DirectLine();
    ELSIF pp.testNext("-IndirectLine") THEN
      DrawPerfTests.IndirectLine();
    ELSIF pp.testNext("-APILine") THEN
      DrawPerfTests.APILine();

    ELSIF pp.testNext("-DirectPoint") THEN
      DrawPerfTests.DirectPoint();
    ELSIF pp.testNext("-IndirectPoint") THEN
      DrawPerfTests.IndirectPoint();
    ELSIF pp.testNext("-APIPoint") THEN
      DrawPerfTests.APIPoint();

    ELSIF pp.testNext("-ClearTest") THEN
      DrawPerfTests.ClearTest();

    ELSIF pp.testNext("-LineTest") THEN
      DrawPerfTests.LineTest();
    ELSIF pp.testNext("-LineDemo") THEN
      DrawPerfTests.LineDemo();
    ELSIF pp.testNext("-Line10") THEN
      DrawPerfTests.Line10();
    ELSIF pp.testNext("-Line100") THEN
      DrawPerfTests.Line100();
    ELSIF pp.testNext("-GouraudLineTest") THEN
      DrawPerfTests.GouraudLineTest();
    ELSIF pp.testNext("-GouraudLineDemo") THEN
      DrawPerfTests.GouraudLineDemo();
    ELSIF pp.testNext("-GouraudLine10") THEN
      DrawPerfTests.GouraudLine10();
    ELSIF pp.testNext("-GouraudLine100") THEN
      DrawPerfTests.GouraudLine100();

    ELSIF pp.testNext("-Bitblt24to8Test") THEN
      DrawPerfTests.Bitblt24to8Test();
    ELSIF pp.testNext("-BitbltTest") THEN
      DrawPerfTests.BitbltTest();
    ELSIF pp.testNext("-ClippedBitbltTest") THEN
      DrawPerfTests.ClippedBitbltTest();
    ELSIF pp.testNext("-SourceKeyTest") THEN
      DrawPerfTests.SourceKeyTest();
    ELSIF pp.testNext("-DestKeyTest") THEN
      DrawPerfTests.DestKeyTest();
    ELSIF pp.testNext("-StretchTest") THEN
      DrawPerfTests.StretchTest();
    ELSIF pp.testNext("-FilteredTest") THEN
      DrawPerfTests.FilteredTest();

    ELSIF pp.testNext("-TriangleTest") THEN
      DrawPerfTests.TriangleTest();

    ELSE
      Usage();
    END;
  EXCEPT
  |  ParseParams.Error => Usage();
  END;
    
  RETURN TRUE;
END Run;

PROCEDURE Usage() = 
BEGIN

  (* Lists all the possible command-line options. *)
  IO.Put("Help:\n");

  IO.Put("     drawperf -DirectLine\n");
  IO.Put("     drawperf -IndirectLine\n");
  IO.Put("     drawperf -APILine\n");

  IO.Put("     drawperf -DirectPoint\n");     
  IO.Put("     drawperf -IndirectPoint\n");     
  IO.Put("     drawperf -APIPoint\n");

  IO.Put("     drawperf -ClearTest\n");

  IO.Put("     drawperf -LineTest\n");
  IO.Put("     drawperf -LineDemo\n");     
  IO.Put("     drawperf -Line10\n");     
  IO.Put("     drawperf -Line100\n");     
  IO.Put("     drawperf -GouraudLineTest\n");
  IO.Put("     drawperf -GouraudLineDemo\n");     
  IO.Put("     drawperf -GouraudLine10\n");     
  IO.Put("     drawperf -GouraudLine100\n");     

  IO.Put("     drawperf -Bitblt24to8Test\n");
  IO.Put("     drawperf -BitbltTest\n");
  IO.Put("     drawperf -ClippedBitbltTest\n");
  IO.Put("     drawperf -SourceKeyTest\n");
  IO.Put("     drawperf -DestKeyTest\n");
  IO.Put("     drawperf -StretchTest\n");
  IO.Put("     drawperf -FilteredTest\n");

  IO.Put("     drawperf -TriangleTest\n");
END Usage;


BEGIN
END DrawPerfShell.




