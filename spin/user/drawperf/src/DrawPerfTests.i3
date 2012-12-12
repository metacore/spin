(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE DrawPerfTests;

PROCEDURE DirectLine();
PROCEDURE IndirectLine();
PROCEDURE APILine();

PROCEDURE DirectPoint();
PROCEDURE IndirectPoint();
PROCEDURE APIPoint();

PROCEDURE ClearTest();

PROCEDURE LineTest();
PROCEDURE LineDemo();
PROCEDURE Line10(); 
PROCEDURE Line100(); 
PROCEDURE GouraudLineTest();
PROCEDURE GouraudLineDemo();
PROCEDURE GouraudLine10();
PROCEDURE GouraudLine100();

PROCEDURE Bitblt24to8Test();
PROCEDURE BitbltTest();
PROCEDURE ClippedBitbltTest();
PROCEDURE SourceKeyTest();
PROCEDURE DestKeyTest();
PROCEDURE StretchTest();
PROCEDURE FilteredTest();

PROCEDURE TriangleTest();

END DrawPerfTests.

