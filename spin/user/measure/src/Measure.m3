(*
 * Copyright 1994, 1995 University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 *
 * HISTORY
 * 31-May-97  David Becker at the University of Washington
 *      Replace SAL with Kernel, Clock and CPU interfaces
 *
 * 01-Oct-96  Przemek Pardyak (pardy) at the University of Washington
 *	Print the clock frequency.
 *
 * 14-Aug-96  Frederick Gray (fgray) at the University of Washington
 *	New interface to interrupt handling.
 *
 * 21-Apr-96  Emin Gun Sirer (egs) at the University of Washington
 *      Converted to the new Auth interface.
 *	Deleted MeasurePrivate now that initialization is done in module main.
 *
 * 18-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added export of the domain.
 *
 * 07-Apr-96  Stefan Savage (savage) at the University of Washington
 *	Added code to initialize Measure domain.
 *
 * 22-Mar-96  Stefan Savage (savage) at the University of Washington
 *	By-hand long division to provide more digits of accuracy for
 *	small cycle counts. 
 *
 * 06-Mar-96  Emin Gun Sirer (egs) at the University of Washington
 *	Fixed yet again to not divide by zero when there are too few samples.
 *      Made the median work.
 *
 * 09-Feb-96  Emin Gun Sirer (egs) at the University of Washington
 *	Added UNUSED to avoid warnings.
 *
 * 10-Jan-96  Emin Gun Sirer (egs) at the University of Washington
 *	Use IO and reader/writers.
 *
 * 08-Jan-96  David Becker (becker) at the University of Washington
 *      Changed mhz to use SAL.GetMachineClockRate() instaed of 133.
 *
 * 16-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *      Fixed the median to not consider unsampled points in the
 *      histogram.
 *
 * 15-Nov-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added synchronization for multithread use.
 *	Added the Hit function which allows samples to be manually added.
 *	Fixed the overhead scheme to not penalize the measurements made
 *      with the Hit interface.
 *
 * 21-Nov-95  Charlie Garrett (garrett) at the University of Washington
 *      Faster square root calculation. These statistical calculations
 *      should not be believed because they throw away so much information.
 *
 * 21-Nov-95  Stefan Savage (savage) at the University of Washington
 *	Replace sum of squares with sum of squares of differences from means
 *	to reduct roundoff (thanks to Wilson for help on this)
 *
 * 28-Oct-95  Brian Bershad (bershad) at the University of Washington
 *	SALSafe --> SAL
 *
 * 22-Aug-95  Emin Gun Sirer (egs) at the University of Washington
 *	Added code to detect the off by one case.
 *	NEEDS LOCKING FOR MULTITHREAD USE.
 *
 * 01-Aug-95  Stefan Savage (savage) at the University of Washington
 *	Created to match timing functions used for OSF/1 and Mach in
 *	SOSP paper.
 *
 *)
UNSAFE (* imports CPUprivate to disable interrupts *)
MODULE Measure;

IMPORT Fmt, Word, IntArraySort, CPU, CPUPrivate;
IMPORT MeasureInterface, Auth, NameServer;
IMPORT IO, DebugOption;

VAR
  overhead: Word.T := 0;
  mhz    : INTEGER;

CONST
  overheadRuns: INTEGER = 10000;

REVEAL
  T = Public BRANDED OBJECT
    name: TEXT;
    lock: MUTEX;
    begin: Word.T;
    end: Word.T;
    index: Word.T;
    hist: Histogram;
  OVERRIDES
    init := Initialize;
    hit := Hit;
    start := Start;
    stop := Stop;
  END;

PROCEDURE Initialize(self: T; name: TEXT; size: INTEGER): T =
  BEGIN
    self.name := name;
    self.begin := 0;
    self.end := 0;
    self.index := 0;
    self.hist := NEW(Histogram, size);
    self.lock := NEW(MUTEX);

    RETURN self;
  END Initialize;

PROCEDURE Start(self: T) =
  BEGIN
    LOCK self.lock DO
      self.begin := CPU.CycleCounter();
    END;
  END Start;

PROCEDURE Stop(self: T) =
  BEGIN
    LOCK self.lock DO
      self.end := CPU.CycleCounter();
      (* Alpha specific hacks... timer only 32 bits *)

      (* To factor out any slop in the top 32 bits *)
      self.begin := Word.And(self.begin,16_ffffffff);
      self.end := Word.And(self.end,16_ffffffff);

      IF Word.GT(self.index, LAST(self.hist^)) THEN
        IO.Put("Error: too many iterations\n");
        IO.Put("index = "&Fmt.Int(self.index)&", histogramSize="&
          Fmt.Int(BYTESIZE(self.hist))&"\n");
      END;

      IF Word.GT(self.end,self.begin) THEN
        self.hist[self.index] := Word.Minus(self.end, self.begin);
      ELSE
        self.hist[self.index] := Word.Plus(Word.Minus(16_ffffffff,self.begin),
                                           self.end);
      END;
      DEC(self.hist[self.index], overhead);
      INC(self.index);
    END;
  END Stop;

PROCEDURE Hit(self: T; time: INTEGER) =
  BEGIN
    LOCK self.lock DO
      IF Word.GT(self.index, LAST(self.hist^)) THEN
        IO.Put("Error: too many iterations\n");
        IO.Put("index = "&Fmt.Int(self.index)&", histogramSize="&
          Fmt.Int(BYTESIZE(self.hist))&"\n");
      END;
      self.hist[self.index] := time;
      INC(self.index);
    END;
  END Hit;

PROCEDURE PrintStats(timer: T) =
  VAR
    histogram : Histogram;
    iterations: INTEGER;
    average: INTEGER;
    median : INTEGER;
    min    : INTEGER;
    max    : INTEGER;
    total  : INTEGER;
    variance : INTEGER;
    stddev : INTEGER;
    sumofdiffs : INTEGER;
    outliers : INTEGER;
  BEGIN
    IF DebugOption.DoStrandDebug OR
      DebugOption.Cswtch OR DebugOption.DoTimings THEN
      IO.Put("Only a Caveman would believe these numbers. Debugging is enabled.\n");
    END;
    histogram := timer.hist;
    IO.Put("For the experiment: " & timer.name & "\n");
    IO.Put("Measured at : " & Fmt.Int(mhz) & "MHz\n");
    iterations := timer.index;
    IO.Put("Iterations = " & Fmt.Int(iterations));
    IF iterations > NUMBER(histogram^) THEN
      IO.Put("Error: Too many iterations\n");
      RETURN;
    END;

(*
 *  outliers := RemoveOutliers(timer);
 *  iterations := iterations - outliers;
 *)
    outliers := 0;
    IO.Put(", " & Fmt.Int(outliers) & " outliers were removed\n");

    total := 0;
    sumofdiffs := 0;
    min := histogram[0];
    max := histogram[0];

    FOR i := 0 TO iterations-1 DO
      total := total + histogram[i];
      IF histogram[i] > max THEN
        max := histogram[i];
      END;
      IF histogram[i] < min THEN
        min := histogram[i];
      END;
    END;
    average := total DIV iterations;

    FOR i := 0 TO iterations-1 DO
      sumofdiffs:= sumofdiffs+((histogram[i]-average)*(histogram[i]-average));
    END;
    IF iterations > 1 THEN
      variance := sumofdiffs DIV (iterations -1);
    ELSE
      variance := 0;
    END;

    TRY
      stddev := CeilingSqrt(variance);
    EXCEPT
    | Imaginary => IO.Put("Imaginary number occurred\n");
    END;

    IF iterations # LAST(histogram^)+1 THEN
      IO.Put("histogram array contains " & Fmt.Int(LAST(histogram^)+1) &
                    "points\n");
    END;

    WITH hist = SUBARRAY(histogram^, 0, iterations) DO
      IntArraySort.Sort(hist);
    END;
    median := histogram[iterations DIV 2];

    IO.Put("Average = " & Fmt.Int(average) & "cycles, " &
      LongDivideToText(average, mhz) & "usecs\n");
    IO.Put("Median = " & Fmt.Int(median) & "cycles, " &
      LongDivideToText(median, mhz) & "usecs\n");

    IO.Put("Min = " & Fmt.Int(min) & "cycles, " &
      LongDivideToText(min, mhz) & "usecs\n");

    IO.Put("Max = " & Fmt.Int(max) & "cycles, " &
      LongDivideToText(max,mhz) & "usecs\n");

    IO.Put("Variance = " & Fmt.Int(variance) & "cycles, " &
      LongDivideToText(variance, mhz) & "usecs\n");

    IO.Put("Std Deviation = " & Fmt.Int(stddev) & "cycles, " &
      LongDivideToText(stddev, mhz) & "usecs\n");


  END PrintStats; 

(* A faster square root calculation. You have your choice of
   Floor, Ceiling or Rounded square root. The Rounded one is
   the integer closest to the true square root. Raises an exception
   when passed a negative number. *)

EXCEPTION Imaginary;

PROCEDURE FloorSqrt(x: INTEGER): INTEGER RAISES {Imaginary} =
  BEGIN
    IF x < 0 THEN
      RAISE Imaginary;
    END;

    (* x is between 0 and 2^(BITSIZE(INTEGER) - 1) - 1, i.e. 2^63 - 1 *)
    (* so its square root is between 0 and 2^(BITSIZE(INTEGER)/2) - 1, 
       i.e. 2^32 - 1 *)

    VAR
      root := 0;
    BEGIN
      FOR bit := 31 TO 0 BY -1 DO
        WITH larger = Word.Or(root, Word.Shift(1, bit)) DO
          WITH LargeSq = larger * larger DO
            IF LargeSq > 0 AND
               LargeSq < x THEN
              root := larger;
            END;
          END;
        END;
      END;
      RETURN root;
    END;
  END FloorSqrt;

PROCEDURE CeilingSqrt(x: INTEGER): INTEGER RAISES {Imaginary} =
  BEGIN
    RETURN FloorSqrt(x) + 1;
  END CeilingSqrt;

(* dumb hack because we don't support floats in the kernel *)
PROCEDURE LongDivideToText(a: INTEGER; b: INTEGER): TEXT =
  VAR
    newText: TEXT;
    numDigits: INTEGER := 2;
    remainder: INTEGER;
  BEGIN
    newText := Fmt.Int(a DIV b)&".";
    remainder := (a MOD b) * 10;
    FOR i:= 1 TO numDigits DO
      newText := newText &Fmt.Int(remainder DIV b);
      remainder := (remainder MOD b) * 10;
    END; 
    RETURN newText;
  END LongDivideToText;

VAR
  timer: T;
  total: Word.T;
  il: CPU.InterruptLevel;

BEGIN
  mhz := CPU.Hertz() DIV 1000000;
  timer := NEW(T).init("Overhead",overheadRuns);
  il := CPUPrivate.SetInterruptMask(CPUPrivate.InterruptClass.High);
  FOR i:= 0 TO overheadRuns - 1 DO
    Start(timer);
    Stop(timer);
  END;
  CPUPrivate.RestoreInterruptMask(il);
  total := 0;
  FOR i := 0 TO timer.index-1 DO
    total := Word.Plus(total, timer.hist[i]);
  END;
  IO.Put("total = "&Fmt.Int(total)&", index = "&Fmt.Int(timer.index)&"\n");
  overhead := Word.Divide(total, timer.index);
  TRY
    EVAL MeasureInterface.Export(NEW(Auth.AuthAlways));
  EXCEPT
    NameServer.Error => IO.Put("Failed to export measure.\n");
  END;
  IO.Put("Measure initialized. Overhead = " &
    Fmt.Int(overhead) & "cycles, " &
    LongDivideToText(overhead, mhz) & "usecs\n");
END Measure.
