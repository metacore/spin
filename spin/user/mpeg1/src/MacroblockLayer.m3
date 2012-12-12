(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE MacroblockLayer;

IMPORT BitIO, GeneralTools, MpegData, Word, PostProcessing, BlockLayer, IO;

PROCEDURE ReadMacroblockAddressIncrement (file: BitIO.T): INTEGER RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  code: INTEGER;
  increment: INTEGER := 0;
BEGIN
  TRY
    (* Begin macroblock decoding.  First, get rid of the macroblock stuffing. *)
    WHILE BitIO.ShowBits(file, 11) = 2_1111 DO
      BitIO.SkipBits(file, 11);    
    END;

    (* Next, read in the macroblock escapes.  For each macroblock escape, add 33 to the increment variable *)
    WHILE BitIO.ShowBits(file, 11) = 2_1000 DO
      BitIO.SkipBits(file, 11);
      increment := increment + 33;
    END;

    (* Lastly, read in the macroblock address increment *)
    code := BitIO.ShowBits(file, 11);

    CASE code OF
    |  1024..2047 => BitIO.SkipBits(file, 1);
                     RETURN increment + 1;
    |  768..1023  => BitIO.SkipBits(file, 3);
                     RETURN increment + 2;
    |  512..767   => BitIO.SkipBits(file, 3);
                     RETURN increment + 3;
    |  384..511   => BitIO.SkipBits(file, 4);
                     RETURN increment + 4;
    |  256..383   => BitIO.SkipBits(file, 4);
                     RETURN increment + 5;
    |  192..255   => BitIO.SkipBits(file, 5);
                     RETURN increment + 6;
    |  128..191   => BitIO.SkipBits(file, 5);
                     RETURN increment + 7;
    |  112..127   => BitIO.SkipBits(file, 7);
                     RETURN increment + 8;
    |  96..111    => BitIO.SkipBits(file, 7);
                     RETURN increment + 9;
    |  88..95     => BitIO.SkipBits(file, 8);
                     RETURN increment + 10;
    |  80..87     => BitIO.SkipBits(file, 8);
                     RETURN increment + 11;
    |  72..79     => BitIO.SkipBits(file, 8);
                     RETURN increment + 12;
    |  64..71     => BitIO.SkipBits(file, 8);
                     RETURN increment + 13;
    |  56..63     => BitIO.SkipBits(file, 8);
                     RETURN increment + 14;
    |  48..55     => BitIO.SkipBits(file, 8);
                     RETURN increment + 15;
    |  46..47     => BitIO.SkipBits(file, 10);
                     RETURN increment + 16;
    |  44..45     => BitIO.SkipBits(file, 10);
                     RETURN increment + 17;
    |  42..43     => BitIO.SkipBits(file, 10);
                     RETURN increment + 18;
    |  40..41     => BitIO.SkipBits(file, 10);
                     RETURN increment + 19;
    |  38..39     => BitIO.SkipBits(file, 10);
                     RETURN increment + 20;
    |  36..37     => BitIO.SkipBits(file, 10);
                     RETURN increment + 21;
    |  35         => BitIO.SkipBits(file, 11);
                     RETURN increment + 22;
    |  34         => BitIO.SkipBits(file, 11);
                     RETURN increment + 23;
    |  33         => BitIO.SkipBits(file, 11);
                     RETURN increment + 24;
    |  32         => BitIO.SkipBits(file, 11);
                     RETURN increment + 25;
    |  31         => BitIO.SkipBits(file, 11);
                     RETURN increment + 26;
    |  30         => BitIO.SkipBits(file, 11);
                     RETURN increment + 27;
    |  29         => BitIO.SkipBits(file, 11);
                     RETURN increment + 28;
    |  28         => BitIO.SkipBits(file, 11);
                     RETURN increment + 29;
    |  27         => BitIO.SkipBits(file, 11);
                     RETURN increment + 30;
    |  26         => BitIO.SkipBits(file, 11);
                     RETURN increment + 31;
    |  25         => BitIO.SkipBits(file, 11);
                     RETURN increment + 32;
    |  24         => BitIO.SkipBits(file, 11);
                     RETURN increment + 33;
    ELSE             RAISE GeneralTools.Bad("Bad macroblock address increment");
    END;

  EXCEPT
  |  GeneralTools.Bad(problem) => RAISE GeneralTools.Bad(problem & " IN MacroblockLayer.ReadMacroblockAddressIncrement");
  END;  
END ReadMacroblockAddressIncrement; 

PROCEDURE ReadMacroblockType (file: BitIO.T; VAR state: MpegData.MpegState)
                             RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  code: INTEGER;
BEGIN
  TRY
    (* I-picture *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.I THEN
      code := BitIO.ShowBits(file, 2);
      CASE code OF
      |  2..3 => BitIO.SkipBits(file, 1);
                 state.macroblockType.macroblockQuant := FALSE;
                 state.macroblockType.macroblockMotionForward := FALSE;
                 state.macroblockType.macroblockMotionBackward := FALSE;
                 state.macroblockType.macroblockPattern := FALSE;
                 state.macroblockType.macroblockIntra := TRUE;
      |  1    => BitIO.SkipBits(file, 2);
                 state.macroblockType.macroblockQuant := TRUE;
                 state.macroblockType.macroblockMotionForward := FALSE;
                 state.macroblockType.macroblockMotionBackward := FALSE;
                 state.macroblockType.macroblockPattern := FALSE;
                 state.macroblockType.macroblockIntra := TRUE;
      ELSE       RAISE GeneralTools.Bad("Bad macroblock type");
      END;
    END;

    (* P-picture *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.P THEN
      code := BitIO.ShowBits(file, 6);
      CASE code OF
      |  32..63 => BitIO.SkipBits(file, 1);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  16..31 => BitIO.SkipBits(file, 2);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  8..15  => BitIO.SkipBits(file, 3);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  6..7   => BitIO.SkipBits(file, 5);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := TRUE;
      |  4..5   => BitIO.SkipBits(file, 5);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  2..3   => BitIO.SkipBits(file, 5);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  1      => BitIO.SkipBits(file, 6);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := TRUE;
      ELSE         RAISE GeneralTools.Bad("Bad macroblock type");
      END;
    END;

    (* B-picture *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
      code := BitIO.ShowBits(file, 6);
      CASE code OF
      |  32..47 => BitIO.SkipBits(file, 2);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  48..63 => BitIO.SkipBits(file, 2);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE; 
      |  16..23 => BitIO.SkipBits(file, 3);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  24..31 => BitIO.SkipBits(file, 3);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  8..11  => BitIO.SkipBits(file, 4);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  12..15 => BitIO.SkipBits(file, 4);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  6..7   => BitIO.SkipBits(file, 5);
                   state.macroblockType.macroblockQuant := FALSE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := TRUE;
      |  4..5   => BitIO.SkipBits(file, 5);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  3      => BitIO.SkipBits(file, 6);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := TRUE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  2      => BitIO.SkipBits(file, 6);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := TRUE;
                   state.macroblockType.macroblockPattern := TRUE;
                   state.macroblockType.macroblockIntra := FALSE;
      |  1      => BitIO.SkipBits(file, 6);
                   state.macroblockType.macroblockQuant := TRUE;
                   state.macroblockType.macroblockMotionForward := FALSE;
                   state.macroblockType.macroblockMotionBackward := FALSE;
                   state.macroblockType.macroblockPattern := FALSE;
                   state.macroblockType.macroblockIntra := TRUE;
      ELSE         RAISE GeneralTools.Bad("Bad macroblock type");
      END;
    END;

    (* D-picture *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.D THEN
      IF BitIO.ShowBits(file, 1) = 1 THEN
        BitIO.SkipBits(file, 1);
        state.macroblockType.macroblockQuant := FALSE;
        state.macroblockType.macroblockMotionForward := FALSE;
        state.macroblockType.macroblockMotionBackward := FALSE;
        state.macroblockType.macroblockPattern := FALSE;
        state.macroblockType.macroblockIntra := TRUE;
      ELSE
        RAISE GeneralTools.Bad("Bad macroblock type");
      END;
    END;      

  EXCEPT
  |  GeneralTools.Bad(problem) => RAISE GeneralTools.Bad(problem & " IN MacroblockLayer.ReadMacroblockType");
  END;
END ReadMacroblockType;

PROCEDURE ReadMotionVector (file: BitIO.T; VAR motionCode: INTEGER)
                           RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  code: INTEGER;
BEGIN
  code := BitIO.ShowBits(file, 10);
  CASE code OF
  |  512..1023 => motionCode := 0;
                  (* Don't skip the bit because we do this at end of routine *)
  |  256..511  => motionCode := 1;
                  BitIO.SkipBits(file, 2);
  |  128..255  => motionCode := 2;
                  BitIO.SkipBits(file, 3);
  |  64..127   => motionCode := 3;
                  BitIO.SkipBits(file, 4);
  |  48..63    => motionCode := 4; 
                  BitIO.SkipBits(file, 6);
  |  40..47    => motionCode := 5;
                  BitIO.SkipBits(file, 7);
  |  32..39    => motionCode := 6;
                  BitIO.SkipBits(file, 7);
  |  24..31    => motionCode := 7;
                  BitIO.SkipBits(file, 7);
  |  22..23    => motionCode := 8;
                  BitIO.SkipBits(file, 9);
  |  20..21    => motionCode := 9;
                  BitIO.SkipBits(file, 9);
  |  18..19    => motionCode := 10;
                  BitIO.SkipBits(file, 9);
  |  17        => motionCode := 11;
                  BitIO.SkipBits(file, 10);
  |  16        => motionCode := 12;
                  BitIO.SkipBits(file, 10);
  |  15        => motionCode := 13;
                  BitIO.SkipBits(file, 10);
  |  14        => motionCode := 14;
                  BitIO.SkipBits(file, 10);
  |  13        => motionCode := 15;
                  BitIO.SkipBits(file, 10);
  |  12        => motionCode := 16;
                  BitIO.SkipBits(file, 10);
  ELSE RAISE GeneralTools.Bad("Bad motion code IN MacroblockLayer.ReadMotionVector");
  END;

  IF BitIO.GetBits(file, 1) = 1 THEN
    motionCode := -motionCode;
  END;
END ReadMotionVector; 

PROCEDURE ReadCodedBlockPattern (file: BitIO.T): INTEGER 
                                RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  codedBlockPattern: INTEGER;
  cbp: INTEGER;
BEGIN
  codedBlockPattern := BitIO.ShowBits(file, 9);

  CASE codedBlockPattern OF
  |  448..511 => cbp := 60;
                 BitIO.SkipBits(file, 3);
  |  416..447 => cbp := 4;
                 BitIO.SkipBits(file, 4);
  |  384..415 => cbp := 8;
                 BitIO.SkipBits(file, 4);
  |  352..383 => cbp := 16;
                 BitIO.SkipBits(file, 4);
  |  320..351 => cbp := 32;
                 BitIO.SkipBits(file, 4);
  |  304..319 => cbp := 12;
                 BitIO.SkipBits(file, 5);
  |  288..303 => cbp := 48;
                 BitIO.SkipBits(file, 5);
  |  272..287 => cbp := 20;
                 BitIO.SkipBits(file, 5);
  |  256..271 => cbp := 40;
                 BitIO.SkipBits(file, 5);
  |  240..255 => cbp := 28;
                 BitIO.SkipBits(file, 5);
  |  224..239 => cbp := 44;
                 BitIO.SkipBits(file, 5);
  |  208..223 => cbp := 52;
                 BitIO.SkipBits(file, 5);
  |  192..207 => cbp := 56;
                 BitIO.SkipBits(file, 5);
  |  176..191 => cbp := 1;
                 BitIO.SkipBits(file, 5);
  |  160..175 => cbp := 61;
                 BitIO.SkipBits(file, 5);
  |  144..159 => cbp := 2;
                 BitIO.SkipBits(file, 5);
  |  128..143 => cbp := 62;
                 BitIO.SkipBits(file, 5);
  |  120..127 => cbp := 24;
                 BitIO.SkipBits(file, 6);
  |  112..119 => cbp := 36;
                 BitIO.SkipBits(file, 6);
  |  104..111 => cbp := 3;
                 BitIO.SkipBits(file, 6);
  |  96..103  => cbp := 63;
                 BitIO.SkipBits(file, 6);
  |  92..95   => cbp := 5;
                 BitIO.SkipBits(file, 7);
  |  88..91   => cbp := 9;
                 BitIO.SkipBits(file, 7);
  |  84..87   => cbp := 17;
                 BitIO.SkipBits(file, 7);
  |  80..83   => cbp := 33;
                 BitIO.SkipBits(file, 7);
  |  76..79   => cbp := 6;
                 BitIO.SkipBits(file, 7);
  |  72..75   => cbp := 10;
                 BitIO.SkipBits(file, 7);
  |  68..71   => cbp := 18;
                 BitIO.SkipBits(file, 7);
  |  64..67   => cbp := 34;
                 BitIO.SkipBits(file, 7);
  |  62..63   => cbp := 7;
                 BitIO.SkipBits(file, 8);
  |  60..61   => cbp := 11;
                 BitIO.SkipBits(file, 8);
  |  58..59   => cbp := 19;
                 BitIO.SkipBits(file, 8);
  |  56..57   => cbp := 35;
                 BitIO.SkipBits(file, 8);
  |  54..55   => cbp := 13;
                 BitIO.SkipBits(file, 8);
  |  52..53   => cbp := 49;
                 BitIO.SkipBits(file, 8);
  |  50..51   => cbp := 21;
                 BitIO.SkipBits(file, 8);
  |  48..49   => cbp := 41;
                 BitIO.SkipBits(file, 8);
  |  46..47   => cbp := 14;
                 BitIO.SkipBits(file, 8);
  |  44..45   => cbp := 50;
                 BitIO.SkipBits(file, 8);
  |  42..43   => cbp := 22;
                 BitIO.SkipBits(file, 8);
  |  40..41   => cbp := 42;
                 BitIO.SkipBits(file, 8);
  |  38..39   => cbp := 15;
                 BitIO.SkipBits(file, 8);
  |  36..37   => cbp := 51;
                 BitIO.SkipBits(file, 8);
  |  34..35   => cbp := 23;
                 BitIO.SkipBits(file, 8);
  |  32..33   => cbp := 43;
                 BitIO.SkipBits(file, 8);
  |  30..31   => cbp := 25;
                 BitIO.SkipBits(file, 8);
  |  28..29   => cbp := 37;
                 BitIO.SkipBits(file, 8);
  |  26..27   => cbp := 26;
                 BitIO.SkipBits(file, 8);
  |  24..25   => cbp := 38;
                 BitIO.SkipBits(file, 8);
  |  22..23   => cbp := 29;
                 BitIO.SkipBits(file, 8);
  |  20..21   => cbp := 45;
                 BitIO.SkipBits(file, 8);
  |  18..19   => cbp := 53;
                 BitIO.SkipBits(file, 8);
  |  16..17   => cbp := 57;
                 BitIO.SkipBits(file, 8);
  |  14..15   => cbp := 30;
                 BitIO.SkipBits(file, 8);
  |  12..13   => cbp := 46;
                 BitIO.SkipBits(file, 8);
  |  10..11   => cbp := 54;
                 BitIO.SkipBits(file, 8);
  |  8..9     => cbp := 58;
                 BitIO.SkipBits(file, 8);
  |  7        => cbp := 31;
                 BitIO.SkipBits(file, 9);
  |  6        => cbp := 47;
                 BitIO.SkipBits(file, 9);
  |  5        => cbp := 55;
                 BitIO.SkipBits(file, 9);
  |  4        => cbp := 59;
                 BitIO.SkipBits(file, 9);
  |  3        => cbp := 27;
                 BitIO.SkipBits(file, 9);
  |  2        => cbp := 39;
                 BitIO.SkipBits(file, 9);
  ELSE RAISE GeneralTools.Bad("Bad coded block pattern IN MacroblockLayer.ReadCodedBlockPattern");
  END;

  RETURN cbp;
END ReadCodedBlockPattern;

PROCEDURE ReconstructMotionVectors (file: BitIO.T; VAR state: MpegData.MpegState) 
                                   RAISES {GeneralTools.Bad} =
VAR
  forwardRSize: INTEGER;
  forwardF: INTEGER;
  backwardRSize: INTEGER; 
  backwardF: INTEGER;
  complementHorizontalForwardR: INTEGER;
  complementVerticalForwardR: INTEGER;
  complementHorizontalBackwardR: INTEGER;
  complementVerticalBackwardR: INTEGER;
  rightLittle: INTEGER;
  downLittle: INTEGER;
  rightBig: INTEGER; 
  downBig: INTEGER;
  max: INTEGER;
  min: INTEGER;
  newVector: INTEGER;
BEGIN
  IF state.macroblockType.macroblockMotionForward THEN
    forwardRSize := state.p.forwardFcode - 1;
    (* should really be a left shift by one place. *)
    forwardF := Word.LeftShift(1, forwardRSize);
    
    (* Set complementHorizontalForwardR. *)
    IF (forwardF = 1) OR (state.motionHorizontalForwardCode = 0) THEN
      complementHorizontalForwardR := 0;
    ELSE
      complementHorizontalForwardR := forwardF - 1 - state.motionHorizontalForwardR;
    END;
    (* Set complementVerticalForwardR. *)
    IF (forwardF = 1) OR (state.motionVerticalForwardCode = 0) THEN
      complementVerticalForwardR := 0;
    ELSE
      complementVerticalForwardR := forwardF - 1 - state.motionVerticalForwardR;
    END;

    rightLittle := state.motionHorizontalForwardCode * forwardF;
    IF rightLittle = 0 THEN
      rightBig := 0;
    ELSE
      IF rightLittle > 0 THEN
        rightLittle := rightLittle - complementHorizontalForwardR;
        rightBig    := rightLittle - (32 * forwardF);
      ELSE
        rightLittle := rightLittle + complementHorizontalForwardR;
        rightBig    := rightLittle + (32 * forwardF);
      END;
    END;
          
    downLittle := state.motionVerticalForwardCode * forwardF;
    IF downLittle = 0 THEN
      downBig := 0;
    ELSE
      IF downLittle > 0 THEN
        downLittle := downLittle - complementVerticalForwardR;
        downBig    := downLittle - (32 * forwardF);
      ELSE 
        downLittle := downLittle + complementVerticalForwardR;
        downBig    := downLittle + (32 * forwardF);
      END;
    END;
 
    (* Check for bad values. *)
    IF rightLittle = forwardF * 16 THEN
      IO.Put("Bad value for rightLittle\n");
    END;
    IF downLittle = forwardF * 16 THEN
      IO.Put("Bad value for downLittle\n");
    END;
      
    max := (16 * forwardF) - 1;
    min := (-16) * forwardF; 

    newVector := state.reconRightForPrev + rightLittle;
    IF ((newVector <= max) AND (newVector >= min)) THEN
      state.reconRightFor := state.reconRightForPrev + rightLittle;
    ELSE
      state.reconRightFor := state.reconRightForPrev + rightBig;
    END;
    state.reconRightForPrev := state.reconRightFor;

    IF state.p.fullPelForwardVector THEN
      (* should really be a left shift by one. *)
      state.reconRightFor := state.reconRightFor * 2;
    END;
    newVector := state.reconDownForPrev + downLittle;
    IF (newVector <= max) AND (newVector >= min) THEN
      state.reconDownFor := state.reconDownForPrev + downLittle;
    ELSE
      state.reconDownFor := state.reconDownForPrev + downBig;
    END;
    state.reconDownForPrev := state.reconDownFor;
    IF state.p.fullPelForwardVector THEN
      (* should really be a left shift by one place. *)
      state.reconDownFor := state.reconDownFor * 2;
    END;
  END;  

  (* Now, do the extra stuff necessary for a B-picture, which means compute the backward
     motion vector. *)
  IF NOT state.macroblockType.macroblockMotionBackward THEN
    IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN 
      state.reconRightBack := state.reconRightBackPrev;
      state.reconDownBack  := state.reconDownBackPrev;
    END;
  ELSE
    backwardRSize := state.p.backwardFcode - 1;
    backwardF := Word.LeftShift(1, backwardRSize);
      
    (* Set complementHorizontalBackwardR. *)
    IF (backwardF = 1) OR (state.motionHorizontalBackwardCode = 0) THEN
      complementHorizontalBackwardR := 0;
    ELSE
      complementHorizontalBackwardR := backwardF - 1 - state.motionHorizontalBackwardR;
    END;
    (* Set complementVerticalBackwardR. *)
    IF (backwardF = 0) OR (state.motionVerticalBackwardCode = 0) THEN
      complementVerticalBackwardR := 0;
    ELSE
      complementVerticalBackwardR := backwardF - 1 - state.motionVerticalBackwardR;
    END;

    rightLittle := state.motionHorizontalBackwardCode * backwardF;
    IF rightLittle = 0 THEN
      rightBig := 0;
    ELSE
      IF rightLittle > 0 THEN
        rightLittle := rightLittle - complementHorizontalBackwardR;
        rightBig    := rightLittle - (32 * backwardF);
      ELSE
        rightLittle := rightLittle + complementHorizontalBackwardR;
        rightBig    := rightLittle + (32 * backwardF);
      END;
    END;
          
    downLittle := state.motionVerticalBackwardCode * backwardF;
    IF downLittle = 0 THEN
      downBig := 0;
    ELSE
      IF downLittle > 0 THEN
        downLittle := downLittle - complementVerticalBackwardR;
        downBig    := downLittle - (32 * backwardF);
      ELSE 
        downLittle := downLittle + complementVerticalBackwardR;
        downBig    := downLittle + (32 * backwardF);
      END;
    END;
 
    (* Check for bad values. *)
    IF rightLittle = backwardF * 16 THEN
      IO.Put("Bad value for rightLittle\n");
    END;
    IF downLittle = backwardF * 16 THEN
      IO.Put("Bad value for downLittle\n");
    END;
      
    max := (16 * backwardF) - 1;
    min := (-16) * backwardF; 
      
    newVector := state.reconRightBackPrev + rightLittle;
    IF ((newVector <= max) AND (newVector >= min)) THEN
      state.reconRightBack := state.reconRightBackPrev + rightLittle;
    ELSE
      state.reconRightBack := state.reconRightBackPrev + rightBig;
    END;
      state.reconRightBackPrev := state.reconRightBack;

    IF state.p.fullPelBackwardVector THEN
      (* should be a left shift. *)
      state.reconRightBack := state.reconRightBack * 2;
    END;
    newVector := state.reconDownBackPrev + downLittle;
    IF (newVector <= max) AND (newVector >= min) THEN
      state.reconDownBack := state.reconDownBackPrev + downLittle;
    ELSE
      state.reconDownBack := state.reconDownBackPrev + downBig;
    END;
    state.reconDownBackPrev := state.reconDownBack;
    IF state.p.fullPelBackwardVector THEN
      (* left shift *)
      state.reconDownBack := state.reconDownBack * 2;
    END;
  END;

END ReconstructMotionVectors;

(* This procedure may deal with more than one macroblock per call. It reads in one macroblock, but if there
   are skipped macroblocks, it will deal with these, and then actually read in the first non-skipped macroblock. *)
PROCEDURE ReadMacroblock (file: BitIO.T; VAR state: MpegData.MpegState) 
                         RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  forwardRSize:               INTEGER;
  backwardRSize:              INTEGER;
  forwardF:                   INTEGER;
  backwardF:                  INTEGER;
  skippedMacroblocks:         INTEGER;
  cbp:                        INTEGER;
  temp:                       BOOLEAN;

BEGIN
  TRY 
    (* Read in the macroblock stuffing, macroblock escapes, and macroblock address increment. *)
    skippedMacroblocks := ReadMacroblockAddressIncrement(file) - 1; 
   
    (* Deal with all of the skipped macroblocks. *)
    FOR i := 1 TO skippedMacroblocks DO
      (* There shouldn't be any skipped macroblocks in an I-picture or D-picture. *)
      IF state.p.pictureCodingType = MpegData.PictureCodingType.I THEN
        (* should raise an exception here. 
        RAISE GeneralTools.Bad("Forbidden skipped macroblock in an I-picture"); *)
        IO.Put("forbidden skipped macroblock in an I-picture\n");
      END;    
      IF state.p.pictureCodingType = MpegData.PictureCodingType.D THEN
        RAISE GeneralTools.Bad("Forbidden skipped macroblock in a D-picture");
      END;
      (* state.macroblockType still refers to the last non-skipped macroblock in this
         picture. In a B-picture, a skipped macroblock shall not follow an intra-coded picture. *)
      IF (state.p.pictureCodingType = MpegData.PictureCodingType.B) AND state.macroblockType.macroblockIntra THEN
        RAISE GeneralTools.Bad("Forbidden skipped macroblock after an intra-coded macroblock in a B-picture");
      END;
      
      (* Everything is OK, so now process the skipped macroblocks. *)
      IF state.p.pictureCodingType = MpegData.PictureCodingType.P THEN
        state.macroblockAddress := state.macroblockAddress + 1;
        state.reconRightFor := 0;
        state.reconDownFor  := 0;
        (* A skipped macroblock in a P-picture is supposed to be equivalent to a 
           macroblock with reconstructed motion vector equal to zero, so make sure
           that the macroblockMotionForward flag is set properly. *)
        temp := state.macroblockType.macroblockIntra;
        state.macroblockType.macroblockIntra := FALSE;
        PostProcessing.AddPredictedMacroblock(state);
        state.macroblockType.macroblockIntra := temp;
        state.reconRightForPrev := 0;
        state.reconDownForPrev  := 0;
      END;
      IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN 
        state.macroblockAddress := state.macroblockAddress + 1;
(*        state.reconRightFor  := state.reconRightForPrev;
        state.reconDownFor   := state.reconDownForPrev;
        state.reconRightBack := state.reconRightBackPrev;
        state.reconDownBack  := state.reconDownBackPrev; *)
        state.reconRightFor  := 0;
        state.reconDownFor   := 0;
        state.reconRightBack := 0;
        state.reconDownBack  := 0; 
        PostProcessing.AddPredictedMacroblock(state);
      END;
      state.dctDCyPast  := 1024;
      state.dctDCcbPast := 1024;
      state.dctDCcrPast := 1024;
    END; (* skipped macroblocks for loop *)  

    (* Now read in the bits for the first non-skipped macroblock. *)
    state.macroblockAddress := state.macroblockAddress + 1;
   
    (* Read in the macroblock type. *)
    ReadMacroblockType(file, state); 

    (* Based on the macroblock type, conditionally read in some or all of the 
       following information. *)
    IF state.macroblockType.macroblockQuant THEN
      IF BitIO.ShowBits(file, 5) # 0 THEN
        state.quantizerScale := BitIO.GetBits(file, 5);
      ELSE
        RAISE GeneralTools.Bad("Bad quantizer scale");
      END;
    END;

    IF state.macroblockType.macroblockMotionForward THEN
      forwardRSize := state.p.forwardFcode - 1;
      forwardF := Word.LeftShift(1, forwardRSize);
      ReadMotionVector(file, state.motionHorizontalForwardCode);
      IF (forwardF # 1) AND (state.motionHorizontalForwardCode # 0) THEN
        state.motionHorizontalForwardR := BitIO.GetBits(file, forwardRSize);          
      END;
      ReadMotionVector(file, state.motionVerticalForwardCode);
      IF (forwardF # 1) AND (state.motionVerticalForwardCode # 0) THEN
        state.motionVerticalForwardR := BitIO.GetBits(file, forwardRSize);
      END;
    ELSE
      IF state.p.pictureCodingType = MpegData.PictureCodingType.P THEN
        state.reconRightFor := 0;
        state.reconDownFor  := 0;
      END;
      IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN 
        state.reconRightFor  := state.reconRightForPrev;
        state.reconDownFor   := state.reconDownForPrev;
      END;
    END;

    IF state.macroblockType.macroblockMotionBackward THEN
      backwardRSize := state.p.backwardFcode - 1;
      backwardF := Word.LeftShift(1, backwardRSize);
      ReadMotionVector(file, state.motionHorizontalBackwardCode);    
      IF (backwardF # 1) AND (state.motionHorizontalBackwardCode # 0) THEN
        state.motionHorizontalBackwardR := BitIO.GetBits(file, backwardRSize);
      END;
      ReadMotionVector(file, state.motionVerticalBackwardCode);
      IF (backwardF # 1) AND (state.motionVerticalBackwardCode # 0) THEN
        state.motionVerticalBackwardR := BitIO.GetBits(file, backwardRSize);
      END;
    ELSE
      IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
        state.reconRightBack := state.reconRightBackPrev;
        state.reconDownBack  := state.reconDownBackPrev;
      END;
    END;

    (* Use cbp to calculate the MPEG state variable, pattern code. *)
    IF state.macroblockType.macroblockPattern THEN
      cbp := ReadCodedBlockPattern(file);
    ELSE 
      cbp := 0;
    END;
    FOR i := 0 TO 5 DO 
      state.patternCode[i] := FALSE;
      IF Word.And(cbp, Word.LeftShift(1, 5-i)) > 0 THEN
        state.patternCode[i] := TRUE;
      END;
      IF state.macroblockType.macroblockIntra THEN
        state.patternCode[i] := TRUE;
      END;
    END;

    (* Now we are done parsing the macroblock-layer MPEG bitstream.  Start decoding this information.
       First, reconstruct the macroblock's motion vectors. *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.P OR 
       state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
      (* This function checks to see if the individual macroblock is actually 
         predictive-coded. *)
      ReconstructMotionVectors(file, state); 
    END;

    (* Read in the six blocks. *)
    FOR i := 0 TO 5 DO
      IF state.patternCode[i] THEN
        state.dctDCdifferential[i] := BlockLayer.ReadBlock(file, state, i);
        (* Reconstruct the DCT coefficients and apply the inverse DCT transform. 
           Should probably do something different for blocks that are not received
           in the macroblock, i.e. if state.patternCode[i] is FALSE. *)
        PostProcessing.DoDCTrecon(state, i);
        PostProcessing.Reverse2D(state, i);
      ELSE
        IF i = 0 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconY1[y][x] := 0;
            END;
          END;
        ELSIF i = 1 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconY2[y][x] := 0;
            END;
          END;
        ELSIF i = 2 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconY3[y][x] := 0;
            END;
          END;
        ELSIF i = 3 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconY4[y][x] := 0;
            END;
          END;
        ELSIF i = 4 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconCb[y][x] := 0;
            END;
          END;
        ELSIF i = 5 THEN
          FOR y := 0 TO 7 DO
            FOR x := 0 TO 7 DO
              state.dctReconCr[y][x] := 0;
            END;
          END;
        END;
      END;
    END; 

    (* Read in the end of macroblock bit for D-pictures *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.D THEN
      IF BitIO.ShowBits(file, 1) = 1 THEN
        BitIO.SkipBits(file, 1);
      ELSE
        RAISE GeneralTools.Bad("Bad end of macroblock bit (D-picture)");
      END;
    END;
	
    (* If macroblock has predictive coding, then save the predicted macroblock YUV-values 
       in our state.current variable. *)
    IF state.macroblockType.macroblockMotionForward OR state.macroblockType.macroblockMotionBackward OR
       (NOT state.macroblockType.macroblockIntra )THEN
      PostProcessing.AddPredictedMacroblock(state);
      (* Add this macroblock's YUV data to our state.current variable. *) 
      PostProcessing.AddYUV(state, 0);
    ELSE
      (* Store this macroblock's YUV data to our state.current variable. *) 
      PostProcessing.SaveYUV(state, 0);
    END;

   
    (* Reset the variables that need this at the end of a macroblock. *)
    IF state.macroblockType.macroblockIntra THEN
      state.pastIntraAddress := state.macroblockAddress;
    ELSE
      state.dctDCyPast  := 1024;
      state.dctDCcbPast := 1024;
      state.dctDCcrPast := 1024;
    END;
  
    IF (state.p.pictureCodingType = MpegData.PictureCodingType.P) THEN
      IF NOT state.macroblockType.macroblockMotionForward THEN
        state.reconRightForPrev := 0;
        state.reconDownForPrev  := 0;
      END;
    END;
    
    (* Unlike in P-pictures, the previous reconstructed motion vectors are not reset to
       zero at this point unless the macroblock is intra-coded. *)
    IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
      IF state.macroblockType.macroblockIntra THEN
        state.reconRightForPrev  := 0;
        state.reconDownForPrev   := 0;
        state.reconRightBackPrev := 0;
        state.reconDownBackPrev  := 0;
      END;
    END;
  
  EXCEPT
  |  GeneralTools.Bad(problem) => RAISE GeneralTools.Bad(problem & " IN MacroblockLayer.ReadMacroblock");
  END;
END ReadMacroblock;

BEGIN
END MacroblockLayer.
