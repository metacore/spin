(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE GeneralTools;

IMPORT BitIO, IO;

FROM BitIO IMPORT T, ShowBits, GetBits, SkipBits, ByteAligned,
                  EOF;

<*INLINE*>
PROCEDURE SkipMarkerBit(file: T) RAISES {Bad, EOF} =
BEGIN
  (*IF GetBits(file, 1) # 1 THEN 
    RAISE Bad("Bad marker bit"); 
  END; *)
  SkipBits(file, 1);
END SkipMarkerBit;

PROCEDURE StartCode(file: T; startCode: CARDINAL) RAISES {Bad, EOF} =
BEGIN
  IF NextStartCode(file) # startCode THEN 
    IO.Put("Next 32 bits equal = ");
    IO.PutInt(ShowBits(file, 32));
    IO.Put("\n");
    RAISE Bad("Bad start code");
  ELSE
    SkipBits(file, 32);
  END;
END StartCode;

PROCEDURE NextStartCode(file: T): CARDINAL RAISES {Bad, EOF} =
BEGIN
  WHILE NOT(ByteAligned(file)) DO
    IF GetBits(file, 1) = 1 THEN 
      IO.Put ("Bad start code stuffing bits\n");
    END;
  END; 
  WHILE ShowBits(file, 24) # 1 DO
    SkipBits(file, 8);
  END;
  RETURN ShowBits(file, 32);
END NextStartCode;

PROCEDURE ReadExtentionAndUserData(file: T) RAISES {Bad, EOF} =
BEGIN
  TRY
    IF NextStartCode(file) = ExtentionStartCode THEN
      SkipBits(file, 32);
      WHILE ShowBits(file, 24) # 1 DO
        SkipBits(file, 8);
      END;
    END;
    IF NextStartCode(file) = UserDataStartCode THEN
      SkipBits(file, 32);
      WHILE ShowBits(file, 24) # 1 DO
        SkipBits(file, 8);
      END;
    END;
  EXCEPT
  |  Bad(problem) => RAISE Bad(problem & " IN GeneralTools.ReadExtentionAndUserData");
  END;
END ReadExtentionAndUserData;

PROCEDURE SkipExtraInformation(file: T) RAISES {Bad, EOF} =
BEGIN
  WHILE ShowBits(file, 1) = 1 DO
    SkipBits(file, 1);
    SkipBits(file, 8); (* currently ignore extra information *)
  END;
  IF ShowBits(file, 1) = 0 THEN
    SkipBits(file, 1);
  ELSE
    RAISE Bad("Bad extra_bit_* IN GeneralTools.SkipExtraInformation");
  END;
END SkipExtraInformation;

BEGIN
END GeneralTools.


