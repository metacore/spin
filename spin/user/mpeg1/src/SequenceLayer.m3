(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE SequenceLayer;

IMPORT BitIO, GeneralTools, MpegData, IO;

FROM BitIO IMPORT T, GetBits, EOF;

PROCEDURE InitScan (VAR state: MpegData.MpegState) =
BEGIN
  (* Initialize the scan array. It defines the zigzag scanning sequence used for the
     DCT coefficients. The first coefficient in zigzag sequence is located at 0's 
     location in the array; the second coefficient is located at 1, third is at 2, and
     so on ...

                                      0    1    5    6   14   15   27   28
                                      2    4    7   13   16   26   29   42
                                      3    8   12   17   25   30   41   43
                                      9   11   18   24   31   40   44   53
                                     10   19   23   32   39   45   52   54
                                     20   22   33   38   46   51   55   60
                                     21   34   37   47   50   56   59   61
                                     35   36   48   49   57   58   62   63    *)

  state.scan[0][0] :=  0;
  state.scan[0][1] :=  1;
  state.scan[0][2] :=  5;
  state.scan[0][3] :=  6;
  state.scan[0][4] := 14;
  state.scan[0][5] := 15;
  state.scan[0][6] := 27;
  state.scan[0][7] := 28;

  state.scan[1][0] :=  2;
  state.scan[1][1] :=  4;
  state.scan[1][2] :=  7;
  state.scan[1][3] := 13;
  state.scan[1][4] := 16;
  state.scan[1][5] := 26;
  state.scan[1][6] := 29;
  state.scan[1][7] := 42;

  state.scan[2][0] :=  3;
  state.scan[2][1] :=  8;
  state.scan[2][2] := 12;
  state.scan[2][3] := 17;
  state.scan[2][4] := 25;
  state.scan[2][5] := 30;
  state.scan[2][6] := 41;
  state.scan[2][7] := 43;

  state.scan[3][0] :=  9;
  state.scan[3][1] := 11;
  state.scan[3][2] := 18;
  state.scan[3][3] := 24;
  state.scan[3][4] := 31;
  state.scan[3][5] := 40;
  state.scan[3][6] := 44;
  state.scan[3][7] := 53;

  state.scan[4][0] := 10;
  state.scan[4][1] := 19;
  state.scan[4][2] := 23;
  state.scan[4][3] := 32;
  state.scan[4][4] := 39;
  state.scan[4][5] := 45;
  state.scan[4][6] := 52;
  state.scan[4][7] := 54;

  state.scan[5][0] := 20;
  state.scan[5][1] := 22;
  state.scan[5][2] := 33;
  state.scan[5][3] := 38;
  state.scan[5][4] := 46;
  state.scan[5][5] := 51;
  state.scan[5][6] := 55;
  state.scan[5][7] := 60;

  state.scan[6][0] := 21;
  state.scan[6][1] := 34;
  state.scan[6][2] := 37;
  state.scan[6][3] := 47;
  state.scan[6][4] := 50;
  state.scan[6][5] := 56;
  state.scan[6][6] := 59;
  state.scan[6][7] := 61;

  state.scan[7][0] := 35;
  state.scan[7][1] := 36;
  state.scan[7][2] := 48;
  state.scan[7][3] := 49;
  state.scan[7][4] := 57;
  state.scan[7][5] := 58;
  state.scan[7][6] := 62;
  state.scan[7][7] := 63;
END InitScan;

PROCEDURE InitIntraQuantizerMatrix (file: T; VAR state: MpegData.MpegState) 
                                   RAISES {EOF} =
BEGIN
  (* Initialize the IntraQuantizerMatrix.  If the load matrix bit is set, then
     read in the values from the bit stream.  These values are read in from the 
     bit stream in zig-zag scanning sequence.  Else, just set to the default values. *)
  
  IF state.sh.loadIntraQuantizerMatrix THEN
    state.sh.intraQuantizerMatrix[0][0] :=  GetBits(file, 8);
    state.sh.intraQuantizerMatrix[0][1] :=  GetBits(file, 8);
    state.sh.intraQuantizerMatrix[1][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][2] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[2][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][5] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[1][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][2] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[3][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[0][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][4] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[4][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][0] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][1] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][4] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[3][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[1][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[2][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][3] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[7][2] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][3] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[3][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[4][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][6] :=  GetBits(file, 8); 

    state.sh.intraQuantizerMatrix[6][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][4] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][5] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[5][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[6][7] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][6] :=  GetBits(file, 8); 
    state.sh.intraQuantizerMatrix[7][7] :=  GetBits(file, 8); 

  ELSE
    state.sh.intraQuantizerMatrix[0][0] :=  8;
    state.sh.intraQuantizerMatrix[0][1] := 16;
    state.sh.intraQuantizerMatrix[0][2] := 19;
    state.sh.intraQuantizerMatrix[0][3] := 22;
    state.sh.intraQuantizerMatrix[0][4] := 26;
    state.sh.intraQuantizerMatrix[0][5] := 27;
    state.sh.intraQuantizerMatrix[0][6] := 29;
    state.sh.intraQuantizerMatrix[0][7] := 34;

    state.sh.intraQuantizerMatrix[1][0] := 16;
    state.sh.intraQuantizerMatrix[1][1] := 16;
    state.sh.intraQuantizerMatrix[1][2] := 22;
    state.sh.intraQuantizerMatrix[1][3] := 24;
    state.sh.intraQuantizerMatrix[1][4] := 27;
    state.sh.intraQuantizerMatrix[1][5] := 29;
    state.sh.intraQuantizerMatrix[1][6] := 34;
    state.sh.intraQuantizerMatrix[1][7] := 37;

    state.sh.intraQuantizerMatrix[2][0] := 19;
    state.sh.intraQuantizerMatrix[2][1] := 22;
    state.sh.intraQuantizerMatrix[2][2] := 26;
    state.sh.intraQuantizerMatrix[2][3] := 27;
    state.sh.intraQuantizerMatrix[2][4] := 29;
    state.sh.intraQuantizerMatrix[2][5] := 34;
    state.sh.intraQuantizerMatrix[2][6] := 34;
    state.sh.intraQuantizerMatrix[2][7] := 38;
 
    state.sh.intraQuantizerMatrix[3][0] := 22;
    state.sh.intraQuantizerMatrix[3][1] := 22;
    state.sh.intraQuantizerMatrix[3][2] := 26;
    state.sh.intraQuantizerMatrix[3][3] := 27;
    state.sh.intraQuantizerMatrix[3][4] := 29;
    state.sh.intraQuantizerMatrix[3][5] := 34;
    state.sh.intraQuantizerMatrix[3][6] := 37;
    state.sh.intraQuantizerMatrix[3][7] := 40;

    state.sh.intraQuantizerMatrix[4][0] := 22;
    state.sh.intraQuantizerMatrix[4][1] := 26;
    state.sh.intraQuantizerMatrix[4][2] := 27;
    state.sh.intraQuantizerMatrix[4][3] := 29;
    state.sh.intraQuantizerMatrix[4][4] := 32;
    state.sh.intraQuantizerMatrix[4][5] := 35;
    state.sh.intraQuantizerMatrix[4][6] := 40;
    state.sh.intraQuantizerMatrix[4][7] := 48;

    state.sh.intraQuantizerMatrix[5][0] := 26;
    state.sh.intraQuantizerMatrix[5][1] := 27;
    state.sh.intraQuantizerMatrix[5][2] := 29;
    state.sh.intraQuantizerMatrix[5][3] := 32;
    state.sh.intraQuantizerMatrix[5][4] := 35;
    state.sh.intraQuantizerMatrix[5][5] := 40;
    state.sh.intraQuantizerMatrix[5][6] := 48;
    state.sh.intraQuantizerMatrix[5][7] := 58;

    state.sh.intraQuantizerMatrix[6][0] := 26;
    state.sh.intraQuantizerMatrix[6][1] := 27;
    state.sh.intraQuantizerMatrix[6][2] := 29;
    state.sh.intraQuantizerMatrix[6][3] := 34;
    state.sh.intraQuantizerMatrix[6][4] := 38;
    state.sh.intraQuantizerMatrix[6][5] := 46;
    state.sh.intraQuantizerMatrix[6][6] := 56;
    state.sh.intraQuantizerMatrix[6][7] := 69;

    state.sh.intraQuantizerMatrix[7][0] := 27;
    state.sh.intraQuantizerMatrix[7][1] := 29;
    state.sh.intraQuantizerMatrix[7][2] := 35;
    state.sh.intraQuantizerMatrix[7][3] := 38;
    state.sh.intraQuantizerMatrix[7][4] := 46;
    state.sh.intraQuantizerMatrix[7][5] := 56;
    state.sh.intraQuantizerMatrix[7][6] := 69;
    state.sh.intraQuantizerMatrix[7][7] := 83;
  END;
END InitIntraQuantizerMatrix;

PROCEDURE InitNonIntraQuantizerMatrix (file: T; VAR state: MpegData.MpegState) 
                                      RAISES {EOF} =
BEGIN
  (* Initialize the NonIntraQuantizerMatrix.  If the load matrix bit is set, then
     read in the values from the bit stream.  Else, just set to the default values. *)

  IF state.sh.loadNonIntraQuantizerMatrix THEN
    state.sh.nonIntraQuantizerMatrix[0][0] :=  GetBits(file, 8);
    state.sh.nonIntraQuantizerMatrix[0][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][2] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[2][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][5] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[1][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][2] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[3][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[0][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][4] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[4][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][0] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][1] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][4] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[3][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[1][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[2][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][3] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[7][2] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][3] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[3][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[4][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][6] :=  GetBits(file, 8); 

    state.sh.nonIntraQuantizerMatrix[6][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][4] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][5] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[5][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[6][7] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][6] :=  GetBits(file, 8); 
    state.sh.nonIntraQuantizerMatrix[7][7] :=  GetBits(file, 8); 

  ELSE
    FOR i := 0 TO 7 DO
      FOR j := 0 TO 7 DO
        state.sh.nonIntraQuantizerMatrix[i][j] := 8;
      END;
    END;    
  END;
END InitNonIntraQuantizerMatrix;

PROCEDURE ReadSequenceHeader(file: T; VAR state: MpegData.MpegState) RAISES {GeneralTools.Bad, EOF} =
BEGIN
  TRY
    GeneralTools.StartCode(file, SequenceHeaderCode);

    state.sh.horizontalSize := GetBits(file, 12);
    state.sh.verticalSize := GetBits(file, 12);
    state.sh.pelAspectRatio := GetBits(file, 4);
    state.sh.pictureRate := GetBits(file, 4);
    state.sh.bitRate := GetBits(file, 18);

    GeneralTools.SkipMarkerBit(file);
    state.sh.vbvBufferSize := GetBits(file, 10);
    state.sh.constrainedParametersFlag := VAL(GetBits(file, 1), BOOLEAN);

    state.sh.loadIntraQuantizerMatrix := VAL(GetBits(file, 1), BOOLEAN);
    InitIntraQuantizerMatrix(file, state);

    state.sh.loadNonIntraQuantizerMatrix := VAL(GetBits(file, 1), BOOLEAN);
    InitNonIntraQuantizerMatrix(file, state);

    GeneralTools.ReadExtentionAndUserData(file);
  EXCEPT
  | GeneralTools.Bad(problem) => RAISE GeneralTools.Bad(problem & " IN SequenceLayer.ReadSequenceHeader");
  END;
END ReadSequenceHeader;

BEGIN
END SequenceLayer.
