(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE BlockLayer;

IMPORT BitIO, Word, GeneralTools, MpegData, IO;

PROCEDURE ReadDCTdcSizeLuminance (file: BitIO.T): INTEGER 
                                 RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  code: CARDINAL := BitIO.ShowBits(file, 7);
BEGIN
  CASE code OF
  |  64..79   => BitIO.SkipBits(file, 3);
                 RETURN 0;  
  |  0..31    => BitIO.SkipBits(file, 2);
                 RETURN 1;
  |  32..63   => BitIO.SkipBits(file, 2);
                 RETURN 2;
  |  80..95   => BitIO.SkipBits(file, 3);
                 RETURN 3;
  |  96..111  => BitIO.SkipBits(file, 3);
                 RETURN 4;
  |  112..119 => BitIO.SkipBits(file, 4);
                 RETURN 5;
  |  120..123 => BitIO.SkipBits(file, 5);
                 RETURN 6;
  |  124..125 => BitIO.SkipBits(file, 6);
                 RETURN 7;
  |  126      => BitIO.SkipBits(file, 7);
                 RETURN 8;
  ELSE           RAISE GeneralTools.Bad("Bad value for dct_dc_size_luminance IN BlockLayer.ReadDCTdcSizeLuminance\n");
  END;    
END ReadDCTdcSizeLuminance;

PROCEDURE ReadDCTdcSizeChrominance (file: BitIO.T): INTEGER 
                                   RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  code: CARDINAL := BitIO.ShowBits(file, 8);
BEGIN
  CASE code OF
  |  0..63    => BitIO.SkipBits(file, 2);
                 RETURN 0;  
  |  64..127  => BitIO.SkipBits(file, 2);
                 RETURN 1;
  |  128..191 => BitIO.SkipBits(file, 2);
                 RETURN 2;
  |  192..223 => BitIO.SkipBits(file, 3);
                 RETURN 3;
  |  224..239 => BitIO.SkipBits(file, 4);
                 RETURN 4;
  |  240..247 => BitIO.SkipBits(file, 5);
                 RETURN 5;
  |  248..251 => BitIO.SkipBits(file, 6);
                 RETURN 6;
  |  252..253 => BitIO.SkipBits(file, 7);
                 RETURN 7;
  |  254      => BitIO.SkipBits(file, 8);
                 RETURN 8;
  ELSE           RAISE GeneralTools.Bad("Bad value for dct_dc_size_chrominance IN BlockLayer.ReadDCTdcSizeChrominance\n");
  END;
END ReadDCTdcSizeChrominance;

(* Used to read in just the first DCT coefficient if the parameter "first" is set to 
   TRUE. Else reads in all of the remaining DCT coefficients. *)
PROCEDURE ReaddctCoeffNext (file: BitIO.T; VAR state: MpegData.MpegState; first: BOOLEAN) 
                           RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  run: INTEGER;
  level: INTEGER;
  code: INTEGER;
  j: INTEGER;
  escape: BOOLEAN;
  done: BOOLEAN := FALSE;

BEGIN
  TRY
    (* If this is an intra-coded macroblock, then we set j (the index into dctZZ) to zero
       before the first dct_coeff_next of the block. *)
    IF state.macroblockType.macroblockIntra THEN
      j := 0;
    ELSE
      j := state.firstRun;
    END;

    (* This is one extra possible VLC if we are looking for the first DCT coefficient, which
       only occurs in predictive-coded macroblocks. *)
    IF first THEN
      IF BitIO.ShowBits(file, 1) = 1 THEN
        BitIO.SkipBits(file, 1);
        run := 0; 
        level := 1; 
        done := TRUE;
      END;
    END;        
      
    (* Calculate all of the rest of the dct coefficients *)
    WHILE (BitIO.ShowBits(file, 2) # 2) OR first DO
      (* Figure out the dctCoeffNext VLC; first, try 2-bit entries. *)
      LOOP
        IF done THEN
          EXIT;
        END;

        escape := FALSE;
        code := BitIO.ShowBits(file, 2);
        CASE code OF
        |  3 => run := 0; level := 1; BitIO.SkipBits(file, 2); EXIT;
        ELSE
        END;

        (* 3-bit *)
        code := BitIO.ShowBits(file, 3);
        CASE code OF
        |  3 => run := 1; level := 1; BitIO.SkipBits(file, 3); EXIT;
        ELSE
        END;
     
        (* 4-bit *)
        code := BitIO.ShowBits(file, 4);
        CASE code OF
        |  4 => run := 0; level := 2; BitIO.SkipBits(file, 4); EXIT;
        |  5 => run := 2; level := 1; BitIO.SkipBits(file, 4); EXIT;
        ELSE
        END;

        (* 5-bit *)
        code := BitIO.ShowBits(file, 5);
        CASE code OF
        |  5 => run := 0; level := 3; BitIO.SkipBits(file, 5); EXIT;
        |  6 => run := 4; level := 1; BitIO.SkipBits(file, 5); EXIT;
        |  7 => run := 3; level := 1; BitIO.SkipBits(file, 5); EXIT;
        ELSE
        END;

        (* 6-bit *)
        code := BitIO.ShowBits(file, 6);
        CASE code OF
        |  1 => BitIO.SkipBits(file, 6);
                (* This is an escape code. Next 6 bits are the run.  The level is a VLC of either 8 or 16 bits. *)
                escape := TRUE;
                run := BitIO.GetBits(file, 6);
                LOOP
                  code := BitIO.ShowBits(file, 8);
                  CASE code OF
                  |  1..127   => level := code;       BitIO.SkipBits(file, 8); EXIT;
                  |  129..255 => level := code - 256; BitIO.SkipBits(file, 8); EXIT;
                  ELSE
                  END;
     
                  code := BitIO.ShowBits(file, 16);
                  CASE code OF
                  | 128..255     => level := code;               BitIO.SkipBits(file, 16); EXIT;
                  | 32769..32896 => level := code - 32768 - 256; BitIO.SkipBits(file, 16); EXIT;
                  ELSE
                    RAISE GeneralTools.Bad("Bad DCT coefficient"); 
                  END;
                END;
        |  4 => run := 7; level := 1; BitIO.SkipBits(file, 6); EXIT;
        |  5 => run := 6; level := 1; BitIO.SkipBits(file, 6); EXIT;
        |  6 => run := 1; level := 2; BitIO.SkipBits(file, 6); EXIT;
        |  7 => run := 5; level := 1; BitIO.SkipBits(file, 6); EXIT;       
        ELSE
        END;

        IF escape THEN
          EXIT;
        END;

        (* 7-bit *)
        code := BitIO.ShowBits(file, 7);
        CASE code OF
        |  4  => run := 2; level := 2; BitIO.SkipBits(file, 7); EXIT;
        |  5  => run := 9; level := 1; BitIO.SkipBits(file, 7); EXIT;
        |  6  => run := 0; level := 4; BitIO.SkipBits(file, 7); EXIT;
        |  7  => run := 8; level := 1; BitIO.SkipBits(file, 7); EXIT;
        ELSE
        END;

        (* 8-bit *)
        code := BitIO.ShowBits(file, 8);
        CASE code OF
        |  32 => run := 13; level := 1; BitIO.SkipBits(file, 8); EXIT;
        |  33 => run := 0;  level := 6; BitIO.SkipBits(file, 8); EXIT;
        |  34 => run := 12; level := 1; BitIO.SkipBits(file, 8); EXIT;
        |  35 => run := 11; level := 1; BitIO.SkipBits(file, 8); EXIT;
        |  36 => run := 3;  level := 2; BitIO.SkipBits(file, 8); EXIT;
        |  37 => run := 1;  level := 3; BitIO.SkipBits(file, 8); EXIT;
        |  38 => run := 0;  level := 5; BitIO.SkipBits(file, 8); EXIT;
        |  39 => run := 10; level := 1; BitIO.SkipBits(file, 8); EXIT;
        ELSE
        END;

        (* 10-bit *)
        code := BitIO.ShowBits(file, 10);
        CASE code OF
        |  8  => run := 16; level := 1; BitIO.SkipBits(file, 10); EXIT;
        |  9  => run := 5;  level := 2; BitIO.SkipBits(file, 10); EXIT;
        | 10  => run := 0;  level := 7; BitIO.SkipBits(file, 10); EXIT;
        | 11  => run := 2;  level := 3; BitIO.SkipBits(file, 10); EXIT;
        | 12  => run := 1;  level := 4; BitIO.SkipBits(file, 10); EXIT;
        | 13  => run := 15; level := 1; BitIO.SkipBits(file, 10); EXIT;
        | 14  => run := 14; level := 1; BitIO.SkipBits(file, 10); EXIT;
        | 15  => run := 4;  level := 2; BitIO.SkipBits(file, 10); EXIT;
        ELSE
        END;
 
        (* 12-bit *)
        code := BitIO.ShowBits(file, 12);
        CASE code OF
        | 16  => run :=  0; level := 11; BitIO.SkipBits(file, 12); EXIT;
        | 17  => run :=  8; level :=  2; BitIO.SkipBits(file, 12); EXIT;
        | 18  => run :=  4; level :=  3; BitIO.SkipBits(file, 12); EXIT;
        | 19  => run :=  0; level := 10; BitIO.SkipBits(file, 12); EXIT;
        | 20  => run :=  2; level :=  4; BitIO.SkipBits(file, 12); EXIT;
        | 21  => run :=  7; level :=  2; BitIO.SkipBits(file, 12); EXIT;
        | 22  => run := 21; level :=  1; BitIO.SkipBits(file, 12); EXIT;
        | 23  => run := 20; level :=  1; BitIO.SkipBits(file, 12); EXIT;
        | 24  => run :=  0; level :=  9; BitIO.SkipBits(file, 12); EXIT;
        | 25  => run := 19; level :=  1; BitIO.SkipBits(file, 12); EXIT;
        | 26  => run := 18; level :=  1; BitIO.SkipBits(file, 12); EXIT;
        | 27  => run :=  1; level :=  5; BitIO.SkipBits(file, 12); EXIT;
        | 28  => run :=  3; level :=  3; BitIO.SkipBits(file, 12); EXIT;
        | 29  => run :=  0; level :=  8; BitIO.SkipBits(file, 12); EXIT;
        | 30  => run :=  6; level :=  2; BitIO.SkipBits(file, 12); EXIT;
        | 31  => run := 17; level :=  1; BitIO.SkipBits(file, 12); EXIT;
        ELSE
        END;

        (* 13-bit *)
        code := BitIO.ShowBits(file, 13);
        CASE code OF
        | 16  => run := 10; level :=  2; BitIO.SkipBits(file, 13); EXIT;
        | 17  => run :=  9; level :=  2; BitIO.SkipBits(file, 13); EXIT;
        | 18  => run :=  5; level :=  3; BitIO.SkipBits(file, 13); EXIT;
        | 19  => run :=  3; level :=  4; BitIO.SkipBits(file, 13); EXIT;
        | 20  => run :=  2; level :=  5; BitIO.SkipBits(file, 13); EXIT;
        | 21  => run :=  1; level :=  7; BitIO.SkipBits(file, 13); EXIT;
        | 22  => run :=  1; level :=  6; BitIO.SkipBits(file, 13); EXIT;
        | 23  => run :=  0; level := 15; BitIO.SkipBits(file, 13); EXIT;
        | 24  => run :=  0; level := 14; BitIO.SkipBits(file, 13); EXIT;
        | 25  => run :=  0; level := 13; BitIO.SkipBits(file, 13); EXIT;
        | 26  => run :=  0; level := 12; BitIO.SkipBits(file, 13); EXIT;
        | 27  => run := 26; level :=  1; BitIO.SkipBits(file, 13); EXIT;
        | 28  => run := 25; level :=  1; BitIO.SkipBits(file, 13); EXIT;
        | 29  => run := 24; level :=  1; BitIO.SkipBits(file, 13); EXIT;
        | 30  => run := 23; level :=  1; BitIO.SkipBits(file, 13); EXIT;
        | 31  => run := 22; level :=  1; BitIO.SkipBits(file, 13); EXIT;
        ELSE
        END;

        (* 14-bit *)
        code := BitIO.ShowBits(file, 14);
        CASE code OF
        | 16  => run :=  0; level := 31; BitIO.SkipBits(file, 14); EXIT;
        | 17  => run :=  0; level := 30; BitIO.SkipBits(file, 14); EXIT;
        | 18  => run :=  0; level := 29; BitIO.SkipBits(file, 14); EXIT; 
        | 19  => run :=  0; level := 28; BitIO.SkipBits(file, 14); EXIT;
        | 20  => run :=  0; level := 27; BitIO.SkipBits(file, 14); EXIT;
        | 21  => run :=  0; level := 26; BitIO.SkipBits(file, 14); EXIT;
        | 22  => run :=  0; level := 25; BitIO.SkipBits(file, 14); EXIT;
        | 23  => run :=  0; level := 24; BitIO.SkipBits(file, 14); EXIT;
        | 24  => run :=  0; level := 23; BitIO.SkipBits(file, 14); EXIT;
        | 25  => run :=  0; level := 22; BitIO.SkipBits(file, 14); EXIT;
        | 26  => run :=  0; level := 21; BitIO.SkipBits(file, 14); EXIT;
        | 27  => run :=  0; level := 20; BitIO.SkipBits(file, 14); EXIT;
        | 28  => run :=  0; level := 19; BitIO.SkipBits(file, 14); EXIT;
        | 29  => run :=  0; level := 18; BitIO.SkipBits(file, 14); EXIT;
        | 30  => run :=  0; level := 17; BitIO.SkipBits(file, 14); EXIT;
        | 31  => run :=  0; level := 16; BitIO.SkipBits(file, 14); EXIT;
        ELSE
        END;

        (* 15-bit *)
        code := BitIO.ShowBits(file, 15);
        CASE code OF
        | 16  => run :=  0; level := 40; BitIO.SkipBits(file, 15); EXIT;
        | 17  => run :=  0; level := 39; BitIO.SkipBits(file, 15); EXIT;
        | 18  => run :=  0; level := 38; BitIO.SkipBits(file, 15); EXIT; 
        | 19  => run :=  0; level := 37; BitIO.SkipBits(file, 15); EXIT;
        | 20  => run :=  0; level := 36; BitIO.SkipBits(file, 15); EXIT;
        | 21  => run :=  0; level := 35; BitIO.SkipBits(file, 15); EXIT;
        | 22  => run :=  0; level := 34; BitIO.SkipBits(file, 15); EXIT;
        | 23  => run :=  0; level := 33; BitIO.SkipBits(file, 15); EXIT;
        | 24  => run :=  0; level := 32; BitIO.SkipBits(file, 15); EXIT;
        | 25  => run :=  1; level := 14; BitIO.SkipBits(file, 15); EXIT;
        | 26  => run :=  1; level := 13; BitIO.SkipBits(file, 15); EXIT;
        | 27  => run :=  1; level := 12; BitIO.SkipBits(file, 15); EXIT;
        | 28  => run :=  1; level := 11; BitIO.SkipBits(file, 15); EXIT;
        | 29  => run :=  1; level := 10; BitIO.SkipBits(file, 15); EXIT;
        | 30  => run :=  1; level :=  9; BitIO.SkipBits(file, 15); EXIT;
        | 31  => run :=  1; level :=  8; BitIO.SkipBits(file, 15); EXIT;
        ELSE
        END;

        (* 16-bit *)
        code := BitIO.ShowBits(file, 16);
        CASE code OF
        | 16  => run :=  1; level := 18; BitIO.SkipBits(file, 16); EXIT;
        | 17  => run :=  1; level := 17; BitIO.SkipBits(file, 16); EXIT;
        | 18  => run :=  1; level := 16; BitIO.SkipBits(file, 16); EXIT; 
        | 19  => run :=  1; level := 15; BitIO.SkipBits(file, 16); EXIT;
        | 20  => run :=  6; level :=  3; BitIO.SkipBits(file, 16); EXIT;
        | 21  => run := 16; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 22  => run := 15; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 23  => run := 14; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 24  => run := 13; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 25  => run := 12; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 26  => run := 11; level :=  2; BitIO.SkipBits(file, 16); EXIT;
        | 27  => run := 31; level :=  1; BitIO.SkipBits(file, 16); EXIT;
        | 28  => run := 30; level :=  1; BitIO.SkipBits(file, 16); EXIT;
        | 29  => run := 29; level :=  1; BitIO.SkipBits(file, 16); EXIT;
        | 30  => run := 28; level :=  1; BitIO.SkipBits(file, 16); EXIT;
        | 31  => run := 27; level :=  1; BitIO.SkipBits(file, 16); EXIT;
        ELSE           
        END;

        (* Should never get here. *)
        RAISE GeneralTools.Bad("Bad DCT coefficient"); 
      END;   

      IF first THEN
        j := run;
        state.firstRun := run;
      ELSE   
        j := j + run + 1;
      END;
 
      (* Now, read the sign bit.  If it's a one, then level is negative. 
         If last VLC was an escape code, then there is no sign bit. *)  
      IF j < 64 THEN
        IF NOT escape THEN  
          IF BitIO.GetBits(file, 1) = 0 THEN
            state.dctZZ[j] := level;
          ELSE
            state.dctZZ[j] := -level;
          END;
        ELSE
          state.dctZZ[j] := level;
        END;
      ELSE
        EXIT;
      END;

      IF first THEN
        EXIT;
      END;
    END;

    (* Skip the end_of_block bits. *)
    IF NOT first THEN
      BitIO.SkipBits(file, 2);
    END;
  EXCEPT
  | GeneralTools.Bad(problem) => RAISE GeneralTools.Bad(problem & " IN BlockLayer.ReaddctCoeffNext");
  END;
END ReaddctCoeffNext; 

PROCEDURE ReadBlock (file: BitIO.T; VAR state: MpegData.MpegState; blockNum: INTEGER): INTEGER 
                    RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  size: CARDINAL;
  dctDCdifferential: INTEGER := 0;

BEGIN
  (* Initialize the dctZZ array *)
  FOR i := 0 TO 63 DO
    state.dctZZ[i] := 0;
  END;

  IF state.patternCode[blockNum] THEN
    IF state.macroblockType.macroblockIntra THEN
      IF blockNum < 4 THEN
        size := ReadDCTdcSizeLuminance(file);
        IF size # 0 THEN
          dctDCdifferential := BitIO.GetBits(file, size);
        END;
      ELSE
        size := ReadDCTdcSizeChrominance(file);
        IF size # 0 THEN
          dctDCdifferential := BitIO.GetBits(file, size);
        END;  
      END;
      (* Calculate the first dct coefficient *)
      IF size # 0 THEN
        IF Word.And(dctDCdifferential, Word.LeftShift(1, size - 1)) # 0 THEN
          state.dctZZ[0] := dctDCdifferential; 
        ELSE
          state.dctZZ[0] := Word.Or(Word.LeftShift(-1, size), dctDCdifferential + 1); 
        END;
      END;
    ELSE
      (* Read in dct_coeff_first. *)
      ReaddctCoeffNext(file, state, TRUE);
    END;

    (* Calculate the rest of the DCT coefficients *)
    IF state.p.pictureCodingType # MpegData.PictureCodingType.D THEN
      ReaddctCoeffNext(file, state, FALSE);
    END;
  END;

  RETURN dctDCdifferential;
END ReadBlock;

BEGIN
END BlockLayer.


