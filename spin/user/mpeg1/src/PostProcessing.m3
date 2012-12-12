(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE PostProcessing;

IMPORT Word, MpegData, IO;

CONST
  ScaleBits = 13;
  One = Word.LeftShift(1, ScaleBits);

PROCEDURE DoDCTrecon(VAR state: MpegData.MpegState; block: INTEGER) =
VAR
  i: INTEGER;
  dctRecon: ARRAY [0..7], [0..7] OF INTEGER;
BEGIN
  (* What type of macroblock is this? If it's intra-coded, then execute this code. *)
  IF state.macroblockType.macroblockIntra THEN
    FOR m := 0 TO 7 DO
      FOR n := 0 TO 7 DO
        i := state.scan[m][n];
        dctRecon[m][n] := (2 * state.dctZZ[i] * state.quantizerScale * state.sh.intraQuantizerMatrix[m][n]) DIV 16;
        IF (Word.And(dctRecon[m][n], 1) = 0) THEN
          IF dctRecon[m][n] < 0 THEN
            dctRecon[m][n] := dctRecon[m][n] + 1;
          END;
          IF dctRecon[m][n] > 0 THEN
            dctRecon[m][n] := dctRecon[m][n] - 1;
          END;
        END;  
        IF dctRecon[m][n] > 2047 THEN
          dctRecon[m][n] := 2047;
        END;
        IF dctRecon[m][n] < -2048 THEN
          dctRecon[m][n] := -2048;
        END;
      END;
    END;

    CASE block OF
    |  0    =>  dctRecon[0][0] := state.dctZZ[0] * 8;
                IF (state.macroblockAddress - state.pastIntraAddress) > 1 THEN
                  dctRecon[0][0] := (128 * 8) + dctRecon[0][0];
                ELSE
                  dctRecon[0][0] := state.dctDCyPast + dctRecon[0][0];
                END;
                (* The first DCT coefficient isn't supposed to be out of the range [0,2047]. 
                   Check for this here. *)
                IF dctRecon[0][0] < 0 THEN
                  dctRecon[0][0] := 0;
                END;
                IF dctRecon[0][0] > 2047 THEN
                  dctRecon[0][0] := 2047;
                END;
                state.dctDCyPast := dctRecon[0][0];
                state.dctReconY1 := dctRecon;
    |  1..3 =>  dctRecon[0][0] := state.dctDCyPast + (state.dctZZ[0] * 8);
                (* The first DCT coefficient isn't supposed to be out of the range [0,2047]. 
                   Check for this here. *)
                IF dctRecon[0][0] < 0 THEN
                  dctRecon[0][0] := 0;
                END;
                IF dctRecon[0][0] > 2047 THEN
                  dctRecon[0][0] := 2047;
                END;
                state.dctDCyPast := dctRecon[0][0];
                IF block = 1 THEN
                  state.dctReconY2 := dctRecon;
                ELSE
                  IF block = 2 THEN
                    state.dctReconY3 := dctRecon;
                  ELSE
                    state.dctReconY4 := dctRecon;
                  END;
                END;
    |  4    =>  dctRecon[0][0] := state.dctZZ[0] * 8;
                IF (state.macroblockAddress - state.pastIntraAddress) > 1 THEN
                  dctRecon[0][0] := (128 * 8) + dctRecon[0][0];
                ELSE
                  dctRecon[0][0] := state.dctDCcbPast + dctRecon[0][0];
                END;
                (* The first DCT coefficient isn't supposed to be out of the range [0,2047]. 
                   Check for this here. *)
                IF dctRecon[0][0] < 0 THEN
                  dctRecon[0][0] := 0;
                END;
                IF dctRecon[0][0] > 2047 THEN
                  dctRecon[0][0] := 2047;
                END;
                state.dctDCcbPast := dctRecon[0][0];
                state.dctReconCb := dctRecon;
    |  5    =>  dctRecon[0][0] := state.dctZZ[0] * 8;
                IF (state.macroblockAddress - state.pastIntraAddress) > 1 THEN
                  dctRecon[0][0] := (128 * 8) + dctRecon[0][0];
                ELSE
                  dctRecon[0][0] := state.dctDCcrPast + dctRecon[0][0];
                END;
                (* The first DCT coefficient isn't supposed to be out of the range [0,2047]. 
                   Check for this here. *)
                IF dctRecon[0][0] < 0 THEN
                  dctRecon[0][0] := 0;
                END;
                IF dctRecon[0][0] > 2047 THEN
                  dctRecon[0][0] := 2047;
                END;
                state.dctDCcrPast := dctRecon[0][0];
                state.dctReconCr := dctRecon;
    ELSE
    END;  
  ELSE
    (* This is for predictive-coded macroblocks in P- and B-pictures. *)
    FOR m := 0 TO 7 DO
      FOR n := 0 TO 7 DO
        i := state.scan[m][n];
        IF state.dctZZ[0] > 0 THEN
          dctRecon[m][n] := (((2 * state.dctZZ[i]) + 1) * state.quantizerScale * 
                            state.sh.nonIntraQuantizerMatrix[m][n]) DIV 16;
        ELSE 
          IF state.dctZZ[0] = 0 THEN
            dctRecon[m][n] := (2 * state.dctZZ[i] * state.quantizerScale * 
                              state.sh.nonIntraQuantizerMatrix[m][n]) DIV 16;
          ELSE (* state.dctZZ[0] < 0 *)
            dctRecon[m][n] := (((2 * state.dctZZ[i]) - 1) * state.quantizerScale * 
                              state.sh.nonIntraQuantizerMatrix[m][n]) DIV 16;
          END;
        END;  
        IF NOT VAL(Word.And(dctRecon[m][n], 1), BOOLEAN) THEN
          IF dctRecon[m][n] > 0 THEN
            dctRecon[m][n] := dctRecon[m][n] + 1;
          END; 
          IF dctRecon[m][n] < 0 THEN
            dctRecon[m][n] := dctRecon[m][n] - 1;
          END;
        END; 
        IF dctRecon[m][n] > 2047 THEN
          dctRecon[m][n] := 2047;
        END;
        IF dctRecon[m][n] < -2048 THEN
          dctRecon[m][n] := -2048;
        END;
        IF state.dctZZ[i] = 0 THEN
          dctRecon[m][n] := 0;
        END;
      END;
    END;

    (* Save the dctRecon array back to our MpegState variable. *)
    CASE block OF
    | 0 => state.dctReconY1 := dctRecon;
    | 1 => state.dctReconY2 := dctRecon;
    | 2 => state.dctReconY3 := dctRecon;
    | 3 => state.dctReconY4 := dctRecon;
    | 4 => state.dctReconCb := dctRecon;
    | 5 => state.dctReconCr := dctRecon;
    ELSE
    END;
  END;
END DoDCTrecon;

PROCEDURE AddPredictedMacroblock(VAR state: MpegData.MpegState) =
VAR
  rightFor:      INTEGER;
  rightHalfFor:  INTEGER;
  downFor:       INTEGER;
  downHalfFor:   INTEGER;
  rightBack:     INTEGER;
  rightHalfBack: INTEGER;
  downBack:      INTEGER;
  downHalfBack:  INTEGER;
  lower:         INTEGER;
  left:          INTEGER;
  index:         INTEGER;
  index1:        INTEGER;
  index2:        INTEGER;
  index3:        INTEGER;
  index4:        INTEGER;
  y, y1, y2, y3, y4:                INTEGER;
  cb, cb1, cb2, cb3, cb4:           INTEGER;
  cr, cr1, cr2, cr3, cr4:           INTEGER;
  LRcb, LRcb1, LRcb2, LRcb3, LRcb4: INTEGER;
  LRcr, LRcr1, LRcr2, LRcr3, LRcr4: INTEGER;
  ULcb, ULcb1, ULcb2, ULcb3, ULcb4: INTEGER; 
  ULcr, ULcr1, ULcr2, ULcr3, ULcr4: INTEGER;
  URcb, URcb1, URcb2, URcb3, URcb4: INTEGER;
  URcr, URcr1, URcr2, URcr3, URcr4: INTEGER;
BEGIN
  (* Calculate the location of the lower left corner of this macroblock in the current frame.  The point of
     reference is the lower left corner of the frame, so if the lower left corner of the macroblock is 10
     pixels above the lower left corner of the frame, then lower is set to 10. *)
  lower := state.sh.verticalSize - (((state.macroblockAddress DIV state.mbWidth) * 16) + 16);
  left  := (state.macroblockAddress MOD state.mbWidth) * 16;
  
  FOR block := 0 TO 5 DO
    IF block = 0 THEN
      (* rightFor and downFor should really be calculated by a right shift by one place. *)
      rightFor := state.reconRightFor DIV 2;
      downFor  := state.reconDownFor DIV 2;
      rightHalfFor := state.reconRightFor - (2 * rightFor);
      downHalfFor := state.reconDownFor - (2 * downFor);
     
      (* And analogous operations must be applied for backward motion vectors: *)
      IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
        rightBack     := state.reconRightBack DIV 2;
        downBack      := state.reconDownBack DIV 2;
        rightHalfBack := state.reconRightBack - (2 * rightBack);
        downHalfBack  := state.reconDownBack - (2 * downBack);
      END;
    END;
    IF block = 4 THEN
      (* This should be a divide by 2 with truncation towards zero, followed by 
         a one bit right shift. *) 
      rightFor := state.reconRightFor DIV 4;
      downFor := state.reconDownFor DIV 4;
      rightHalfFor := (state.reconRightFor DIV 2) - (2 * rightFor);
      downHalfFor := (state.reconDownFor DIV 2) - (2 * downFor);

      (* similar ops for the backward motion vectors. *)
      IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
        rightBack := state.reconRightBack DIV 4;
        downBack := state.reconDownBack DIV 4;
        rightHalfBack := (state.reconRightBack DIV 2) - (2 * rightBack);
        downHalfBack := (state.reconDownBack DIV 2) - (2 * downBack);
      END;
    END;

    (* Could check that the motion vectors don't lead outside the reference picture's
       boundaries. *)

    IF (*state.macroblockType.macroblockMotionForward*) NOT (state.macroblockType.macroblockIntra) THEN
      FOR i := 0 TO 7 DO
        FOR j := 0 TO 7 DO
          (* Since the picture size may not be a perfect multiple of 16, there may be parts of some 
             macroblocks that are outside of the picture along the bottom and right sides of the picture.
             Filter these out here. *) 
          IF ((block = 0) AND ((lower+8+i) >= 0)   AND ((left+j)       < state.sh.horizontalSize)) OR
             ((block = 1) AND ((lower+8+i) >= 0)   AND ((left+8+j)     < state.sh.horizontalSize)) OR
             ((block = 2) AND ((lower+i)  >= 0)    AND ((left+j)       < state.sh.horizontalSize)) OR
             ((block = 3) AND ((lower+i)  >= 0)    AND ((left+8+j)     < state.sh.horizontalSize)) OR
             ((block = 4) AND ((lower+(2*i)) >= 0) AND ((left+(2*j)+1) < state.sh.horizontalSize)) OR
             ((block = 5) AND ((lower+(2*i)) >= 0) AND ((left+(2*j)+1) < state.sh.horizontalSize)) THEN
            CASE block OF
            | 0 => index := (((lower+8+i)   * state.sh.horizontalSize) + left+j)       * 3; 
            | 1 => index := (((lower+8+i)   * state.sh.horizontalSize) + left+8+j)     * 3;
            | 2 => index := (((lower+i)     * state.sh.horizontalSize) + left+j)       * 3; 
            | 3 => index := (((lower+i)     * state.sh.horizontalSize) + left+8+j)     * 3;
            | 4 => index := (((lower+(2*i)) * state.sh.horizontalSize) + (left+(2*j))) * 3;
            | 5 => index := (((lower+(2*i)) * state.sh.horizontalSize) + (left+(2*j))) * 3;
            END;
            
            (* Set up the indexes into the reference pictures.  *)
            index1 := index + ((-(downFor*state.sh.horizontalSize)+rightFor)*3);
            index2 := index + ((-((downFor+1)*state.sh.horizontalSize)+rightFor)*3);
            index3 := index + ((-(downFor*state.sh.horizontalSize)+(rightFor+1))*3);
            index4 := index + ((-((downFor+1)*state.sh.horizontalSize)+(rightFor+1))*3);

            (* Calculate all the possible index + offset combinations. *)
            y     := index;
            cb    := index + 1;
            cr    := index + 2;
            y1    := index1;
            cb1   := index1 + 1;
            cr1   := index1 + 2;
            y2    := index2;
            cb2   := index2 + 1;
            cr2   := index2 + 2;
            y3    := index3;
            cb3   := index3 + 1;
            cr3   := index3 + 2; 
            y4    := index4;
            cb4   := index4 + 1;
            cr4   := index4 + 2;
            LRcb  := cb  + 3;
            LRcr  := cr  + 3;
            LRcb1 := cb1 + 3;
            LRcr1 := cr1 + 3;
            LRcb2 := cb2 + 3;
            LRcr2 := cr2 + 3;
            LRcb3 := cb3 + 3;
            LRcr3 := cr3 + 3;
            LRcb4 := cb4 + 3;
            LRcr4 := cr4 + 3;
            ULcb  := cb    + (state.sh.horizontalSize * 3);
            ULcr  := cr    + (state.sh.horizontalSize * 3);
            ULcb1 := cb1   + (state.sh.horizontalSize * 3);
            ULcr1 := cr1   + (state.sh.horizontalSize * 3);
            ULcb2 := cb2   + (state.sh.horizontalSize * 3);
            ULcr2 := cr2   + (state.sh.horizontalSize * 3);
            ULcb3 := cb3   + (state.sh.horizontalSize * 3);
            ULcr3 := cr3   + (state.sh.horizontalSize * 3);
            ULcb4 := cb4   + (state.sh.horizontalSize * 3);
            ULcr4 := cr4   + (state.sh.horizontalSize * 3);
            URcb  := LRcb  + (state.sh.horizontalSize * 3);
            URcr  := LRcr  + (state.sh.horizontalSize * 3);
            URcb1 := LRcb1 + (state.sh.horizontalSize * 3);
            URcr1 := LRcr1 + (state.sh.horizontalSize * 3);
            URcb2 := LRcb2 + (state.sh.horizontalSize * 3);
            URcr2 := LRcr2 + (state.sh.horizontalSize * 3);
            URcb3 := LRcb3 + (state.sh.horizontalSize * 3);
            URcr3 := LRcr3 + (state.sh.horizontalSize * 3);
            URcb4 := LRcb4 + (state.sh.horizontalSize * 3);
            URcr4 := LRcr4 + (state.sh.horizontalSize * 3);
            
            IF (NOT VAL(rightHalfFor, BOOLEAN)) AND (NOT VAL(downHalfFor, BOOLEAN)) THEN             
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) THEN
                CASE block OF
                | 0..3 => state.current[y]    := state.past[y1];
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *)
                          state.current[cb]   := state.past[cb1];
                          state.current[LRcb] := state.past[LRcb1];
                          state.current[ULcb] := state.past[ULcb1];
                          state.current[URcb] := state.past[URcb1];
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *)
                          state.current[cr]   := state.past[cr1];
                          state.current[LRcr] := state.past[LRcr1];
                          state.current[ULcr] := state.past[ULcr1];
                          state.current[URcr] := state.past[URcr1];                     
                END;
              END;
            END;

            IF (NOT VAL(rightHalfFor, BOOLEAN)) AND VAL(downHalfFor, BOOLEAN) THEN
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y2 >= 0) AND (URcr2 <= 230399) THEN
                CASE block OF
                | 0..3 => state.current[y]    := (state.past[y1] + state.past[y2]) DIV 2;
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *)
                          state.current[cb]   := (state.past[cb1] + state.past[cb2]) DIV 2;
                          state.current[LRcb] := (state.past[LRcb1] + state.past[LRcb2]) DIV 2;
                          state.current[ULcb] := (state.past[ULcb1] + state.past[ULcb2]) DIV 2;
                          state.current[URcb] := (state.past[URcb1] + state.past[URcb2]) DIV 2;
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *)
                          state.current[cr]   := (state.past[cr1] + state.past[cr2]) DIV 2;
                          state.current[LRcr] := (state.past[LRcr1] + state.past[LRcr2]) DIV 2;
                          state.current[ULcr] := (state.past[ULcr1] + state.past[ULcr2]) DIV 2;
                          state.current[URcr] := (state.past[URcr1] + state.past[URcr2]) DIV 2;
                END;
              END; 
            END;

            IF VAL(rightHalfFor, BOOLEAN) AND (NOT VAL(downHalfFor, BOOLEAN)) THEN   
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y3 >= 0) AND (URcr3 <= 230399) THEN
                CASE block OF
                | 0..3 => state.current[y]    := (state.past[y1] + state.past[y3]) DIV 2;
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *) 
                          state.current[cb]   := (state.past[cb1] + state.past[cb3]) DIV 2;
                          state.current[LRcb] := (state.past[LRcb1] + state.past[LRcb3]) DIV 2;
                          state.current[ULcb] := (state.past[ULcb1] + state.past[ULcb3]) DIV 2;
                          state.current[URcb] := (state.past[URcb1] + state.past[URcb3]) DIV 2;
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *) 
                          state.current[cr]   := (state.past[cr1] + state.past[cr3]) DIV 2;
                          state.current[LRcr] := (state.past[LRcr1] + state.past[LRcr3]) DIV 2;
                          state.current[ULcr] := (state.past[ULcr1] + state.past[ULcr3]) DIV 2;
                          state.current[URcr] := (state.past[URcr1] + state.past[URcr3]) DIV 2;
                END;        
              END;
            END;

            IF VAL(rightHalfFor, BOOLEAN) AND VAL(downHalfFor, BOOLEAN) THEN
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y2 >= 0) AND (URcr2 <= 230399) AND (y3 >= 0) AND (URcr3 <= 230399) AND
                 (y4 >= 0) AND (URcr4 <= 230399) THEN
                CASE block OF
                | 0..3 => state.current[y]    := (state.past[y1]    + state.past[y2] + 
                                                  state.past[y3]    + state.past[y4]) DIV 4;
                | 4    => state.current[cb]   := (state.past[cb1]   + state.past[cb2] + 
                                                  state.past[cb3]   + state.past[cb4]) DIV 4;
                          state.current[LRcb] := (state.past[LRcb1] + state.past[LRcb2] + 
                                                  state.past[LRcb3] + state.past[LRcb4]) DIV 4;
                          state.current[ULcb] := (state.past[ULcb1] + state.past[ULcb2] + 
                                                  state.past[ULcb3] + state.past[ULcb4]) DIV 4; 
                          state.current[URcb] := (state.past[URcb1] + state.past[URcb2] + 
                                                  state.past[URcb3] + state.past[URcb4]) DIV 4; 
                | 5    => state.current[cr]   := (state.past[cr1]   + state.past[cr2] +
                                                  state.past[cr3]   + state.past[cr4]) DIV 4;
                          state.current[LRcr] := (state.past[LRcr1] + state.past[LRcr2] +
                                                  state.past[LRcr3] + state.past[LRcr4]) DIV 4;
                          state.current[ULcr] := (state.past[ULcr1] + state.past[ULcr2] + 
                                                  state.past[ULcr3] + state.past[ULcr4]) DIV 4; 
                          state.current[URcr] := (state.past[URcr1] + state.past[URcr2] + 
                                                  state.past[URcr3] + state.past[URcr4]) DIV 4; 
                END;              
              END;
            END;
          END;
        END;
      END;
    END;

    IF (*state.macroblockType.macroblockMotionBackward*) NOT (state.macroblockType.macroblockIntra) AND
       (state.p.pictureCodingType = MpegData.PictureCodingType.B) THEN
      FOR i := 0 TO 7 DO
        FOR j := 0 TO 7 DO
          (* Since the picture size may not be a perfect multiple of 16, there may be parts of some 
             macroblocks that are outside of the picture along the bottom and right sides of the picture.
             Filter these out here. *) 
          IF ((block = 0) AND ((lower+8+i) >= 0)   AND ((left+j)       < state.sh.horizontalSize)) OR
             ((block = 1) AND ((lower+8+i) >= 0)   AND ((left+8+j)     < state.sh.horizontalSize)) OR
             ((block = 2) AND ((lower+i)  >= 0)    AND ((left+j)       < state.sh.horizontalSize)) OR
             ((block = 3) AND ((lower+i)  >= 0)    AND ((left+8+j)     < state.sh.horizontalSize)) OR
             ((block = 4) AND ((lower+(2*i)) >= 0) AND ((left+(2*j)+1) < state.sh.horizontalSize)) OR
             ((block = 5) AND ((lower+(2*i)) >= 0) AND ((left+(2*j)+1) < state.sh.horizontalSize)) THEN
            CASE block OF
            | 0 => index := (((lower+8+i)   * state.sh.horizontalSize) + left+j)       * 3; 
            | 1 => index := (((lower+8+i)   * state.sh.horizontalSize) + left+8+j)     * 3;
            | 2 => index := (((lower+i)     * state.sh.horizontalSize) + left+j)       * 3; 
            | 3 => index := (((lower+i)     * state.sh.horizontalSize) + left+8+j)     * 3;
            | 4 => index := (((lower+(2*i)) * state.sh.horizontalSize) + (left+(2*j))) * 3;
            | 5 => index := (((lower+(2*i)) * state.sh.horizontalSize) + (left+(2*j))) * 3;
            END;
            
            (* Set up the indexes into the reference pictures.  *)
            index1 := index + ((-(downBack*state.sh.horizontalSize)+rightBack)*3);
            index2 := index + ((-((downBack+1)*state.sh.horizontalSize)+rightBack)*3);
            index3 := index + ((-(downBack*state.sh.horizontalSize)+(rightBack+1))*3);
            index4 := index + ((-((downBack+1)*state.sh.horizontalSize)+(rightBack+1))*3);

            (* Calculate all the possible index + offset combinations. *)
            y     := index;
            cb    := index + 1;
            cr    := index + 2;
            y1    := index1;
            cb1   := index1 + 1;
            cr1   := index1 + 2;
            y2    := index2;
            cb2   := index2 + 1;
            cr2   := index2 + 2;
            y3    := index3;
            cb3   := index3 + 1;
            cr3   := index3 + 2; 
            y4    := index4;
            cb4   := index4 + 1;
            cr4   := index4 + 2;
            LRcb  := cb  + 3;
            LRcr  := cr  + 3;
            LRcb1 := cb1 + 3;
            LRcr1 := cr1 + 3;
            LRcb2 := cb2 + 3;
            LRcr2 := cr2 + 3;
            LRcb3 := cb3 + 3;
            LRcr3 := cr3 + 3;
            LRcb4 := cb4 + 3;
            LRcr4 := cr4 + 3;
            ULcb  := cb    + (state.sh.horizontalSize * 3);
            ULcr  := cr    + (state.sh.horizontalSize * 3);
            ULcb1 := cb1   + (state.sh.horizontalSize * 3);
            ULcr1 := cr1   + (state.sh.horizontalSize * 3);
            ULcb2 := cb2   + (state.sh.horizontalSize * 3);
            ULcr2 := cr2   + (state.sh.horizontalSize * 3);
            ULcb3 := cb3   + (state.sh.horizontalSize * 3);
            ULcr3 := cr3   + (state.sh.horizontalSize * 3);
            ULcb4 := cb4   + (state.sh.horizontalSize * 3);
            ULcr4 := cr4   + (state.sh.horizontalSize * 3);
            URcb  := LRcb  + (state.sh.horizontalSize * 3);
            URcr  := LRcr  + (state.sh.horizontalSize * 3);
            URcb1 := LRcb1 + (state.sh.horizontalSize * 3);
            URcr1 := LRcr1 + (state.sh.horizontalSize * 3);
            URcb2 := LRcb2 + (state.sh.horizontalSize * 3);
            URcr2 := LRcr2 + (state.sh.horizontalSize * 3);
            URcb3 := LRcb3 + (state.sh.horizontalSize * 3);
            URcr3 := LRcr3 + (state.sh.horizontalSize * 3);
            URcb4 := LRcb4 + (state.sh.horizontalSize * 3);
            URcr4 := LRcr4 + (state.sh.horizontalSize * 3);
            
            IF (NOT VAL(rightHalfBack, BOOLEAN)) AND (NOT VAL(downHalfBack, BOOLEAN)) THEN             
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) THEN
                CASE block OF
                | 0..3 => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[y]    := (state.current[y] + state.future[y1]) DIV 2;
                          ELSE
                            state.current[y]    := state.future[y1];
                          END;
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *)
                          IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cb]   := (state.current[cb]   + state.future[cb1])   DIV 2;
                            state.current[LRcb] := (state.current[LRcb] + state.future[LRcb1]) DIV 2;
                            state.current[ULcb] := (state.current[ULcb] + state.future[ULcb1]) DIV 2;
                            state.current[URcb] := (state.current[URcb] + state.future[URcb1]) DIV 2;
                          ELSE
                            state.current[cb]   := state.future[cb1];
                            state.current[LRcb] := state.future[LRcb1];
                            state.current[ULcb] := state.future[ULcb1];
                            state.current[URcb] := state.future[URcb1];
                          END;
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *)
                          IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cr]   := (state.current[cr]   + state.future[cr1])   DIV 2;
                            state.current[LRcr] := (state.current[LRcr] + state.future[LRcr1]) DIV 2;
                            state.current[ULcr] := (state.current[ULcr] + state.future[ULcr1]) DIV 2;
                            state.current[URcr] := (state.current[URcr] + state.future[URcr1]) DIV 2;
                          ELSE
                            state.current[cr]   := state.future[cr1];
                            state.current[LRcr] := state.future[LRcr1];
                            state.current[ULcr] := state.future[ULcr1];
                            state.current[URcr] := state.future[URcr1];
                          END;                     
                END;
              END;
            END;

            IF (NOT VAL(rightHalfBack, BOOLEAN)) AND VAL(downHalfBack, BOOLEAN) THEN
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y2 >= 0) AND (URcr2 <= 230399) THEN  
                CASE block OF
                | 0..3 => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[y]    := (state.current[y] + ((state.future[y1] + state.future[y2]) DIV 2)) DIV 2;
                          ELSE
                            state.current[y]    := (state.future[y1] + state.future[y2]) DIV 2;
                          END;
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *)
                          IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cb]   := (state.current[cb]   + 
                                                   ((state.future[cb1]   + state.future[cb2])   DIV 2)) DIV 2;
                            state.current[LRcb] := (state.current[LRcb] + 
                                                   ((state.future[LRcb1] + state.future[LRcb2]) DIV 2)) DIV 2;
                            state.current[ULcb] := (state.current[ULcb] + 
                                                   ((state.future[ULcb1] + state.future[ULcb2]) DIV 2)) DIV 2;
                            state.current[URcb] := (state.current[URcb] + 
                                                   ((state.future[URcb1] + state.future[URcb2]) DIV 2)) DIV 2;
                          ELSE
                            state.current[cb]   := (state.future[cb1]   + state.future[cb2])   DIV 2;
                            state.current[LRcb] := (state.future[LRcb1] + state.future[LRcb2]) DIV 2;
                            state.current[ULcb] := (state.future[ULcb1] + state.future[ULcb2]) DIV 2;
                            state.current[URcb] := (state.future[URcb1] + state.future[URcb2]) DIV 2;
                          END;
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *)
                          IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cr]   := (state.current[cr]   + 
                                                   ((state.future[cr1]   + state.future[cr2])   DIV 2)) DIV 2;
                            state.current[LRcr] := (state.current[LRcr] + 
                                                   ((state.future[LRcr1] + state.future[LRcr2]) DIV 2)) DIV 2;
                            state.current[ULcr] := (state.current[ULcr] + 
                                                   ((state.future[ULcr1] + state.future[ULcr2]) DIV 2)) DIV 2;
                            state.current[URcr] := (state.current[URcr] + 
                                                   ((state.future[URcr1] + state.future[URcr2]) DIV 2)) DIV 2; 
                          ELSE
                            state.current[cr]   := (state.future[cr1]   + state.future[cr2])   DIV 2;
                            state.current[LRcr] := (state.future[LRcr1] + state.future[LRcr2]) DIV 2;
                            state.current[ULcr] := (state.future[ULcr1] + state.future[ULcr2]) DIV 2;
                            state.current[URcr] := (state.future[URcr1] + state.future[URcr2]) DIV 2;
                          END;
                END;
              END;
            END;            

            IF VAL(rightHalfBack, BOOLEAN) AND (NOT VAL(downHalfBack, BOOLEAN)) THEN   
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y3 >= 0) AND (URcr3 <= 230399) THEN  
                CASE block OF
                | 0..3 => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[y]  := (state.current[y] + ((state.future[y1] + state.future[y3]) DIV 2)) DIV 2;
                          ELSE 
                            state.current[y]  := (state.future[y1] + state.future[y3]) DIV 2;
                          END;   
                | 4    => (* Need to draw 4 pixels for each pixel in the Cb block. *) 
                          IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cb]   := (state.current[cb] + 
                                                   ((state.future[cb1] + state.future[cb3]) DIV 2)) DIV 2;
                            state.current[LRcb] := (state.current[LRcb] + 
                                                   ((state.future[LRcb1] + state.future[LRcb3]) DIV 2)) DIV 2;
                            state.current[ULcb] := (state.current[ULcb] + 
                                                   ((state.future[ULcb1] + state.future[ULcb3]) DIV 2)) DIV 2;
                            state.current[URcb] := (state.current[URcb] +
                                                   ((state.future[URcb1] + state.future[URcb3]) DIV 2)) DIV 2;
                          ELSE
                            state.current[cb]   := (state.future[cb1]   + state.future[cb3]) DIV 2;
                            state.current[LRcb] := (state.future[LRcb1] + state.future[LRcb3]) DIV 2;
                            state.current[ULcb] := (state.future[ULcb1] + state.future[ULcb3]) DIV 2;
                            state.current[URcb] := (state.future[URcb1] + state.future[URcb3]) DIV 2;
                          END;
                | 5    => (* Need to draw 4 pixels for each pixel in the Cr block. *) 
                          IF state.macroblockType.macroblockMotionForward THEN  
                            state.current[cr]   := (state.current[cr] + 
                                                   ((state.future[cr1] + state.future[cr3]) DIV 2)) DIV 2;
                            state.current[LRcr] := (state.current[LRcr] + 
                                                   ((state.future[LRcr1] + state.future[LRcr3]) DIV 2)) DIV 2;
                            state.current[ULcr] := (state.current[ULcr] + 
                                                   ((state.future[ULcr1] + state.future[ULcr3]) DIV 2)) DIV 2;
                            state.current[URcr] := (state.current[URcr] + 
                                                   ((state.future[URcr1] + state.future[URcr3]) DIV 2)) DIV 2;
                          ELSE
                            state.current[cr]   := (state.future[cr1]   + state.future[cr3]) DIV 2;
                            state.current[LRcr] := (state.future[LRcr1] + state.future[LRcr3]) DIV 2;
                            state.current[ULcr] := (state.future[ULcr1] + state.future[ULcr3]) DIV 2;
                            state.current[URcr] := (state.future[URcr1] + state.future[URcr3]) DIV 2;
                          END;
                END;
              END;        
            END;

            IF VAL(rightHalfBack, BOOLEAN) AND VAL(downHalfBack, BOOLEAN) THEN
              IF (y >= 0) AND (URcr <= 230399) AND (y1 >= 0) AND (URcr1 <= 230399) AND
                 (y2 >= 0) AND (URcr2 <= 230399) AND (y3 >= 0) AND (URcr3 <= 230399) AND
                 (y4 >= 0) AND (URcr4 <= 230399) THEN  
                CASE block OF
                | 0..3 => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[y]    := (state.current[y] + ((state.future[y1]    + state.future[y2] + 
                                                                         state.future[y3]    + state.future[y4]) DIV 4)) DIV 2;
                          ELSE
                            state.current[y]    := (state.future[y1]    + state.future[y2] + 
                                                    state.future[y3]    + state.future[y4]) DIV 4;
                          END;
                | 4    => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cb]   := (state.current[cb] + ((state.future[cb1]   + state.future[cb2] + 
                                                                        state.future[cb3]   + state.future[cb4]) DIV 4)) DIV 2;
                            state.current[LRcb] := (state.current[LRcb] + ((state.future[LRcb1] + state.future[LRcb2] + 
                                                                      state.future[LRcb3] + state.future[LRcb4]) DIV 4)) DIV 2;
                            state.current[ULcb] := (state.current[ULcb] + ((state.future[ULcb1] + state.future[ULcb2] + 
                                                                     state.future[ULcb3] + state.future[ULcb4]) DIV 4)) DIV 2; 
                            state.current[URcb] := (state.current[URcb] + ((state.future[URcb1] + state.future[URcb2] + 
                                                                     state.future[URcb3] + state.future[URcb4]) DIV 4)) DIV 2; 
                          ELSE
                            state.current[cb]   := (state.future[cb1]   + state.future[cb2] + 
                                                    state.future[cb3]   + state.future[cb4]) DIV 4;
                            state.current[LRcb] := (state.future[LRcb1] + state.future[LRcb2] + 
                                                    state.future[LRcb3] + state.future[LRcb4]) DIV 4;
                            state.current[ULcb] := (state.future[ULcb1] + state.future[ULcb2] + 
                                                    state.future[ULcb3] + state.future[ULcb4]) DIV 4; 
                            state.current[URcb] := (state.future[URcb1] + state.future[URcb2] + 
                                                    state.future[URcb3] + state.future[URcb4]) DIV 4; 
                          END;
                | 5    => IF state.macroblockType.macroblockMotionForward THEN
                            state.current[cr]   := (state.current[cr] + ((state.future[cr1]   + state.future[cr2] +
                                                                        state.future[cr3]   + state.future[cr4]) DIV 4)) DIV 2;
                            state.current[LRcr] := (state.current[LRcr] + ((state.future[LRcr1] + state.future[LRcr2] +
                                                                      state.future[LRcr3] + state.future[LRcr4]) DIV 4)) DIV 2;
                            state.current[ULcr] := (state.current[ULcr] + ((state.future[ULcr1] + state.future[ULcr2] + 
                                                                     state.future[ULcr3] + state.future[ULcr4]) DIV 4)) DIV 2; 
                            state.current[URcr] := (state.current[URcr] + ((state.future[URcr1] + state.future[URcr2] + 
                                                                     state.future[URcr3] + state.future[URcr4]) DIV 4)) DIV 2; 
                          ELSE
                            state.current[cr]   := (state.future[cr1]   + state.future[cr2] +
                                                    state.future[cr3]   + state.future[cr4]) DIV 4;
                            state.current[LRcr] := (state.future[LRcr1] + state.future[LRcr2] +
                                                    state.future[LRcr3] + state.future[LRcr4]) DIV 4;
                            state.current[ULcr] := (state.future[ULcr1] + state.future[ULcr2] + 
                                                    state.future[ULcr3] + state.future[ULcr4]) DIV 4; 
                            state.current[URcr] := (state.future[URcr1] + state.future[URcr2] + 
                                                    state.future[URcr3] + state.future[URcr4]) DIV 4; 
                          END;
                END;              
              END;
            END;
          END;
        END;
      END;
    END;
  END;  
END AddPredictedMacroblock;

PROCEDURE SaveYUV(VAR state: MpegData.MpegState; time: INTEGER) =

VAR
  left:  INTEGER;
  lower: INTEGER;
  index: INTEGER;
  temp1: INTEGER;
  temp2: INTEGER;
  temp3: INTEGER;
BEGIN
  (* Calculate the location of the lower left corner of this macroblock in the current frame.  The point of
     reference is the lower left corner of the frame, so if the lower left corner of the macroblock is 10
     pixels above the lower left corner of the frame, then lower is set to 10. *)
  lower := state.sh.verticalSize - (((state.macroblockAddress DIV state.mbWidth) * 16) + 16);
  left  := (state.macroblockAddress MOD state.mbWidth) * 16;

  FOR i := 0 TO 15 DO
    FOR j := 0 TO 15 DO
      IF ((lower + (16 - i)) > 0) AND ((left + j) < state.sh.horizontalSize) THEN 
        index := ((lower)*state.sh.horizontalSize + left + (15-i)*state.sh.horizontalSize + j)*3;	
        
        IF i < 8 AND j < 8 THEN
          temp1 := state.dctReconY1[i][j];
        END;
        IF i < 8 AND j > 7 THEN
          temp1 := state.dctReconY2[i][j-8];
        END;
        IF i > 7 AND j < 8 THEN
          temp1 := state.dctReconY3[i-8][j];
        END;
        IF i > 7 AND j > 7 THEN
          temp1 := state.dctReconY4[i-8][j-8];
        END;
        temp2 := state.dctReconCb[i DIV 2][j DIV 2];
        temp3 := state.dctReconCr[i DIV 2][j DIV 2];

        IF time = 0 THEN
	  state.current[index]   := temp1;
	  state.current[index+1] := temp2;
	  state.current[index+2] := temp3;
        END;
        IF time = 1 THEN
          state.future[index]   := temp1;
          state.future[index+1] := temp2;
          state.future[index+2] := temp3;
        END;
      END;
    END;
  END;
END SaveYUV;

(* This procedure is used for predictive-coded macroblocks. It is the same as
   SaveYUV except that it adds to the state.current or state.future values, rather
   than just assigning to them. *)
PROCEDURE AddYUV(VAR state: MpegData.MpegState; time: INTEGER) =

VAR
  left:  INTEGER;
  lower: INTEGER;
  index: INTEGER;
  temp1: INTEGER;
  temp2: INTEGER;
  temp3: INTEGER;
BEGIN
  (* Calculate the location of the lower left corner of this macroblock in the current frame.  The point of
     reference is the lower left corner of the frame, so if the lower left corner of the macroblock is 10
     pixels above the lower left corner of the frame, then lower is set to 10. *)
  lower := state.sh.verticalSize - (((state.macroblockAddress DIV state.mbWidth) * 16) + 16);
  left  := (state.macroblockAddress MOD state.mbWidth) * 16;

  FOR i := 0 TO 15 DO
    FOR j := 0 TO 15 DO
      IF ((lower + (16 - i)) > 0) AND ((left + j) < state.sh.horizontalSize) THEN 
        index := ((lower)*state.sh.horizontalSize + left + (15-i)*state.sh.horizontalSize + j)*3;	
        
        IF i < 8 AND j < 8 THEN
          temp1 := state.dctReconY1[i][j];
        END;
        IF i < 8 AND j > 7 THEN
          temp1 := state.dctReconY2[i][j-8];
        END;
        IF i > 7 AND j < 8 THEN
          temp1 := state.dctReconY3[i-8][j];
        END;
        IF i > 7 AND j > 7 THEN
          temp1 := state.dctReconY4[i-8][j-8];
        END;
        temp2 := state.dctReconCb[i DIV 2][j DIV 2];
        temp3 := state.dctReconCr[i DIV 2][j DIV 2];

        IF time = 0 THEN
	  temp1   := state.current[index]   + temp1;
	  temp2   := state.current[index+1] + temp2;
	  temp3   := state.current[index+2] + temp3;
          (* The state.current and dctRecon* values should each be in the range [0, 255], 
             but since we are adding them together we should check again. *)
          IF temp1 > 255 THEN
            temp1 := 255;
          END;
          IF temp2 > 255 THEN
            temp2 := 255;
          END;
          IF temp3 > 255 THEN
            temp3 := 255;
          END;
          (* Some of these values may not be legit, i.e. if the block wasn't passed in for this
             macroblock. Just don't save those values to state.current. *)
          IF ((i < 8) AND (j < 8) AND state.patternCode[0]) OR
             ((i < 8) AND (j > 7) AND state.patternCode[1]) OR
             ((i > 7) AND (j < 8) AND state.patternCode[2]) OR
             ((i > 7) AND (j > 7) AND state.patternCode[3]) THEN
            state.current[index] := temp1;
          END;
          IF state.patternCode[4] THEN
            state.current[index+1] := temp2;
          END;
          IF state.patternCode[5] THEN
            state.current[index+2] := temp3;
          END;
        END;
        IF time = 1 THEN
          temp1   := state.future[index]   + temp1;
	  temp2   := state.future[index+1] + temp2;
	  temp3   := state.future[index+2] + temp3;
          (* The state.current and dctRecon* values should each be in the range [0, 255], 
             but since we are adding them together we should check again. *)
          IF temp1 > 255 THEN
            temp1 := 255;
          END;
          IF temp2 > 255 THEN
            temp2 := 255;
          END;
          IF temp3 > 255 THEN
            temp3 := 255;
          END;
          IF ((i < 8) AND (j < 8) AND state.patternCode[0]) OR
             ((i < 8) AND (j > 7) AND state.patternCode[1]) OR
             ((i > 7) AND (j < 8) AND state.patternCode[2]) OR
             ((i > 7) AND (j > 7) AND state.patternCode[3]) THEN
            state.future[index] := temp1;
          END;
          IF state.patternCode[4] THEN
            state.future[index+1] := temp2;
          END;
          IF state.patternCode[5] THEN
            state.future[index+2] := temp3;
          END;
        END;
      END;
    END;
  END;
END AddYUV;

PROCEDURE Reverse2D(VAR state: MpegData.MpegState; blockNum: INTEGER) =
VAR
  tmp0, tmp1, tmp2, tmp3: INTEGER;
  tmp10, tmp11, tmp12, tmp13: INTEGER;
  z1, z2, z3, z4, z5: INTEGER;
  block: ARRAY [0..7], [0..7] OF INTEGER;
BEGIN
  CASE blockNum OF
  |  0 => block := state.dctReconY1;
  |  1 => block := state.dctReconY2;
  |  2 => block := state.dctReconY3;
  |  3 => block := state.dctReconY4;
  |  4 => block := state.dctReconCb;
  |  5 => block := state.dctReconCr;
  ELSE
  END;

  FOR row := 0 TO 7 DO
      (* reverse the even part of the forward DCT *)
  (* the rotator is sqrt(2)*c(-6) *)
  z1   := (block[2,row] + block[6,row]) * 
          ((541196100 * One) DIV 1000000000);
  tmp2 := z1 - block[6,row] * 
          ((1847759065 * One) DIV 1000000000);
  tmp3 := z1 + block[2,row] * 
          ((765366865 * One) DIV 1000000000);
  tmp0 := Word.LeftShift(block[0,row] + block[4,row], ScaleBits);
  tmp1 := Word.LeftShift(block[0,row] - block[4,row], ScaleBits);
  tmp10 := tmp0 + tmp3;
  tmp13 := tmp0 - tmp3;
  tmp11 := tmp1 + tmp2;
  tmp12 := tmp1 - tmp2;

  (* odd part, the matrix is unitary and hence *)
  (* its transpose is its inverse *)
  z1 := block[7,row] + block[1,row];
  z2 := block[5,row] + block[3,row];
  z3 := block[7,row] + block[3,row];
  z4 := block[5,row] + block[1,row];
  z5 := (z3 + z4) * 
        ((1175875602 * One) DIV 1000000000);

  tmp0 := block[7,row] * 
          ((298631336 * One) DIV 1000000000);
  tmp1 := block[5,row] * 
          ((2053119869 * One) DIV 1000000000);
  tmp2 := block[3,row] * 
          ((3072711026 * One) DIV 1000000000);
  tmp3 := block[1,row] * 
          ((1501321110 * One) DIV 1000000000);
  z1 := z1 * 
        ((899976223 * One) DIV 1000000000);
  z2 := z2 * 
        ((2562915447 * One) DIV 1000000000);
  z3 := z3 * 
        ((1961570560 * One) DIV 1000000000);
  z4 := z4 * 
        ((390180644 * One) DIV 1000000000);
  DEC(z3, z5);
  DEC(z4, z5);
  DEC(tmp0, z1 + z3);
  DEC(tmp1, z2 + z4);
  DEC(tmp2, z2 + z3);
  DEC(tmp3, z1 + z4);

  (* output stage *)
  block[0,row] := (tmp10 + tmp3) DIV (One DIV 2);
  block[7,row] := (tmp10 - tmp3) DIV (One DIV 2);
  block[1,row] := (tmp11 + tmp2) DIV (One DIV 2);
  block[6,row] := (tmp11 - tmp2) DIV (One DIV 2);
  block[2,row] := (tmp12 + tmp1) DIV (One DIV 2);
  block[5,row] := (tmp12 - tmp1) DIV (One DIV 2);
  block[3,row] := (tmp13 + tmp0) DIV (One DIV 2);
  block[4,row] := (tmp13 - tmp0) DIV (One DIV 2);

  END;
  FOR col := 0 TO 7 DO
      (* reverse the even part of the forward DCT *)
  (* the rotator is sqrt(2)*c(-6) *)
  z1   := (block[col,2] + block[col,6]) * 
          ((541196100 * One) DIV 1000000000);
  tmp2 := z1 - block[col,6] * 
          ((1847759065 * One) DIV 1000000000);
  tmp3 := z1 + block[col,2] * 
          ((765366865 * One) DIV 1000000000);
  tmp0 := Word.LeftShift(block[col,0] + block[col,4], ScaleBits);
  tmp1 := Word.LeftShift(block[col,0] - block[col,4], ScaleBits);
  tmp10 := tmp0 + tmp3;
  tmp13 := tmp0 - tmp3;
  tmp11 := tmp1 + tmp2;
  tmp12 := tmp1 - tmp2;

  (* odd part, the matrix is unitary and hence *)
  (* its transpose is its inverse *)
  z1 := block[col,7] + block[col,1];
  z2 := block[col,5] + block[col,3];
  z3 := block[col,7] + block[col,3];
  z4 := block[col,5] + block[col,1];
  z5 := (z3 + z4) * 
        ((1175875602 * One) DIV 1000000000);

  tmp0 := block[col,7] * 
          ((298631336 * One) DIV 1000000000);
  tmp1 := block[col,5] * 
          ((2053119869 * One) DIV 1000000000); 
  tmp2 := block[col,3] * 
          ((3072711026 * One) DIV 1000000000);
  tmp3 := block[col,1] * 
          ((1501321110 * One) DIV 1000000000);
  z1 := z1 * 
        ((899976223 * One) DIV 1000000000);
  z2 := z2 * 
        ((2562915447 * One) DIV 1000000000);
  z3 := z3 * 
        ((1961570560 * One) DIV 1000000000);
  z4 := z4 * 
        ((390180644 * One) DIV 1000000000);
  DEC(z3, z5);
  DEC(z4, z5);
  DEC(tmp0, z1 + z3);
  DEC(tmp1, z2 + z4);
  DEC(tmp2, z2 + z3);
  DEC(tmp3, z1 + z4);

  (* output stage *)
  block[col,0] := (tmp10 + tmp3) DIV (One * 16);
  block[col,7] := (tmp10 - tmp3) DIV (One * 16);
  block[col,1] := (tmp11 + tmp2) DIV (One * 16);
  block[col,6] := (tmp11 - tmp2) DIV (One * 16);
  block[col,2] := (tmp12 + tmp1) DIV (One * 16);
  block[col,5] := (tmp12 - tmp1) DIV (One * 16);
  block[col,3] := (tmp13 + tmp0) DIV (One * 16);
  block[col,4] := (tmp13 - tmp0) DIV (One * 16);

  END;

(* Now we have the inverse transformed pixel values supposedly in the range [-256, 255]. Limit them to be
   in the range [0, 255]. *)
  FOR i := 0 TO 7 DO
    FOR j := 0 TO 7 DO
      IF block[i][j] < 0 THEN
        block[i][j] := 0;
      END;
      IF block[i][j] > 255 THEN
        block[i][j] := 255;
      END;
    END;
  END;

  CASE blockNum OF
  |  0 => state.dctReconY1 := block;
  |  1 => state.dctReconY2 := block;
  |  2 => state.dctReconY3 := block;
  |  3 => state.dctReconY4 := block;
  |  4 => state.dctReconCb := block;
  |  5 => state.dctReconCr := block;
  ELSE
  END;
END Reverse2D;

PROCEDURE YUVtoRGB(VAR state: MpegData.MpegState; time: INTEGER) =
VAR
  temp1: INTEGER;
  temp2: INTEGER;
  temp3: INTEGER;
  index: INTEGER;

BEGIN
  FOR i := 0 TO state.sh.verticalSize-1 DO
    FOR j := 0 TO state.sh.horizontalSize-1 DO   
      index := ((i * state.sh.horizontalSize) + j) * 3;

      IF time = 0 THEN
        temp1 := (1164*state.current[index]
                  + 1596*(state.current[index+2] - 128)) DIV 1000;
        temp2 := (1164*(state.current[index] - 16)
                  - 813*(state.current[index+2] - 128)
                  - 392*(state.current[index+1] - 128)) DIV 1000;
        temp3 := (1164*(state.current[index] - 16)
                  + 2017*(state.current[index+1] - 128)) DIV 1000;
      ELSE
        temp1 := (1164*state.future[index]
                  + 1596*(state.future[index+2] - 128)) DIV 1000;
        temp2 := (1164*(state.future[index] - 16) 
                  - 813*(state.future[index+2] - 128)
                  - 392*(state.future[index+1] - 128)) DIV 1000;
        temp3 := (1164*(state.future[index] - 16) 
                     + 2017*(state.future[index+1] - 128)) DIV 1000;
      END;
   
      (* Clamp the RGB values to the range [0, 255]. *)
      IF temp1 < 0 THEN
        temp1 := 0;
      END;
      IF temp1 > 255 THEN
        temp1 := 255;
      END;
      IF temp2 < 0 THEN
        temp2 := 0;
      END;
      IF temp2 > 255 THEN
        temp2 := 255;
      END;
      IF temp3 < 0 THEN
        temp3 := 0;
      END;
      IF temp3 > 255 THEN
        temp3 := 255;
      END;

      state.current[index]   := temp1;
      state.current[index+1] := temp2;
      state.current[index+2] := temp3;
    END;
  END;
END YUVtoRGB;

BEGIN
END PostProcessing.



