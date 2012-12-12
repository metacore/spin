(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE SliceLayer;

IMPORT GeneralTools, BitIO, MpegData, MacroblockLayer, IO, PictureLayer;

(* Returns TRUE if the next 32 bits in the file parameter are not a valid slice start code; returns FALSE otherwise. *)
PROCEDURE ReadSlice (file: BitIO.T; VAR state: MpegData.MpegState): BOOLEAN 
                    RAISES {BitIO.EOF, GeneralTools.Bad} =
VAR
  startCode: INTEGER;
BEGIN
  TRY
    (* Slice layer decoded here *)
    startCode := GeneralTools.NextStartCode(file);   
  
    IF startCode < SliceLowCode OR startCode > SliceHighCode THEN 
      RETURN TRUE;
    END;    

    (* GeneralTools.NextStartCode only returns the start code; it doesn't skip the bits, so
       do it here. *)  
    BitIO.SkipBits(file, 32);
    (* Calculate the previous macroblock address based on the slice start code. This is a sanity 
       check in case there was an error in the last slice. Do resetting of other variables that
       need this at the start of a slice. *)
    state.macroblockAddress  := ((startCode - 256 - 1) * state.mbWidth) - 1;
    state.pastIntraAddress   := -2;
    state.dctDCyPast         := 1024;
    state.dctDCcbPast        := 1024;
    state.dctDCcrPast        := 1024;
    state.quantizerScale     := BitIO.GetBits(file, 5);
    state.reconRightForPrev  := 0;
    state.reconDownForPrev   := 0;
    state.reconRightBackPrev := 0;
    state.reconDownBackPrev  := 0;
  
    IF state.quantizerScale = 0 THEN
      RAISE GeneralTools.Bad("Bad quantizer scale value (0)");
    END;

    GeneralTools.SkipExtraInformation(file);

    (* Decode all of the macroblocks in this slice. *)
    REPEAT
      MacroblockLayer.ReadMacroblock(file, state);
    UNTIL BitIO.ShowBits(file, 23) = 0; 

    RETURN FALSE;
  EXCEPT
  |  GeneralTools.Bad(problem) => IO.Put(problem & " IN SliceLayer.ReadSlice\n");
                                  LOOP
                                    startCode := GeneralTools.NextStartCode(file);
                                    IO.Put("Error Code! start code = "); IO.PutInt(startCode);
                                    IO.Put("\n");
                                    IF ((startCode > SliceLowCode) AND (startCode < SliceHighCode)) THEN
                                      RETURN FALSE;
                                    END;
                                    IF (startCode = PictureLayer.PictureStartCode) THEN
                                      RETURN TRUE; 
                                    END;
                                    BitIO.SkipBits(file, 32);
                                  END;                            
  END;
END ReadSlice;

BEGIN
END SliceLayer.
