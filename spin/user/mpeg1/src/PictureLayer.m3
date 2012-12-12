(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE PictureLayer;

IMPORT BitIO, GeneralTools, MpegData, SliceLayer, IO;

PROCEDURE ReadPicture (file: BitIO.T; VAR state: MpegData.MpegState) RAISES {GeneralTools.Bad, BitIO.EOF} =
VAR
  temp : CARDINAL;
BEGIN
  TRY
    GeneralTools.StartCode(file, PictureStartCode);
    state.p.temporalReference := BitIO.GetBits(file, 10);
    temp := BitIO.GetBits(file, 3);
    IF temp >= NUMBER(MpegData.PictureCodingType) THEN
      RAISE GeneralTools.Bad("Bad picture_coding_type");
    END;
    state.p.pictureCodingType := VAL(temp, MpegData.PictureCodingType);
    state.p.vbvDelay := BitIO.GetBits(file, 16);
    IF state.p.pictureCodingType = MpegData.PictureCodingType.P OR state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
      state.p.fullPelForwardVector := VAL(BitIO.GetBits(file, 1), BOOLEAN);
      state.p.forwardFcode := BitIO.GetBits(file, 3);
      IF state.p.forwardFcode = 0 THEN
        RAISE GeneralTools.Bad("Bad forward_f_code");
      END;
    END;
    IF state.p.pictureCodingType = MpegData.PictureCodingType.B THEN
      state.p.fullPelBackwardVector := VAL(BitIO.GetBits(file, 1), BOOLEAN);
      state.p.backwardFcode := BitIO.GetBits(file, 3);
      IF state.p.backwardFcode = 0 THEN
        RAISE GeneralTools.Bad("Bad backward_f_code");
      END;
    END;
    GeneralTools.SkipExtraInformation(file);
    GeneralTools.ReadExtentionAndUserData(file);
  
  (* Decode the MPEG Slice layer. ReadSlice returns TRUE if the next bits were not a
     valid slice start code. *)
  LOOP
    IF SliceLayer.ReadSlice(file, state) THEN
      EXIT; 
    END;
  END; (* of slice loop *)

  EXCEPT
  | GeneralTools.Bad(reason) => RAISE GeneralTools.Bad(reason & " IN PictureLayer.ReadPicture");
  END; 
END ReadPicture;

BEGIN
END PictureLayer.
