(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

MODULE GroupOfPicturesLayer;

IMPORT BitIO, GeneralTools, MpegData;

FROM BitIO IMPORT T, GetBits, EOF;

PROCEDURE ReadGroupOfPicturesHeader(file: T; VAR state: MpegData.MpegState) RAISES {GeneralTools.Bad, EOF} =
BEGIN
  TRY
    GeneralTools.StartCode(file, GroupStartCode);
    state.gopH.timeCode.dropFrameFlag := VAL(GetBits(file, 1), BOOLEAN);
    state.gopH.timeCode.hours := GetBits(file, 5);
    state.gopH.timeCode.minutes := GetBits(file, 6);
    GeneralTools.SkipMarkerBit(file);
    state.gopH.timeCode.seconds := GetBits(file, 6);
    state.gopH.timeCode.pictures := GetBits(file, 6);
    state.gopH.closedGOP := VAL(GetBits(file, 1), BOOLEAN);
    state.gopH.brokenLink := VAL(GetBits(file, 1), BOOLEAN);
    GeneralTools.ReadExtentionAndUserData(file);
  EXCEPT
  |  GeneralTools.Bad(reason) => RAISE GeneralTools.Bad(reason & " IN GroupOfPicturesLayer.ReadGOPheader");
  END;
END ReadGroupOfPicturesHeader;

BEGIN
END GroupOfPicturesLayer.
