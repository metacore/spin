(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE GroupOfPicturesLayer;

IMPORT BitIO, GeneralTools, MpegData;

CONST
  GroupStartCode     = 16_1B8;

PROCEDURE ReadGroupOfPicturesHeader(file: BitIO.T; VAR state: MpegData.MpegState) RAISES {GeneralTools.Bad, BitIO.EOF};

END GroupOfPicturesLayer.
