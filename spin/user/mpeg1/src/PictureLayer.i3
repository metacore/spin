(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE PictureLayer;

IMPORT BitIO, GeneralTools, MpegData;

CONST
  PictureStartCode   = 16_100;

PROCEDURE ReadPicture(file: BitIO.T; VAR state: MpegData.MpegState) RAISES {GeneralTools.Bad, BitIO.EOF};

END PictureLayer.
