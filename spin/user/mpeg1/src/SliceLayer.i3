(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE SliceLayer;

IMPORT BitIO, GeneralTools, MpegData; 

CONST
  SliceLowCode       = 16_101;
  SliceHighCode      = 16_1AF;
  
PROCEDURE ReadSlice (file: BitIO.T; VAR state: MpegData.MpegState): BOOLEAN RAISES {BitIO.EOF, GeneralTools.Bad};

END SliceLayer.
