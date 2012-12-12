(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE MpegPlay;

IMPORT BitIO, MpegData; 

PROCEDURE MpegPlay (file: BitIO.T; VAR state: MpegData.MpegState);

END MpegPlay.