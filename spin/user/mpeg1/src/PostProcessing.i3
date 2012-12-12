(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE PostProcessing;

IMPORT MpegData;

PROCEDURE DoDCTrecon(VAR state: MpegData.MpegState; block: INTEGER);
PROCEDURE Reverse2D(VAR state: MpegData.MpegState; blockNum: INTEGER);
PROCEDURE AddPredictedMacroblock(VAR state: MpegData.MpegState);
PROCEDURE SaveYUV(VAR state: MpegData.MpegState; time: INTEGER);
PROCEDURE AddYUV(VAR state: MpegData.MpegState; time: INTEGER);
PROCEDURE YUVtoRGB(VAR state: MpegData.MpegState; time: INTEGER);

END PostProcessing.
