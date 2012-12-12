(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE BlockLayer;

IMPORT BitIO, GeneralTools, MpegData;

PROCEDURE ReadBlock (file: BitIO.T; VAR state: MpegData.MpegState; blockNum: INTEGER): INTEGER 
                    RAISES {BitIO.EOF, GeneralTools.Bad};
END BlockLayer.
