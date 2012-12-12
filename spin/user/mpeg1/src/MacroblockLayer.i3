(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE MacroblockLayer;

IMPORT BitIO, GeneralTools, MpegData;

PROCEDURE ReadMacroblock (file: BitIO.T; VAR state: MpegData.MpegState) 
                         RAISES {BitIO.EOF, GeneralTools.Bad}; 

END MacroblockLayer.
