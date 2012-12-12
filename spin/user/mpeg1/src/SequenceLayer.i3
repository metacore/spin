(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE SequenceLayer;

IMPORT BitIO, GeneralTools, MpegData;
  
CONST
  SequenceHeaderCode = 16_1B3;
  SequenceEndCode    = 16_1B7;
  SequenceErrorCode  = 16_1B4;
 
PROCEDURE InitScan (VAR state: MpegData.MpegState);
PROCEDURE InitIntraQuantizerMatrix (file: BitIO.T; VAR state: MpegData.MpegState)
                                   RAISES {BitIO.EOF};
PROCEDURE InitNonIntraQuantizerMatrix (file: BitIO.T; VAR state: MpegData.MpegState)
                                      RAISES {BitIO.EOF};
PROCEDURE ReadSequenceHeader (file: BitIO.T; VAR state: MpegData.MpegState) 
                             RAISES {GeneralTools.Bad, BitIO.EOF};

END SequenceLayer.