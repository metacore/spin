(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE GeneralTools;

IMPORT BitIO;

EXCEPTION
  Bad(TEXT);

TYPE
  Byte = BITS 8 FOR [0..255];

CONST
  UserDataStartCode  = 16_1B2;
  ExtentionStartCode = 16_1B5;

PROCEDURE SkipMarkerBit(file: BitIO.T) RAISES {Bad, BitIO.EOF};
PROCEDURE StartCode(file: BitIO.T; startCode: CARDINAL) RAISES {Bad, BitIO.EOF};
PROCEDURE NextStartCode(file: BitIO.T): CARDINAL RAISES {Bad, BitIO.EOF};
PROCEDURE SkipExtraInformation(file: BitIO.T) RAISES {Bad, BitIO.EOF};
PROCEDURE ReadExtentionAndUserData(file: BitIO.T) RAISES {Bad, BitIO.EOF};

END GeneralTools.
