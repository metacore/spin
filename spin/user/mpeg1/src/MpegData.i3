(*
 * HISTORY
 * 25-Apr-97  Michael Berg (berg) at the University of Washington
 *      Created.
 *)      

INTERFACE MpegData;

TYPE
  Byte = BITS 8 FOR [0..255];

  (* Sequence layer data structure *)
  SequenceHeader = RECORD
    horizontalSize              : CARDINAL;
    verticalSize                : CARDINAL;
    pelAspectRatio              : [0..15];
    pictureRate                 : [0..15];
    bitRate                     : CARDINAL;
    vbvBufferSize               : CARDINAL;
    constrainedParametersFlag   : BOOLEAN;
    loadIntraQuantizerMatrix    : BOOLEAN;
    intraQuantizerMatrix        : ARRAY [0..7], [0..7] OF Byte;
    loadNonIntraQuantizerMatrix : BOOLEAN;
    nonIntraQuantizerMatrix     : ARRAY [0..7], [0..7] OF Byte;
  END;

  (* Group of Pictures layer data structures *)
  TimeCode =  RECORD
    dropFrameFlag : BOOLEAN;
    hours         : [0..23];
    minutes       : [0..59];
    seconds       : [0..59];
    pictures      : [0..59];
  END;

  GroupOfPicturesHeader = RECORD
    timeCode   : TimeCode;
    closedGOP  : BOOLEAN;
    brokenLink : BOOLEAN;
  END;

  (* Picture layer data structures *)
  PictureCodingType = {Forbidden, I, P, B, D};

  PictureHeader = RECORD
    temporalReference     : INTEGER;
    pictureCodingType     : PictureCodingType;
    vbvDelay              : CARDINAL;
    fullPelForwardVector  : BOOLEAN;
    forwardFcode          : [0..7];
    fullPelBackwardVector : BOOLEAN;
    backwardFcode         : [0..7];
  END;

  (* Macroblock layer data structure *)
  MacroblockTypeRecord = RECORD
    macroblockQuant          : BOOLEAN;
    macroblockMotionForward  : BOOLEAN;
    macroblockMotionBackward : BOOLEAN;
    macroblockPattern        : BOOLEAN;
    macroblockIntra          : BOOLEAN;
  END;

  (* MPEG data structure. Includes the entire state of the MPEG decoder. *)
  MpegState = RECORD
    sh                           : SequenceHeader;
    gopH                         : GroupOfPicturesHeader;
    p                            : PictureHeader;
    current                      : UNTRACED REF ARRAY OF Byte;
    past                         : UNTRACED REF ARRAY OF Byte;
    future                       : UNTRACED REF ARRAY OF Byte;
    lastTemporalReference        : INTEGER;
    futureTemporalReference      : INTEGER;
    macroblockAddress            : INTEGER;
    quantizerScale               : CARDINAL;
    macroblockType               : MacroblockTypeRecord;
    motionHorizontalForwardCode  : INTEGER;
    motionHorizontalForwardR     : CARDINAL;
    motionVerticalForwardCode    : INTEGER;
    motionVerticalForwardR       : CARDINAL;
    motionHorizontalBackwardCode : INTEGER;
    motionHorizontalBackwardR    : CARDINAL;
    motionVerticalBackwardCode   : INTEGER;
    motionVerticalBackwardR      : CARDINAL;
    reconRightFor                : INTEGER;
    reconDownFor                 : INTEGER;
    reconRightForPrev            : INTEGER;
    reconDownForPrev             : INTEGER;
    reconRightBack               : INTEGER;
    reconDownBack                : INTEGER;
    reconRightBackPrev           : INTEGER;
    reconDownBackPrev            : INTEGER;
    patternCode                  : ARRAY [0..5] OF BOOLEAN;
    dctDCdifferential            : ARRAY [0..5] OF INTEGER;
    dctZZ                        : ARRAY [0..63] OF INTEGER;
    dctReconY1                   : ARRAY [0..7], [0..7] OF INTEGER;
    dctReconY2                   : ARRAY [0..7], [0..7] OF INTEGER;
    dctReconY3                   : ARRAY [0..7], [0..7] OF INTEGER;
    dctReconY4                   : ARRAY [0..7], [0..7] OF INTEGER;
    dctReconCb                   : ARRAY [0..7], [0..7] OF INTEGER;
    dctReconCr                   : ARRAY [0..7], [0..7] OF INTEGER;
    scan                         : ARRAY [0..7], [0..7] OF INTEGER;
    pastIntraAddress             : INTEGER;
    dctDCyPast                   : INTEGER;
    dctDCcbPast                  : INTEGER;
    dctDCcrPast                  : INTEGER;
    mbWidth                      : INTEGER;
    mode                         : INTEGER;
    firstRun                     : INTEGER;
  END;

END MpegData.
