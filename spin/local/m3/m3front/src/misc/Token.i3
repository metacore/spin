(* Copyright (C) 1992, Digital Equipment Corporation           *)
(* All rights reserved.                                        *)
(* See the file COPYRIGHT for a full description.              *)

(* File: Token.i3                                              *)
(* Last modified on Tue Dec 20 10:36:38 PST 1994 by kalsow     *)
(*      modified on Sat Mar 16 00:31:16 1991 by muller         *)

(*
 * HISTORY
 * 23-May-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added T.tFUNCTIONAL
 *
 * 03-Mar-96  Wilson Hsieh (whsieh) at the University of Washington
 *	added T.tALIGNED
 *
 * 14-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for CVAR
 *        added tCVAR token
 *
 * 08-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for IMPLICIT exceptions
 *
 * 25-Oct-95  Wilson Hsieh (whsieh) at the University of Washington
 *	support for bounded
 *
 *)

INTERFACE Token;

IMPORT M3ID;

TYPE
  T = {tEOF,

       (* lexical classes *)
       tIDENT,
       tCARDCONST,
       tREALCONST, tLONGREALCONST, tEXTENDEDCONST,
       tCHARCONST,
       tTEXTCONST,

       (* operators *)
       tPLUS, tMINUS, tASTERISK, tSLASH, tASSIGN, tAMPERSAND, tDOT, tCOMMA,
       tSEMI, tLPAREN, tLBRACKET, tLBRACE, tARROW, tEQUAL, tSHARP, tLESS,
       tGREATER, tLSEQUAL, tGREQUAL, tDOTDOT, tCOLON, tRPAREN, tRBRACKET,
       tRBRACE, tBAR, tSUBTYPE, tIMPLIES, tENDPRAGMA,

       (* pragmas (that escape from the scanner) *)
       tINLINE, tEXTERNAL, tASSERT, tUNUSED, tOBSOLETE, tTRACE,
       tCALLCONV, tFATAL, tIMPLICIT,

       (* reserved words *)
       tAND, tALIGNED, tANY, tARRAY, tAS,
       tBEGIN, tBITS,
       tBRANDED, tBY,
       tCASE, tCONST,
       tCVAR,
       tDIV, tDO,
       tELSE, tELSIF, tEND,
       tBOUNDED,  (* order must be maintained with order of keywords *)
       tEVAL, tEXCEPT, tEXCEPTION, tEXIT, tEXPORTS, 
       tFINALLY, tFOR, tFROM,
       tFUNCTIONAL,
       tGENERIC,
       tIF, tIMPORT, tIN, tINTERFACE,
       tLOCK, tLOOP,
       tMETHODS, tMOD, tMODULE,
       tNOT,
       tOBJECT, tOF, tOR, tOVERRIDES,
       tPROCEDURE,
       tRAISE, tRAISES, tREADONLY, tRECORD, tREF, tREPEAT, tRETURN, tREVEAL,
       tSET,
       tTHEN, tTO, tTRY, tTYPE, tTYPECASE,
       tUNSAFE, tUNTIL, tUNTRACED,
       tVALUE, tVAR,
       tWHILE, tWITH
       };

CONST
  First_Literal  = T.tIDENT;
  Last_Literal   = T.tTEXTCONST;
  First_Operator = T.tPLUS;
  Last_Operator  = T.tENDPRAGMA;
  First_Pragma   = T.tINLINE;
  Last_Pragma    = T.tIMPLICIT;
  First_Keyword  = T.tAND;
  Last_Keyword   = T.tWITH;

TYPE
  Set = SET OF T;

CONST
  EmptySet = Set {};

  DeclStart = Set {T.tCONST, T.tTYPE, T.tREVEAL, T.tVAR,
                   T.tEXTERNAL, T.tINLINE, T.tUNUSED, T.tOBSOLETE,
                   T.tBOUNDED, T.tIMPLICIT, T.tFUNCTIONAL,
                   T.tEXCEPTION, T.tPROCEDURE, T.tFATAL, T.tCALLCONV};

  TypeStart = Set {T.tIDENT, T.tARRAY, T.tBITS, T.tBRANDED, T.tLBRACE,
                   T.tUNTRACED, T.tOBJECT, T.tPROCEDURE, T.tRECORD,
		   T.tREF, T.tSET, T.tLBRACKET, T.tLPAREN};

  ExprStart = Set {T.tNOT, T.tPLUS, T.tMINUS, T.tIDENT, T.tCARDCONST,
                   T.tLONGREALCONST, T.tREALCONST, T.tEXTENDEDCONST,
                   T.tCHARCONST, T.tTEXTCONST, T.tLPAREN,
                   T.tARRAY, T.tBITS, T.tRECORD, T.tSET};

  StmtStart = Set {T.tCASE, T.tEXIT, T.tEVAL, T.tFOR, T.tIF, T.tLOCK,
                   T.tLOOP, T.tRAISE, T.tREPEAT, T.tRETURN, T.tTRY,
		   T.tTYPECASE, T.tWHILE, T.tWITH, T.tBEGIN, T.tASSERT}
		   + ExprStart + DeclStart;

VAR (*CONST*)
  name: ARRAY T OF M3ID.T;

PROCEDURE Initialize ();

END Token.
