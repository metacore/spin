(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 25-Jan-96  Wilson Hsieh (whsieh) at the University of Washington
 *	support for SIZE fault
 *      added new fault to ExceptionCode, and ExceptionNames
 *      added constants FirstFault and LastFault
 *
 * 16-Dec-95 Przemek Pardyak (pardy) at the University of Washington
 *	Added NoHandlerInvoked exception.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 19-Sep-95  Brian Bershad (bershad) at the University of Washington
 *	Created
 *
 * Standalone definitions of SEVERE runtime exceptions that are
 * caught or raised by spin kernel services.
 *)
INTERFACE SpinException;

TYPE ExceptionCode = {NoException, 
(* VM related faults *)
		      ProtectionFault,
                      BadAddress,
(* Checked runtime errors *)
                      UnhandledException,
                      NotInRaisesList,
(* Program Faults *)                     
                      ASSERTFailed,
                      ValueOutOfRange,
                      SubscriptOutOfRange,
                      IncompatibleArrayShapes,
                      AttemptToDereferenceNIL,
                      NARROWFailed,
                      FunctionDidNotReturnValue,
                      UnhandledValueInCASEStatement,
                      UnhandledTypeInTYPECASEStatement,
                      StackOverflow,
                      ViewSize,
                      ViewAlignment,
(* Event handling *)
                      NoHandlerInvoked,
(* Unaligned Fault *)
                      UnalignedFault,
(* Arithmetic Fault *)
                      ArithmeticFault,
(* Unknown *)
		      UnknownException};

CONST
  FirstFault = ExceptionCode.ASSERTFailed;
  LastFault = ExceptionCode.ViewAlignment;

CONST ExceptionNames = ARRAY ExceptionCode OF TEXT {
            "No exception",
(* VM related faults *)
            "Protection fault",
            "Bad address",
(* Exception related faults *)
            "Unhandled exception",
            "Not in raises list",
(* Program Faults *)                     
            "ASSERT failed",
            "Value out of range",
	    "Subscript out of range",
	    "Incompatible array shapes",
	    "Attempt to dereference NIL",
	    "NARROW failed",
            "Function did not return a value",
            "Unhandled value in CASE statement",
            "Unhandled type in TYPECASE statement",
            "Stack overflow",
            "VIEW size fault",
            "VIEW alignment fault",
(* Event handling *)
            "No handler invoked for event that expects result",
(* Unaligned Fault *)
            "alignment fault",
(* Arithmetic Fault *)
            "Arithmetic fault",
(* Unknown *)
            "Unknown exception"};

TYPE ExceptionInfo = RECORD
	code: ExceptionCode;
	msg: TEXT;
  END;

<* IMPLICIT *> EXCEPTION Exception(ExceptionInfo);

END SpinException.



