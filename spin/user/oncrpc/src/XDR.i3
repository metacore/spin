(*
   XDR.i3
   Marshalling routines for SunRPC.
   David Nichols, Xerox PARC
   July, 1991

   Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)
(* HISTORY 
 * 08-Feb-96  Marc Fiuczynski (mef) at the University of Washington
 *	Took out FloatMode;
 *
 *)

INTERFACE XDR;

IMPORT RPCOS;
IMPORT ExceptionArg (*, FloatMode*);
IMPORT Thread;

EXCEPTION Failed(Failure);
TYPE
  Failure = ExceptionArg.T BRANDED OBJECT END;
(* xxx mef
  FloatConversionFailure = Failure BRANDED OBJECT
					     flag: FloatMode.Flag;
					   END;
*)

(* Decoding routines. *)
PROCEDURE GetInteger (s: Source): RPCOS.UINT32 RAISES {Failed, Thread.Alerted};
PROCEDURE GetShort (s: Source): RPCOS.UINT16 RAISES {Failed, Thread.Alerted};
PROCEDURE GetChar (s: Source): CHAR RAISES {Failed, Thread.Alerted};
PROCEDURE GetBoolean (s: Source): BOOLEAN RAISES {Failed, Thread.Alerted};
(* mef: not in SPIN.
PROCEDURE GetReal (s: Source): REAL RAISES {Failed, Thread.Alerted};
PROCEDURE GetLongReal (s: Source): LONGREAL RAISES {Failed, Thread.Alerted};
*)
PROCEDURE GetText (s: Source): TEXT RAISES {Failed, Thread.Alerted};
PROCEDURE GetBytes (s: Source; VAR v: ARRAY OF CHAR)
  RAISES {Failed, Thread.Alerted};

(* Encoding routines. *)
PROCEDURE PutInteger (s: Sink; v: RPCOS.UINT32) RAISES {Failed, Thread.Alerted};
PROCEDURE PutShort (s: Sink; v: RPCOS.UINT16) RAISES {Failed, Thread.Alerted};
PROCEDURE PutChar (s: Sink; v: CHAR) RAISES {Failed, Thread.Alerted};
PROCEDURE PutBoolean (s: Sink; v: BOOLEAN) RAISES {Failed, Thread.Alerted};
(* mef: not in SPIN 
PROCEDURE PutReal (s: Sink; v: REAL) RAISES {Failed, Thread.Alerted};
PROCEDURE PutLongReal (s: Sink; v: LONGREAL) RAISES {Failed, Thread.Alerted};
*)
PROCEDURE PutText (s: Sink; v: TEXT) RAISES {Failed, Thread.Alerted};
PROCEDURE PutBytes (s: Sink; READONLY v: ARRAY OF CHAR)
  RAISES {Failed, Thread.Alerted};


(* To create Sink or Source, you must either get one from one of the
   specific interfaces (XDRMem, etc.) or you have to provide routines for
   buffering the data to and from the stream. *)
TYPE
  (* An object for decoding data from a source (e.g.  the network). *)
  Source = OBJECT
           METHODS
             GetWord32  (): RPCOS.UINT32 RAISES {Failed, Thread.Alerted};
             GetBytes (VAR v: ARRAY OF CHAR) RAISES {Failed, Thread.Alerted};
             GetText  (len: CARDINAL): TEXT  RAISES {Failed, Thread.Alerted};
           END;

  (* An object for encoding data to a sink. *)
  Sink = OBJECT
         METHODS
           PutWord32  (v: INTEGER) RAISES {Failed, Thread.Alerted};
           PutBytes (READONLY v: ARRAY OF CHAR) RAISES {Failed, Thread.Alerted};
           PutText (READONLY t: TEXT; len: CARDINAL) RAISES {Failed, Thread.Alerted};
         END;

END XDR.
