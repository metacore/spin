(*
   XDRMem.i3
   XDR on a simple memory buffer.
   David Nichols, Xerox PARC
   February, 1992

   Copyright (c) 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE XDRMem;

IMPORT XDR;

TYPE
  Source <: XDR.Source;
  Sink <: XDR.Sink;

(* Get a new source aimed at a chunk of memory. *)
PROCEDURE NewSource (buf: REF ARRAY OF CHAR): Source;

(* Set the starting point for fetching data. *)
PROCEDURE SetSourcePos (s: Source; pos: INTEGER) RAISES {XDR.Failed};

(* Set the bounds for fetching data.  Default is length of the buffer. *)
PROCEDURE SetSourceLen (s: Source; len: INTEGER) RAISES {XDR.Failed};


(* Get a new sink that places data in the buffer. *)
PROCEDURE NewSink (buf: REF ARRAY OF CHAR): Sink;

(* Set the place to start putting data. *)
PROCEDURE SetSinkPos (s: Sink; pos: INTEGER) RAISES {XDR.Failed};

(* Get the current spot. *)
PROCEDURE GetSinkPos (s: Sink): INTEGER;


END XDRMem.
