(*
   XDRRec.i3
   XDR with record marking on readers and writers.
   David Nichols, Xerox PARC
   July, 1991

   $Id: XDRRec.i3,v 1.1 1996/02/09 18:08:20 mef Exp $
 *)

(* Copyright (c) 1991, 1992 Xerox Corporation.  All rights reserved.

   Use and copying of this software and preparation of derivative works
   based upon this software are permitted.  Any distribution of this
   software or derivative works must comply with all applicable United
   States export control laws.  This software is made available AS IS, and
   Xerox Corporation makes no warranty about the software, its performance
   or its conformity to any specification. *)

INTERFACE XDRRec;

(* XDR sources and sinks aimed at streams and using the XDR record-marking
   protocol. *)

IMPORT Rd, Thread, Wr, XDR;

TYPE
  Source <: XDR.Source;
  Sink <: XDR.Sink;

PROCEDURE NewSource (rd: Rd.T): Source;

(* Zap all data upto the next record boundary.  XDR.Get* calls will fail
   when a source reaches a record boundary until the next call to
   NextRecord. *)
PROCEDURE NextRecord (s: Source) RAISES {XDR.Failed, Thread.Alerted};


PROCEDURE NewSink (wr: Wr.T): Sink;

(* Place a record boundary at the current point and flush the underlying
   writer. *)
PROCEDURE NewRecord (s: Sink) RAISES {XDR.Failed, Thread.Alerted};

END XDRRec.
