(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 29-May-96  Charles Garrett (garrett) at the University of Washington
 *	Added RecordStubs.
 *
 * 20-Apr-96  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 09-Jan-96 Przemek Pardyak (pardy) at the University of Washington
 *	Switched to a single dispatcher exception.
 *
 * 22-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	DebugDispatch RAISES clause changed to pass on all exceptions.
 *
 * 2-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Got rid of SetNumOfArgs and SetNoResult.
 *
 * 19-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Added Debug flag.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)

INTERFACE DispatcherPrivate;
IMPORT Dispatcher, SpinException, Thread, DispatcherRep, RT0;
IMPORT DomainPrivate; (* FIXME: this violates modularity, change it *)

(******************************************************************************
 *
 * 
 *
 *****************************************************************************)

(* FIXME: opt levels:
     0 - DebugDispatch
     1 - pessimistic
     2 - 
     3 - optimized
     4 - unrolled
     5 - better unrolling
     6 - cloning
     7 - inlining *)
   
CONST
  MaxOptLevel  = 5;
  BootOptLevel = 0;
  UseOptLevel  = 4;

VAR
  DefaultOptLevel := BootOptLevel;

VAR
  traceAll := FALSE;

PROCEDURE SetOptLevel(event: PROCANY; opt_level: INTEGER)
                      RAISES { Dispatcher.Error };

PROCEDURE SetDefaultOptLevel(opt_level: INTEGER)
                             RAISES { Dispatcher.Error };

(******************************************************************************
 *
 * 
 *
 *****************************************************************************)

CONST
  (* MaxNumOfHandlersForDebug = 255;*)
  MaxNumOfHandlersForDebug = 1000;
  MaxNumOfArgumentsForDebug = 15;

TYPE
  DebugDispatchArgs = ARRAY [1..MaxNumOfArgumentsForDebug] OF INTEGER;

TYPE
  AsynchClosure = Thread.Closure OBJECT
    args      : DebugDispatchArgs;
    aliasDesc : DispatcherRep.AliasDescT;
    OVERRIDES
      apply := AsynchDispatch;
    END;
  
PROCEDURE DebugDispatch (    desc : DispatcherRep.AliasDescT;
                         VAR args : DebugDispatchArgs): INTEGER 
                        RAISES ANY;

PROCEDURE AsynchDispatch(self: AsynchClosure): REFANY;

(******************************************************************************
 *
 * 
 *
 *****************************************************************************)

PROCEDURE Init(verbose: BOOLEAN);
(* initialize internal data structures, called once on boot time before *)
(* any module uses the dispatcher *)

PROCEDURE Reinitialize();
(* reinitialize internal data structures to accomodate dynamically loaded *)
(* modules, called before any of new modules main body is executed *)

PROCEDURE DestroyEvents(modules: UNTRACED REF ARRAY OF RT0.ModulePtr)
                         : BOOLEAN;

PROCEDURE DestroyHandlers(ranges: DomainPrivate.RangeT): BOOLEAN;

(* print debugging messages *)
VAR 
  Debug: BOOLEAN := FALSE;

TYPE
  StubEvent = RECORD
    stub: PROCANY;
    event: PROCANY;
  END;

PROCEDURE RecordStubs(): UNTRACED REF ARRAY OF StubEvent;
(* tell the profiler about stubs created for dispatching. *)

(* <* OBSOLETE *> *)
PROCEDURE RaiseNoHandlerInvoked () RAISES { SpinException.Exception };

(* <* OBSOLETE *> *)
PROCEDURE SetSave (event: PROCANY)
                   RAISES { Dispatcher.Error };

PROCEDURE KeepStub(event: PROCANY; flag: BOOLEAN := TRUE)
                   RAISES { Dispatcher.Error };

PROCEDURE Trace (event: PROCANY; on: BOOLEAN) RAISES { Dispatcher.Error };
PROCEDURE TraceAll (on: BOOLEAN);
PROCEDURE DumpTrace ();
PROCEDURE ResetTrace ();

PROCEDURE GetOptLevel (event: PROCANY): INTEGER RAISES { Dispatcher.Error };

PROCEDURE Bypass (event:PROCANY; proc:PROCANY) RAISES { Dispatcher.Error };

END DispatcherPrivate.
