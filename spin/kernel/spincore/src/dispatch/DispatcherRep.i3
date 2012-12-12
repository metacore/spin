(*
 * Copyright 1995, University of Washington
 * All rights reserved.
 * See COPYRIGHT file for a full description
 *
 * HISTORY
 * 20-May-96  Przemek Pardyak (pardy) at the University of Washington
 *	Changed the dispatcher interface.
 *
 * 20-Apr-95  Przemek Pardyak (pardy) at the University of Washington
 *	Converted to real PROCANY-s.
 *
 * 24-Jan-96  Brian Bershad (bershad) at the University of Washington
 *	Auth support.
 *
 * 10-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Reduced memory overhead by splitting EventDesc.T into EventDesc.T
 *	and ProcDesc.T.
 *
 * 02-Dec-95  Przemek Pardyak (pardy) at the University of Washington
 *	Cleaned up the code.  Changed some names to conform to our
 *	naming convention.
 *
 * 13-Nov-95  Przemek Pardyak (pardy) at the University of Washington
 *	Switched to the simplified StrongRef interface.
 *
 * 06-Oct-95  Emin Gun Sirer (egs) at the University of Washington
 *	Copyright.
 *
 * 02-Apr-95  Przemyslaw Pardyak (pardy) at the University of Washington
 *      Created. Event dispatcher.
 *)

INTERFACE DispatcherRep;
IMPORT Dispatcher, Auth, Word, RTProcDescF;

(* n_handlers = -1 => unregistered event                                     *)
(* no lock has to be grabbed on dispatch, we reply on atomic update to       *)
(* the fields in the alias descriptor                                        *)

TYPE
  EventDescT = REF RECORD 
    (* information about the original procedure *)
    event       : PROCANY;           (* pointer to the original procedure *)
    procDesc    : RTProcDescF.T;     (* procedure descriptor *)
    saveRegs    : BOOLEAN;           (* should the dispatcher save registers *)
    aliases     : AliasDescT;        (* list of all aliases                 *)
    asynchAlias : AliasDescT;        (* alias for asynchronous dispatch *)
    auth        : Auth.T;            (* authorizer                           *)

    (* information about event handling *)
    resultHandlers : Dispatcher.ResultHandlers; (* result handling info *)
    handlers       : REF ARRAY OF Word.T;(* current list of handlers *)
    nHandlers      : INTEGER;            (* current number of handlers *)
    nImposed       : INTEGER;            (* current number of imposed guards *)
    bindings       : Dispatcher.Binding; (* installed bindings *)
    lastBinding    : Dispatcher.Binding; (* pointer to the last one *)
    defBinding     : Dispatcher.Binding; (* default binding *)
    nUseClosure    : INTEGER;           (* number of handlers using closures *)
    nUseCancel     : INTEGER;           (* number of handlers using closures *)
    keepStub       : BOOLEAN;           (* always use the unoptimized stub *)
    trace          : BOOLEAN; 

    (* different stubs generated for the event *)
    stub     : PROCANY;                   (* current stub *)
    stubs    : ARRAY [0 .. 2] OF PROCANY; (* cache of stubs *)
    optLevel : INTEGER;                   (* level of optimizations          *)

    lock: MUTEX;    
  END;

(* all the stubs assume that the handlers field is the first one *)
TYPE
  AliasDescT = REF RECORD 
    handlers  : ADDRESS;                      (* current list of handlers *)
    aid       : PROCANY;                      (* alias identifier      *)
    eventDesc : EventDescT;                   (* shared descriptor     *)
    next      : AliasDescT;                   (* link to the next descriptor *)
    lock      : MUTEX;                        (* synchronization *)
  END;

(******************************************************************************
 *
 * marshalled dispatch information
 *
 *****************************************************************************)

(* Each time the state of an event is changed (the handlers, guards,
   result or default handler, or their properties) the dispatcher
   creates the new dispatch information for the event.  The dispatch
   information contains all the data necessary a dispatch routine
   needs to dispatch an event.  This includes identity of all
   handlers, guards, imposed guards, result handler, default handler,
   their closures, and their ordering.  This data is marshalled into a
   single linear array of words which is interpretted by the dispatch
   routine.  This marshalled dispatch information is not necessary
   only for optimized assembly dispatch routines that do not contain
   all of the dispatch information embedded directly in them
   (optimization level 1 and 2).  The marshalled dispatch information
   is interpretted as a record of type DispatchDesc followed by a
   series of records of type HandlerDesc, where the last record
   contains NIL in its handler field and serves as a sentinel for the
   dispatch routine.  A record of type HandlerDesc is followed by a
   seriese of records of type ImposedDesc, where the number of those
   fields is equal to the value of nImposed field of the HandlerDesc
   record. *)

TYPE
  DispatchDesc = RECORD
    defaultResult     : INTEGER;  (* initial value of the result *)
    defaultHandler    : PROCANY;  (* called if no other handler is called *)
    defaultClosure    : REFANY;   (* its closure *)
    useDefaultClosure : BOOLEAN;  
    resultHandler     : PROCANY;  (* called for each returned result *)
    resultClosure     : REFANY;   (* its closure *)
    useResultClosure  : BOOLEAN;  
  END;

  HandlerDesc = RECORD
    nImposed          : INTEGER; (* number of imposed guards *)
    guard             : PROCANY;
    handler           : PROCANY;
    cancel            : BITS BITSIZE(Word.T) FOR BOOLEAN;
    useGuardClosure   : BITS BITSIZE(Word.T) FOR BOOLEAN;
    useHandlerClosure : BITS BITSIZE(Word.T) FOR BOOLEAN;
    guardClosure      : REFANY;
    handlerClosure    : REFANY;
  END;

  ImposedDesc = RECORD
    guard               : PROCANY;
    useGuardClosure     : BITS BITSIZE(Word.T) FOR BOOLEAN;
    guardClosure        : REFANY;
    postGuard           : PROCANY;
    usePostGuardClosure : BITS BITSIZE(Word.T) FOR BOOLEAN;
    postGuardClosure    : REFANY;
  END;

END DispatcherRep.
