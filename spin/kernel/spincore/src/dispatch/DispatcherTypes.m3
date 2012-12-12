MODULE DispatcherTypes;

IMPORT RTProcDescF, RTProcDesc, IO, RTType, RTTypeSRC, RT0;
IMPORT CPU, Dispatcher, EventDesc, DispatcherPrivate;
IMPORT RTOS;
(* FIXME: pass simple putter to RTProcDesc.Print* *)

(******************************************************************************
 *
 * Dynamic typechecking
 *
 *****************************************************************************)

(*
 * typecheck a binding
 * check whether it the guard and the handler take closures
 *)
PROCEDURE TypecheckBinding (    eventProcDesc     : RTProcDescF.T;
                                guardProcDesc     : RTProcDescF.T;
                                handlerProcDesc   : RTProcDescF.T;
                                saveRegs          : BOOLEAN;
                                guardClosure      : REFANY;
                                handlerClosure    : REFANY; 
                            VAR useGuardClosure   : BOOLEAN;
                            VAR useHandlerClosure : BOOLEAN;
                            VAR passGuardArgs     : BOOLEAN)
                           RAISES { Dispatcher.Error } =
  BEGIN
    useGuardClosure   := FALSE;
    useHandlerClosure := FALSE;

    TypecheckHandler(eventProcDesc, handlerProcDesc, saveRegs,
                     handlerClosure, useHandlerClosure);

    IF guardProcDesc # NIL THEN
      TypecheckGuard(eventProcDesc, guardProcDesc, 
                     guardClosure, useGuardClosure, passGuardArgs);
    END;
  END TypecheckBinding;

(*
 * typecheck a handler and its closure
 *)
(* FIXME: allow skipping results *)
PROCEDURE TypecheckHandler(    eventProcDesc     : RTProcDescF.T;
                               handlerProcDesc   : RTProcDescF.T;
                               saveRegs          : BOOLEAN;
                               handlerClosure    : REFANY; 
                           VAR useHandlerClosure : BOOLEAN)
                          RAISES { Dispatcher.Error } =
  VAR
    eventType   : RTProcDesc.ProcType;
    handlerType : RTProcDesc.ProcType;
    ok: BOOLEAN;
  BEGIN
    ok := TRUE;
    eventType := eventProcDesc.type;
    handlerType := handlerProcDesc.type;

    (* are the types equal? *)
    ok := EqualProcType(eventType, handlerType, FALSE, handlerClosure);

    IF NOT ok THEN
      (* does the handler take a closure? *)
      ok := EqualProcType(eventType,handlerType, TRUE, handlerClosure);
      IF ok THEN useHandlerClosure := TRUE; END;
    END;

    IF NOT ok AND saveRegs THEN
      (* does the handler save registers? *)
      ok := SaveProcType(eventType, handlerType, FALSE);

      IF NOT ok THEN
        (* does the handler save registers and take a closure? *)
        ok := SaveProcType(eventType, handlerType, TRUE);
        IF ok THEN useHandlerClosure := TRUE; END;
      END;
    END;
 
    (* if closure not NIL then must take a closure *)
    IF ok THEN
      ok := handlerClosure = NIL OR useHandlerClosure;
    END;

    (* print error message *)
    IF NOT ok THEN
      IF DispatcherPrivate.Debug THEN
        IO.PutError("Dispatcher: handler type mismatch (");
        IF NOT useHandlerClosure THEN IO.Put ("no "); END;
        IO.Put("closure)\n");
        IO.Put("  event type:\n  "); RTProcDesc.PrintProcType(eventType);
        IO.Put("  handler type:\n  "); RTProcDesc.PrintProcType(handlerType);
      END;
      IO.PutError("Dispatcher: type error (handler)\n");
      RAISE Dispatcher.Error(Dispatcher.ErrorCode.TypeError);
    END;
  END TypecheckHandler;

(*
 * typecheck a guard and its closure
 * applies also to imposed guards
 *)
PROCEDURE TypecheckGuard(    eventProcDesc     : RTProcDescF.T;
                             guardProcDesc     : RTProcDescF.T;
                             guardClosure      : REFANY;
                         VAR useGuardClosure   : BOOLEAN;
                         VAR passGuardArgs     : BOOLEAN)
                        RAISES { Dispatcher.Error } =
  VAR
    eventType   : RTProcDesc.ProcType;
    guardType   : RTProcDesc.ProcType;
    ok: BOOLEAN;
  BEGIN
    (* make sure that guards are functional *)
    guardType := guardProcDesc.type;

    (* guards should be functional unless overriden *)
    (*
    IF RTProcDesc.IsFunctional(guardType) THEN
      IO.PutError("Dispatcher: type error, guard not functional: ");
      (* FIXME
      RTProcDesc.PrintProc(guardProcDesc.proc);
      *)
      IO.Put("\n");
      RAISE Dispatcher.Error(Dispatcher.ErrorCode.TypeError);
    END;
    *)

    ok := TRUE;
    eventType := eventProcDesc.type;
    guardType := guardProcDesc.type;

    ok :=  PredicateProcType(eventType, guardType, 
                             useGuardClosure, passGuardArgs, guardClosure);
    
    (* print error message *)
    IF NOT ok THEN
      IF DispatcherPrivate.Debug THEN
        IO.PutError("Dispatcher: guard type mismatch (");
        IF NOT useGuardClosure THEN IO.Put ("no "); END;
        IO.Put("closure)\n");
        IO.Put("  event type:\n  "); RTProcDesc.PrintProcType(eventType);
        IO.Put("\n  guard type:\n  "); RTProcDesc.PrintProcType(guardType);
        IO.Put("\n");
      END;
      IO.PutError("Dispatcher: type error (guard)\n");
      RAISE Dispatcher.Error(Dispatcher.ErrorCode.TypeError);
    END;
  END TypecheckGuard;

(*
 *
 *)
PROCEDURE TypecheckResultHandler(    eventProcDesc   : RTProcDescF.T;
                                     handlerProcDesc : RTProcDescF.T;
                                     closure         : REFANY;
                                 VAR useClosure      : BOOLEAN;
                                 VAR passArgs        : BOOLEAN)
                                RAISES { Dispatcher.Error } =
  VAR
    eventType   : RTProcDesc.ProcType := eventProcDesc.type;
    handlerType : RTProcDesc.ProcType := handlerProcDesc.type;
    ok          : BOOLEAN := TRUE;
    eHasResult  : BOOLEAN;
    eNumOfArgs  : INTEGER;
    eResultType : INTEGER;
    hHasResult  : BOOLEAN;
    hNumOfArgs  : INTEGER;
    extra       : INTEGER;
    offset      : INTEGER;
  BEGIN
    (* check the result type of the event *)
    eNumOfArgs := RTProcDesc.NumberOfArgs(eventType);
    eHasResult := RTProcDesc.HasResult(eventType);
    IF eHasResult THEN
      eResultType  := RTProcDesc.ResultType(eventType);
    END;

    (* check the number of arguments: it's the number of event's arguments *)
    (* plus 4 if event has a result, and plus 2 otherwise *)
    (* one extra argument if it takes closures, no result *)
    hNumOfArgs  := RTProcDesc.NumberOfArgs(handlerType);
    hHasResult  := RTProcDesc.HasResult(handlerType);
    extra := hNumOfArgs - eNumOfArgs;
    offset := 1;

    IF hHasResult OR (eHasResult AND extra # 4 AND extra # 5) OR 
      (NOT eHasResult AND extra # 2 AND extra # 3) 
     THEN
      IF hNumOfArgs # 0 AND hNumOfArgs # 1 THEN
        ok := FALSE;
      END;
    END;

    (* check for closure *)
    IF ok AND (extra = 5 OR extra = 3 OR hNumOfArgs = 1) THEN
      IF CheckClosure(handlerType, 1, closure) THEN
        useClosure := TRUE;
        DEC(extra);
        INC(offset);
      ELSE
        ok := FALSE;
      END;
    ELSE
      useClosure := FALSE;
    END;

    (* check if arguments should be passed *)
    IF ok AND (hNumOfArgs = 0 OR hNumOfArgs = 1) THEN
      passArgs := FALSE;
    ELSE
      passArgs := TRUE;
    END;

    (* check the result types, the first one should be passed by variable, *)
    (* the second one by value, both same type as the event's result *)
    IF ok AND eHasResult AND passArgs THEN
      IF extra = 4 AND
        RTProcDesc.ArgType(handlerType, offset) = eResultType AND
        RTProcDesc.ArgMode(handlerType, offset) = RT0.FormalModeVAR AND
        RTProcDesc.ArgType(handlerType, offset+1) = eResultType AND
        RTProcDesc.ArgMode(handlerType, offset+1) = RT0.FormalModeVALUE 
       THEN
        DEC(extra, 2);
        INC(offset, 2);
      ELSE
        ok := FALSE;
      END;
    END;
    
    (* last two extra arguments are one boolean passed by value and *)
    (* one refany passed by variable *)
    IF ok AND passArgs AND NOT(extra = 2 AND
      RTProcDesc.ArgType(handlerType, offset) = BooleanUID AND
      RTProcDesc.ArgMode(handlerType, offset) = RT0.FormalModeVALUE AND
      RTProcDesc.ArgType(handlerType, offset+1) = RefAnyUID AND
      RTProcDesc.ArgMode(handlerType, offset+1) = RT0.FormalModeVAR)
     THEN
      ok := FALSE;
    END;

    (* the remaining arguments should be the event arguments *)
    IF ok AND passArgs THEN
      eventType := eventProcDesc.type;
      extra := hNumOfArgs - eNumOfArgs;
      FOR i := 1 TO eNumOfArgs DO
        IF (RTProcDesc.ArgType(eventType, i) # 
          RTProcDesc.ArgType(handlerType, extra+i)) OR
          (RTProcDesc.ArgMode(eventType, i) # 
          RTProcDesc.ArgMode(handlerType, extra+i))
         THEN
          ok := FALSE;
          EXIT;
        END;
      END;
    END;

    (* print a message and raise an exception if something is wrong *)
    IF NOT ok THEN
      IF DispatcherPrivate.Debug THEN
        IO.PutError("Dispatcher: result handler type mismatch:\n(");
        IF NOT useClosure THEN IO.Put ("no "); END;
        IO.Put("closure)\n");
        IO.Put("  event type: "); 
        RTProcDesc.PrintProcType(eventProcDesc.type);
        IO.Put("\n  handler type: "); 
        RTProcDesc.PrintProcType(handlerProcDesc.type);
        IO.Put("\n"); 
      END;
      IO.PutError("Dispatcher: type error (resultHandler)\n");
      RTOS.Crash();
      RAISE Dispatcher.Error(Dispatcher.ErrorCode.TypeError);
    END;
  END TypecheckResultHandler; 

(*
 * is it legal to raise this event asynchronously
 *)
PROCEDURE IsLegalAsynchronous(procDesc: RTProcDescF.T) : BOOLEAN =
  VAR 
    type : RTProcDesc.ProcType := procDesc.type;
    numOfArgs : INTEGER := RTProcDesc.NumberOfArgs(type);
    hasResult : BOOLEAN := RTProcDesc.HasResult(type);
  BEGIN
    (* event that returns results cannot be asynchronous *)
    IF hasResult THEN RETURN FALSE; END;

    (* event that takes a VAR or READONLY argument cannot be asynchronous *)
    FOR i := 1 TO numOfArgs DO
      WITH mode = RTProcDesc.ArgMode(type, i) DO
        IF mode = RT0.FormalModeVAR OR mode = RT0.FormalModeCONST THEN
          RETURN FALSE;
        END;
      END;
    END;

    (* it is OK otherwise *)
    RETURN TRUE;
  END IsLegalAsynchronous;

(*
 *  verify that two procedural types are structurally identical 
 *)

PROCEDURE EqualProcType(type1, type2: RTProcDesc.ProcType;
                        useClosure: BOOLEAN;
                        closure: REFANY): BOOLEAN =
  VAR 
    nArgs1, nArgs2 : INTEGER;
    res1, res2 : BOOLEAN;
    off: INTEGER;
  BEGIN
    (* one of them is a legalized C procedure *)
    IF type1 = NIL OR type2 = NIL THEN RETURN TRUE; END;

    (* identical *)
    IF type1 = type2 THEN RETURN NOT useClosure; END;

    (* check the number of arguments *)
    nArgs1 := RTProcDesc.NumberOfArgs(type1);
    nArgs2 := RTProcDesc.NumberOfArgs(type2);
    IF (useClosure AND nArgs1 + 1 # nArgs2) OR 
      (NOT useClosure AND nArgs1 # nArgs2)
     THEN 
      RETURN FALSE; 
    END;

    (* check the results flag *)
    res1  := RTProcDesc.HasResult(type1);
    res2  := RTProcDesc.HasResult(type2);
    IF res1 # res2 THEN
      RETURN FALSE; 
    END;

    (* check type of closure *)
    IF useClosure AND NOT CheckClosure(type2, 1, closure) THEN
      RETURN FALSE;
    END;

    (* check types of arguments and results *)
    (* all type ids and argument passing mode have to be equal *)
    IF useClosure THEN off := 1; ELSE off := 0; END;
    FOR i := 1 TO nArgs1 DO
      IF (RTProcDesc.ArgType(type1, i) # RTProcDesc.ArgType(type2, i+off)) OR
        (RTProcDesc.ArgMode(type1, i) # RTProcDesc.ArgMode(type2, i+off))
       THEN
        RETURN FALSE;
      END;
    END;
    RETURN (NOT res1 OR
           (RTProcDesc.ResultType(type1) = RTProcDesc.ResultType(type2)));
  END EqualProcType; 

(*
 *  verify that two procedural types have the same number and types of
 *  arguments but the second one's result if of BOOLEAN type
 *)

PROCEDURE PredicateProcType (type1, type2: RTProcDesc.ProcType;
                             VAR useClosure : BOOLEAN;
                             VAR passArgs   : BOOLEAN;
                             closure: REFANY): BOOLEAN =
  VAR
    nArgs1, nArgs2 : INTEGER;
    hasResult2: BOOLEAN;
    off: INTEGER;
  BEGIN
    (* one of them is a legalized C procedure *)
    IF type1 = NIL OR type2 = NIL THEN RETURN TRUE; END;

    (* check the number of arguments *)
    nArgs1 := RTProcDesc.NumberOfArgs(type1);
    nArgs2 := RTProcDesc.NumberOfArgs(type2);
    IF nArgs2 = nArgs1 AND closure = NIL THEN
      (* same arguments *)
      passArgs   := TRUE;
      useClosure := FALSE;
    ELSIF nArgs2 = nArgs1 + 1 THEN
      (* closure plus same arguments *)
      passArgs   := TRUE;
      useClosure := TRUE;
    ELSIF nArgs2 = 0 THEN
      (* no arguments *)
      passArgs   := FALSE;
      useClosure := FALSE;
    ELSIF nArgs2 = 1 THEN
      (* closure *)
      passArgs   := FALSE;
      useClosure := TRUE;
    ELSE
      RETURN FALSE;
    END;
      
    (* check the results flag *)
    hasResult2 := RTProcDesc.HasResult(type2);
    IF NOT hasResult2 THEN
      RETURN FALSE; 
    END;

    (* check the type of closure *)
    IF useClosure AND NOT CheckClosure(type2, 1, closure) THEN
      RETURN FALSE;
    END;

    (* check types of arguments *)
    (* all type ids and argument passing mode have to be equal *)
    IF passArgs THEN
      IF useClosure THEN off := 1; ELSE off := 0; END;
      FOR i := 1 TO nArgs1 DO
        IF (RTProcDesc.ArgType(type1, i) # RTProcDesc.ArgType(type2, i+off)) OR
          (RTProcDesc.ArgMode(type1, i) # RTProcDesc.ArgMode(type2, i+off))
         THEN
          RETURN FALSE;
        END;
      END;
    END;

    (* check that the result type of the second type is BOOLEAN *)
    IF RTProcDesc.ResultType(type2) # BooleanUID THEN
    END;
    RETURN RTProcDesc.ResultType(type2) = BooleanUID;
  END PredicateProcType; 

(*
 *  verify that the second procedure type is equal to the first one
 *  plus saved registers
 *)

PROCEDURE SaveProcType(<* UNUSED *> type1, type2: RTProcDesc.ProcType;
                       <* UNUSED *> closure: BOOLEAN): BOOLEAN =
  BEGIN
    RETURN TRUE;                                                    (* FIXME *)
  END SaveProcType;

(*
 * typecheck closure
 *)

PROCEDURE CheckClosure (type: RTProcDesc.ProcType;
                        nArg: INTEGER;
                        closure: REFANY): BOOLEAN =
  VAR
    cType     : INTEGER;
    cTypeDefn : RT0.TypeDefn;
  BEGIN
    cType := RTProcDesc.ArgType(type, nArg);

    (* if closure type is refany, no need to test, if closure not NIL then *)
    (* it has to be a subtype of the closure type if closure is NIL then *)
    (* check that the closure type is a subtype of refany *)
    IF cType # RefAnyUID THEN
      cTypeDefn := RTTypeSRC.FindType(cType);
      IF cTypeDefn = NIL OR NOT cTypeDefn.traced = 1 OR
        (closure # NIL AND
         NOT RTType.IsSubtype(TYPECODE(closure), cTypeDefn.typecode))
       THEN
        RETURN FALSE; 
      END;
    END;

    (* closure has to be passed by value *)
    RETURN RTProcDesc.ArgMode(type, nArg) = RT0.FormalModeVALUE;
  END CheckClosure;

(*
 * obtain type information for an event descriptor
 *)

PROCEDURE NumberOfArgs (eventDesc: EventDesc.T) : INTEGER =
  BEGIN
    RETURN RTProcDesc.NumberOfArgs(eventDesc.procDesc.type);
  END NumberOfArgs;

PROCEDURE HasResult (eventDesc: EventDesc.T): BOOLEAN =
  BEGIN
    RETURN RTProcDesc.HasResult(eventDesc.procDesc.type);
  END HasResult; 

(*
 * find the UIDs of fixed types used by the dispatcher
 *)

VAR
  BooleanUID   : INTEGER;
  RefAnyUID    : INTEGER;
  SavedRegsUID : INTEGER;

PROCEDURE BooleanDummy () : BOOLEAN =
  BEGIN
    RETURN TRUE;
  END BooleanDummy;

PROCEDURE RefAnyDummy () : REFANY =
  BEGIN
    RETURN NIL;
  END RefAnyDummy;

PROCEDURE SavedRegsDummy () : UNTRACED REF CPU.CalleeSavedRegs =
  BEGIN
    RETURN NIL;
  END SavedRegsDummy; 

PROCEDURE SetTypeUIDs () =
  BEGIN
    BooleanUID   := RTProcDesc.ResultType(RTProcDesc.GetType(BooleanDummy));
    RefAnyUID    := RTProcDesc.ResultType(RTProcDesc.GetType(RefAnyDummy));
    SavedRegsUID := RTProcDesc.ResultType(RTProcDesc.GetType(SavedRegsDummy));
  END SetTypeUIDs;

PROCEDURE Init () =
  BEGIN
    (* initialize typechecking *)
    SetTypeUIDs();
  END Init;

BEGIN
END DispatcherTypes.
