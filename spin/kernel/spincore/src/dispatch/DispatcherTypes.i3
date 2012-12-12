INTERFACE DispatcherTypes;

IMPORT Dispatcher, EventDesc, RTProcDescF;

PROCEDURE TypecheckBinding (    eventProcDesc     : RTProcDescF.T;
                                guardProcDesc     : RTProcDescF.T;
                                handlerProcDesc   : RTProcDescF.T;
                                saveRegs          : BOOLEAN;
                                guardClosure      : REFANY;
                                handlerClosure    : REFANY; 
                            VAR useGuardClosure   : BOOLEAN;
                            VAR useHandlerClosure : BOOLEAN;
                            VAR passGuardArgs     : BOOLEAN)
                           RAISES { Dispatcher.Error };

PROCEDURE TypecheckHandler (    eventProcDesc     : RTProcDescF.T;
                                handlerProcDesc   : RTProcDescF.T;
                                saveRegs          : BOOLEAN;
                                handlerClosure    : REFANY; 
                            VAR useHandlerClosure : BOOLEAN)
                           RAISES { Dispatcher.Error };

PROCEDURE TypecheckGuard (    eventProcDesc     : RTProcDescF.T;
                              guardProcDesc     : RTProcDescF.T;
                              guardClosure      : REFANY;
                          VAR useGuardClosure   : BOOLEAN;
                          VAR passGuardArgs     : BOOLEAN)
                         RAISES { Dispatcher.Error };

PROCEDURE TypecheckResultHandler (    eventProcDesc   : RTProcDescF.T;
                                      handlerProcDesc : RTProcDescF.T;
                                      closure         : REFANY;
                                  VAR useClosure      : BOOLEAN;
                                  VAR passArgs        : BOOLEAN)
                                 RAISES { Dispatcher.Error };

PROCEDURE IsLegalAsynchronous (procDesc: RTProcDescF.T) : BOOLEAN;

PROCEDURE NumberOfArgs (eventDesc: EventDesc.T) : INTEGER;

PROCEDURE HasResult (eventDesc: EventDesc.T): BOOLEAN;

PROCEDURE Init ();

END DispatcherTypes.
