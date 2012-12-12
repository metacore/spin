UNSAFE (* import SalExtern interface *)
MODULE SalDep EXPORTS Sal, Clock;

IMPORT SalExtern;
IMPORT IO;

PROCEDURE InitThreads(verbose: BOOLEAN) =
  BEGIN
    IF verbose THEN 
      IO.Put("Sal.InitThreads\n"); 
    END; 
  END InitThreads;

PROCEDURE Init(verbose: BOOLEAN) =
  BEGIN
    IF verbose THEN 
      IO.Put("Sal.Init\n"); 
    END; 

    (* alpha does this.  does x86 need it?
    InitNonPreemptive();
    MemoryForSAL.Init(verboseinit);
     *)
  END Init;

PROCEDURE TimeOfDay((*OUT*) VAR t: TimeVal) =
  BEGIN
    t := TimeVal{SalExtern.time.tv_sec, SalExtern.time.tv_usec};
  END TimeOfDay;


PROCEDURE InterruptRate(): CARDINAL =  (* Clock chip ticks per second *)
  BEGIN
    RETURN SalExtern.hz DIV 4096;
  END InterruptRate;

BEGIN
END SalDep.
