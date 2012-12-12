UNSAFE (* import SalExtern interface *)
MODULE SalDep EXPORTS Sal, Clock;

IMPORT IO, Word;
IMPORT Region, KernelRegions, SalExtern;
IMPORT DispatcherPrivate, Dispatcher, ClockPrivate;
IMPORT CPU, MemoryForSAL;
IMPORT Clock;

PROCEDURE DoScan (VAR pollfiles : ARRAY OF Pollfd; 
		  VAR retval: INTEGER;
		  READONLY atv: Clock.TimeVal;
		  hasTimeout: BOOLEAN): INTEGER =
  BEGIN
    RETURN SalExtern.do_scan(ADR(pollfiles[0]), NUMBER(pollfiles),
			     retval, atv, hasTimeout);
  END DoScan;

PROCEDURE InitNonPreemptive() =
  VAR region: Region.T;
  BEGIN
    WITH begin = LOOPHOLE(SalExtern.salbegin, Word.T),
         end   = LOOPHOLE(SalExtern.salend, Word.T) 
     DO
      region.begin  := begin;
      region.length := end - begin;
    END;
    KernelRegions.NonPreemptibleRegions.add(region);
  END InitNonPreemptive; 


PROCEDURE DecUnixClockHandler(<*UNUSED*>VAR ss: CPU.SavedState) =
  BEGIN
    SalExtern.hardclock();
    (* FIXME: 
    SalExtern.cycleclock();
    *)
  END DecUnixClockHandler;

PROCEDURE InitThreads(<*UNUSED*>verbose: BOOLEAN) =
  BEGIN
    SalExtern.kmeminit_thread();
  END InitThreads;

PROCEDURE Init(verboseinit: BOOLEAN) = 
  BEGIN
    IO.Put("Initializing sal\n");
    TRY
      DispatcherPrivate.Bypass(CPU.CycleCounter, SalExtern.rpcc);
      IF verboseinit THEN 
        IO.Put("Timestamp, CopyMemory and ZeroMemory bypassed\n");
      END;
      InitNonPreemptive();
      SalExtern.inittodr(SalExtern.version_build_time);
      EVAL Dispatcher.InstallHandler(ClockPrivate.ClockTick, NIL,
                                     DecUnixClockHandler);
    EXCEPT
    | Dispatcher.Error =>
      IO.Put("SalDep.Init: Couldn't install handler on ClockTick\n");
    END;
    MemoryForSAL.Init(verboseinit);
  END Init;

PROCEDURE TimeOfDay((*OUT*) VAR t: TimeVal) =
  BEGIN
    t := TimeVal{SalExtern.time.tv_sec, SalExtern.time.tv_usec};
  END TimeOfDay;


PROCEDURE InterruptRate (): CARDINAL =  (* Clock chip ticks per second *)
  BEGIN
    RETURN SalExtern.get_rpb_clock() DIV 4096;
  END InterruptRate;

BEGIN
END SalDep.
