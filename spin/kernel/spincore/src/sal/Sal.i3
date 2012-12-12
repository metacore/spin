INTERFACE Sal;

IMPORT Word, Ctypes, Clock;
IMPORT SalDep;

PROCEDURE Init(verbose: BOOLEAN);
PROCEDURE InitThreads(verbose: BOOLEAN);

PROCEDURE PutChar(char: CHAR);
PROCEDURE Printf(string: ADDRESS; arg:INTEGER); (* baby printf for m3 *)

PROCEDURE GetPhysicalMemoryStart(): Word.T;
PROCEDURE GetPhysicalMemorySize(): Word.T;
PROCEDURE GetTracedStart(): ADDRESS;
PROCEDURE GetTracedEnd(): ADDRESS;

PROCEDURE UntracedAlloc(size: INTEGER): ADDRESS; (* spin malloc *)
PROCEDURE MallocStats();

(* threadcore handles these Copy events *)
PROCEDURE CopyIn(userSrc, kernDest, count: Word.T);
PROCEDURE CopyOut(kernSrc, userDest, count: Word.T);

PROCEDURE Halt();

CONST
  POLLNORM = 1;
  POLLIN = POLLNORM;
  POLLPRI  = 2;
  POLLOUT  = 4;
  POLLERR  = 8;
  POLLHUP  = 16;
  POLLNVAL = 32;
  SELDEBUG = FALSE;
  
TYPE Pollfd = RECORD
  selectproc: SalDep.SelectProc;
  descriptor: Word.T;
  events, revents: Ctypes.short_int;
END;
  
PROCEDURE DoScan(VAR pollfiles : ARRAY OF Pollfd; 
		 (*OUT*) VAR retval: INTEGER;
		 READONLY timeout: Clock.TimeVal;
		 hasTimeout: BOOLEAN): INTEGER;
  

END Sal.
