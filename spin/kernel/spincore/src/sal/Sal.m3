UNSAFE (* to import SalExtern *)
MODULE Sal;

IMPORT SalExtern, Word, RTMisc;

PROCEDURE CopyIn(src, dest, count: Word.T) =
  BEGIN
    RTMisc.Copy(LOOPHOLE(src, ADDRESS), LOOPHOLE(dest, ADDRESS), count);
  END CopyIn;
  
PROCEDURE CopyOut(src, dest, count: Word.T) =
  BEGIN
    RTMisc.Copy(LOOPHOLE(src, ADDRESS), LOOPHOLE(dest, ADDRESS), count);
  END CopyOut;

PROCEDURE PutChar(c: CHAR) =
  BEGIN
    SalExtern.putchar(c);
  END PutChar;

PROCEDURE Printf(string: ADDRESS; arg:INTEGER) = (* baby printf for m3 *)
  BEGIN
    SalExtern.printf(string,arg);
  END Printf;

PROCEDURE GetPhysicalMemoryStart(): Word.T =
  BEGIN
    RETURN SalExtern.pmem_start;
  END GetPhysicalMemoryStart;

PROCEDURE GetPhysicalMemorySize(): Word.T =
  BEGIN
    RETURN SalExtern.pmem_size;
  END GetPhysicalMemorySize;

PROCEDURE GetTracedStart(): ADDRESS =
  BEGIN
    RETURN SalExtern.traced_start;
  END GetTracedStart; 

PROCEDURE GetTracedEnd(): ADDRESS =
  BEGIN
    RETURN SalExtern.traced_end;
  END GetTracedEnd; 

PROCEDURE UntracedAlloc(size: INTEGER): ADDRESS =
  BEGIN	
    RETURN SalExtern.spin_malloc(size);
  END UntracedAlloc;

PROCEDURE MallocStats() = 
  BEGIN
    SalExtern.mallocstats();
  END MallocStats;

PROCEDURE Halt() = 
  BEGIN
    SalExtern.standalone_halt();
  END Halt;

BEGIN
END Sal.
