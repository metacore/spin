#include <iostream.h>
#include <spin_trans.h>
#include "utils.h"
#include "macrodef.h"

#include "GenList.h"
#include "GenVHSet.h"
#include "GenVHBag.h"
#include "GenBBag.h"
#include "GenAVLIndex.h"

#include "OO7.h"
#include "BenchParams.h"
#include "GenParams.h"
#include "VarParams.h"

#include "utils.h"
#include "rds_interface.h"
#include "spin_trans.h"

extern "C" {
#include "rvm.h"
#include "rds.h"
}

long NumBytes;
long NumMallocs;
long NumDerefs;
char *dummyptr;
char *static_addr;

extern char *dataFile;
extern DBPoolRoot *DBRoot;
extern int debugMode;
extern int printHeap;
extern int TotalDBSize;

extern struct rusage startUsage, endUsage;
extern struct timeval startWallTime;
extern struct timeval endWallTime;
extern struct timezone ignoreTimeZone;

extern void Compute_Database_Sizes();

extern double ComputeWallClockTime(struct timeval *startWallTime, 
				   struct timeval *endWallTime);
				   
extern double ComputeUserTime(struct rusage *startUsage, 
			      struct rusage *endUsage);
			      
extern double ComputeSystemTime(struct rusage *startUsage, 
	           struct rusage *endUsage);


// Containers for the database and indexes.
// All variables are ***pointers***  to sets and indexes
// Design library = Set of Composite Parts

extern void *DesignLib;
extern GenAVLIndex<int, CompositePart *> *DesignLibIdIndex;

// Set of Atomic Parts
extern GenVHSet<AtomicPart *> *AtomicDB;
extern GenAVLIndex<int, AtomicPart *> *AtomicIdIndex;
extern GenAVLIndex<int, AtomicPart *> *AtomicDateIndex;

// Set of Documents
extern GenVHSet<Document *> *DocumentDB;
extern GenAVLIndex<int, Document *> *DocumentIdIndex;
extern GenAVLIndex<char *, Document *> *DocumentTitleIndex;

// Set of Assemblies
extern GenVHSet<BaseAssembly *> *BaseAssemblyDB;
extern GenAVLIndex<int, BaseAssembly *> *BaseAssemblyIdIndex;

// Data structures to hold the database
// Set of Modules
extern GenVHSet<Module *> *ModuleDB;
extern GenAVLIndex<int, Module *> *ModuleIdIndex;

extern GenList<BaseAssembly *> *private_cp;
extern GenList<BaseAssembly *> *shared_cp;


void
PrintHeapInfo(char* initstr)
{
  int HighWater;
  printf("%s: Persistent Heap Info\n", initstr);
//  DBPool->PrintSummary(); fflush(stdout);
//  DBPool->PrintStats(); fflush(stdout);
//  printf("****** Total number of mallocs in program = %ld******\n", NumMallocs);
//  printf("****** Total size of mallocs in program = %ld Bytes******\n", NumBytes);
//  printf("****size of struct mhead**** = %d\n", sizeof(struct mhead));
//  printf("****total overhead of struct mhead***** = %ld\n", 
//       NumMallocs*sizeof(struct mhead));
  printf("%s: Temporary heap Info\n", initstr); 
  //TempDBPool->PrintSummary(); fflush(stdout);
}

extern char *use_raw_device;
rvm_tid_t *cur_tid;

void 
SetRange(long*, char*, long)
{
}

void
GenDBInitialize(char *configFileName)
{ 
  int err;
  rvm_return_t retval;
  rvm_length_t cur_brk;
  unsigned long heapSize;

  // perform basic initialization stuff
  cur_brk = show_break(256*8192);

  cur_tid = rvm_malloc_tid();
  *cur_tid = trans_begin(TRANSMODE_NOATOMICITY);
  if (use_raw_device) {
      dataFile = use_raw_device;
  }
  printf("using %s as the data file.\n", dataFile);
  static_addr = LoadHeap(dataFile);
}


void
GenDBSetRoot()
{
    SETRANGE(static_addr, sizeof(void*));
    *(void**)static_addr = DBRoot;
}

void
GenDBFinalize()
{
  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to generate database: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));
  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));

  cout.flush();

  // the tid used for doing persistent mallocs.
  Commit(cur_tid);
  rvm_free_tid(cur_tid);

  // flush the log buffer to disk
  FlushLog();
  // perform truncation of log -- this propagates updates back to data file
  TruncateLog();
  trans_print_malloc_stat();
  // terminate rvm 
}

void
GetSetsAndIndexes()
{
  DesignLib = (GenVHSet<CompositePart *> *)((DBPoolRoot *)DBRoot)->DesignLib;
  AtomicDB = (GenVHSet<AtomicPart *> *)((DBPoolRoot *)DBRoot)->AtomicDB;

  DocumentDB = (GenVHSet<Document *> *)((DBPoolRoot *)DBRoot)->DocumentDB;

  DesignLibIdIndex = (GenAVLIndex<int, CompositePart *> *)
    ((DBPoolRoot *)DBRoot)->DesignLibIdIndex;
  AtomicIdIndex = (GenAVLIndex<int, AtomicPart *> *)((DBPoolRoot *)DBRoot)->AtomicIdIndex;
  AtomicDateIndex = (GenAVLIndex<int, AtomicPart *> *)
    ((DBPoolRoot *)DBRoot)->AtomicDateIndex;

  DocumentIdIndex = (GenAVLIndex<int, Document *> *)((DBPoolRoot *)DBRoot)->DocumentIdIndex;
  DocumentTitleIndex = (GenAVLIndex<char *, Document *> *)
    ((DBPoolRoot *)DBRoot)->DocumentTitleIndex;

  BaseAssemblyDB = (GenVHSet<BaseAssembly *> *)((DBPoolRoot *)DBRoot)->BaseAssemblyDB;
  BaseAssemblyIdIndex = (GenAVLIndex<int, BaseAssembly *> *)
    ((DBPoolRoot *)DBRoot)->BaseAssemblyIdIndex;
  ModuleDB = (GenVHSet<Module *> *)((DBPoolRoot *)DBRoot)->ModuleDB;
  ModuleIdIndex = (GenAVLIndex<int, Module *> *)((DBPoolRoot *)DBRoot)->ModuleIdIndex;
  private_cp = (GenList<BaseAssembly *> *)((DBPoolRoot *)DBRoot)->private_cp;
  shared_cp = (GenList<BaseAssembly *> *)((DBPoolRoot *)DBRoot)->shared_cp;
}

void
Open_Database(char *arg)
{
}

void
Setup_References()
{
    DBRoot = *(DBPoolRoot**)static_addr;
    GetSetsAndIndexes();
}

void
BenchInitialize()
{  
  rvm_return_t retval;
  unsigned long heapSize;
  char *addr;
  if (use_raw_device) {
      dataFile = use_raw_device;
  }
  printf("using %s as the data file.\n", dataFile);

  cur_tid = rvm_malloc_tid();
  BeginTransaction(cur_tid);

  // get wall clock time
  gettimeofday(&startWallTime, &ignoreTimeZone);
  printf("*****time to load persistent heap*********\n");

  // get starting usage values.
  getrusage(RUSAGE_SELF, &startUsage);
  static_addr = LoadHeap(dataFile);

  Setup_References();

  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to load persistent heap: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));

  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));
  printf("reset spy.\n");
  Commit(cur_tid);
  USyscall_System("spy reset");
  USyscall_System("spy on");
}

void
BenchFinalize()
{
    USyscall_System("spy off");
    USyscall_System("spy dump");
    FlushLog();
    TruncateLog();
}

void
BeginTransaction(rvm_tid_t *tid)
{
    extern int log_mode;
    if (log_mode)
      log_mode = TRANSMODE_PAGEGRAINLOGGING;
    *tid = trans_begin(log_mode);
}
void
Commit(rvm_tid_t *tid)
{
    trans_commit(*tid);
}
void
FlushLog()
{
}

/* print break point and limit */
rvm_length_t
show_break(rvm_length_t prog_brk_size)
{
  rvm_length_t    cur_brk;
  struct rlimit   rlp;

  /* get current break point */
  errno = 0;
  if ((cur_brk=(rvm_length_t)sbrk(0)) == -1)
    {
      printf("\n? Error getting current break point\n");
      printf("    errno = %d\n",errno);
      exit(1);
    }

  /* get system maximum */
  errno = 0;
  if (getrlimit(RLIMIT_DATA,&rlp) < 0)
    {
      printf("\n? Error getting data segment limit\n");
      printf("    errno = %d\n",errno);
      exit(1);
    }
  
  /* print the limits */
  cur_brk = RVM_ROUND_LENGTH_UP_TO_PAGE_SIZE(cur_brk+prog_brk_size);
  printf("\nCurrent break point:         0x%lx\n",cur_brk);
  printf("Maximum data segment length: 0x%x\n\n",rlp.rlim_max);
  return cur_brk;
}

void TruncateLog()
{
}
