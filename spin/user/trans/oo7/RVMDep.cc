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
#include <iostream.h>

#include "utils.h"

extern "C" {
#include "rvm.h"
}


long NumMallocs;
long NumBytes;
long NumDerefs;
char *dummyptr;
char *static_addr;

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


char *LOG_FILE = "log_file";
char *OO7_DATA_FILE = "OO7_data_file";

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

rvm_tid_t *cur_tid;

void
PrintHeapInfo (char* initstr)
{
  printf("%s: Persistent Heap Info\n", initstr);
}
extern char*use_raw_device;


void
GenDBInitialize(char *configFileName)
{ 
    int err;
    rvm_return_t retval;
    rvm_options_t *options;
    rvm_length_t cur_brk;
    unsigned long heapSize;
    //long j, numPagesInSegment;

    if (use_raw_device) {
	LOG_FILE = use_raw_device;
	/* OO7_DATA_FILE = "/dev/rrz3f"; */
    }
    printf("log file=%s, data file=%s.\n", LOG_FILE, OO7_DATA_FILE);

    // perform basic initialization stuff
    RVMInitialize();

    SetOptions(LOG_FILE);
    
    cur_brk = show_break(256*RVM_PAGE_SIZE);

    cur_tid = rvm_malloc_tid();
    BeginTransaction(cur_tid);
    
    static_addr = LoadHeap(OO7_DATA_FILE);
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
  // terminate rvm 
  Terminate();
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

  // perform basic initialization stuff
    if (use_raw_device) {
	LOG_FILE = use_raw_device;
	/* OO7_DATA_FILE = "/dev/rrz3f"; */
    }
    printf("log file=%s, data file=%s.\n", LOG_FILE, OO7_DATA_FILE);
  RVMInitialize();
  SetOptions(LOG_FILE);

  // get wall clock time
  gettimeofday(&startWallTime, &ignoreTimeZone);

  printf("*****time to load persistent heap*********\n");

  // get starting usage values.
  getrusage(RUSAGE_SELF, &startUsage);
  
  static_addr = LoadHeap(OO7_DATA_FILE);

  Setup_References();

  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to load persistent heap: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));

  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));
}

void
BenchFinalize()
{
  FlushLog();
  TruncateLog();
  Terminate();
}
rvm_tid_t *cur_tid;


void
Terminate()
{
  rvm_return_t retval;
  if((retval = rvm_terminate())!=RVM_SUCCESS)
    {
      cout << "error in rvm_terminate. ret = " << rvm_return(retval) << endl;
      exit(1);
    }
}
TrunacteLog()
{
}
void
FlushLog()
{
  rvm_return_t retval;
  if((retval = rvm_flush())!=RVM_SUCCESS)
    {
      cout << "error in rvm_flush. ret = " << rvm_return(retval) << endl;
      exit(1);
    }
}
void
SetRange(rvm_tid_t *tid, char *addr, rvm_length_t len)
{
  rvm_return_t retval;
  if((retval = rvm_set_range(tid, addr, len))!=RVM_SUCCESS)
    {
      cout << "error in setrange. retval = " 
	<< rvm_return(retval) << endl;
      exit(1);
    }
}
