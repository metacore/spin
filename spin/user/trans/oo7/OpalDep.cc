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

#include <fstream.h>

#include "threads/ThreadPackage.h"
#include "opal/Opal.h"
#include "misc/MemoryPool.h"

#ifdef LOG
#include "Logger.h"
#endif

MemoryPool *__savedMP;
MemoryPool *__newMP;
SegRef *DBSegRef;
MemoryPool *DBPool;
SegRef *TempDBSegRef;
MemoryPool *TempDBPool;
Address DBAddr;

long NumMallocs;
long NumBytes;
long NumDerefs;
char *dummyptr;
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

// the function IncrementDerefCount  counts (or will eventually count) 
// the total number of dereferences made to all objects in the program
void
IncrementDerefCount()
{
  ++NumDerefs;
}

void
PrintHeapInfo(char* initstr)
{
  int HighWater;
  printf("%s: Persistent Heap Info\n", initstr);
  DBPool->PrintSummary(); fflush(stdout);
//  DBPool->PrintStats(); fflush(stdout);
//  printf("****** Total number of mallocs in program = %ld******\n", NumMallocs);
//  printf("****** Total size of mallocs in program = %ld Bytes******\n", NumBytes);
//  printf("****size of struct mhead**** = %d\n", sizeof(struct mhead));
//  printf("****total overhead of struct mhead***** = %ld\n", 
//	 NumMallocs*sizeof(struct mhead));
  printf("%s: Temporary heap Info\n", initstr); 
  TempDBPool->PrintSummary(); fflush(stdout);
}

// the functions MyPushMemoryPool and MyPopMemoryPool currently do
// nothing. this is because there is only one segment (memorypool) in 
// the current version of opal and the thread's memory pool is set in
// GENDB_INITALIZE. 
// Later , when we have multiple segments we will need to push & pop 
// the appropriate memory pools based on the pointer passed in.

int
MyPushMemoryPool(int sz, void *p)
{
  if(printHeap) {
	NumMallocs++;
	NumBytes += sz;
  }
  return 0;
}

void
MyPopMemoryPool()
{
}

MemoryPool * 
MyMemoryPoolInit(Address start, Address end) 
{ 
  MemoryPool *ds; 
  ds = (MemoryPool *)start; 
  ds->Initialize(start + 
      ((sizeof(*ds) + sizeof(Address)-1) & ~(sizeof(Address)-1)), end); 
  return(ds); 
} 


void
GenDBInitialize()
{  
  Compute_Database_Sizes();
  int numMemPools = (int) (((TotalDBSize*4)/MemoryPoolSize) + 1);

  DBSegRef = opal->NewSegment(numMemPools*MemoryPoolSize); 
  Address fa = DBSegRef->FirstAddress(); 
  Address la = DBSegRef->LastAddress(); 
  DBPool = MyMemoryPoolInit(fa, la); 
  if (PostedError()) { 
    PrintOpalError("Failed MemoryPool create"); 
    exit(1); 
  }
  //DBPool->PackedModeOn(); 
  cout << "Created All MemoryPools...\n"; 
  cout.flush(); 
  dummyptr = new char;

  TempDBSegRef = opal->NewSegment(((numMemPools/4)+1)*MemoryPoolSize); 
  fa = TempDBSegRef->FirstAddress(); 
  la = TempDBSegRef->LastAddress(); 
  TempDBPool = MyMemoryPoolInit(fa, la); 
  if (PostedError()) { 
    PrintOpalError("Failed Temp MemoryPool create"); 
    exit(1); 
  }
//  TempDBPool->PackedModeOn();

  __savedMP = GetMemoryPool();
  thisthread->SetMemoryPool(DBPool); 
}


void
GenDBSetRoot()
{
  DBPool->SetRoot( (untyped)DBRoot);
}

void
SwitchToTemp()
{
  thisthread->SetMemoryPool(TempDBPool);
}

void

SwitchBackToOriginal()
{
  thisthread->SetMemoryPool(DBPool);
}

void
GenDBFinalize()
{
  thisthread->SetMemoryPool(__savedMP);
  DBSegRef->RefPassive(); 
  DBSegRef->Force(DBSegRef->FirstAddress(), DBSegRef->LastAddress());
  char buffer[100];
  FILE* addressFile = fopen("OO7.address", "w");
  if(!addressFile)
    {
      cout << "unable to open file OO7.address for writing\n";
      cout.flush();
      exit(1);
    }
  DBSegRef->Publish();
  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to generate database: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));
  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));

  cout.flush();
  fprintf(addressFile, "%ld\n", (Address)DBRoot);
  fclose(addressFile);
  PrintHeapInfo("GenDBFinalize");
}

void
GetAddresses(char *addressFileName)
{
  char buf[100];
  char *c;
  FILE *addressFile;

  addressFile = fopen(addressFileName, "r");

  if(!addressFile)
    {
      cout << "unable to open address file " << addressFileName << "\n" ;
      exit(1);
    }
  fscanf(addressFile, "%s\n", &buf);
  cout << "buffer is " << buf ;
  DBAddr = strtol(buf, &c, 10);
  if (*c != (char)0) {
    cerr << "Not a hex number.\n";
    exit(1);
  }
}

void
ResolveSegmentAddresses()
{
//  cout << "Resolving address to DB MemoryPool (" 
//    << hex(DBAddr) << ")...\n";
//  cout.flush();
  DBSegRef = opal->ResolveAddress(DBAddr);
  if (PostedError()) {
    PrintOpalError("Cannot resolve DB address");
    exit(1);
  }
}

void
AttachSegments()
{
//  cout << "Attaching design lib MemoryPool..\n";
//  cout.flush();
  opal->Attach(DBSegRef);
  if (PostedError()) {
    PrintOpalError("Failed segment attach");
    exit(1);
  }
//  cout << "Segment attached at starting address = " << hex(DBSegRef->FirstAddress()) << " and ending address = "
//	<< hex(DBSegRef->LastAddress()) << " \n";
//  cout.flush();
  DBRoot = (DBPoolRoot*)DBAddr;

}

void
GetSetsAndIndexes()
{
  DesignLib = (GenVHSet<CompositePart *> *)DBRoot->DesignLib;
  AtomicDB = (GenVHSet<AtomicPart *> *)DBRoot->AtomicDB;

  DocumentDB = (GenVHSet<Document *> *)DBRoot->DocumentDB;

  DesignLibIdIndex = (GenAVLIndex<int, CompositePart *> *)
    DBRoot->DesignLibIdIndex;
  AtomicIdIndex = (GenAVLIndex<int, AtomicPart *> *)DBRoot->AtomicIdIndex;
  AtomicDateIndex = (GenAVLIndex<int, AtomicPart *> *)
    DBRoot->AtomicDateIndex;

  DocumentIdIndex = (GenAVLIndex<int, Document *> *)DBRoot->DocumentIdIndex;
  DocumentTitleIndex = (GenAVLIndex<char *, Document *> *)
    DBRoot->DocumentTitleIndex;

  BaseAssemblyDB = (GenVHSet<BaseAssembly *> *)DBRoot->BaseAssemblyDB;
  BaseAssemblyIdIndex = (GenAVLIndex<int, BaseAssembly *> *)
    DBRoot->BaseAssemblyIdIndex;
  ModuleDB = (GenVHSet<Module *> *)DBRoot->ModuleDB;
  ModuleIdIndex = (GenAVLIndex<int, Module *> *)DBRoot->ModuleIdIndex;
  private_cp = (GenList<BaseAssembly *> *)DBRoot->private_cp;
  shared_cp = (GenList<BaseAssembly *> *)DBRoot->shared_cp;
}

void
Open_Database(char *arg)
{
  GetAddresses(arg);
}

void
Setup_References()
{
  ResolveSegmentAddresses();
  AttachSegments();
  GetSetsAndIndexes();
}

void
BenchInitialize()
{  
  DBPool = (MemoryPool *) DBSegRef->FirstAddress();
  //DBPool->PackedModeOn();

  Compute_Database_Sizes(); // used to decide the size of the temp memorypool.

  int numMemPools = (int) (((TotalDBSize*4)/MemoryPoolSize) + 1);

  TempDBSegRef = opal->NewSegment(((numMemPools/4)+1)*MemoryPoolSize); 
  Address fa = TempDBSegRef->FirstAddress(); 
  Address la = TempDBSegRef->LastAddress(); 
  TempDBPool = MyMemoryPoolInit(fa, la); 
  if (PostedError()) { 
    PrintOpalError("Failed Temp MemoryPool create"); 
    exit(1); 
  }

  __savedMP = GetMemoryPool();
  thisthread->SetMemoryPool(DBPool); 
}

void *
search(void *key, long *base, int numObjs)
{
  // running time is O(n) where n is the number of objects in array.
  int i;
  for(i=0;i<numObjs;i++)
    if((long)key == base[i])
      return (void *)base[i];
  return 0; // not found
}

void *
binary_search(long key, long *base, int first, int last)
{
  int mid;
  assert(first<=last);
  if(first==last)
    {
      if(key == base[first])
	return (void *)&base[first];
      else return 0;
    }
  else if((last-first)==1)
    {
      if(key == base[first])
	return (void *)&base[first];
      else if(key == base[last])
	return (void *)&base[last];
      else
	return 0;
    }
  else
    {
      mid = (last+first)/2;
      if(key < base[mid])
	return binary_search(key, base, first, mid);
      else if(key == base[mid])
	return (void *)&base[mid];
      else
	return binary_search(key, base, mid, last);
    }
  
}

void *
vtop_callback(void *va)
{
  return va;
}

int
size_callback(void *va)
{
  return 40; // test value. 
}

void
BenchFinalize()
{
#ifdef LOG
  // do vtop logging
  MainLoggerVtoPLogging(vtop_callback, size_callback);
  // do some cleanup in the logger.
  //MainLoggerTerminate();
#endif

  thisthread->SetMemoryPool(__savedMP);
  // make sure  that changes to the database are written  out.
  DBSegRef->RefPassive(); 
  DBSegRef->Force(DBSegRef->FirstAddress(), DBSegRef->LastAddress());

  gettimeofday(&endWallTime, &ignoreTimeZone);
  printf("Wall-Clock time to close persistent store: %f seconds.\n",
	 ComputeWallClockTime(&startWallTime, &endWallTime));
  getrusage(RUSAGE_SELF, &endUsage);
  printf("System time: %f seconds, User time: %f seconds\n", 
	 ComputeSystemTime(&startUsage, &endUsage),
	 ComputeUserTime(&startUsage, &endUsage));

  cout.flush();


#ifdef COUNT_DEREFS  
  printf("*** Total number of dereferences in the program = %ld***\n", NumDerefs);
#endif
}
